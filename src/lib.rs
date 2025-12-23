use indexmap::{Equivalent, IndexSet};
use itertools::Itertools;
use serde::ser::Error as _;
use serde::{
    ser::{
        SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant, SerializeTuple,
        SerializeTupleStruct, SerializeTupleVariant,
    },
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{cell::Cell, fmt::Debug, hash::Hash, marker::PhantomData, ops::Index, sync::LazyLock};
use thiserror::Error;

const ANONYMOUS_ALPHABET: &[u8; 16] = b"abcdefghijklmnop";
static ANONYMOUS_NAMES: LazyLock<&str> = LazyLock::new(|| {
    let mut string = String::with_capacity(16 + (256 - 16) * 2 + (4096 - 256 - 16) * 3);
    for mut i_anonymous in 0..4096 {
        while i_anonymous > 0 {
            string.push(ANONYMOUS_ALPHABET[i_anonymous & 0xf] as char);
            i_anonymous >>= 4;
        }
    }
    string.leak()
});

pub fn anonymous_name(i_name: u32) -> &'static str {
    let buffer = *ANONYMOUS_NAMES;
    let i_name = usize::try_from(i_name).expect("usize is at least 32 bits");
    match i_name {
        0..16 => &buffer[i_name..][..1],
        16..256 => &buffer[16 + i_name * 2..][..2],
        256..4096 => &buffer[16 + 256 * 2 + i_name * 3..][..3],
        4096.. => unreachable!("too many anonymous names required: {i_name}"),
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Serialize, Deserialize)]
enum SchemaBuilder {
    Bool,

    I8,
    I16,
    I32,
    I64,
    I128,

    U8,
    U16,
    U32,
    U64,
    U128,

    F32,
    F64,
    Char,

    String,
    Bytes,

    Skip,
    OptionNone,
    OptionSome(Box<SchemaBuilder>),

    Unit(Option<TypeName>),
    Newtype(TypeName, Box<SchemaBuilder>),

    Map(Box<SchemaBuilder>, Box<SchemaBuilder>),
    Sequence(Box<SchemaBuilder>),

    Union(Vec<SchemaBuilder>),

    /// Tuple, tuple struct, tuple variant, struct or struct variant.
    Record {
        name: Option<TypeName>,
        field_names: Option<NameListIndex>,
        field_types: Vec<SchemaBuilder>,
        length: u32,
    },
}

impl SchemaBuilder {
    fn unify(&mut self, other: Self) -> Result<(), Self> {
        match (&mut *self, other) {
            (SchemaBuilder::Union(lefts), right) => {
                if lefts.is_empty() {
                    *self = right;
                } else {
                    right.add_to_nonempty_union(lefts);
                }
                Ok(())
            }
            (left, mut right @ SchemaBuilder::Union(_)) => {
                std::mem::swap(left, &mut right);
                left.unify(right)
            }
            (
                SchemaBuilder::Newtype(left_name, left_inner),
                SchemaBuilder::Newtype(right_name, right_inner),
            ) => {
                if *left_name == right_name {
                    left_inner.union(*right_inner);
                    Ok(())
                } else {
                    Err(SchemaBuilder::Newtype(right_name, right_inner))
                }
            }
            (SchemaBuilder::OptionSome(left), SchemaBuilder::OptionSome(right)) => {
                left.union(*right);
                Ok(())
            }
            (
                SchemaBuilder::Record {
                    name: left_name,
                    field_names: left_field_names,
                    field_types: left_field_types,
                    length: left_length,
                },
                SchemaBuilder::Record {
                    name: right_name,
                    field_names: right_field_names,
                    field_types: right_field_types,
                    length: right_length,
                },
            ) => {
                if (*left_name, *left_field_names, *left_length)
                    == (right_name, right_field_names, right_length)
                {
                    left_field_types
                        .iter_mut()
                        .zip(right_field_types)
                        .for_each(|(left, right)| left.union(right));
                    Ok(())
                } else {
                    Err(SchemaBuilder::Record {
                        name: right_name,
                        field_names: right_field_names,
                        field_types: right_field_types,
                        length: right_length,
                    })
                }
            }
            (
                SchemaBuilder::Map(left_keys, left_values),
                SchemaBuilder::Map(right_keys, right_values),
            ) => {
                left_keys.union(*right_keys);
                left_values.union(*right_values);
                Ok(())
            }
            (SchemaBuilder::Sequence(left), SchemaBuilder::Sequence(right)) => {
                left.union(*right);
                Ok(())
            }
            (left, right) => {
                if *left == right {
                    Ok(())
                } else {
                    Err(right)
                }
            }
        }
    }

    fn union(&mut self, other: Self) {
        if let Err(other) = self.unify(other) {
            let left = std::mem::take(self);
            match self {
                SchemaBuilder::Union(schemas) => *schemas = vec![left, other],
                _ => unreachable!(),
            }
        }
    }

    fn add_to_nonempty_union(self, lefts: &mut Vec<SchemaBuilder>) {
        assert!(!lefts.is_empty());
        match self {
            SchemaBuilder::Union(rights) => {
                rights
                    .into_iter()
                    .for_each(|right| right.add_to_nonempty_union(lefts));
            }
            right => {
                let right = lefts
                    .iter_mut()
                    .try_fold(right, |right, left| match left.unify(right) {
                        Ok(()) => Err(()),
                        Err(recovered) => Ok(recovered),
                    })
                    .ok();
                lefts.extend(right);
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
struct TypeName(NameIndex, Option<NameIndex>);

impl Default for SchemaBuilder {
    fn default() -> Self {
        Self::Union(Vec::new())
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Schema {
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,

    U8,
    U16,
    U32,
    U64,
    U128,

    F32,
    F64,
    Char,

    String,
    Bytes,

    OptionNone,
    OptionSome(SchemaIndex),

    Unit,
    UnitStruct(NameIndex),
    UnitVariant(NameIndex, NameIndex),

    NewtypeStruct(NameIndex, SchemaIndex),
    NewtypeVariant(NameIndex, NameIndex, SchemaIndex),

    Sequence(SchemaIndex),
    Map(SchemaIndex, SchemaIndex),

    Tuple(u32, SchemaListIndex),
    TupleStruct(NameIndex, u32, SchemaListIndex),
    TupleVariant(NameIndex, NameIndex, u32, SchemaListIndex),

    Struct(NameIndex, NameListIndex, SchemaListIndex),
    StructVariant(NameIndex, NameIndex, NameListIndex, SchemaListIndex),

    Union(SchemaListIndex),
    Skip,
}

macro_rules! u32_indices {
    ($($index_ty:ident => $error:ident,)+) => {
        $(
            #[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $index_ty(nonmax::NonMaxU32);

            impl $index_ty {
                pub const RESERVED: $index_ty = $index_ty(nonmax::NonMaxU32::new(u32::MAX - 1).unwrap());
            }

            impl From<$index_ty> for u32 {
                fn from(index: $index_ty) -> u32 {
                    index.0.into()
                }
            }

            impl From<$index_ty> for usize {
                fn from(index: $index_ty) -> usize {
                    usize::try_from(u32::from(index.0)).expect("usize must be at least 32-bits")
                }
            }

            impl TryFrom<usize> for $index_ty {
                type Error = SerError;

                fn try_from(value: usize) -> Result<Self, Self::Error> {
                    u32::try_from(value)
                        .ok()
                        .and_then(nonmax::NonMaxU32::new)
                        .map($index_ty)
                        .ok_or(SerError::$error)
                }
            }

            impl TryFrom<u32> for $index_ty {
                type Error = SerError;

                fn try_from(value: u32) -> Result<Self, Self::Error> {
                    nonmax::NonMaxU32::new(value)
                        .map($index_ty)
                        .ok_or(SerError::$error)
                }
            }
        )+
    };
}
u32_indices! {
    SchemaIndex => TooManySchemas,
    SchemaListIndex => TooManySchemaLists,
    NameIndex => TooManyNames,
    NameListIndex => TooManyNameLists,
    TraceIndex => TooManyValues,
}

pub fn to_value<SerializeT>(value: &SerializeT) -> Result<Value, SerError>
where
    SerializeT: ?Sized + Serialize,
{
    let mut serializer = RootSchemaBuilder::default();
    let root = SerializeT::serialize(value, &mut serializer)?;
    serializer.root = root;
    serializer.into_value()
}

#[derive(Serialize, Debug, Clone)]
pub struct Pool<ValueT, ValueIndexT> {
    pub inner: IndexSet<ValueT>,

    #[serde(skip)]
    pub _dummy: PhantomData<ValueIndexT>,
}

impl<ValueT, ValueIndexT> IntoIterator for Pool<ValueT, ValueIndexT> {
    type IntoIter = <IndexSet<ValueT> as IntoIterator>::IntoIter;
    type Item = ValueT;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<ValueT, ValueIndexT> Default for Pool<ValueT, ValueIndexT> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
            _dummy: PhantomData,
        }
    }
}

impl<ValueT, ValueIndexT> Index<ValueIndexT> for Pool<ValueT, ValueIndexT>
where
    usize: From<ValueIndexT>,
{
    type Output = ValueT;

    fn index(&self, index: ValueIndexT) -> &Self::Output {
        self.inner
            .get_index(usize::from(index))
            .expect("no such index in pool")
    }
}

impl<ValueT, ValueIndexT> Pool<ValueT, ValueIndexT>
where
    ValueT: Hash + Eq,
    ValueIndexT: TryFrom<usize, Error = SerError>,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, value: ValueT) -> Result<ValueIndexT, SerError> {
        ValueIndexT::try_from(self.inner.insert_full(value).0)
    }

    pub fn intern_from<FromT>(&mut self, value: FromT) -> Result<ValueIndexT, SerError>
    where
        ValueT: From<FromT>,
    {
        ValueIndexT::try_from(self.inner.insert_full(value.into()).0)
    }

    pub fn intern_borrowed<'s, 'q, QueryT>(
        &'s mut self,
        query: &'q QueryT,
    ) -> Result<ValueIndexT, SerError>
    where
        QueryT: ?Sized + Hash + Equivalent<ValueT>,
        ValueT: From<&'q QueryT>,
    {
        let index = match self.inner.get_full(query) {
            Some((index, _)) => index,
            None => self.inner.insert_full(query.into()).0,
        };
        ValueIndexT::try_from(index)
    }
}

pub fn zigzag64_encode(value: i64) -> u64 {
    ((value << 1) ^ (value >> 63)) as u64
}

pub fn zigzag64_decode(value: u64) -> i64 {
    (value >> 1) as i64 ^ (-((value & 1) as i64))
}

pub fn zigzag128_encode(value: i128) -> u128 {
    ((value << 1) ^ (value >> 127)) as u128
}

pub fn zigzag128_decode(value: u128) -> i128 {
    (value >> 1) as i128 ^ (-((value & 1) as i128))
}

#[derive(Debug, Error)]
pub enum SerError {
    #[error("too many nodes for u32")]
    TooManySchemas,

    #[error("too many node lists for u32")]
    TooManySchemaLists,

    #[error("too many structs for u32")]
    TooManyNames,

    #[error("too many field lists for u32")]
    TooManyNameLists,

    #[error("too many byte strings for u32")]
    TooManyByteStrings,

    #[error("too many values for u32")]
    TooManyValues,

    #[error("custom: {0}")]
    Custom(String),
}

impl serde::ser::Error for SerError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        SerError::Custom(msg.to_string())
    }
}

#[derive(Default, Clone, Serialize)]
struct RootSchemaBuilder {
    data: Vec<u8>,
    names: Pool<&'static str, NameIndex>,
    name_lists: Pool<Box<[NameIndex]>, NameListIndex>,
    schemas: Pool<Schema, SchemaIndex>,
    schema_lists: Pool<Box<[SchemaIndex]>, SchemaListIndex>,
    root: SchemaBuilder,
}

#[derive(Default, Clone, Serialize)]
pub struct ValueData(Vec<u8>);

#[derive(Clone)]
pub struct Value {
    pub schema: RootSchema,
    pub root_index: SchemaIndex,
    pub data: ValueData,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct RootSchema {
    schemas: Box<[Schema]>,
    names: Box<[Box<str>]>,
    name_lists: Box<[Box<[NameIndex]>]>,
    schema_lists: Box<[Box<[SchemaIndex]>]>,
}

impl RootSchema {
    fn name(&self, index: NameIndex) -> &str {
        &self.names[usize::from(index)]
    }

    fn name_list(&self, index: NameListIndex) -> &[NameIndex] {
        &self.name_lists[usize::from(index)]
    }

    fn schema(&self, index: SchemaIndex) -> Schema {
        self.schemas[usize::from(index)]
    }

    fn schema_list(&self, index: SchemaListIndex) -> &[SchemaIndex] {
        &self.schema_lists[usize::from(index)]
    }

    fn dump(&self, indent: &mut String, index: SchemaIndex) {
        indent.push_str("  ");
        let schema = self.schema(index);
        match schema {
            Schema::Bool
            | Schema::I8
            | Schema::I16
            | Schema::I32
            | Schema::I64
            | Schema::I128
            | Schema::U8
            | Schema::U16
            | Schema::U32
            | Schema::U64
            | Schema::U128
            | Schema::F32
            | Schema::F64
            | Schema::Char
            | Schema::String
            | Schema::Bytes
            | Schema::Unit
            | Schema::OptionNone
            | Schema::Skip => eprintln!("{indent}{schema:?},"),

            Schema::UnitStruct(name) => eprintln!("{}{},", indent, self.name(name)),
            Schema::UnitVariant(name, variant) => {
                eprintln!("{}{}::{},", indent, self.name(name), self.name(variant))
            }

            Schema::OptionSome(inner) => {
                eprintln!("{}::Option::Some(", indent);
                self.dump(indent, inner);
                eprintln!("{indent}),")
            }
            Schema::NewtypeStruct(name, inner) => {
                eprintln!("{}{}(", indent, self.name(name));
                self.dump(indent, inner);
                eprintln!("{indent}),")
            }
            Schema::NewtypeVariant(name, variant, inner) => {
                eprintln!("{}{}::{}(", indent, self.name(name), self.name(variant));
                self.dump(indent, inner);
                eprintln!("{indent}),")
            }
            Schema::Map(key, value) => {
                eprintln!("{indent}{{");
                self.dump(indent, key);
                self.dump(indent, value);
                eprintln!("{indent}}},")
            }
            Schema::Sequence(item) => {
                eprintln!("{indent}[");
                self.dump(indent, item);
                eprintln!("{indent}],")
            }

            Schema::Tuple(_, schema_list) => {
                eprintln!("{indent}(");
                for &schema in self.schema_list(schema_list) {
                    self.dump(indent, schema);
                }
                eprintln!("{indent}),")
            }

            Schema::TupleStruct(name, _, schema_list) => {
                eprintln!("{}{}(", indent, self.name(name));
                for &schema in self.schema_list(schema_list) {
                    self.dump(indent, schema);
                }
                eprintln!("{indent}),")
            }
            Schema::TupleVariant(name, variant, _, schema_list) => {
                eprintln!("{}{}::{}(", indent, self.name(name), self.name(variant));
                for &schema in self.schema_list(schema_list) {
                    self.dump(indent, schema);
                }
                eprintln!("{indent}),")
            }

            Schema::Struct(name, name_list, type_list) => {
                eprintln!("{}{} {{", indent, self.name(name));
                indent.push_str("  ");
                for (&name, &schema) in self
                    .name_list(name_list)
                    .iter()
                    .zip(self.schema_list(type_list))
                {
                    eprintln!("{}{}:", indent, self.name(name));
                    self.dump(indent, schema);
                }
                indent.truncate(indent.len() - 2);
                eprintln!("{indent}}},")
            }
            Schema::StructVariant(name, variant, name_list, type_list) => {
                eprintln!("{}{}::{} {{", indent, self.name(name), self.name(variant));
                indent.push_str("  ");
                for (&name, &schema) in self
                    .name_list(name_list)
                    .iter()
                    .zip(self.schema_list(type_list))
                {
                    eprintln!("{}{}:", indent, self.name(name));
                    self.dump(indent, schema);
                }
                indent.truncate(indent.len() - 2);
                eprintln!("{indent}}},")
            }

            Schema::Union(type_list) => {
                eprintln!("{indent}<");
                indent.push_str("  ");
                for &schema in self.schema_list(type_list) {
                    self.dump(indent, schema);
                }
                indent.truncate(indent.len() - 2);
                eprintln!("{indent}>,")
            }
        }
        indent.truncate(indent.len() - 2);
    }
}

impl SchemaBuilder {
    fn build(self, root: &mut RootSchemaBuilder) -> Result<SchemaIndex, SerError> {
        let schema = match self {
            SchemaBuilder::Bool => Schema::Bool,
            SchemaBuilder::I8 => Schema::I8,
            SchemaBuilder::I16 => Schema::I16,
            SchemaBuilder::I32 => Schema::I32,
            SchemaBuilder::I64 => Schema::I64,
            SchemaBuilder::I128 => Schema::I128,

            SchemaBuilder::U8 => Schema::U8,
            SchemaBuilder::U16 => Schema::U16,
            SchemaBuilder::U32 => Schema::U32,
            SchemaBuilder::U64 => Schema::U64,
            SchemaBuilder::U128 => Schema::U128,

            SchemaBuilder::F32 => Schema::F32,
            SchemaBuilder::F64 => Schema::F64,
            SchemaBuilder::Char => Schema::Char,

            SchemaBuilder::String => Schema::String,
            SchemaBuilder::Bytes => Schema::Bytes,

            SchemaBuilder::Skip => Schema::Skip,
            SchemaBuilder::OptionNone => Schema::OptionNone,
            SchemaBuilder::OptionSome(inner) => {
                let inner = inner.build(root)?;
                Schema::OptionSome(inner)
            }
            SchemaBuilder::Unit(None) => Schema::Unit,
            SchemaBuilder::Unit(Some(TypeName(name, None))) => Schema::UnitStruct(name),
            SchemaBuilder::Unit(Some(TypeName(name, Some(variant)))) => {
                Schema::UnitVariant(name, variant)
            }
            SchemaBuilder::Newtype(type_name, inner) => {
                let inner = inner.build(root)?;
                match type_name {
                    TypeName(name, None) => Schema::NewtypeStruct(name, inner),
                    TypeName(name, Some(variant)) => Schema::NewtypeVariant(name, variant, inner),
                }
            }
            SchemaBuilder::Map(key, value) => Schema::Map(key.build(root)?, value.build(root)?),
            SchemaBuilder::Sequence(item) => Schema::Sequence(item.build(root)?),
            SchemaBuilder::Union(schemas) => {
                let mut schemas = schemas
                    .into_iter()
                    .map(|schema| schema.build(root))
                    .collect::<Result<Vec<_>, _>>()?;
                schemas.sort_unstable();
                schemas.dedup();
                Schema::Union(root.schema_lists.intern_from(schemas)?)
            }
            SchemaBuilder::Record {
                name,
                field_names,
                field_types,
                length,
            } => {
                let field_types = field_types
                    .into_iter()
                    .map(|schema| schema.build(root))
                    .collect::<Result<Vec<_>, _>>()?;
                let field_types = root.schema_lists.intern_from(field_types)?;
                match (name, field_names) {
                    (None, None) => Schema::Tuple(length, field_types),
                    (Some(TypeName(name, None)), None) => {
                        Schema::TupleStruct(name, length, field_types)
                    }
                    (Some(TypeName(name, Some(variant))), None) => {
                        Schema::TupleVariant(name, variant, length, field_types)
                    }
                    (None, Some(_field_names)) => {
                        unreachable!("anonymous structs don't exist in rust!")
                    }
                    (Some(TypeName(name, None)), Some(field_names)) => {
                        Schema::Struct(name, field_names, field_types)
                    }
                    (Some(TypeName(name, Some(variant))), Some(field_names)) => {
                        Schema::StructVariant(name, variant, field_names, field_types)
                    }
                }
            }
        };
        root.schemas.intern(schema)
    }
}

impl RootSchemaBuilder {
    fn into_value(mut self) -> Result<Value, SerError> {
        let root = std::mem::take(&mut self.root);
        let root_index = root.build(&mut self)?;
        let schema = RootSchema {
            schemas: self.schemas.into_iter().collect::<Vec<_>>().into(),
            names: self
                .names
                .into_iter()
                .map(Into::into)
                .collect::<Vec<_>>()
                .into(),
            name_lists: self.name_lists.into_iter().collect::<Vec<_>>().into(),
            schema_lists: self.schema_lists.into_iter().collect::<Vec<_>>().into(),
        };
        schema.dump(&mut String::new(), root_index);
        Ok(Value {
            schema,
            root_index,
            data: ValueData(self.data),
        })
    }

    fn push_struct_name(&mut self, name: &'static str) -> Result<TypeName, SerError> {
        let name = self.names.intern(name)?;
        self.push_u32(name.into());
        Ok(TypeName(name, None))
    }

    fn push_variant_name(
        &mut self,
        name: &'static str,
        variant: &'static str,
    ) -> Result<TypeName, SerError> {
        let name = self.names.intern(name)?;
        let variant = self.names.intern(variant)?;
        self.push_u32(name.into());
        self.push_u32(variant.into());
        Ok(TypeName(name, Some(variant)))
    }

    fn intern_field_name(&mut self, name: &'static str) -> Result<NameIndex, SerError> {
        self.names.intern(name)
    }

    fn fill_reserved_field_name_list<NameListT>(
        &mut self,
        index: TraceIndex,
        names: NameListT,
    ) -> Result<NameListIndex, SerError>
    where
        Box<[NameIndex]>: From<NameListT>,
    {
        let names = self.name_lists.intern_from(names)?;
        self.fill_reserved_bytes(index, &u32::from(names).to_le_bytes());
        Ok(names)
    }

    fn push_u32(&mut self, integer: u32) {
        self.data.extend(integer.to_le_bytes());
    }

    fn push_u32_length(&mut self, length: usize) -> Result<(), SerError> {
        self.data.extend(
            u32::try_from(length)
                .map_err(|_| SerError::TooManyValues)?
                .to_le_bytes(),
        );
        Ok(())
    }

    fn push_trace(&mut self, trace: Trace) {
        self.data.push(trace.into());
    }

    fn reserve_u32(&mut self) -> Result<TraceIndex, SerError> {
        self.reserve_bytes(std::mem::size_of::<u32>())
    }

    fn reserve_bytes(&mut self, size: usize) -> Result<TraceIndex, SerError> {
        let index = TraceIndex::try_from(self.data.len())?;
        self.data.extend(std::iter::repeat_n(!0, size));
        Ok(index)
    }

    fn push_length_bytes(&mut self, bytes: &[u8]) -> Result<(), SerError> {
        self.push_u32_length(bytes.len())?;
        self.data.extend(bytes);
        Ok(())
    }

    fn fill_reserved_bytes(&mut self, index: TraceIndex, data: &[u8]) {
        self.data[index.into()..][..data.len()].copy_from_slice(data);
    }
}

macro_rules! fn_serialize_as_u8 {
    ($(($fn_name:ident, $value_type:ty, $schema:ident),)+) => {
        $(
            fn $fn_name(self, value: $value_type) -> Result<Self::Ok, Self::Error> {
                self.push_trace(Trace::$schema);
                self.data.push(value as u8);
                Ok(SchemaBuilder::$schema)
            }
        )+
    };
}

macro_rules! fn_serialize_as_le_bytes {
    ($(($fn_name:ident, $value_type:ty, $schema:ident ),)+) => {
        $(
            fn $fn_name(self, value: $value_type) -> Result<Self::Ok, Self::Error> {

                self.push_trace(Trace::$schema);
                self.data.extend_from_slice(&value.to_le_bytes());
                Ok(SchemaBuilder::$schema)
            }
        )+
    };
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize)]
#[repr(u8)]
pub enum Trace {
    Bool = 0,

    I8,
    I16,
    I32,
    I64,
    I128,

    U8,
    U16,
    U32,
    U64,
    U128,

    F32,
    F64,
    Char,

    String,
    Bytes,

    Skip,
    OptionNone,
    OptionSome,

    Unit,
    UnitStruct,
    UnitVariant,

    NewtypeStruct,
    NewtypeVariant,

    Map,
    Sequence,

    Tuple,
    TupleStruct,
    TupleVariant,

    Struct,
    StructVariant,
}

impl Trace {
    const ALL: [Self; 31] = [
        Self::Bool,
        Self::I8,
        Self::I16,
        Self::I32,
        Self::I64,
        Self::I128,
        Self::U8,
        Self::U16,
        Self::U32,
        Self::U64,
        Self::U128,
        Self::F32,
        Self::F64,
        Self::Char,
        Self::String,
        Self::Bytes,
        Self::Skip,
        Self::OptionNone,
        Self::OptionSome,
        Self::Unit,
        Self::UnitStruct,
        Self::UnitVariant,
        Self::NewtypeStruct,
        Self::NewtypeVariant,
        Self::Map,
        Self::Sequence,
        Self::Tuple,
        Self::TupleStruct,
        Self::TupleVariant,
        Self::Struct,
        Self::StructVariant,
    ];
}

impl TryFrom<u8> for Trace {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::ALL.get(usize::from(value)).copied().ok_or(value)
    }
}

impl From<Trace> for u8 {
    fn from(value: Trace) -> Self {
        value as Self
    }
}

impl<'a> Serializer for &'a mut RootSchemaBuilder {
    type Ok = SchemaBuilder;
    type Error = SerError;

    type SerializeSeq = SequenceSchemaBuilder<'a>;
    type SerializeTuple = TupleSchemaBuilder<'a>;
    type SerializeTupleStruct = TupleSchemaBuilder<'a>;
    type SerializeTupleVariant = TupleSchemaBuilder<'a>;
    type SerializeMap = MapSchemaBuilder<'a>;
    type SerializeStruct = StructSchemaBuilder<'a>;
    type SerializeStructVariant = StructSchemaBuilder<'a>;

    fn_serialize_as_u8! {
        (serialize_bool, bool, Bool),
        (serialize_i8, i8, I8),
        (serialize_u8, u8, U8),
    }

    fn_serialize_as_le_bytes! {
        (serialize_i16, i16, I16),
        (serialize_i32, i32, I32),
        (serialize_i64, i64, I64),
        (serialize_i128, i128, I128),
        (serialize_u16, u16, U16),
        (serialize_u32, u32, U32),
        (serialize_u64, u64, U64),
        (serialize_u128, u128, U128),
        (serialize_f32, f32, F32),
        (serialize_f64, f64, F64),
    }

    fn serialize_char(self, value: char) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::Char);
        self.push_u32(u32::from(value));
        Ok(SchemaBuilder::Char)
    }

    fn serialize_str(self, value: &str) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::String);
        self.push_length_bytes(value.as_bytes())?;
        Ok(SchemaBuilder::String)
    }

    fn serialize_bytes(self, value: &[u8]) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::Bytes);
        self.push_length_bytes(value)?;
        Ok(SchemaBuilder::Bytes)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::OptionNone);
        Ok(SchemaBuilder::OptionNone)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.push_trace(Trace::OptionSome);
        T::serialize(value, &mut *self).map(|schema| SchemaBuilder::OptionSome(Box::new(schema)))
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::Unit);
        Ok(SchemaBuilder::Unit(None))
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::UnitStruct);
        Ok(SchemaBuilder::Unit(Some(self.push_struct_name(name)?)))
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::UnitVariant);
        Ok(SchemaBuilder::Unit(Some(
            self.push_variant_name(name, variant)?,
        )))
    }

    fn serialize_newtype_struct<T>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.push_trace(Trace::NewtypeStruct);
        Ok(SchemaBuilder::Newtype(
            self.push_struct_name(name)?,
            Box::new(T::serialize(value, &mut *self)?),
        ))
    }

    fn serialize_newtype_variant<T>(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.push_trace(Trace::NewtypeVariant);
        Ok(SchemaBuilder::Newtype(
            self.push_variant_name(name, variant)?,
            Box::new(T::serialize(value, &mut *self)?),
        ))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        self.push_trace(Trace::Sequence);
        Ok(SequenceSchemaBuilder {
            reserved_length: self.reserve_u32()?,
            schema: SchemaBuilder::default(),
            length: 0,
            parent: self,
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.push_trace(Trace::Tuple);
        self.push_u32_length(len)?;
        Ok(TupleSchemaBuilder {
            name: None,
            schemas: Vec::with_capacity(len),
            parent: self,
            length: u32::try_from(len).map_err(|_| SerError::TooManyValues)?,
        })
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.push_trace(Trace::TupleStruct);
        self.push_u32_length(len)?;
        Ok(TupleSchemaBuilder {
            name: Some(self.push_struct_name(name)?),
            schemas: Vec::with_capacity(len),
            parent: self,
            length: u32::try_from(len).map_err(|_| SerError::TooManyValues)?,
        })
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.push_trace(Trace::TupleVariant);
        self.push_u32_length(len)?;
        Ok(TupleSchemaBuilder {
            name: Some(self.push_variant_name(name, variant)?),
            schemas: Vec::with_capacity(len),
            parent: self,
            length: u32::try_from(len).map_err(|_| SerError::TooManyValues)?,
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        self.push_trace(Trace::Map);
        Ok(MapSchemaBuilder {
            reserved_length: self.reserve_u32()?,
            key_schema: SchemaBuilder::default(),
            value_schema: SchemaBuilder::default(),
            length: 0,
            parent: self,
        })
    }

    fn serialize_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.push_trace(Trace::Struct);
        Ok(StructSchemaBuilder {
            name: self.push_struct_name(name)?,
            reserved_field_name_list: self.reserve_u32()?,
            field_names: Vec::with_capacity(len),
            field_types: Vec::with_capacity(len),
            parent: self,
        })
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.push_trace(Trace::StructVariant);
        Ok(StructSchemaBuilder {
            name: self.push_variant_name(name, variant)?,
            reserved_field_name_list: self.reserve_u32()?,
            field_names: Vec::with_capacity(len),
            field_types: Vec::with_capacity(len),
            parent: self,
        })
    }

    #[inline]
    fn is_human_readable(&self) -> bool {
        false
    }
}

struct SequenceSchemaBuilder<'a> {
    parent: &'a mut RootSchemaBuilder,
    reserved_length: TraceIndex,
    schema: SchemaBuilder,
    length: u32,
}

impl SerializeSeq for SequenceSchemaBuilder<'_> {
    type Ok = SchemaBuilder;
    type Error = SerError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.length += 1;
        self.schema.union(T::serialize(value, &mut *self.parent)?);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.parent
            .fill_reserved_bytes(self.reserved_length, &self.length.to_le_bytes());
        Ok(SchemaBuilder::Sequence(Box::new(self.schema)))
    }
}

struct MapSchemaBuilder<'a> {
    parent: &'a mut RootSchemaBuilder,
    reserved_length: TraceIndex,
    key_schema: SchemaBuilder,
    value_schema: SchemaBuilder,
    length: u32,
}

impl SerializeMap for MapSchemaBuilder<'_> {
    type Ok = SchemaBuilder;
    type Error = SerError;

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.length += 1;
        self.key_schema.union(T::serialize(key, &mut *self.parent)?);
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value_schema
            .union(T::serialize(value, &mut *self.parent)?);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.parent
            .fill_reserved_bytes(self.reserved_length, &self.length.to_le_bytes());
        Ok(SchemaBuilder::Map(
            Box::new(self.key_schema),
            Box::new(self.value_schema),
        ))
    }
}

struct TupleSchemaBuilder<'a> {
    name: Option<TypeName>,
    schemas: Vec<SchemaBuilder>,
    parent: &'a mut RootSchemaBuilder,
    length: u32,
}

impl SerializeTuple for TupleSchemaBuilder<'_> {
    type Ok = SchemaBuilder;
    type Error = SerError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.schemas.push(T::serialize(value, &mut *self.parent)?);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(SchemaBuilder::Record {
            name: self.name,
            field_names: None,
            field_types: self.schemas,
            length: self.length,
        })
    }
}

impl SerializeTupleStruct for TupleSchemaBuilder<'_> {
    type Ok = SchemaBuilder;
    type Error = SerError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        <Self as SerializeTuple>::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        <Self as SerializeTuple>::end(self)
    }
}

impl SerializeTupleVariant for TupleSchemaBuilder<'_> {
    type Ok = SchemaBuilder;
    type Error = SerError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        <Self as SerializeTuple>::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        <Self as SerializeTuple>::end(self)
    }
}

struct StructSchemaBuilder<'a> {
    name: TypeName,
    reserved_field_name_list: TraceIndex,
    field_names: Vec<NameIndex>,
    field_types: Vec<SchemaBuilder>,
    parent: &'a mut RootSchemaBuilder,
}

impl SerializeStruct for StructSchemaBuilder<'_> {
    type Ok = SchemaBuilder;
    type Error = SerError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.field_names.push(self.parent.intern_field_name(key)?);
        self.field_types
            .push(T::serialize(value, &mut *self.parent)?);
        Ok(())
    }

    fn skip_field(&mut self, key: &'static str) -> Result<(), Self::Error> {
        self.field_names.push(self.parent.intern_field_name(key)?);
        self.field_types.push(SchemaBuilder::Skip);
        self.parent.push_trace(Trace::Skip);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let length = u32::try_from(self.field_names.len()).map_err(|_| SerError::TooManyValues)?;
        let field_names = Some(
            self.parent
                .fill_reserved_field_name_list(self.reserved_field_name_list, self.field_names)?,
        );
        Ok(SchemaBuilder::Record {
            name: Some(self.name),
            field_names,
            field_types: self.field_types,
            length,
        })
    }
}

impl SerializeStructVariant for StructSchemaBuilder<'_> {
    type Ok = SchemaBuilder;
    type Error = SerError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        <Self as SerializeStruct>::serialize_field(self, key, value)
    }

    fn skip_field(&mut self, key: &'static str) -> Result<(), Self::Error> {
        <Self as SerializeStruct>::skip_field(self, key)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        <Self as SerializeStruct>::end(self)
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let tail = Cell::new(&*self.data.0);
        let cursor = ValueCursor::start(&self.schema, self.root_index, &tail);
        (&self.schema, self.root_index, cursor).serialize(serializer)
    }
}

#[derive(Copy, Clone)]
struct ValueCursor<'a> {
    root: &'a RootSchema,
    schema: Schema,
    trace: TraceNode,
    data: &'a [u8],
    tail: &'a Cell<&'a [u8]>,
}

#[derive(Copy, Clone)]
enum CheckResult<'a> {
    Simple,
    Discriminated(u32, ValueCursor<'a>),
}

impl Debug for CheckResult<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple => f.debug_struct("Simple").finish(),
            Self::Discriminated(discriminator, cursor) => f
                .debug_struct("Discriminated")
                .field("discriminator", &discriminator)
                .field("schema", &cursor.schema)
                .finish(),
        }
    }
}

impl<'a> ValueCursor<'a> {
    fn start(root: &'a RootSchema, schema: SchemaIndex, tail: &'a Cell<&'a [u8]>) -> Self {
        Self {
            root,
            schema: root.schema(schema),
            trace: tail.pop_trace_node(),
            tail,
            data: tail.get(),
        }
    }

    fn pop_child(&self, schema: SchemaIndex) -> Self {
        Self {
            root: self.root,
            schema: self.root.schema(schema),
            trace: self.tail.pop_trace_node(),
            data: self.tail.get(),
            tail: self.tail,
        }
    }

    fn traced_child(&self, schema: SchemaIndex, trace: TraceNode) -> Self {
        Self {
            root: self.root,
            schema: self.root.schema(schema),
            trace,
            data: self.tail.get(),
            tail: self.tail,
        }
    }

    fn serialize_inner<S>(&self, serializer: S, inner: SchemaIndex) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.pop_child(inner).serialize(serializer)
    }

    fn serialize_tuple<S>(
        &self,
        serializer: S,
        schema_length: u32,
        schema_list: SchemaListIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let schema_list = self.root.schema_list(schema_list);
        let schema_length = usize::try_from(schema_length).expect("usize at least 32-bits");
        assert_eq!(schema_list.len(), schema_length);

        let mut serializer = serializer.serialize_tuple(schema_length)?;
        for &schema in schema_list {
            serializer.serialize_element(&self.pop_child(schema))?
        }
        serializer.end()
    }

    fn serialize_map<S>(
        &self,
        serializer: S,
        length: usize,
        key: SchemaIndex,
        value: SchemaIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut serializer = serializer.serialize_map(Some(length))?;
        for _ in 0..length {
            serializer.serialize_key(&self.pop_child(key))?;
            serializer.serialize_value(&self.pop_child(value))?;
        }
        serializer.end()
    }

    fn serialize_sequence<S>(
        &self,
        serializer: S,
        length: usize,
        item: SchemaIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut serializer = serializer.serialize_seq(Some(length))?;
        for _ in 0..length {
            serializer.serialize_element(&self.pop_child(item))?;
        }
        serializer.end()
    }

    fn serialize_struct<S>(
        &self,
        serializer: S,
        name_list: NameListIndex,
        schema_list: SchemaListIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.serialize_tuple(
            serializer,
            u32::try_from(self.root.name_list(name_list).len())
                .expect("too many struct fields in trace"),
            schema_list,
        )
    }

    fn check(&self) -> Option<CheckResult<'a>> {
        let matches = match dbg!((self.trace, self.schema)) {
            (TraceNode::Bool, Schema::Bool)
            | (TraceNode::I8, Schema::I8)
            | (TraceNode::I16, Schema::I16)
            | (TraceNode::I32, Schema::I32)
            | (TraceNode::I64, Schema::I64)
            | (TraceNode::I128, Schema::I128)
            | (TraceNode::U8, Schema::U8)
            | (TraceNode::U16, Schema::U16)
            | (TraceNode::U32, Schema::U32)
            | (TraceNode::U64, Schema::U64)
            | (TraceNode::U128, Schema::U128)
            | (TraceNode::F32, Schema::F32)
            | (TraceNode::F64, Schema::F64)
            | (TraceNode::Char, Schema::Char)
            | (TraceNode::String, Schema::String)
            | (TraceNode::Bytes, Schema::Bytes)
            | (TraceNode::None, Schema::OptionNone)
            | (TraceNode::Some, Schema::OptionSome(_))
            | (TraceNode::Unit, Schema::Unit)
            | (TraceNode::Map, Schema::Map(_, _))
            | (TraceNode::Sequence, Schema::Sequence(_))
            | (TraceNode::Skip, Schema::Skip) => true,

            (TraceNode::UnitStruct(trace_name), Schema::UnitStruct(schema_name))
            | (TraceNode::NewtypeStruct(trace_name), Schema::NewtypeStruct(schema_name, _)) => {
                trace_name == schema_name
            }

            (
                TraceNode::UnitVariant(trace_name, trace_variant),
                Schema::UnitVariant(schema_name, schema_variant),
            )
            | (
                TraceNode::NewtypeVariant(trace_name, trace_variant),
                Schema::NewtypeVariant(schema_name, schema_variant, _),
            ) => (trace_name, trace_variant) == (schema_name, schema_variant),

            (TraceNode::Tuple(trace_length), Schema::Tuple(schema_length, _)) => {
                trace_length == schema_length
            }
            (
                TraceNode::TupleStruct(trace_length, trace_name),
                Schema::TupleStruct(schema_name, schema_length, _),
            ) => (trace_length, trace_name) == (schema_length, schema_name),
            (
                TraceNode::TupleVariant(trace_length, trace_name, trace_variant),
                Schema::TupleVariant(schema_name, schema_variant, schema_length, _),
            ) => {
                (trace_length, trace_name, trace_variant)
                    == (schema_length, schema_name, schema_variant)
            }

            (
                TraceNode::Struct(trace_name, trace_name_list),
                Schema::Struct(schema_name, schema_name_list, _),
            ) => (trace_name, trace_name_list) == (schema_name, schema_name_list),
            (
                TraceNode::StructVariant(trace_name, trace_variant, trace_name_list),
                Schema::StructVariant(schema_name, schema_variant, schema_name_list, _),
            ) => {
                (trace_name, trace_variant, trace_name_list)
                    == (schema_name, schema_variant, schema_name_list)
            }

            (trace, Schema::Union(schema_list)) => {
                return self
                    .root
                    .schema_list(schema_list)
                    .iter()
                    .map(|&schema| self.traced_child(schema, trace))
                    .find_position(|child| dbg!(child.check()).is_some())
                    .map(|(discriminant, child)| {
                        CheckResult::Discriminated(
                            u32::try_from(discriminant).expect("too many types in union"),
                            child,
                        )
                    });
            }

            _ => false,
        };

        matches.then_some(CheckResult::Simple)
    }

    fn finish_serialize<S>(
        &self,
        serializer: S,
        checked: CheckResult<'_>,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let data = self.tail;
        if let CheckResult::Discriminated(discriminant, child) = checked {
            return serializer.serialize_newtype_variant(
                "AnonymousEnum",
                discriminant,
                anonymous_name(discriminant),
                &child,
            );
        }
        match self.schema {
            Schema::Bool => serializer.serialize_bool(data.pop_bool()),
            Schema::I8 => serializer.serialize_i8(data.pop_i8()),
            Schema::I16 => serializer.serialize_i16(data.pop_i16()),
            Schema::I32 => serializer.serialize_i32(data.pop_i32()),
            Schema::I64 => serializer.serialize_i64(data.pop_i64()),
            Schema::I128 => serializer.serialize_i128(data.pop_i128()),
            Schema::U8 => serializer.serialize_u8(data.pop_u8()),
            Schema::U16 => serializer.serialize_u16(data.pop_u16()),
            Schema::U32 => serializer.serialize_u32(data.pop_u32()),
            Schema::U64 => serializer.serialize_u64(data.pop_u64()),
            Schema::U128 => serializer.serialize_u128(data.pop_u128()),
            Schema::F32 => serializer.serialize_f32(data.pop_f32()),
            Schema::F64 => serializer.serialize_f64(data.pop_f64()),
            Schema::Char => serializer.serialize_char(data.pop_char()),
            Schema::String => serializer.serialize_str(data.pop_str(data.pop_length_u32())),
            Schema::Bytes => serializer.serialize_bytes(data.pop_slice(data.pop_length_u32())),

            Schema::Unit
            | Schema::UnitStruct(_)
            | Schema::UnitVariant(_, _)
            | Schema::OptionNone
            | Schema::Skip => serializer.serialize_unit(),

            Schema::OptionSome(inner)
            | Schema::NewtypeStruct(_, inner)
            | Schema::NewtypeVariant(_, _, inner) => self.serialize_inner(serializer, inner),

            Schema::Map(key, value) => {
                self.serialize_map(serializer, data.pop_length_u32(), key, value)
            }
            Schema::Sequence(item) => {
                self.serialize_sequence(serializer, data.pop_length_u32(), item)
            }

            Schema::Tuple(schema_length, schema_list)
            | Schema::TupleStruct(_, schema_length, schema_list)
            | Schema::TupleVariant(_, _, schema_length, schema_list) => {
                self.serialize_tuple(serializer, schema_length, schema_list)
            }

            Schema::Struct(_, schema_name_list, schema_type_list)
            | Schema::StructVariant(_, _, schema_name_list, schema_type_list) => {
                self.serialize_struct(serializer, schema_name_list, schema_type_list)
            }

            Schema::Union(_) => unreachable!("union finish called with simple check result"),
        }
    }
}

impl Serialize for ValueCursor<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.tail.set(self.data);
        self.finish_serialize(
            serializer,
            dbg!(self.check()).expect("schema-trace mismatch"),
        )
    }
}

pub struct Described<T>(pub T);

impl<T> Serialize for Described<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        to_value(self)
            .map_err(S::Error::custom)?
            .serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for Described<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor<T>(PhantomData<T>);
        impl<'de, T> serde::de::Visitor for Visitor<T>
        where
            T: Deserialize<'de>,
        {
            type Value = Described<T>;
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                seq.next_element::<Schema>()
                // TODO finish deserializing the top-level tuple!
            }
        }
        deserializer.deserialize_tuple(3, visitor)
    }
}

pub struct ValueDeserializer<'s, DeserializerT>(&'s Schema, DeserializerT);

impl<'s, 'de, DeserializerT> Deserializer<'de> for ValueDeserializer<'s, DeserializerT>
where
    DeserializerT: Deserializer<'de>,
{
    type Error = DeserializerT::Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_tuple_struct<V>(
        self,
        name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    #[inline]
    fn is_human_readable(&self) -> bool {
        false
    }
}

#[derive(Copy, Debug, Clone)]
enum TraceNode {
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,

    U8,
    U16,
    U32,
    U64,
    U128,

    F32,
    F64,
    Char,

    String,
    Bytes,

    None,
    Some,

    Unit,
    UnitStruct(NameIndex),
    UnitVariant(NameIndex, NameIndex),

    NewtypeStruct(NameIndex),
    NewtypeVariant(NameIndex, NameIndex),

    Sequence,
    Map,

    Tuple(u32),
    TupleStruct(u32, NameIndex),
    TupleVariant(u32, NameIndex, NameIndex),

    Struct(NameIndex, NameListIndex),
    StructVariant(NameIndex, NameIndex, NameListIndex),

    Skip,
}

trait ReadTraceExt<'data> {
    fn pop_u8(&self) -> u8;
    fn pop_slice(&self, len: usize) -> &'data [u8];

    fn pop_str(&self, len: usize) -> &'data str {
        str::from_utf8(self.pop_slice(len)).expect("invalid utf-8 in traced string")
    }

    fn pop_u16(&self) -> u16 {
        u16::from_le_bytes(
            self.pop_slice(std::mem::size_of::<u16>())
                .try_into()
                .expect("impossible"),
        )
    }

    fn pop_u32(&self) -> u32 {
        u32::from_le_bytes(
            self.pop_slice(std::mem::size_of::<u32>())
                .try_into()
                .expect("impossible"),
        )
    }

    fn pop_u64(&self) -> u64 {
        u64::from_le_bytes(
            self.pop_slice(std::mem::size_of::<u64>())
                .try_into()
                .expect("impossible"),
        )
    }

    fn pop_u128(&self) -> u128 {
        u128::from_le_bytes(
            self.pop_slice(std::mem::size_of::<u128>())
                .try_into()
                .expect("impossible"),
        )
    }

    fn pop_trace_node(&self) -> TraceNode {
        let trace = Trace::try_from(self.pop_u8()).expect("invalid trace");
        match trace {
            Trace::Bool => TraceNode::Bool,
            Trace::I8 => TraceNode::I8,
            Trace::I16 => TraceNode::I16,
            Trace::I32 => TraceNode::I32,
            Trace::I64 => TraceNode::I64,
            Trace::I128 => TraceNode::I128,
            Trace::U8 => TraceNode::U8,
            Trace::U16 => TraceNode::U16,
            Trace::U32 => TraceNode::U32,
            Trace::U64 => TraceNode::U64,
            Trace::U128 => TraceNode::U128,
            Trace::F32 => TraceNode::F32,
            Trace::F64 => TraceNode::F64,
            Trace::Char => TraceNode::Char,
            Trace::String => TraceNode::String,
            Trace::Bytes => TraceNode::Bytes,

            Trace::Skip => TraceNode::Skip,
            Trace::OptionNone => TraceNode::None,
            Trace::OptionSome => TraceNode::Some,

            Trace::Unit => TraceNode::Unit,

            Trace::UnitStruct => TraceNode::UnitStruct(self.pop_name()),
            Trace::UnitVariant => TraceNode::UnitVariant(self.pop_name(), self.pop_name()),

            Trace::NewtypeStruct => TraceNode::NewtypeStruct(self.pop_name()),
            Trace::NewtypeVariant => TraceNode::NewtypeVariant(self.pop_name(), self.pop_name()),

            Trace::Map => TraceNode::Map,
            Trace::Sequence => TraceNode::Sequence,

            Trace::Tuple => TraceNode::Tuple(self.pop_u32()),
            Trace::TupleStruct => TraceNode::TupleStruct(self.pop_u32(), self.pop_name()),
            Trace::TupleVariant => {
                TraceNode::TupleVariant(self.pop_u32(), self.pop_name(), self.pop_name())
            }

            Trace::Struct => TraceNode::Struct(self.pop_name(), self.pop_name_list()),
            Trace::StructVariant => {
                TraceNode::StructVariant(self.pop_name(), self.pop_name(), self.pop_name_list())
            }
        }
    }

    fn pop_name(&self) -> NameIndex {
        self.pop_u32().try_into().expect("name index too large")
    }

    fn pop_name_list(&self) -> NameListIndex {
        self.pop_u32()
            .try_into()
            .expect("name list index too large")
    }

    fn pop_bool(&self) -> bool {
        self.pop_u8() != 0
    }

    fn pop_i8(&self) -> i8 {
        self.pop_u8() as i8
    }

    fn pop_i16(&self) -> i16 {
        self.pop_u16() as i16
    }

    fn pop_i32(&self) -> i32 {
        self.pop_u32() as i32
    }

    fn pop_i64(&self) -> i64 {
        self.pop_u64() as i64
    }

    fn pop_i128(&self) -> i128 {
        self.pop_u128() as i128
    }

    fn pop_char(&self) -> char {
        char::try_from(self.pop_u32()).expect("expected char")
    }

    fn pop_f32(&self) -> f32 {
        f32::from_bits(self.pop_u32())
    }

    fn pop_f64(&self) -> f64 {
        f64::from_bits(self.pop_u64())
    }

    fn pop_length_u32(&self) -> usize {
        usize::try_from(self.pop_u32()).expect("usize needs to be at least 32 bits")
    }
}

impl<'data> ReadTraceExt<'data> for Cell<&'data [u8]> {
    fn pop_u8(&self) -> u8 {
        let mut data = self.get();
        let byte = *data.split_off_first().expect("expected byte");
        self.set(data);
        byte
    }

    fn pop_slice(&self, len: usize) -> &'data [u8] {
        let (head, tail) = self.get().split_at(len);
        self.set(tail);
        head
    }
}
