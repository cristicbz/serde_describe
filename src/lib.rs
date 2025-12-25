use indexmap::{Equivalent, IndexSet};
use itertools::Itertools;
use serde::{
    de::{
        DeserializeSeed, Deserializer, EnumAccess, Error as _, Expected, IgnoredAny, MapAccess,
        SeqAccess, Unexpected, VariantAccess,
    },
    ser::{
        Error as _, SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant,
        SerializeTuple, SerializeTupleStruct, SerializeTupleVariant, Serializer,
    },
    Deserialize, Serialize,
};
use std::{cell::Cell, fmt::Debug, hash::Hash, marker::PhantomData, ops::Index, sync::LazyLock};
use thiserror::Error;

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

    #[inline]
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
    #[inline]
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
                #[inline]
                fn from(index: $index_ty) -> u32 {
                    index.0.into()
                }
            }

            impl From<$index_ty> for usize {
                #[inline]
                fn from(index: $index_ty) -> usize {
                    usize::try_from(u32::from(index.0)).expect("usize must be at least 32-bits")
                }
            }

            impl TryFrom<usize> for $index_ty {
                type Error = SerError;

                #[inline]
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

                #[inline]
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

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<ValueT, ValueIndexT> Default for Pool<ValueT, ValueIndexT> {
    #[inline]
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

    #[inline]
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
    #[inline]
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

#[derive(Clone, Copy, Debug, Error)]
#[error("no such name with index {0:?}")]
pub struct NoSuchNameError(NameIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such name list with index {0:?}")]
pub struct NoSuchNameListError(NameListIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such schema with index {0:?}")]
pub struct NoSuchSchemaError(SchemaIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such schema list with index {0:?}")]
pub struct NoSuchSchemaListError(SchemaListIndex);

#[derive(Clone, Copy, Debug, Error)]
pub enum DumpError {
    #[error("dump error: {0}")]
    NoSuchName(#[from] NoSuchNameError),

    #[error("dump error: {0}")]
    NoSuchNameList(#[from] NoSuchNameListError),

    #[error("dump error: {0}")]
    NoSuchSchema(#[from] NoSuchSchemaError),

    #[error("dump error: {0}")]
    NoSuchSchemaList(#[from] NoSuchSchemaListError),
}

impl RootSchema {
    #[inline]
    fn name(&self, index: NameIndex) -> Result<&str, NoSuchNameError> {
        self.names
            .get(usize::from(index))
            .map(|string| &**string)
            .ok_or(NoSuchNameError(index))
    }

    #[inline]
    fn name_list(&self, index: NameListIndex) -> Result<&[NameIndex], NoSuchNameListError> {
        self.name_lists
            .get(usize::from(index))
            .map(|list| &**list)
            .ok_or(NoSuchNameListError(index))
    }

    #[inline]
    fn schema(&self, index: SchemaIndex) -> Result<Schema, NoSuchSchemaError> {
        self.schemas
            .get(usize::from(index))
            .copied()
            .ok_or(NoSuchSchemaError(index))
    }

    #[inline]
    fn schema_list(&self, index: SchemaListIndex) -> Result<&[SchemaIndex], NoSuchSchemaListError> {
        self.schema_lists
            .get(usize::from(index))
            .map(|list| &**list)
            .ok_or(NoSuchSchemaListError(index))
    }

    fn dump(&self, indent: &mut String, index: SchemaIndex) -> Result<(), DumpError> {
        if indent.is_empty() {
            eprintln!("SCHEMA:")
        }
        indent.push_str("  ");
        let schema = self.schema(index)?;
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
            | Schema::Skip => eprintln!("{indent}::{schema:?},"),
            Schema::Unit => eprintln!("{indent}(),"),
            Schema::OptionNone => eprintln!("{indent}::Option::None"),

            Schema::UnitStruct(name) => eprintln!("{}{},", indent, self.name(name)?),
            Schema::UnitVariant(name, variant) => {
                eprintln!("{}{}::{},", indent, self.name(name)?, self.name(variant)?)
            }

            Schema::OptionSome(inner) => {
                eprintln!("{}::Option::Some(", indent);
                self.dump(indent, inner)?;
                eprintln!("{indent}),")
            }
            Schema::NewtypeStruct(name, inner) => {
                eprintln!("{}{}(", indent, self.name(name)?);
                self.dump(indent, inner)?;
                eprintln!("{indent}),")
            }
            Schema::NewtypeVariant(name, variant, inner) => {
                eprintln!("{}{}::{}(", indent, self.name(name)?, self.name(variant)?);
                self.dump(indent, inner)?;
                eprintln!("{indent}),")
            }
            Schema::Map(key, value) => {
                eprintln!("{indent}{{");
                self.dump(indent, key)?;
                self.dump(indent, value)?;
                eprintln!("{indent}}},")
            }
            Schema::Sequence(item) => {
                eprintln!("{indent}[");
                self.dump(indent, item)?;
                eprintln!("{indent}],")
            }

            Schema::Tuple(_, schema_list) => {
                eprintln!("{indent}(");
                for &schema in self.schema_list(schema_list)? {
                    self.dump(indent, schema)?;
                }
                eprintln!("{indent}),")
            }

            Schema::TupleStruct(name, _, schema_list) => {
                eprintln!("{}{}(", indent, self.name(name)?);
                for &schema in self.schema_list(schema_list)? {
                    self.dump(indent, schema)?;
                }
                eprintln!("{indent}),")
            }
            Schema::TupleVariant(name, variant, _, schema_list) => {
                eprintln!("{}{}::{}(", indent, self.name(name)?, self.name(variant)?);
                for &schema in self.schema_list(schema_list)? {
                    self.dump(indent, schema)?;
                }
                eprintln!("{indent}),")
            }

            Schema::Struct(name, name_list, type_list) => {
                eprintln!("{}{} {{", indent, self.name(name)?);
                indent.push_str("  ");
                for (&name, &schema) in self
                    .name_list(name_list)?
                    .iter()
                    .zip(self.schema_list(type_list)?)
                {
                    eprintln!("{}{}:", indent, self.name(name)?);
                    self.dump(indent, schema)?;
                }
                indent.truncate(indent.len() - 2);
                eprintln!("{indent}}},")
            }
            Schema::StructVariant(name, variant, name_list, type_list) => {
                eprintln!("{}{}::{} {{", indent, self.name(name)?, self.name(variant)?);
                indent.push_str("  ");
                for (&name, &schema) in self
                    .name_list(name_list)?
                    .iter()
                    .zip(self.schema_list(type_list)?)
                {
                    eprintln!("{}{}:", indent, self.name(name)?);
                    self.dump(indent, schema)?;
                }
                indent.truncate(indent.len() - 2);
                eprintln!("{indent}}},")
            }

            Schema::Union(type_list) => {
                eprintln!("{indent}<");
                indent.push_str("  ");
                for &schema in self.schema_list(type_list)? {
                    self.dump(indent, schema)?;
                }
                indent.truncate(indent.len() - 2);
                eprintln!("{indent}>,")
            }
        }
        indent.truncate(indent.len() - 2);
        if indent.is_empty() {
            eprintln!("\n\n")
        }
        Ok(())
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
        schema.dump(&mut String::new(), root_index).unwrap();
        Ok(Value {
            schema,
            root_index,
            data: ValueData(self.data),
        })
    }

    #[inline]
    fn push_struct_name(&mut self, name: &'static str) -> Result<TypeName, SerError> {
        let name = self.names.intern(name)?;
        self.push_u32(name.into());
        Ok(TypeName(name, None))
    }

    #[inline]
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

    #[inline]
    fn intern_field_name(&mut self, name: &'static str) -> Result<NameIndex, SerError> {
        self.names.intern(name)
    }

    #[inline]
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

    #[inline]
    fn push_u32(&mut self, integer: u32) {
        self.data.extend(integer.to_le_bytes());
    }

    #[inline]
    fn push_u32_length(&mut self, length: usize) -> Result<(), SerError> {
        self.data.extend(
            u32::try_from(length)
                .map_err(|_| SerError::TooManyValues)?
                .to_le_bytes(),
        );
        Ok(())
    }

    #[inline]
    fn push_trace(&mut self, trace: Trace) {
        self.data.push(trace.into());
    }

    #[inline]
    fn reserve_u32(&mut self) -> Result<TraceIndex, SerError> {
        self.reserve_bytes(std::mem::size_of::<u32>())
    }

    #[inline]
    fn reserve_bytes(&mut self, size: usize) -> Result<TraceIndex, SerError> {
        let index = TraceIndex::try_from(self.data.len())?;
        self.data.extend(std::iter::repeat_n(!0, size));
        Ok(index)
    }

    #[inline]
    fn push_length_bytes(&mut self, bytes: &[u8]) -> Result<(), SerError> {
        self.push_u32_length(bytes.len())?;
        self.data.extend(bytes);
        Ok(())
    }

    #[inline]
    fn fill_reserved_bytes(&mut self, index: TraceIndex, data: &[u8]) {
        self.data[index.into()..][..data.len()].copy_from_slice(data);
    }
}

macro_rules! fn_serialize_as_u8 {
    ($(($fn_name:ident, $value_type:ty, $schema:ident),)+) => {
        $(
            #[inline]
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
            #[inline]
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

    #[inline]
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::ALL.get(usize::from(value)).copied().ok_or(value)
    }
}

impl From<Trace> for u8 {
    #[inline]
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

    #[inline]
    fn serialize_char(self, value: char) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::Char);
        self.push_u32(u32::from(value));
        Ok(SchemaBuilder::Char)
    }

    #[inline]
    fn serialize_str(self, value: &str) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::String);
        self.push_length_bytes(value.as_bytes())?;
        Ok(SchemaBuilder::String)
    }

    #[inline]
    fn serialize_bytes(self, value: &[u8]) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::Bytes);
        self.push_length_bytes(value)?;
        Ok(SchemaBuilder::Bytes)
    }

    #[inline]
    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::OptionNone);
        Ok(SchemaBuilder::OptionNone)
    }

    #[inline]
    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.push_trace(Trace::OptionSome);
        T::serialize(value, &mut *self).map(|schema| SchemaBuilder::OptionSome(Box::new(schema)))
    }

    #[inline]
    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::Unit);
        Ok(SchemaBuilder::Unit(None))
    }

    #[inline]
    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::UnitStruct);
        Ok(SchemaBuilder::Unit(Some(self.push_struct_name(name)?)))
    }

    #[inline]
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

    #[inline]
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

    #[inline]
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

    #[inline]
    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        self.push_trace(Trace::Sequence);
        Ok(SequenceSchemaBuilder {
            reserved_length: self.reserve_u32()?,
            schema: SchemaBuilder::default(),
            length: 0,
            parent: self,
        })
    }

    #[inline]
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

    #[inline]
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

    #[inline]
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

    #[inline]
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

    #[inline]
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

    #[inline]
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

    #[inline]
    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.length += 1;
        self.schema.union(T::serialize(value, &mut *self.parent)?);
        Ok(())
    }

    #[inline]
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

    #[inline]
    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.length += 1;
        self.key_schema.union(T::serialize(key, &mut *self.parent)?);
        Ok(())
    }

    #[inline]
    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value_schema
            .union(T::serialize(value, &mut *self.parent)?);
        Ok(())
    }

    #[inline]
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

    #[inline]
    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.schemas.push(T::serialize(value, &mut *self.parent)?);
        Ok(())
    }

    #[inline]
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

    #[inline]
    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        <Self as SerializeTuple>::serialize_element(self, value)
    }

    #[inline]
    fn end(self) -> Result<Self::Ok, Self::Error> {
        <Self as SerializeTuple>::end(self)
    }
}

impl SerializeTupleVariant for TupleSchemaBuilder<'_> {
    type Ok = SchemaBuilder;
    type Error = SerError;

    #[inline]
    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        <Self as SerializeTuple>::serialize_element(self, value)
    }

    #[inline]
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

    #[inline]
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.field_names.push(self.parent.intern_field_name(key)?);
        self.field_types
            .push(T::serialize(value, &mut *self.parent)?);
        Ok(())
    }

    #[inline]
    fn skip_field(&mut self, key: &'static str) -> Result<(), Self::Error> {
        self.field_names.push(self.parent.intern_field_name(key)?);
        self.field_types.push(SchemaBuilder::Skip);
        self.parent.push_trace(Trace::Skip);
        Ok(())
    }

    #[inline]
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

    #[inline]
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        <Self as SerializeStruct>::serialize_field(self, key, value)
    }

    #[inline]
    fn skip_field(&mut self, key: &'static str) -> Result<(), Self::Error> {
        <Self as SerializeStruct>::skip_field(self, key)
    }

    #[inline]
    fn end(self) -> Result<Self::Ok, Self::Error> {
        <Self as SerializeStruct>::end(self)
    }
}

impl Serialize for Value {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let tail = Cell::new(&*self.data.0);
        let cursor = ValueCursor::start(&self.schema, self.root_index, &tail);
        (&self.schema, (self.root_index, cursor)).serialize(serializer)
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
    #[inline]
    fn start(root: &'a RootSchema, schema: SchemaIndex, tail: &'a Cell<&'a [u8]>) -> Self {
        Self {
            root,
            schema: root.schema(schema).unwrap(),
            trace: tail.pop_trace_node(),
            tail,
            data: tail.get(),
        }
    }

    #[inline]
    fn pop_child(&self, schema: SchemaIndex) -> Self {
        Self {
            root: self.root,
            schema: self.root.schema(schema).unwrap(),
            trace: self.tail.pop_trace_node(),
            data: self.tail.get(),
            tail: self.tail,
        }
    }

    #[inline]
    fn traced_child(&self, schema: SchemaIndex, trace: TraceNode) -> Self {
        Self {
            root: self.root,
            schema: self.root.schema(schema).unwrap(),
            trace,
            data: self.tail.get(),
            tail: self.tail,
        }
    }

    #[inline]
    fn serialize_inner<S>(&self, serializer: S, inner: SchemaIndex) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.pop_child(inner).serialize(serializer)
    }

    #[inline]
    fn serialize_tuple<S>(
        &self,
        serializer: S,
        schema_length: u32,
        schema_list: SchemaListIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let schema_list = self.root.schema_list(schema_list).unwrap();
        let schema_length = usize::try_from(schema_length).expect("usize at least 32-bits");
        assert_eq!(schema_list.len(), schema_length);

        let mut serializer = serializer.serialize_tuple(schema_length)?;
        for &schema in schema_list {
            serializer.serialize_element(&self.pop_child(schema))?
        }
        serializer.end()
    }

    #[inline]
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

    #[inline]
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

    #[inline]
    fn serialize_struct<S>(
        &self,
        serializer: S,
        name_list: NameListIndex,
        schema_list: SchemaListIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let schema_list = self.root.schema_list(schema_list).unwrap();
        let name_list = self.root.name_list(name_list).unwrap();
        let schema_length = schema_list.len();
        assert_eq!(name_list.len(), schema_length);
        let actual_length = todo!();

        let mut serializer = serializer.serialize_tuple(schema_length)?;
        for &schema in schema_list {
            let child = self.pop_child(schema);
            match child
                .check()
                .expect("schema-trace mismatch in field serialization")
            {
                CheckResult::Simple => serializer.serialize_element(&child)?,
                CheckResult::Discriminated(discriminator, child) => {
                    serializer.serialize_element(&discriminator)?;
                    serializer.serialize_element(&child)?;
                }
            }
        }
        serializer.end()
    }

    #[inline]
    fn check(&self) -> Option<CheckResult<'a>> {
        let matches = match (self.trace, self.schema) {
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
                    .unwrap()
                    .iter()
                    .map(|&schema| self.traced_child(schema, trace))
                    .find_position(|child| child.check().is_some())
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

    #[inline]
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
                UNION_ENUM_NAME,
                discriminant,
                UNION_VARIANT_NAMES
                    [usize::try_from(discriminant).expect("usize should be at least 32 bits")],
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
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.tail.set(self.data);
        self.finish_serialize(serializer, self.check().expect("schema-trace mismatch"))
    }
}

#[derive(Copy, Clone)]
pub struct Described<T>(pub T);

impl<T> Serialize for Described<T>
where
    T: Serialize,
{
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self::to_value(&self.0)
            .map_err(S::Error::custom)?
            .serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for Described<T>
where
    T: Deserialize<'de>,
{
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct SchemaPairVisitor<T>(PhantomData<T>);
        impl<'de, T> serde::de::Visitor<'de> for SchemaPairVisitor<T>
        where
            T: Deserialize<'de>,
        {
            type Value = Described<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(
                    formatter,
                    "a described object: (schema, described-elsewhere object) pair"
                )
            }

            #[inline]
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let schema = seq
                    .next_element::<RootSchema>()?
                    .ok_or_else(|| A::Error::invalid_length(0, &self))?;

                Ok(Described(
                    seq.next_element_seed(DescribedElsewhere(&schema, self.0))?
                        .ok_or_else(|| {
                            A::Error::custom(
                                "missing described-elsewhere pair in described object pair",
                            )
                        })?,
                ))
            }
        }

        deserializer.deserialize_tuple(2, SchemaPairVisitor(PhantomData))
    }
}

pub struct DescribedElsewhere<'schema, T>(&'schema RootSchema, PhantomData<T>);

impl<'schema, T> Copy for DescribedElsewhere<'schema, T> {}
impl<'schema, T> Clone for DescribedElsewhere<'schema, T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<'de, 'schema, T> DeserializeSeed<'de> for DescribedElsewhere<'schema, T>
where
    T: Deserialize<'de>,
{
    type Value = T;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor<'schema, T>(DescribedElsewhere<'schema, T>);
        impl<'de, 'schema, T> serde::de::Visitor<'de> for Visitor<'schema, T>
        where
            T: Deserialize<'de>,
        {
            type Value = T;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(
                    formatter,
                    "a described-elsewhere object: (schema index, object data) pair"
                )
            }

            #[inline]
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let schema = seq
                    .next_element::<SchemaIndex>()?
                    .ok_or_else(|| A::Error::invalid_length(0, &self))?;
                let seed = SchemaDeserializer {
                    root: (self.0).0,
                    schema: (self.0).0.schema(schema).map_err(A::Error::custom)?,
                    inner: (self.0).1,
                };
                seq.next_element_seed(seed)?.ok_or_else(|| {
                    A::Error::custom("missing object data in described-elsewhere object")
                })
            }
        }

        deserializer.deserialize_tuple(2, Visitor(self))
    }
}

trait DeferredDeserialize<'de>: Expected {
    type Visitor: serde::de::Visitor<'de>;

    fn into_visitor(self) -> Self::Visitor;

    fn call<DeserializerT>(
        self,
        deserializer: DeserializerT,
    ) -> Result<<Self::Visitor as serde::de::Visitor<'de>>::Value, DeserializerT::Error>
    where
        DeserializerT: serde::de::Deserializer<'de>;
}

trait CanonicalVisit<'de>: DeferredDeserialize<'de> {
    type CanonicalInput: Deserialize<'de>;

    fn canonical_visit<ErrorT>(
        self,
        value: Self::CanonicalInput,
    ) -> Result<<Self::Visitor as serde::de::Visitor<'de>>::Value, ErrorT>
    where
        ErrorT: serde::de::Error;
}

macro_rules! deferred {
    (mod $mod_name:ident { $($fn_name:ident($($arg_name:ident : $arg_type:ty),*) $(=> { $visit_name:ident($visit_type:ty) })?, )+ }) => {
        mod $mod_name {
            $(
                deferred! { @single $fn_name($($arg_name: $arg_type),*) $(=> { $visit_name($visit_type) })? }
            )+
        }
    };
    (@single $fn_name:ident($($arg_name:ident : $arg_type:ty),*) => { $visit_name:ident($visit_type:ty) }) => {
        deferred! { @single $fn_name($($arg_name: $arg_type),*) }
        impl<'de, VisitorT> super::CanonicalVisit<'de> for $fn_name<VisitorT>
        where VisitorT: serde::de::Visitor<'de> {
            type CanonicalInput = $visit_type;

            #[inline]
            fn canonical_visit<ErrorT>(
                self,
                value: Self::CanonicalInput,
            ) -> Result<<Self::Visitor as serde::de::Visitor<'de>>::Value, ErrorT>
            where ErrorT: serde::de::Error
            {
                self.visitor.$visit_name(value)
            }
        }
    };
    (@single $fn_name:ident($($arg_name:ident : $arg_type:ty),*)) => {
        #[allow(unused, non_camel_case_types)]
        pub struct $fn_name<VisitorT> {
            $(pub $arg_name: $arg_type,)*
            pub visitor: VisitorT,
        }

        impl<'de, VisitorT> super::Expected for $fn_name<VisitorT>
        where VisitorT: serde::de::Visitor<'de> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.visitor.expecting(f)
            }
        }

        impl<'de, VisitorT> super::DeferredDeserialize<'de> for $fn_name<VisitorT>
        where VisitorT: serde::de::Visitor<'de>
        {
            type Visitor = VisitorT;

            #[inline]
            fn into_visitor(self) -> VisitorT {
                self.visitor
            }

            #[inline]
            fn call<DeserializerT>(self, deserializer: DeserializerT) -> Result<VisitorT::Value, DeserializerT::Error>
            where DeserializerT: serde::de::Deserializer<'de>
            {
                deserializer.$fn_name($(self.$arg_name,)* self.visitor)
            }
        }
    };
}

deferred! {
    mod deferred  {
        deserialize_any(),
        deserialize_bool() => { visit_bool(bool) },

        deserialize_i8() => { visit_i8(i8) },
        deserialize_i16() => { visit_i16(i16) },
        deserialize_i32() => { visit_i32(i32) },
        deserialize_i64() => { visit_i64(i64) },
        deserialize_i128() => { visit_i128(i128) },

        deserialize_u8() => { visit_u8(u8) },
        deserialize_u16() => { visit_u16(u16) },
        deserialize_u32() => { visit_u32(u32) },
        deserialize_u64() => { visit_u64(u64) },
        deserialize_u128() => { visit_u128(u128) },

        deserialize_f32() => { visit_f32(f32) },
        deserialize_f64() => { visit_f64(f64) },

        deserialize_char() => { visit_char(char) },
        deserialize_str(),
        deserialize_string(),
        deserialize_bytes(),
        deserialize_byte_buf(),
        deserialize_option(),
        deserialize_unit(),
        deserialize_unit_struct(name: &'static str),
        deserialize_newtype_struct(name: &'static str),
        deserialize_seq(),

        deserialize_tuple(len: usize),
        deserialize_tuple_struct(name: &'static str, len: usize),
        deserialize_map(),
        deserialize_struct(name: &'static str, fields: &'static [&'static str]),
        deserialize_enum(name: &'static str, variants: &'static [&'static str]),
        deserialize_identifier(),
        deserialize_ignored_any(),
    }
}

#[derive(Copy, Clone)]
struct SchemaDeserializer<'schema, InnerT> {
    root: &'schema RootSchema,
    schema: Schema,
    inner: InnerT,
}

impl<'schema, InnerT> SchemaDeserializer<'schema, InnerT> {
    #[inline]
    fn forward<ErrorT>(self, schema: SchemaIndex) -> Result<Self, ErrorT>
    where
        ErrorT: serde::de::Error,
    {
        Ok(Self {
            root: self.root,
            schema: self.root.schema(schema).map_err(ErrorT::custom)?,
            inner: self.inner,
        })
    }
}

impl<'de, 'schema, SeedT> DeserializeSeed<'de> for SchemaDeserializer<'schema, SeedT>
where
    SeedT: DeserializeSeed<'de>,
{
    type Value = <SeedT as DeserializeSeed<'de>>::Value;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        self.inner.deserialize(SchemaDeserializer {
            root: self.root,
            schema: self.schema,
            inner: deserializer,
        })
    }
}

type CallValue<'de, CallT> =
    <<CallT as DeferredDeserialize<'de>>::Visitor as serde::de::Visitor<'de>>::Value;
type CallResult<'de, CallT, DeserializerT> =
    Result<CallValue<'de, CallT>, <DeserializerT as Deserializer<'de>>::Error>;

impl<'s, 'de, DeserializerT> SchemaDeserializer<'s, DeserializerT>
where
    DeserializerT: Deserializer<'de>,
{
    #[inline]
    fn do_deserialize_seq<VisitorT>(
        self,
        item: SchemaIndex,
        visitor: VisitorT,
    ) -> Result<VisitorT::Value, DeserializerT::Error>
    where
        VisitorT: serde::de::Visitor<'de>,
    {
        self.inner.deserialize_seq(SchemaSeqDeserializer {
            root: self.root,
            item: self
                .root
                .schema(item)
                .map_err(DeserializerT::Error::custom)?,
            inner: visitor,
        })
    }

    #[inline]
    fn do_deserialize_tuple<VisitorT>(
        self,
        field_schemas: SchemaListIndex,
        visitor: VisitorT,
    ) -> Result<VisitorT::Value, DeserializerT::Error>
    where
        VisitorT: serde::de::Visitor<'de>,
    {
        let items = self
            .root
            .schema_list(field_schemas)
            .map_err(DeserializerT::Error::custom)?;
        self.inner.deserialize_tuple(
            items.len(),
            SchemaTupleDeserializer {
                root: self.root,
                items,
                inner: visitor,
            },
        )
    }

    #[inline]
    fn do_deserialize_map<VisitorT>(
        self,
        key: SchemaIndex,
        value: SchemaIndex,
        visitor: VisitorT,
    ) -> Result<VisitorT::Value, DeserializerT::Error>
    where
        VisitorT: serde::de::Visitor<'de>,
    {
        self.inner.deserialize_map(SchemaMapDeserializer {
            root: self.root,
            key: self
                .root
                .schema(key)
                .map_err(DeserializerT::Error::custom)?,
            value: self
                .root
                .schema(value)
                .map_err(DeserializerT::Error::custom)?,
            inner: visitor,
        })
    }

    #[inline]
    fn do_deserialize_struct<VisitorT>(
        self,
        field_names: NameListIndex,
        field_schemas: SchemaListIndex,
        visitor: VisitorT,
    ) -> Result<VisitorT::Value, DeserializerT::Error>
    where
        VisitorT: serde::de::Visitor<'de>,
    {
        let field_names = self
            .root
            .name_list(field_names)
            .map_err(DeserializerT::Error::custom)?;
        let field_schemas = self
            .root
            .schema_list(field_schemas)
            .map_err(DeserializerT::Error::custom)?;
        let length = field_names.len();
        if length != field_schemas.len() {
            return Err(DeserializerT::Error::custom(
                "bad schema: field name length and field schema length mismatch",
            ));
        }
        self.inner.deserialize_tuple(
            length,
            SchemaStructDeserializer {
                root: self.root,
                field_names,
                field_schemas,
                inner: visitor,
                next_value_schema: None,
            },
        )
    }

    #[inline]
    fn deserialize_if<CallT>(
        self,
        condition: impl FnOnce(Schema) -> bool,
        call: CallT,
    ) -> CallResult<'de, CallT, DeserializerT>
    where
        CallT: DeferredDeserialize<'de>,
    {
        match self.schema {
            Schema::Union(variants) => self.deserialize_union(variants, call),
            Schema::OptionSome(inner)
            | Schema::NewtypeStruct(_, inner)
            | Schema::NewtypeVariant(_, _, inner) => call.call(self.forward(inner)?),
            actual if condition(actual) => call.call(self.inner),
            _ => self.invalid_type_error(&call),
        }
    }

    #[inline]
    fn deserialize_float<CallT>(
        self,
        convert_f64: impl FnOnce(f64) -> CallT::CanonicalInput,
        call: CallT,
    ) -> CallResult<'de, CallT, DeserializerT>
    where
        CallT: CanonicalVisit<'de>,
        CallT::CanonicalInput: From<f32>,
    {
        match self.schema {
            Schema::Union(variants) => self.deserialize_union(variants, call),
            Schema::OptionSome(inner)
            | Schema::NewtypeStruct(_, inner)
            | Schema::NewtypeVariant(_, _, inner) => call.call(self.forward(inner)?),
            Schema::F32 => call.canonical_visit(f32::deserialize(self.inner)?.into()),
            Schema::F64 => call.canonical_visit(convert_f64(f64::deserialize(self.inner)?)),
            _ => self.invalid_type_error(&call),
        }
    }

    #[inline]
    fn deserialize_integer<CallT>(self, call: CallT) -> CallResult<'de, CallT, DeserializerT>
    where
        CallT: CanonicalVisit<'de>,
        CallT::CanonicalInput: TryFrom<u8>
            + TryFrom<u16>
            + TryFrom<u32>
            + TryFrom<u64>
            + TryFrom<u128>
            + TryFrom<i8>
            + TryFrom<i16>
            + TryFrom<i32>
            + TryFrom<i64>
            + TryFrom<i128>,
    {
        macro_rules! integer_conversion {
            ($type:ident, $visit_fn:ident, Other($text:expr)) => {
                integer_conversion!(@common, $type, $visit_fn, integer, {Unexpected::Other($text)})
            };
            ($type:ident, $visit_fn:ident, $unexpected:ident) => {
                integer_conversion!(@common, $type, $visit_fn, integer, Unexpected::$unexpected(integer.into()))
            };
            (@common, $type:ident, $visit_fn:ident, $int:ident, $unexpected:expr) => {
                {
                    let $int = $type::deserialize((self).inner)?;
                    match TryFrom::try_from($int) {
                        Ok(integer) => call.canonical_visit(integer),
                        Err(_) => <CallT::Visitor as serde::de::Visitor<'de>>::$visit_fn(call.into_visitor(), $int),
                    }
                }
            };
        }

        match self.schema {
            Schema::Union(variants) => self.deserialize_union(variants, call),
            Schema::OptionSome(inner)
            | Schema::NewtypeStruct(_, inner)
            | Schema::NewtypeVariant(_, _, inner) => call.call(self.forward(inner)?),

            Schema::I8 => integer_conversion!(i8, visit_i8, Signed),
            Schema::I16 => integer_conversion!(i16, visit_i16, Signed),
            Schema::I32 => integer_conversion!(i32, visit_i32, Signed),
            Schema::I64 => integer_conversion!(i64, visit_i64, Signed),
            Schema::I128 => integer_conversion!(i128, visit_i128, Other("128-bit signed integer")),

            Schema::U8 => integer_conversion!(u8, visit_u8, Unsigned),
            Schema::U16 => integer_conversion!(u16, visit_u16, Unsigned),
            Schema::U32 => integer_conversion!(u32, visit_u32, Unsigned),
            Schema::U64 => integer_conversion!(u64, visit_u64, Unsigned),
            Schema::U128 => {
                integer_conversion!(u128, visit_u128, Other("128-bit unsigned integer"))
            }

            _ => self.invalid_type_error(&call),
        }
    }

    #[inline]
    fn deserialize_union<CallT>(
        self,
        variants: SchemaListIndex,
        call: CallT,
    ) -> CallResult<'de, CallT, DeserializerT>
    where
        CallT: DeferredDeserialize<'de>,
    {
        struct UnionVisitor<'schema, CallT>(&'schema RootSchema, &'schema [SchemaIndex], CallT);

        impl<'de, 'schema, CallT> serde::de::Visitor<'de> for UnionVisitor<'schema, CallT>
        where
            CallT: DeferredDeserialize<'de>,
        {
            type Value = CallValue<'de, CallT>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a union discriminant")
            }

            #[inline]
            fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::EnumAccess<'de>,
            {
                let (variant, data) = data.variant_seed(VariantSeed)?;
                data.newtype_variant_seed(ResolvedUnion(
                    self.0,
                    usize::try_from(variant)
                        .ok()
                        .and_then(|variant| self.1.get(variant).copied())
                        .ok_or_else(|| {
                            A::Error::invalid_value(Unexpected::Unsigned(variant), &"variant")
                        })?,
                    self.2,
                ))
            }
        }

        struct ResolvedUnion<'schema, CallT>(&'schema RootSchema, SchemaIndex, CallT);

        impl<'de, 'schema, CallT> DeserializeSeed<'de> for ResolvedUnion<'schema, CallT>
        where
            CallT: DeferredDeserialize<'de>,
        {
            type Value = <CallT::Visitor as serde::de::Visitor<'de>>::Value;

            #[inline]
            fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: Deserializer<'de>,
            {
                self.2.call(SchemaDeserializer {
                    root: self.0,
                    schema: self.0.schema(self.1).map_err(D::Error::custom)?,
                    inner: deserializer,
                })
            }
        }

        struct VariantSeed;
        impl<'de> DeserializeSeed<'de> for VariantSeed {
            type Value = u64;

            fn deserialize<D>(self, deserializer: D) -> Result<u64, D::Error>
            where
                D: Deserializer<'de>,
            {
                deserializer.deserialize_identifier(self)
            }
        }

        impl<'de> serde::de::Visitor<'de> for VariantSeed {
            type Value = u64;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a union discriminant")
            }

            #[inline]
            fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(value)
            }

            #[inline]
            fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_str(
                    str::from_utf8(value).map_err(|_| E::custom("non-utf8 union variant name"))?,
                )
            }

            #[inline]
            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                (if value.len() > 2 {
                    u64::from_str_radix(&value[2..], 16).ok()
                } else {
                    None
                })
                .ok_or_else(|| E::custom(format!("bad union variant name {value}")))
            }
        }

        let schema_list = self
            .root
            .schema_list(variants)
            .map_err(DeserializerT::Error::custom)?;
        let (variant_names, _) = UNION_VARIANT_NAMES
            .split_at_checked(schema_list.len())
            .ok_or_else(|| DeserializerT::Error::custom("too many variants in union"))?;
        self.inner.deserialize_enum(
            UNION_ENUM_NAME,
            variant_names,
            UnionVisitor(self.root, schema_list, call),
        )
    }

    fn unexpected(self) -> Result<Unexpected<'de>, DeserializerT::Error> {
        Ok(match self.schema {
            Schema::Bool => Unexpected::Bool(bool::deserialize(self.inner)?),

            Schema::I8 => Unexpected::Signed(i64::from(i8::deserialize(self.inner)?)),
            Schema::I16 => Unexpected::Signed(i64::from(i16::deserialize(self.inner)?)),
            Schema::I32 => Unexpected::Signed(i64::from(i32::deserialize(self.inner)?)),
            Schema::I64 => Unexpected::Signed(i64::deserialize(self.inner)?),
            Schema::I128 => Unexpected::Other("128-bit signed integer"),

            Schema::U8 => Unexpected::Unsigned(u64::from(u8::deserialize(self.inner)?)),
            Schema::U16 => Unexpected::Unsigned(u64::from(u16::deserialize(self.inner)?)),
            Schema::U32 => Unexpected::Unsigned(u64::from(u32::deserialize(self.inner)?)),
            Schema::U64 => Unexpected::Unsigned(u64::deserialize(self.inner)?),
            Schema::U128 => Unexpected::Other("128-bit unsigned integer"),

            Schema::F32 => Unexpected::Float(f64::from(f32::deserialize(self.inner)?)),
            Schema::F64 => Unexpected::Float(f64::deserialize(self.inner)?),
            Schema::Char => Unexpected::Char(char::deserialize(self.inner)?),

            Schema::String => <&'de str>::deserialize(self.inner)
                .map(Unexpected::Str)
                .unwrap_or(Unexpected::Other("string")),
            Schema::Bytes => <&'de [u8]>::deserialize(self.inner)
                .map(Unexpected::Bytes)
                .unwrap_or(Unexpected::Other("bytes")),

            Schema::OptionNone | Schema::OptionSome(_) => Unexpected::Option,

            Schema::Unit => Unexpected::Unit,
            Schema::UnitStruct(_) => Unexpected::Other("unit struct"),
            Schema::UnitVariant(_, _) => Unexpected::UnitVariant,

            Schema::NewtypeStruct(_, _) => Unexpected::NewtypeStruct,
            Schema::NewtypeVariant(_, _, _) => Unexpected::NewtypeVariant,

            Schema::Sequence(_) => Unexpected::Seq,
            Schema::Map(_, _) => Unexpected::Map,

            Schema::Tuple(_, _) => Unexpected::Other("tuple"),
            Schema::TupleStruct(_, _, _) => Unexpected::Other("tuple struct"),
            Schema::TupleVariant(_, _, _, _) => Unexpected::TupleVariant,

            Schema::Struct(_, _, _) => Unexpected::Other("struct"),
            Schema::StructVariant(_, _, _, _) => Unexpected::StructVariant,

            Schema::Union(_) => Unexpected::Other("enum, skippable field or untagged union"),
            Schema::Skip => Unexpected::Other("skipped field"),
        })
    }

    fn invalid_type_error<ExpectedT>(
        self,
        expected: &dyn Expected,
    ) -> Result<ExpectedT, DeserializerT::Error> {
        Err(DeserializerT::Error::invalid_type(
            self.unexpected()?,
            expected,
        ))
    }
}

macro_rules! deserialize_simple {
    ($fn_name:ident, $schema:pat) => {
        deserialize_simple!{@helper, $fn_name, self, visitor, (self.deserialize_if(|schema| matches!(schema, $schema), deferred::$fn_name { visitor }))}
    };

    ($fn_name:ident, @integer) => {
        deserialize_simple!{@helper, $fn_name, self, visitor, (self.deserialize_integer(deferred::$fn_name { visitor }))}
    };

    ($fn_name:ident, @float) => {
        deserialize_simple!{@helper, $fn_name, self, visitor, (self.deserialize_float(|value| value as _, deferred::$fn_name { visitor }))}
    };

    (@helper, $fn_name:ident, $self:ident, $visitor:ident, $with:tt) => {
        fn $fn_name<V>($self, $visitor: V) -> Result<V::Value, Self::Error>
        where
            V: serde::de::Visitor<'de>,
        {
            $with
        }
    };
}

impl<'s, 'de, DeserializerT> Deserializer<'de> for SchemaDeserializer<'s, DeserializerT>
where
    DeserializerT: Deserializer<'de>,
{
    type Error = DeserializerT::Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.schema {
            Schema::Bool => self.inner.deserialize_bool(visitor),

            Schema::I8 => self.inner.deserialize_i8(visitor),
            Schema::I16 => self.inner.deserialize_i16(visitor),
            Schema::I32 => self.inner.deserialize_i32(visitor),
            Schema::I64 => self.inner.deserialize_i64(visitor),
            Schema::I128 => self.inner.deserialize_i128(visitor),

            Schema::U8 => self.inner.deserialize_u8(visitor),
            Schema::U16 => self.inner.deserialize_u16(visitor),
            Schema::U32 => self.inner.deserialize_u32(visitor),
            Schema::U64 => self.inner.deserialize_u64(visitor),
            Schema::U128 => self.inner.deserialize_u128(visitor),

            Schema::F32 => self.inner.deserialize_f32(visitor),
            Schema::F64 => self.inner.deserialize_f64(visitor),
            Schema::Char => self.inner.deserialize_char(visitor),

            Schema::String => self.inner.deserialize_str(visitor),
            Schema::Bytes => self.inner.deserialize_bytes(visitor),

            Schema::OptionNone => self
                .inner
                .deserialize_unit(serde::de::IgnoredAny)
                .and_then(|_| visitor.visit_none()),
            Schema::OptionSome(inner) => visitor.visit_some(self.forward(inner)?),
            Schema::Unit | Schema::UnitStruct(_) | Schema::UnitVariant(_, _) => {
                self.deserialize_unit(visitor)
            }

            Schema::NewtypeStruct(_, inner) | Schema::NewtypeVariant(_, _, inner) => {
                visitor.visit_newtype_struct(self.forward(inner)?)
            }
            Schema::Sequence(item) => self.do_deserialize_seq(item, visitor),
            Schema::Map(key, value) => self.do_deserialize_map(key, value, visitor),
            Schema::Tuple(_, field_schemas)
            | Schema::TupleStruct(_, _, field_schemas)
            | Schema::TupleVariant(_, _, _, field_schemas) => {
                self.do_deserialize_tuple(field_schemas, visitor)
            }
            Schema::Struct(_, field_names, field_schemas)
            | Schema::StructVariant(_, _, field_names, field_schemas) => {
                self.do_deserialize_struct(field_names, field_schemas, visitor)
            }
            Schema::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_any { visitor })
            }
            Schema::Skip => self.invalid_type_error(&visitor),
        }
    }

    deserialize_simple! { deserialize_bool, Schema::Bool }
    deserialize_simple! { deserialize_i8, @integer }
    deserialize_simple! { deserialize_i16, @integer }
    deserialize_simple! { deserialize_i32, @integer }
    deserialize_simple! { deserialize_i64, @integer }
    deserialize_simple! { deserialize_i128, @integer }
    deserialize_simple! { deserialize_u8, @integer }
    deserialize_simple! { deserialize_u16, @integer }
    deserialize_simple! { deserialize_u32, @integer }
    deserialize_simple! { deserialize_u64, @integer }
    deserialize_simple! { deserialize_u128, @integer }
    deserialize_simple! { deserialize_f32, @float }
    deserialize_simple! { deserialize_f64, @float }
    deserialize_simple! { deserialize_char, Schema::Char }

    // TODO: schema evolution string <-> bytes
    deserialize_simple! { deserialize_str, Schema::String }
    deserialize_simple! { deserialize_string, Schema::String }
    deserialize_simple! { deserialize_bytes, Schema::Bytes }
    deserialize_simple! { deserialize_byte_buf, Schema::Bytes }
    deserialize_simple! { deserialize_unit, Schema::Unit | Schema::UnitStruct(_) | Schema::UnitVariant(_, _) }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.schema {
            Schema::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_option { visitor })
            }
            Schema::OptionNone => self
                .inner
                .deserialize_unit(IgnoredAny)
                .and_then(|_| visitor.visit_none()),
            Schema::OptionSome(inner) => visitor.visit_some(self.forward(inner)?),
            Schema::Skip => self.invalid_type_error(&visitor),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.schema {
            Schema::Union(variants) => self.deserialize_union(
                variants,
                deferred::deserialize_newtype_struct { name, visitor },
            ),
            Schema::OptionSome(inner) => self
                .forward(inner)?
                .deserialize_newtype_struct(name, visitor),
            Schema::NewtypeStruct(_, inner) | Schema::NewtypeVariant(_, _, inner) => {
                visitor.visit_newtype_struct(self.forward(inner)?)
            }
            Schema::Skip => self.invalid_type_error(&visitor),
            _ => visitor.visit_newtype_struct(self),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.schema {
            Schema::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_seq { visitor })
            }

            Schema::NewtypeStruct(_, inner)
            | Schema::NewtypeVariant(_, _, inner)
            | Schema::OptionSome(inner) => self.forward(inner)?.deserialize_seq(visitor),

            Schema::Sequence(item) => self.do_deserialize_seq(item, visitor),
            Schema::Tuple(_, field_schemas)
            | Schema::TupleStruct(_, _, field_schemas)
            | Schema::TupleVariant(_, _, _, field_schemas) => {
                self.do_deserialize_tuple(field_schemas, visitor)
            }
            _ => self.invalid_type_error(&visitor),
        }
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.schema {
            Schema::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_tuple { len, visitor })
            }

            Schema::NewtypeStruct(_, inner)
            | Schema::NewtypeVariant(_, _, inner)
            | Schema::OptionSome(inner) => self.forward(inner)?.deserialize_tuple(len, visitor),

            Schema::Tuple(_, field_schemas)
            | Schema::TupleStruct(_, _, field_schemas)
            | Schema::TupleVariant(_, _, _, field_schemas) => {
                self.do_deserialize_tuple(field_schemas, visitor)
            }

            _ => self.invalid_type_error(&visitor),
        }
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.schema {
            Schema::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_map { visitor })
            }

            Schema::NewtypeStruct(_, inner)
            | Schema::NewtypeVariant(_, _, inner)
            | Schema::OptionSome(inner) => self.forward(inner)?.deserialize_map(visitor),

            Schema::Map(key, value) => self.do_deserialize_map(key, value, visitor),
            Schema::Struct(_, field_names, field_schemas)
            | Schema::StructVariant(_, _, field_names, field_schemas) => {
                self.do_deserialize_struct(field_names, field_schemas, visitor)
            }

            _ => self.invalid_type_error(&visitor),
        }
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.schema {
            Schema::Union(variants) => self.deserialize_union(
                variants,
                deferred::deserialize_struct {
                    name,
                    fields,
                    visitor,
                },
            ),

            Schema::NewtypeStruct(_, inner)
            | Schema::NewtypeVariant(_, _, inner)
            | Schema::OptionSome(inner) => self
                .forward(inner)?
                .deserialize_struct(name, fields, visitor),

            Schema::Struct(_, field_names, field_schemas)
            | Schema::StructVariant(_, _, field_names, field_schemas) => {
                self.do_deserialize_struct(field_names, field_schemas, visitor)
            }
            Schema::Map(key, value) => self.do_deserialize_map(key, value, visitor),

            _ => self.invalid_type_error(&visitor),
        }
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.schema {
            Schema::Union(schemas) => self.deserialize_union(
                schemas,
                deferred::deserialize_enum {
                    name,
                    variants,
                    visitor,
                },
            ),

            Schema::NewtypeStruct(_, inner) | Schema::OptionSome(inner) => self
                .forward(inner)?
                .deserialize_enum(name, variants, visitor),

            Schema::Skip => self.invalid_type_error(&visitor),
            _ => visitor.visit_enum(self),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.schema {
            Schema::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_identifier { visitor })
            }
            Schema::OptionSome(inner) => self.forward(inner)?.deserialize_identifier(visitor),
            Schema::UnitVariant(_, variant)
            | Schema::TupleVariant(_, variant, _, _)
            | Schema::StructVariant(_, variant, _, _) => {
                visitor.visit_str(self.root.name(variant).map_err(Self::Error::custom)?)
            }
            _ => self.invalid_type_error(&visitor),
        }
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_any(IgnoredAny)?;
        visitor.visit_unit()
    }

    #[inline]
    fn is_human_readable(&self) -> bool {
        false
    }
}

impl<'s, 'de, DeserializerT> EnumAccess<'de> for SchemaDeserializer<'s, DeserializerT>
where
    DeserializerT: Deserializer<'de>,
{
    type Error = DeserializerT::Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        match self.schema {
            Schema::UnitVariant(_, variant)
            | Schema::NewtypeVariant(_, variant, _)
            | Schema::StructVariant(_, variant, _, _) => seed
                .deserialize(NameDeserializer {
                    name: self
                        .root
                        .name(variant)
                        .map_err(DeserializerT::Error::custom)?,
                    phantom: PhantomData,
                })
                .map(|value| (value, self)),
            _ => self.invalid_type_error(&"enum variant"),
        }
    }
}

impl<'s, 'de, DeserializerT> VariantAccess<'de> for SchemaDeserializer<'s, DeserializerT>
where
    DeserializerT: Deserializer<'de>,
{
    type Error = DeserializerT::Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        self.deserialize_unit(IgnoredAny).map(|_| ())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        seed.deserialize(self)
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_tuple(len, visitor)
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_struct("", fields, visitor)
    }
}

pub struct SchemaSeqDeserializer<'schema, InnerT> {
    root: &'schema RootSchema,
    item: Schema,
    inner: InnerT,
}

impl<'schema, 'de, VisitorT> serde::de::Visitor<'de> for SchemaSeqDeserializer<'schema, VisitorT>
where
    VisitorT: serde::de::Visitor<'de>,
{
    type Value = VisitorT::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.expecting(formatter)
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        self.inner.visit_seq(SchemaSeqDeserializer {
            root: self.root,
            item: self.item,
            inner: seq,
        })
    }
}

impl<'schema, 'de, SeqAccessT> SeqAccess<'de> for SchemaSeqDeserializer<'schema, SeqAccessT>
where
    SeqAccessT: SeqAccess<'de>,
{
    type Error = SeqAccessT::Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        self.inner.next_element_seed(SchemaDeserializer {
            root: self.root,
            schema: self.item,
            inner: seed,
        })
    }
}

pub struct SchemaMapDeserializer<'schema, InnerT> {
    root: &'schema RootSchema,
    key: Schema,
    value: Schema,
    inner: InnerT,
}

impl<'schema, 'de, VisitorT> serde::de::Visitor<'de> for SchemaMapDeserializer<'schema, VisitorT>
where
    VisitorT: serde::de::Visitor<'de>,
{
    type Value = VisitorT::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.expecting(formatter)
    }

    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        self.inner.visit_map(SchemaMapDeserializer {
            root: self.root,
            key: self.key,
            value: self.value,
            inner: map,
        })
    }
}

impl<'schema, 'de, MapAccessT> MapAccess<'de> for SchemaMapDeserializer<'schema, MapAccessT>
where
    MapAccessT: MapAccess<'de>,
{
    type Error = MapAccessT::Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        self.inner.next_key_seed(SchemaDeserializer {
            root: self.root,
            schema: self.key,
            inner: seed,
        })
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        self.inner.next_value_seed(SchemaDeserializer {
            root: self.root,
            schema: self.value,
            inner: seed,
        })
    }

    #[inline]
    fn next_entry_seed<K, V>(
        &mut self,
        kseed: K,
        vseed: V,
    ) -> Result<Option<(K::Value, V::Value)>, Self::Error>
    where
        K: DeserializeSeed<'de>,
        V: DeserializeSeed<'de>,
    {
        self.inner.next_entry_seed(
            SchemaDeserializer {
                root: self.root,
                schema: self.key,
                inner: kseed,
            },
            SchemaDeserializer {
                root: self.root,
                schema: self.value,
                inner: vseed,
            },
        )
    }
}

pub struct SchemaTupleDeserializer<'schema, InnerT> {
    root: &'schema RootSchema,
    items: &'schema [SchemaIndex],
    inner: InnerT,
}

impl<'schema, 'de, VisitorT> serde::de::Visitor<'de> for SchemaTupleDeserializer<'schema, VisitorT>
where
    VisitorT: serde::de::Visitor<'de>,
{
    type Value = VisitorT::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.expecting(formatter)
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        self.inner.visit_seq(SchemaTupleDeserializer {
            root: self.root,
            items: self.items,
            inner: seq,
        })
    }
}

impl<'schema, 'de, SeqAccessT> SeqAccess<'de> for SchemaTupleDeserializer<'schema, SeqAccessT>
where
    SeqAccessT: SeqAccess<'de>,
{
    type Error = SeqAccessT::Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if let Some(&schema) = self.items.split_off_first() {
            self.inner.next_element_seed(SchemaDeserializer {
                root: self.root,
                schema: self.root.schema(schema).map_err(Self::Error::custom)?,
                inner: seed,
            })
        } else {
            Ok(None)
        }
    }
}

pub struct SchemaStructDeserializer<'schema, InnerT> {
    root: &'schema RootSchema,
    field_names: &'schema [NameIndex],
    field_schemas: &'schema [SchemaIndex],
    next_value_schema: Option<Schema>,
    inner: InnerT,
}

impl<'schema, 'de, SeqAccessT> SchemaStructDeserializer<'schema, SeqAccessT>
where
    SeqAccessT: SeqAccess<'de>,
{
    fn next(&mut self) -> Result<Option<(&'schema str, Schema)>, SeqAccessT::Error> {
        while let (Some(&name), Some(&schema)) = (
            self.field_names.split_off_first(),
            self.field_schemas.split_off_first(),
        ) {
            let name = self.root.name(name).map_err(SeqAccessT::Error::custom)?;
            let schema = self
                .root
                .schema(schema)
                .map_err(SeqAccessT::Error::custom)?;

            // Resolve embedded discriminants for unions, skipping over skipped fields.
            let schema_list = match schema {
                // Skipped field, continue outer loop.
                Schema::Skip => {
                    self.inner.next_element::<()>()?;
                    continue;
                }

                // A union field which has its discriminant embedded in the struct tuple.
                Schema::Union(schema_list) => schema_list,

                // A non-union field, directly serialized in the struct tuple.
                schema => return Ok(Some((name, schema))),
            };
            let schema_list = self
                .root
                .schema_list(schema_list)
                .map_err(SeqAccessT::Error::custom)?;

            // Read the embedded discriminant from the tuple and pick the correct schema out of the
            // union.
            let embedded_discriminant = self.inner.next_element::<u32>()?.ok_or_else(|| {
                SeqAccessT::Error::custom("missing embedded discriminant in struct")
            })?;
            let schema = *usize::try_from(embedded_discriminant)
                .ok()
                .and_then(|discriminant| schema_list.get(discriminant))
                .ok_or_else(|| SeqAccessT::Error::custom("bad embedded discriminant for field"))?;
            let schema = self
                .root
                .schema(schema)
                .map_err(SeqAccessT::Error::custom)?;

            return Ok(Some((name, schema)));
        }

        Ok(None)
    }
}

impl<'schema, 'de, VisitorT> serde::de::Visitor<'de> for SchemaStructDeserializer<'schema, VisitorT>
where
    VisitorT: serde::de::Visitor<'de>,
{
    type Value = VisitorT::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.expecting(formatter)
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        self.inner.visit_map(SchemaStructDeserializer {
            root: self.root,
            field_names: self.field_names,
            field_schemas: self.field_schemas,
            inner: seq,
            next_value_schema: None,
        })
    }
}

impl<'schema, 'de, SeqAccessT> MapAccess<'de> for SchemaStructDeserializer<'schema, SeqAccessT>
where
    SeqAccessT: SeqAccess<'de>,
{
    type Error = SeqAccessT::Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        match self.next() {
            Ok(Some((name, schema))) => {
                self.next_value_schema = Some(schema);
                seed.deserialize(NameDeserializer {
                    name,
                    phantom: PhantomData,
                })
                .map(Some)
            }
            Ok(None) => Ok(None),
            Err(error) => Err(error),
        }
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        self.inner
            .next_element_seed(SchemaDeserializer {
                root: self.root,
                schema: self
                    .next_value_schema
                    .take()
                    .expect("called next_value_seed without next_key_seed in struct"),
                inner: seed,
            })?
            .ok_or_else(|| Self::Error::custom("missing value for key"))
    }

    #[inline]
    fn next_entry_seed<K, V>(
        &mut self,
        kseed: K,
        vseed: V,
    ) -> Result<Option<(K::Value, V::Value)>, Self::Error>
    where
        K: DeserializeSeed<'de>,
        V: DeserializeSeed<'de>,
    {
        match self.next() {
            Ok(Some((name, schema))) => {
                self.next_value_schema = Some(schema);
                let key = kseed.deserialize(NameDeserializer {
                    name,
                    phantom: PhantomData,
                })?;
                let value = self
                    .inner
                    .next_element_seed(SchemaDeserializer {
                        root: self.root,
                        schema,
                        inner: vseed,
                    })?
                    .ok_or_else(|| Self::Error::custom("missing value for key"))?;
                Ok(Some((key, value)))
            }
            Ok(None) => Ok(None),
            Err(error) => Err(error),
        }
    }
}

pub struct NameDeserializer<'schema, ErrorT> {
    name: &'schema str,
    phantom: PhantomData<ErrorT>,
}

impl<'schema, 'de, ErrorT> Deserializer<'de> for NameDeserializer<'schema, ErrorT>
where
    ErrorT: serde::de::Error,
{
    type Error = ErrorT;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_str(self.name)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
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

const NUM_ANONYMOUS_NAMES: usize = 4096;
static UNION_ENUM_NAME: &str = "Union";
static UNION_VARIANT_NAMES: LazyLock<&[&str]> = LazyLock::new(|| {
    let mut names = Vec::with_capacity(NUM_ANONYMOUS_NAMES);
    for i_anonymous in 0..NUM_ANONYMOUS_NAMES {
        names.push(&*format!("V_{:04X}", i_anonymous).leak());
    }
    names.leak()
});
