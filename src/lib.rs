use indexmap::{Equivalent, IndexSet};
use serde::{
    ser::{
        SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant, SerializeTuple,
        SerializeTupleStruct, SerializeTupleVariant,
    },
    Deserialize, Serialize, Serializer,
};
use std::{hash::Hash, marker::PhantomData, ops::Index};
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
    None,
    Some(Box<SchemaBuilder>),

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
                    left_inner.unify(*right_inner)
                } else {
                    Err(SchemaBuilder::Newtype(right_name, right_inner))
                }
            }
            (SchemaBuilder::Some(left), SchemaBuilder::Some(right)) => left.unify(*right),
            (
                SchemaBuilder::Record {
                    name: left_name,
                    field_names: left_field_names,
                    field_types: left_field_types,
                },
                SchemaBuilder::Record {
                    name: right_name,
                    field_names: right_field_names,
                    field_types: right_field_types,
                },
            ) => {
                if (&*left_name, &*left_field_names, left_field_types.len())
                    == (&right_name, &right_field_names, right_field_types.len())
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
            let left = std::mem::replace(self, SchemaBuilder::Union(Vec::new()));
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
pub struct TypeName(pub NameIndex, pub Option<NameIndex>);

impl Default for SchemaBuilder {
    fn default() -> Self {
        Self::Union(Vec::new())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
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

    None,
    Some(SchemaIndex),

    Unit,
    UnitStruct(NameIndex),
    UnitVariant(NameIndex, NameIndex),

    NewtypeStruct(NameIndex, SchemaIndex),
    NewtypeVariant(NameIndex, NameIndex, SchemaIndex),

    Sequence(SchemaIndex),
    Map(SchemaIndex, SchemaIndex),

    Tuple(SchemaListIndex),
    TupleStruct(NameIndex, SchemaListIndex),
    TupleVariant(NameIndex, NameIndex, SchemaListIndex),

    Struct(NameIndex, NameListIndex, SchemaListIndex),
    StructVariant(NameIndex, NameIndex, NameListIndex, SchemaListIndex),

    Union(SchemaListIndex),
    Skip,
}

macro_rules! u32_indices {
    ($($index_ty:ident => $error:ident,)+) => {
        $(
            #[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $index_ty(u32);

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
                    match u32::try_from(value) {
                        Ok(index) => Ok($index_ty(index)),
                        _ => Err(SerError::$error),
                    }
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
    PartialU32Index => TooManyValues,
    PartialStringIndex => TooManyStrings,
    PartialCharIndex => TooManyStrings,
    PartialByteStringIndex => TooManyByteStrings,
    PartialByteIndex => TooManyByteStrings,
    PartialBytes => TooManyByteStrings,
    PartialValueIndex => TooManyValues,
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

    #[error("too many strings for u32")]
    TooManyStrings,

    #[error("too many byte strings for u32")]
    TooManyByteStrings,

    #[error("too many values for u32")]
    TooManyValues,

    #[error("attempted to serialize map key without value")]
    UnpairedMapKey,

    #[error("attempted to serialize map value without key")]
    UnpairedMapValue,

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
    data: ValueDataBuilder,
    names: Pool<&'static str, NameIndex>,
    name_lists: Pool<Box<[NameIndex]>, NameListIndex>,
    schemas: Pool<Schema, SchemaIndex>,
    schema_lists: Pool<Box<[SchemaIndex]>, SchemaListIndex>,
    root: SchemaBuilder,
}

#[derive(Default, Clone, Serialize)]
pub struct ValueDataBuilder {
    traces: Vec<Trace>,
    names: Vec<NameIndex>,
    bytes: Vec<u8>,
    chars: String,
}

#[derive(Clone)]
pub struct Value {
    pub schema: RootSchema,
    pub root_index: SchemaIndex,
    pub data: ValueDataBuilder,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct RootSchema {
    schemas: Box<[Schema]>,
    names: Box<[Box<str>]>,
    name_lists: Box<[Box<[NameIndex]>]>,
    schema_lists: Box<[Box<[SchemaIndex]>]>,
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
            SchemaBuilder::None => Schema::None,
            SchemaBuilder::Some(inner) => {
                let inner = inner.build(root)?;
                Schema::Some(inner)
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
            } => {
                let field_types = field_types
                    .into_iter()
                    .map(|schema| schema.build(root))
                    .collect::<Result<Vec<_>, _>>()?;
                let field_types = root.schema_lists.intern_from(field_types)?;
                match (name, field_names) {
                    (None, None) => Schema::Tuple(field_types),
                    (Some(TypeName(name, None)), None) => Schema::TupleStruct(name, field_types),
                    (Some(TypeName(name, Some(variant))), None) => {
                        Schema::TupleVariant(name, variant, field_types)
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
        Ok(Value {
            schema,
            root_index,
            data: self.data,
        })
    }

    fn push_struct_name(&mut self, name: &'static str) -> Result<TypeName, SerError> {
        let name = self.names.intern(name)?;
        self.data.names.push(name);
        Ok(TypeName(name, None))
    }

    fn push_variant_name(
        &mut self,
        name: &'static str,
        variant: &'static str,
    ) -> Result<TypeName, SerError> {
        let name = self.names.intern(name)?;
        let variant = self.names.intern(variant)?;
        self.data.names.extend([name, variant]);
        Ok(TypeName(name, Some(variant)))
    }

    fn push_field_name(&mut self, name: &'static str) -> Result<NameIndex, SerError> {
        let name = self.names.intern(name)?;
        self.data.names.push(name);
        Ok(name)
    }

    fn push_trace(&mut self, trace: Trace) {
        self.data.traces.push(trace);
    }

    fn reserve_integer<T>(&mut self) -> Result<PartialByteIndex, SerError> {
        self.reserve_bytes(std::mem::size_of::<T>())
    }

    fn reserve_bytes(&mut self, size: usize) -> Result<PartialByteIndex, SerError> {
        let index = PartialByteIndex::try_from(self.data.bytes.len())?;
        self.data.bytes.extend(std::iter::repeat_n(!0, size));
        Ok(index)
    }

    fn fill_reserved_bytes(&mut self, index: PartialByteIndex, data: &[u8]) {
        self.data.bytes[index.into()..][..data.len()].copy_from_slice(data);
    }
}

macro_rules! fn_serialize_as_u8 {
    ($(($fn_name:ident, $value_type:ty, $schema:ident),)+) => {
        $(
            fn $fn_name(self, value: $value_type) -> Result<Self::Ok, Self::Error> {
                self.data.bytes.push(value as u8);
                self.push_trace(Trace::$schema);
                Ok(SchemaBuilder::$schema)
            }
        )+
    };
}

macro_rules! fn_serialize_as_le_bytes {
    ($(($fn_name:ident, $value_type:ty, $schema:ident ),)+) => {
        $(
            fn $fn_name(self, value: $value_type) -> Result<Self::Ok, Self::Error> {

                self.data.bytes.extend_from_slice(&value.to_le_bytes());
                self.push_trace(Trace::$schema);
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
    None,
    Some,

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

    Reserved = 255,
}

impl Trace {
    const ALL: [Self; 26] = [
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
        Self::None,
        Self::Some,
        Self::UnitStruct,
        Self::UnitVariant,
        Self::NewtypeVariant,
        Self::Sequence,
        Self::TupleStruct,
        Self::TupleVariant,
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
        self.data.chars.push(value);
        Ok(SchemaBuilder::Char)
    }

    fn serialize_str(self, value: &str) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::String);
        self.data
            .bytes
            .extend_from_slice(&value.len().to_le_bytes());
        self.data.chars.push_str(value);
        Ok(SchemaBuilder::String)
    }

    fn serialize_bytes(self, value: &[u8]) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::Bytes);
        self.data
            .bytes
            .extend_from_slice(&value.len().to_le_bytes());
        self.data.bytes.extend_from_slice(value);
        Ok(SchemaBuilder::Bytes)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::None);
        Ok(SchemaBuilder::None)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.push_trace(Trace::Some);
        T::serialize(value, &mut *self).map(|schema| SchemaBuilder::Some(Box::new(schema)))
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
        variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::UnitVariant);
        self.data
            .bytes
            .extend_from_slice(&variant_index.to_le_bytes());
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
        variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.push_trace(Trace::NewtypeVariant);
        self.data
            .bytes
            .extend_from_slice(&variant_index.to_le_bytes());
        Ok(SchemaBuilder::Newtype(
            self.push_variant_name(name, variant)?,
            Box::new(T::serialize(value, &mut *self)?),
        ))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        self.push_trace(Trace::Sequence);
        Ok(SequenceSchemaBuilder {
            reserved_length: self.reserve_integer::<usize>()?,
            schema: SchemaBuilder::default(),
            length: 0,
            parent: self,
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.push_trace(Trace::Tuple);
        self.data.bytes.extend_from_slice(&len.to_le_bytes());
        Ok(TupleSchemaBuilder {
            name: None,
            schemas: Vec::with_capacity(len),
            parent: self,
        })
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.push_trace(Trace::TupleStruct);
        self.data.bytes.extend_from_slice(&len.to_le_bytes());
        Ok(TupleSchemaBuilder {
            name: Some(self.push_struct_name(name)?),
            schemas: Vec::with_capacity(len),
            parent: self,
        })
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.push_trace(Trace::TupleVariant);
        self.data
            .bytes
            .extend_from_slice(&variant_index.to_le_bytes());
        self.data.bytes.extend_from_slice(&len.to_le_bytes());
        Ok(TupleSchemaBuilder {
            name: Some(self.push_variant_name(name, variant)?),
            schemas: Vec::with_capacity(len),
            parent: self,
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        self.push_trace(Trace::Map);
        Ok(MapSchemaBuilder {
            reserved_length: self.reserve_integer::<usize>()?,
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
        self.data.bytes.extend_from_slice(&len.to_le_bytes());
        Ok(StructSchemaBuilder {
            name: self.push_struct_name(name)?,
            fields: Vec::with_capacity(len),
            parent: self,
        })
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.push_trace(Trace::StructVariant);
        self.data
            .bytes
            .extend_from_slice(&variant_index.to_le_bytes());
        self.data.bytes.extend_from_slice(&len.to_le_bytes());
        Ok(StructSchemaBuilder {
            name: self.push_variant_name(name, variant)?,
            fields: Vec::with_capacity(len),
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
    reserved_length: PartialByteIndex,
    schema: SchemaBuilder,
    length: usize,
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
    reserved_length: PartialByteIndex,
    key_schema: SchemaBuilder,
    value_schema: SchemaBuilder,
    length: usize,
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
    fields: Vec<(NameIndex, SchemaBuilder)>,
    parent: &'a mut RootSchemaBuilder,
}

impl SerializeStruct for StructSchemaBuilder<'_> {
    type Ok = SchemaBuilder;
    type Error = SerError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.fields.push((
            self.parent.push_field_name(key)?,
            T::serialize(value, &mut *self.parent)?,
        ));
        Ok(())
    }

    fn skip_field(&mut self, key: &'static str) -> Result<(), Self::Error> {
        self.fields
            .push((self.parent.push_field_name(key)?, SchemaBuilder::Skip));
        self.parent.push_trace(Trace::Skip);
        Ok(())
    }

    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.fields
            .sort_unstable_by_key(|&(name_index, _)| self.parent.names[name_index]);
        let (field_names, field_types): (Vec<_>, Vec<_>) = self.fields.into_iter().unzip();
        let field_names = Some(self.parent.name_lists.intern_from(field_names)?);
        Ok(SchemaBuilder::Record {
            name: Some(self.name),
            field_names,
            field_types,
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
