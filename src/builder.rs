use crate::{
    errors::SerError,
    indices::{
        FieldIndex, FieldListIndex, NameIndex, NameListIndex, SchemaNodeIndex, SchemaNodeListIndex,
        TraceIndex, TypeName,
    },
    pool::Pool,
    schema::{Schema, SchemaNode},
    trace::Trace,
};
use serde::{
    ser::{
        SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant, SerializeTuple,
        SerializeTupleStruct, SerializeTupleVariant, Serializer,
    },
    Deserialize, Serialize,
};

#[derive(Default, Clone)]
pub struct Value(pub(crate) Vec<u8>);

#[derive(Default, Clone)]
pub struct SchemaBuilder {
    names: Pool<&'static str, NameIndex>,
    name_lists: Pool<Box<[NameIndex]>, NameListIndex>,
    nodes: Pool<SchemaNode, SchemaNodeIndex>,
    node_lists: Pool<Box<[SchemaNodeIndex]>, SchemaNodeListIndex>,
    field_lists: Pool<Box<[FieldIndex]>, FieldListIndex>,
    root: SchemaBuilderNode,
}

impl SchemaBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn serialize_value<ValueT>(&mut self, value: &ValueT) -> Result<Value, SerError>
    where
        ValueT: Serialize,
    {
        let mut data = Vec::new();
        let new_root = value.serialize(RootSerializer {
            data: &mut data,
            names: &mut self.names,
            name_lists: &mut self.name_lists,
            schemas: &mut self.nodes,
            schema_lists: &mut self.node_lists,
            field_lists: &mut self.field_lists,
        })?;
        self.root.union(new_root);
        Ok(Value(data))
    }

    pub fn build(mut self) -> Result<Schema, SerError> {
        let schema = Schema {
            root_index: std::mem::take(&mut self.root).build(&mut self)?,
            nodes: self.nodes.into_iter().collect::<Vec<_>>().into(),
            names: self
                .names
                .into_iter()
                .map(Into::into)
                .collect::<Vec<_>>()
                .into(),
            name_lists: self.name_lists.into_iter().collect::<Vec<_>>().into(),
            node_lists: self.node_lists.into_iter().collect::<Vec<_>>().into(),
            field_lists: self.field_lists.into_iter().collect::<Vec<_>>().into(),
        };
        schema.dump(&mut String::new(), schema.root_index).unwrap();
        Ok(schema)
    }
}

pub(crate) struct RootSerializer<'a> {
    data: &'a mut Vec<u8>,
    names: &'a mut Pool<&'static str, NameIndex>,
    name_lists: &'a mut Pool<Box<[NameIndex]>, NameListIndex>,
    schemas: &'a mut Pool<SchemaNode, SchemaNodeIndex>,
    schema_lists: &'a mut Pool<Box<[SchemaNodeIndex]>, SchemaNodeListIndex>,
    field_lists: &'a mut Pool<Box<[FieldIndex]>, FieldListIndex>,
}

impl RootSerializer<'_> {
    #[inline]
    fn reborrow<'b>(&'b mut self) -> RootSerializer<'b> where {
        RootSerializer {
            data: &mut self.data,
            names: &mut self.names,
            name_lists: &mut self.name_lists,
            schemas: &mut self.schemas,
            schema_lists: &mut self.schema_lists,
            field_lists: &mut self.field_lists,
        }
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
    fn reserve_field_presence(&mut self, length: usize) -> Result<TraceIndex, SerError> {
        self.reserve_bytes(std::mem::size_of::<u32>() * length)
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

    #[inline]
    fn write_field_presence(
        &mut self,
        index: TraceIndex,
        field: FieldIndex,
    ) -> Result<TraceIndex, SerError> {
        self.fill_reserved_bytes(index, &u32::from(field).to_le_bytes());
        TraceIndex::try_from(usize::from(index) + std::mem::size_of::<u32>())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub(crate) enum SchemaBuilderNode {
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
    OptionSome(Box<SchemaBuilderNode>),

    Unit(Option<TypeName>),
    Newtype(TypeName, Box<SchemaBuilderNode>),

    Map(Box<SchemaBuilderNode>, Box<SchemaBuilderNode>),
    Sequence(Box<SchemaBuilderNode>),

    Union(Vec<SchemaBuilderNode>),

    /// Tuple, tuple struct, tuple variant, struct or struct variant.
    Record {
        name: Option<TypeName>,
        field_names: Option<NameListIndex>,
        field_types: Vec<SchemaBuilderNode>,
        skippable: Vec<FieldIndex>,
        length: u32,
    },
}

impl SchemaBuilderNode {
    fn unify(&mut self, other: Self) -> Result<(), Self> {
        match (&mut *self, other) {
            (SchemaBuilderNode::Union(lefts), right) => {
                if lefts.is_empty() {
                    *self = right;
                } else {
                    right.add_to_nonempty_union(lefts);
                }
                Ok(())
            }
            (left, mut right @ SchemaBuilderNode::Union(_)) => {
                std::mem::swap(left, &mut right);
                left.unify(right)
            }
            (
                SchemaBuilderNode::Newtype(left_name, left_inner),
                SchemaBuilderNode::Newtype(right_name, right_inner),
            ) => {
                if *left_name == right_name {
                    left_inner.union(*right_inner);
                    Ok(())
                } else {
                    Err(SchemaBuilderNode::Newtype(right_name, right_inner))
                }
            }
            (SchemaBuilderNode::OptionSome(left), SchemaBuilderNode::OptionSome(right)) => {
                left.union(*right);
                Ok(())
            }
            (
                SchemaBuilderNode::Record {
                    name: left_name,
                    field_names: left_field_names,
                    field_types: left_field_types,
                    skippable: left_skippable,
                    length: left_length,
                },
                SchemaBuilderNode::Record {
                    name: right_name,
                    field_names: right_field_names,
                    field_types: right_field_types,
                    skippable: right_skippable,
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
                    left_skippable.extend(right_skippable);
                    left_skippable.sort_unstable();
                    left_skippable.dedup();
                    Ok(())
                } else {
                    Err(SchemaBuilderNode::Record {
                        name: right_name,
                        field_names: right_field_names,
                        field_types: right_field_types,
                        skippable: right_skippable,
                        length: right_length,
                    })
                }
            }
            (
                SchemaBuilderNode::Map(left_keys, left_values),
                SchemaBuilderNode::Map(right_keys, right_values),
            ) => {
                left_keys.union(*right_keys);
                left_values.union(*right_values);
                Ok(())
            }
            (SchemaBuilderNode::Sequence(left), SchemaBuilderNode::Sequence(right)) => {
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
                SchemaBuilderNode::Union(schemas) => *schemas = vec![left, other],
                _ => unreachable!(),
            }
        }
    }

    fn add_to_nonempty_union(self, lefts: &mut Vec<SchemaBuilderNode>) {
        assert!(!lefts.is_empty());
        match self {
            SchemaBuilderNode::Union(rights) => {
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

impl Default for SchemaBuilderNode {
    #[inline]
    fn default() -> Self {
        Self::Union(Vec::new())
    }
}

impl SchemaBuilderNode {
    fn build(self, builder: &mut SchemaBuilder) -> Result<SchemaNodeIndex, SerError> {
        let built = match self {
            SchemaBuilderNode::Bool => SchemaNode::Bool,
            SchemaBuilderNode::I8 => SchemaNode::I8,
            SchemaBuilderNode::I16 => SchemaNode::I16,
            SchemaBuilderNode::I32 => SchemaNode::I32,
            SchemaBuilderNode::I64 => SchemaNode::I64,
            SchemaBuilderNode::I128 => SchemaNode::I128,

            SchemaBuilderNode::U8 => SchemaNode::U8,
            SchemaBuilderNode::U16 => SchemaNode::U16,
            SchemaBuilderNode::U32 => SchemaNode::U32,
            SchemaBuilderNode::U64 => SchemaNode::U64,
            SchemaBuilderNode::U128 => SchemaNode::U128,

            SchemaBuilderNode::F32 => SchemaNode::F32,
            SchemaBuilderNode::F64 => SchemaNode::F64,
            SchemaBuilderNode::Char => SchemaNode::Char,

            SchemaBuilderNode::String => SchemaNode::String,
            SchemaBuilderNode::Bytes => SchemaNode::Bytes,

            SchemaBuilderNode::OptionNone => SchemaNode::OptionNone,
            SchemaBuilderNode::OptionSome(inner) => {
                let inner = inner.build(builder)?;
                SchemaNode::OptionSome(inner)
            }
            SchemaBuilderNode::Unit(None) => SchemaNode::Unit,
            SchemaBuilderNode::Unit(Some(TypeName(name, None))) => SchemaNode::UnitStruct(name),
            SchemaBuilderNode::Unit(Some(TypeName(name, Some(variant)))) => {
                SchemaNode::UnitVariant(name, variant)
            }
            SchemaBuilderNode::Newtype(type_name, inner) => {
                let inner = inner.build(builder)?;
                match type_name {
                    TypeName(name, None) => SchemaNode::NewtypeStruct(name, inner),
                    TypeName(name, Some(variant)) => {
                        SchemaNode::NewtypeVariant(name, variant, inner)
                    }
                }
            }
            SchemaBuilderNode::Map(key, value) => {
                SchemaNode::Map(key.build(builder)?, value.build(builder)?)
            }
            SchemaBuilderNode::Sequence(item) => SchemaNode::Sequence(item.build(builder)?),
            SchemaBuilderNode::Union(variants) => {
                let mut variants = variants
                    .into_iter()
                    .map(|variant| variant.build(builder))
                    .collect::<Result<Vec<_>, _>>()?;
                variants.sort_unstable();
                variants.dedup();
                SchemaNode::Union(builder.node_lists.intern_from(variants)?)
            }
            SchemaBuilderNode::Record {
                name,
                field_names,
                field_types,
                length,
                mut skippable,
            } => {
                skippable.retain(|&index| {
                    !matches!(
                        &field_types[usize::from(index)],
                        SchemaBuilderNode::Union(variants) if variants.is_empty()
                    )
                });
                let field_types = field_types
                    .into_iter()
                    .map(|field_type| field_type.build(builder))
                    .collect::<Result<Vec<_>, _>>()?;
                let field_types = builder.node_lists.intern_from(field_types)?;
                match (name, field_names) {
                    (None, None) => SchemaNode::Tuple(length, field_types),
                    (Some(TypeName(name, None)), None) => {
                        SchemaNode::TupleStruct(name, length, field_types)
                    }
                    (Some(TypeName(name, Some(variant))), None) => {
                        SchemaNode::TupleVariant(name, variant, length, field_types)
                    }
                    (None, Some(_field_names)) => {
                        unreachable!("anonymous structs don't exist in rust!")
                    }
                    (Some(TypeName(name, None)), Some(field_names)) => {
                        let skip_list = builder.field_lists.intern_from(skippable)?;
                        SchemaNode::Struct(name, field_names, skip_list, field_types)
                    }
                    (Some(TypeName(name, Some(variant))), Some(field_names)) => {
                        let skip_list = builder.field_lists.intern_from(skippable)?;
                        SchemaNode::StructVariant(
                            name,
                            variant,
                            field_names,
                            skip_list,
                            field_types,
                        )
                    }
                }
            }
        };
        builder.nodes.intern(built)
    }
}

macro_rules! fn_serialize_as_u8 {
    ($(($fn_name:ident, $value_type:ty, $node:ident),)+) => {
        $(
            #[inline]
            fn $fn_name(mut self, value: $value_type) -> Result<Self::Ok, Self::Error> {
                self.push_trace(Trace::$node);
                self.data.push(value as u8);
                Ok(SchemaBuilderNode::$node)
            }
        )+
    };
}

macro_rules! fn_serialize_as_le_bytes {
    ($(($fn_name:ident, $value_type:ty, $node:ident ),)+) => {
        $(
            #[inline]
            fn $fn_name(mut self, value: $value_type) -> Result<Self::Ok, Self::Error> {

                self.push_trace(Trace::$node);
                self.data.extend_from_slice(&value.to_le_bytes());
                Ok(SchemaBuilderNode::$node)
            }
        )+
    };
}

impl<'a> Serializer for RootSerializer<'a> {
    type Ok = SchemaBuilderNode;
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
    fn serialize_char(mut self, value: char) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::Char);
        self.push_u32(u32::from(value));
        Ok(SchemaBuilderNode::Char)
    }

    #[inline]
    fn serialize_str(mut self, value: &str) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::String);
        self.push_length_bytes(value.as_bytes())?;
        Ok(SchemaBuilderNode::String)
    }

    #[inline]
    fn serialize_bytes(mut self, value: &[u8]) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::Bytes);
        self.push_length_bytes(value)?;
        Ok(SchemaBuilderNode::Bytes)
    }

    #[inline]
    fn serialize_none(mut self) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::OptionNone);
        Ok(SchemaBuilderNode::OptionNone)
    }

    #[inline]
    fn serialize_some<T>(mut self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.push_trace(Trace::OptionSome);
        T::serialize(value, self).map(|inner| SchemaBuilderNode::OptionSome(Box::new(inner)))
    }

    #[inline]
    fn serialize_unit(mut self) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::Unit);
        Ok(SchemaBuilderNode::Unit(None))
    }

    #[inline]
    fn serialize_unit_struct(mut self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::UnitStruct);
        Ok(SchemaBuilderNode::Unit(Some(self.push_struct_name(name)?)))
    }

    #[inline]
    fn serialize_unit_variant(
        mut self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.push_trace(Trace::UnitVariant);
        Ok(SchemaBuilderNode::Unit(Some(
            self.push_variant_name(name, variant)?,
        )))
    }

    #[inline]
    fn serialize_newtype_struct<T>(
        mut self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.push_trace(Trace::NewtypeStruct);
        Ok(SchemaBuilderNode::Newtype(
            self.push_struct_name(name)?,
            Box::new(T::serialize(value, self)?),
        ))
    }

    #[inline]
    fn serialize_newtype_variant<T>(
        mut self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.push_trace(Trace::NewtypeVariant);
        Ok(SchemaBuilderNode::Newtype(
            self.push_variant_name(name, variant)?,
            Box::new(T::serialize(value, self)?),
        ))
    }

    #[inline]
    fn serialize_seq(mut self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        self.push_trace(Trace::Sequence);
        Ok(SequenceSchemaBuilder {
            reserved_length: self.reserve_u32()?,
            item: SchemaBuilderNode::default(),
            length: 0,
            parent: self,
        })
    }

    #[inline]
    fn serialize_tuple(mut self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
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
        mut self,
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
        mut self,
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
    fn serialize_map(mut self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        self.push_trace(Trace::Map);
        Ok(MapSchemaBuilder {
            reserved_length: self.reserve_u32()?,
            key_schema: SchemaBuilderNode::default(),
            value_schema: SchemaBuilderNode::default(),
            length: 0,
            parent: self,
        })
    }

    #[inline]
    fn serialize_struct(
        mut self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.push_trace(Trace::Struct);
        StructSchemaBuilder::new(self.push_struct_name(name)?, len, self)
    }

    #[inline]
    fn serialize_struct_variant(
        mut self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.push_trace(Trace::StructVariant);
        StructSchemaBuilder::new(self.push_variant_name(name, variant)?, len, self)
    }

    #[inline]
    fn is_human_readable(&self) -> bool {
        false
    }
}

pub(crate) struct SequenceSchemaBuilder<'a> {
    parent: RootSerializer<'a>,
    reserved_length: TraceIndex,
    item: SchemaBuilderNode,
    length: u32,
}

impl<'a> SerializeSeq for SequenceSchemaBuilder<'a> {
    type Ok = SchemaBuilderNode;
    type Error = SerError;

    #[inline]
    fn serialize_element<'b, T>(&'b mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.length += 1;
        let new_schema = {
            let parent = self.parent.reborrow();
            let new_schema = T::serialize(value, parent)?;
            new_schema
        };
        self.item.union(new_schema);
        Ok(())
    }

    #[inline]
    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.parent
            .fill_reserved_bytes(self.reserved_length, &self.length.to_le_bytes());
        Ok(SchemaBuilderNode::Sequence(Box::new(self.item)))
    }
}

pub(crate) struct MapSchemaBuilder<'a> {
    parent: RootSerializer<'a>,
    reserved_length: TraceIndex,
    key_schema: SchemaBuilderNode,
    value_schema: SchemaBuilderNode,
    length: u32,
}

impl SerializeMap for MapSchemaBuilder<'_> {
    type Ok = SchemaBuilderNode;
    type Error = SerError;

    #[inline]
    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.length += 1;
        self.key_schema
            .union(T::serialize(key, self.parent.reborrow())?);
        Ok(())
    }

    #[inline]
    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value_schema
            .union(T::serialize(value, self.parent.reborrow())?);
        Ok(())
    }

    #[inline]
    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.parent
            .fill_reserved_bytes(self.reserved_length, &self.length.to_le_bytes());
        Ok(SchemaBuilderNode::Map(
            Box::new(self.key_schema),
            Box::new(self.value_schema),
        ))
    }
}

pub(crate) struct TupleSchemaBuilder<'a> {
    parent: RootSerializer<'a>,
    name: Option<TypeName>,
    schemas: Vec<SchemaBuilderNode>,
    length: u32,
}

impl SerializeTuple for TupleSchemaBuilder<'_> {
    type Ok = SchemaBuilderNode;
    type Error = SerError;

    #[inline]
    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.schemas
            .push(T::serialize(value, self.parent.reborrow())?);
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(SchemaBuilderNode::Record {
            name: self.name,
            field_names: None,
            field_types: self.schemas,
            length: self.length,
            skippable: Vec::new(),
        })
    }
}

impl SerializeTupleStruct for TupleSchemaBuilder<'_> {
    type Ok = SchemaBuilderNode;
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
    type Ok = SchemaBuilderNode;
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

pub(crate) struct StructSchemaBuilder<'a> {
    parent: RootSerializer<'a>,
    name: TypeName,
    reserved_field_name_list: TraceIndex,
    reserved_field_presence: TraceIndex,
    field_names: Vec<NameIndex>,
    field_types: Vec<SchemaBuilderNode>,
    skipped: Vec<FieldIndex>,
}

impl<'a> StructSchemaBuilder<'a> {
    pub fn new(
        name: TypeName,
        length: usize,
        mut parent: RootSerializer<'a>,
    ) -> Result<Self, SerError> {
        let reserved_field_name_list = parent.reserve_u32()?;
        parent.push_u32_length(length)?;
        Ok(Self {
            name,
            reserved_field_name_list,
            reserved_field_presence: parent.reserve_field_presence(length)?,
            field_names: Vec::with_capacity(length),
            field_types: Vec::with_capacity(length),
            skipped: Vec::new(),
            parent,
        })
    }
}

impl SerializeStruct for StructSchemaBuilder<'_> {
    type Ok = SchemaBuilderNode;
    type Error = SerError;

    #[inline]
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.reserved_field_presence = self.parent.write_field_presence(
            self.reserved_field_presence,
            FieldIndex::try_from(self.field_names.len())?,
        )?;
        self.field_names.push(self.parent.intern_field_name(key)?);
        self.field_types
            .push(T::serialize(value, self.parent.reborrow())?);
        Ok(())
    }

    #[inline]
    fn skip_field(&mut self, key: &'static str) -> Result<(), Self::Error> {
        self.skipped.push(self.field_names.len().try_into()?);
        self.field_names.push(self.parent.intern_field_name(key)?);
        self.field_types.push(SchemaBuilderNode::default());
        Ok(())
    }

    #[inline]
    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        let length = u32::try_from(self.field_names.len()).map_err(|_| SerError::TooManyValues)?;
        let field_names = Some(
            self.parent
                .fill_reserved_field_name_list(self.reserved_field_name_list, self.field_names)?,
        );
        Ok(SchemaBuilderNode::Record {
            name: Some(self.name),
            field_names,
            field_types: self.field_types,
            skippable: self.skipped,
            length,
        })
    }
}

impl SerializeStructVariant for StructSchemaBuilder<'_> {
    type Ok = SchemaBuilderNode;
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
