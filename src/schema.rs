use serde::{de::DeserializeSeed, Deserialize, Serialize};
use std::{hash::Hash, marker::PhantomData};
use thiserror::Error;

use crate::{
    builder::Value,
    indices::{
        FieldNameIndex, FieldNameListIndex, IndexIsEmpty, IsEmpty, MemberIndex, MemberListIndex,
        SchemaNodeIndex, SchemaNodeListIndex, TypeNameIndex, VariantNameIndex,
    },
    pool::{ReadonlyNonEmptyPool, ReadonlyPool},
    DescribedBy,
};

/// A saved schema that describes serialized data in a non-self-describing format.
///
/// Produced via a [`crate::SchemaBuilder`] which traces the various serialized types, see that
/// type's documentation for a complete example.
///
/// For simple use-cases where the [`Schema`] should be serialized together with the data, use
/// the [`crate::SelfDescribed`] wrapper, which obviates the need for an explicitly exposed
/// [`Schema`] object.
#[derive(Clone, Serialize, Deserialize)]
pub struct Schema {
    pub(crate) root_index: SchemaNodeIndex,
    pub(crate) nodes: ReadonlyPool<SchemaNode, SchemaNodeIndex>,
    pub(crate) node_lists: ReadonlyPool<Box<[SchemaNodeIndex]>, SchemaNodeListIndex>,
    pub(crate) member_lists: ReadonlyPool<Box<[MemberIndex]>, MemberListIndex>,
    pub(crate) field_name_lists: ReadonlyNonEmptyPool<Box<[FieldNameIndex]>, FieldNameListIndex>,
    pub(crate) field_names: ReadonlyNonEmptyPool<Box<str>, FieldNameIndex>,
    pub(crate) variant_names: ReadonlyNonEmptyPool<Box<str>, VariantNameIndex>,
    pub(crate) type_names: ReadonlyNonEmptyPool<Box<str>, TypeNameIndex>,
}

impl Schema {
    /// Returns a [`serde::de::DeserializeSeed`] for a value to be deserialized using this schema.
    ///
    /// If you don't need a shared schema, use the much simpler [`crate::SelfDescribed`] wrapper
    /// wrapper.
    pub fn describe_type<'schema, 'de, T>(&'schema self) -> DescribedBy<'schema, PhantomData<T>>
    where
        T: Deserialize<'de>,
    {
        DescribedBy(PhantomData, self)
    }

    /// Returns a [`serde::Serialize`]-able wrapper for a [`crate::Value`].
    ///
    /// You will need to provide the schema again at deserialization-time using
    /// [`Self::describe_type`] or [`Self::describe_seed`].
    pub fn describe_value<'schema>(&'schema self, value: Value) -> DescribedBy<'schema, Value> {
        DescribedBy(value, self)
    }

    /// Returns a [`serde::Serialize`]-able wrapper for a reference to a [`crate::Value`]..
    ///
    /// You will need to provide the schema again at deserialization-time using
    /// [`Self::describe_type`] or [`Self::describe_seed`].
    pub fn describe_value_ref<'schema, 'value>(
        &'schema self,
        value: &'value Value,
    ) -> DescribedBy<'schema, &'value Value> {
        DescribedBy(value, self)
    }

    /// Wraps a [`serde::de::DeserializeSeed`] to be deserialized using this schema.
    ///
    /// If you don't need your own seed, you can use [`Self::describe_type`] instead.
    ///
    /// Example
    /// -------
    /// ```rust
    /// use serde::de::{Deserialize, Deserializer, DeserializeSeed};
    /// use serde_describe::{SchemaBuilder, DescribedBy};
    ///
    /// /// Deserializes an integer by multiplying it by a given constant.
    /// pub struct Multiplier {
    ///     pub by: u32,
    /// }
    ///
    /// impl<'de> serde::de::DeserializeSeed<'de> for Multiplier {
    ///     type Value = u32;
    ///
    ///     fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    ///     where
    ///         D: Deserializer<'de>,
    ///     {
    ///         Ok(u32::deserialize(deserializer)? * self.by)
    ///     }
    /// }
    ///
    /// let mut builder = SchemaBuilder::new();
    /// let value = builder.trace_value(&10u32)?;
    /// let schema = builder.build()?;
    ///
    /// let serialized = postcard::to_stdvec(
    ///     &schema.describe_value(value)
    /// )?;
    /// let DescribedBy(deserialized, _) = schema
    ///     .describe_seed(Multiplier { by: 2 })
    ///     .deserialize(&mut postcard::Deserializer::from_bytes(&serialized))?;
    ///
    /// assert_eq!(deserialized, 20);
    ///
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn describe_seed<'schema, 'de, SeedT>(
        &'schema self,
        seed: SeedT,
    ) -> DescribedBy<'schema, SeedT>
    where
        SeedT: DeserializeSeed<'de>,
    {
        DescribedBy(seed, self)
    }

    #[inline]
    pub(crate) fn node(&self, index: SchemaNodeIndex) -> Result<SchemaNode, NoSuchSchemaError> {
        self.nodes
            .get(index)
            .copied()
            .ok_or(NoSuchSchemaError(index))
    }

    #[inline]
    pub(crate) fn field_name_list(
        &self,
        index: FieldNameListIndex,
    ) -> Result<&[FieldNameIndex], NoSuchFieldNameListError> {
        self.field_name_lists
            .get(index)
            .map(|list| &**list)
            .ok_or(NoSuchFieldNameListError(index))
    }

    #[inline]
    pub(crate) fn node_list(
        &self,
        index: SchemaNodeListIndex,
    ) -> Result<&[SchemaNodeIndex], NoSuchNodeListError> {
        self.node_lists.get(index).ok_or(NoSuchNodeListError(index))
    }

    #[inline]
    pub(crate) fn member_list(
        &self,
        index: MemberListIndex,
    ) -> Result<&[MemberIndex], NoSuchFieldListError> {
        self.member_lists
            .get(index)
            .ok_or(NoSuchFieldListError(index))
    }

    #[inline]
    pub(crate) fn field_name(&self, index: FieldNameIndex) -> Result<&str, NoSuchFieldNameError> {
        self.field_names
            .get(index)
            .map(|list| &**list)
            .ok_or(NoSuchFieldNameError(index))
    }

    #[inline]
    pub(crate) fn variant_name(
        &self,
        index: VariantNameIndex,
    ) -> Result<&str, NoSuchVariantNameError> {
        self.variant_names
            .get(index)
            .map(|list| &**list)
            .ok_or(NoSuchVariantNameError(index))
    }

    #[inline]
    pub(crate) fn type_name(&self, index: TypeNameIndex) -> Result<&str, NoSuchTypeNameError> {
        self.type_names
            .get(index)
            .map(|string| &**string)
            .ok_or(NoSuchTypeNameError(index))
    }
}

#[derive(Clone, Copy, Debug, Error)]
#[error("no such field name with index {0:?}")]
pub(crate) struct NoSuchFieldNameError(FieldNameIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such type name with index {0:?}")]
pub(crate) struct NoSuchTypeNameError(TypeNameIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such variant name with index {0:?}")]
pub(crate) struct NoSuchVariantNameError(VariantNameIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such field name list with index {0:?}")]
pub(crate) struct NoSuchFieldNameListError(FieldNameListIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such node with index {0:?}")]
pub(crate) struct NoSuchSchemaError(SchemaNodeIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such node list with index {0:?}")]
pub(crate) struct NoSuchNodeListError(SchemaNodeListIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such field list with index {0:?}")]
pub(crate) struct NoSuchFieldListError(MemberListIndex);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub(crate) enum SchemaNode {
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
    OptionSome(SchemaNodeIndex),

    Unit,
    UnitStruct(TypeNameIndex),
    UnitVariant(TypeNameIndex, VariantNameIndex),

    NewtypeStruct(TypeNameIndex, SchemaNodeIndex),
    NewtypeVariant(TypeNameIndex, VariantNameIndex, SchemaNodeIndex),

    Sequence(SchemaNodeIndex),
    Map(SchemaNodeIndex, SchemaNodeIndex),

    Tuple(u32, SchemaNodeListIndex),
    TupleStruct(TypeNameIndex, u32, SchemaNodeListIndex),
    TupleVariant(TypeNameIndex, VariantNameIndex, u32, SchemaNodeListIndex),

    Struct(
        TypeNameIndex,
        FieldNameListIndex,
        MemberListIndex,
        SchemaNodeListIndex,
    ),
    StructVariant(
        TypeNameIndex,
        VariantNameIndex,
        FieldNameListIndex,
        MemberListIndex,
        SchemaNodeListIndex,
    ),

    Union(SchemaNodeListIndex),
}

impl IsEmpty for SchemaNode {
    type Borrowed = Self;
    const BORROWED_EMPTY: &Self::Borrowed = &SchemaNode::Union(SchemaNodeListIndex::EMPTY);

    fn is_empty(&self) -> bool {
        matches!(self, Self::BORROWED_EMPTY)
    }
}
