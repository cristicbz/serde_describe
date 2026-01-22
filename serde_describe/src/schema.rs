use serde::{Deserialize, Deserializer, Serialize, de::DeserializeSeed};
use std::{hash::Hash, marker::PhantomData};

use crate::{
    DescribedBy,
    indices::{
        FieldNameIndex, FieldNameListIndex, IndexIsEmpty, IsEmpty, MemberIndex, MemberListIndex,
        SchemaNodeIndex, SchemaNodeListIndex, TypeNameIndex, VariantNameIndex,
    },
    pool::{ReadonlyNonEmptyPool, ReadonlyPool},
    trace::Trace,
};

/// A saved schema that describes serialized data in a non-self-describing format.
///
/// Produced via a [`SchemaBuilder`][`crate::SchemaBuilder`] which traces the various serialized
/// types, see its documentation for a complete example. Crucially, a given [`Schema`] can only be
/// used to serialize the data that was traced by the same
/// [`SchemaBuilder`][`crate::SchemaBuilder`] that produced it.
///
/// For simple use-cases where the [`Schema`] should be serialized together with the data, use
/// the [`SelfDescribed`][`crate::SelfDescribed`] wrapper, which obviates the need for an
/// explicitly managed [`Schema`] object.
#[derive(Clone, Debug)]
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
    /// Deserializes a value that was previously serialized with [`Self::describe_trace`].
    ///
    /// If you don't need a shared schema, use the much simpler [`crate::SelfDescribed`] wrapper
    /// wrapper.
    pub fn deserialize_described<'schema, 'de, DeserializeT, DeserializerT>(
        &'schema self,
        deserializer: DeserializerT,
    ) -> Result<DeserializeT, DeserializerT::Error>
    where
        DeserializeT: Deserialize<'de>,
        DeserializerT: Deserializer<'de>,
    {
        Ok(DescribedBy(PhantomData, self).deserialize(deserializer)?.0)
    }

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

    /// Returns a [`serde::Serialize`]-able wrapper for a [`Trace`][`crate::Trace`].
    ///
    /// You will need to provide the schema again at deserialization-time using
    /// [`Self::describe_type`] or [`Self::describe_seed`].
    pub fn describe_trace<'schema>(&'schema self, trace: Trace) -> DescribedBy<'schema, Trace> {
        DescribedBy(trace, self)
    }

    /// Returns a [`serde::Serialize`]-able wrapper for a reference to a
    /// [`Trace`][`crate::Trace`].
    ///
    /// You will need to provide the schema again at deserialization-time using
    /// [`Self::describe_type`] or [`Self::describe_seed`].
    pub fn describe_trace_ref<'schema, 'trace>(
        &'schema self,
        value: &'trace Trace,
    ) -> DescribedBy<'schema, &'trace Trace> {
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
    /// let trace = builder.trace(&10u32)?;
    /// let schema = builder.build()?;
    ///
    /// let serialized = postcard::to_stdvec(
    ///     &schema.describe_trace(trace)
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

#[derive(Clone, Copy, Debug)]
pub(crate) struct NoSuchFieldNameError(FieldNameIndex);

impl std::fmt::Display for NoSuchFieldNameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "no such field name with index {:?}", self.0)
    }
}

impl std::error::Error for NoSuchFieldNameError {}

#[derive(Clone, Copy, Debug)]
pub(crate) struct NoSuchTypeNameError(TypeNameIndex);

impl std::fmt::Display for NoSuchTypeNameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "no such type name with index {:?}", self.0)
    }
}

impl std::error::Error for NoSuchTypeNameError {}

#[derive(Clone, Copy, Debug)]
pub(crate) struct NoSuchVariantNameError(VariantNameIndex);

impl std::fmt::Display for NoSuchVariantNameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "no such variant name with index {:?}", self.0)
    }
}

impl std::error::Error for NoSuchVariantNameError {}

#[derive(Clone, Copy, Debug)]
pub(crate) struct NoSuchFieldNameListError(FieldNameListIndex);

impl std::fmt::Display for NoSuchFieldNameListError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "no such field name list with index {:?}", self.0)
    }
}

impl std::error::Error for NoSuchFieldNameListError {}

#[derive(Clone, Copy, Debug)]
pub(crate) struct NoSuchSchemaError(SchemaNodeIndex);

impl std::fmt::Display for NoSuchSchemaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "no such node with index {:?}", self.0)
    }
}

impl std::error::Error for NoSuchSchemaError {}

#[derive(Clone, Copy, Debug)]
pub(crate) struct NoSuchNodeListError(SchemaNodeListIndex);

impl std::fmt::Display for NoSuchNodeListError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "no such node list with index {:?}", self.0)
    }
}

impl std::error::Error for NoSuchNodeListError {}

#[derive(Clone, Copy, Debug)]
pub(crate) struct NoSuchFieldListError(MemberListIndex);

impl std::fmt::Display for NoSuchFieldListError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "no such field list with index {:?}", self.0)
    }
}

impl std::error::Error for NoSuchFieldListError {}

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

    Tuple(SchemaNodeListIndex),
    TupleStruct(TypeNameIndex, SchemaNodeListIndex),
    TupleVariant(TypeNameIndex, VariantNameIndex, SchemaNodeListIndex),

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

#[derive(Serialize)]
enum VersionedSchemaSerializeProxy<'a> {
    V0 {
        root_index: SchemaNodeIndex,
        nodes: &'a ReadonlyPool<SchemaNode, SchemaNodeIndex>,
        node_lists: &'a ReadonlyPool<Box<[SchemaNodeIndex]>, SchemaNodeListIndex>,
        member_lists: &'a ReadonlyPool<Box<[MemberIndex]>, MemberListIndex>,
        field_name_lists: &'a ReadonlyNonEmptyPool<Box<[FieldNameIndex]>, FieldNameListIndex>,
        field_names: &'a ReadonlyNonEmptyPool<Box<str>, FieldNameIndex>,
        variant_names: &'a ReadonlyNonEmptyPool<Box<str>, VariantNameIndex>,
        type_names: &'a ReadonlyNonEmptyPool<Box<str>, TypeNameIndex>,
    },
}

#[derive(Deserialize)]
enum VersionedSchemaDeserializeProxy {
    V0 {
        root_index: SchemaNodeIndex,
        nodes: ReadonlyPool<SchemaNode, SchemaNodeIndex>,
        node_lists: ReadonlyPool<Box<[SchemaNodeIndex]>, SchemaNodeListIndex>,
        member_lists: ReadonlyPool<Box<[MemberIndex]>, MemberListIndex>,
        field_name_lists: ReadonlyNonEmptyPool<Box<[FieldNameIndex]>, FieldNameListIndex>,
        field_names: ReadonlyNonEmptyPool<Box<str>, FieldNameIndex>,
        variant_names: ReadonlyNonEmptyPool<Box<str>, VariantNameIndex>,
        type_names: ReadonlyNonEmptyPool<Box<str>, TypeNameIndex>,
    },
}

impl Serialize for Schema {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        VersionedSchemaSerializeProxy::V0 {
            root_index: self.root_index,
            nodes: &self.nodes,
            node_lists: &self.node_lists,
            member_lists: &self.member_lists,
            field_name_lists: &self.field_name_lists,
            field_names: &self.field_names,
            variant_names: &self.variant_names,
            type_names: &self.type_names,
        }
        .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Schema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        match VersionedSchemaDeserializeProxy::deserialize(deserializer)? {
            VersionedSchemaDeserializeProxy::V0 {
                root_index,
                nodes,
                node_lists,
                member_lists,
                field_name_lists,
                field_names,
                variant_names,
                type_names,
            } => Ok(Self {
                root_index,
                nodes,
                node_lists,
                member_lists,
                field_name_lists,
                field_names,
                variant_names,
                type_names,
            }),
        }
    }
}
