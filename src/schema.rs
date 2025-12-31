use serde::{
    de::{DeserializeSeed, Error as _},
    ser::Error as _,
    Deserialize, Serialize,
};
use std::{hash::Hash, marker::PhantomData};
use thiserror::Error;
use zerocopy::transmute_ref;

use crate::{
    builder::Value,
    errors::SerError,
    indices::{
        FieldIndex, FieldListIndex, NameIndex, NameListIndex, SchemaNodeIndex, SchemaNodeListIndex,
    },
    pool::Pool,
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
#[derive(Clone, Debug)]
pub struct Schema {
    pub(crate) root_index: SchemaNodeIndex,

    pub(crate) name_lists_offset: u32,
    pub(crate) field_lists_offset: u32,
    pub(crate) names_offset: u32,

    pub(crate) nodes: Box<[SchemaNode]>,

    // A concatenation of:
    // <NODE_LIST_OFFSETS>
    // <NAME_LIST_OFFSETS>
    // <FIELD_LIST_OFFSETS>
    // <NAME_OFFSETS>
    // <NODE_LIST_INDICES>
    // <NAME_LIST_INDICES>
    // <FIELD_LIST_INDICES>
    pub(crate) integers: Box<[u32]>,

    // A concatenation of:
    // <NAMES>
    pub(crate) chars: Box<str>,
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

    pub(crate) fn new(
        root_index: SchemaNodeIndex,
        nodes: Pool<SchemaNode, SchemaNodeIndex>,
        node_lists: &Pool<Box<[SchemaNodeIndex]>, SchemaNodeListIndex>,
        name_lists: &Pool<Box<[NameIndex]>, NameListIndex>,
        field_lists: &Pool<Box<[FieldIndex]>, FieldListIndex>,
        names: &Pool<&'static str, NameIndex>,
    ) -> Result<Self, SerError> {
        let name_lists_offset = u32::from(SchemaNodeListIndex::try_from(node_lists.len())?);
        let field_lists_offset = u32::from(NameListIndex::try_from(
            node_lists.len() + name_lists.len(),
        )?);
        let names_offset = u32::from(FieldListIndex::try_from(
            node_lists.len() + name_lists.len() + field_lists.len() + 1,
        )?);

        let num_offsets =
            node_lists.len() + name_lists.len() + field_lists.len() + 1 + names.len() + 1;
        let num_node_list_indices = node_lists.iter().map(|list| list.len()).sum::<usize>();
        let num_name_list_indices = name_lists.iter().map(|list| list.len()).sum::<usize>();
        let num_field_list_indices = field_lists.iter().map(|list| list.len()).sum::<usize>();
        let num_integers =
            num_offsets + num_node_list_indices + num_name_list_indices + num_field_list_indices;
        SchemaNodeIndex::try_from(num_integers)?;

        let mut integers = Vec::with_capacity(num_integers);
        integers.extend(
            // node_list, name_list and field_list offsets (in `integers`)
            node_lists
                .iter()
                .map(|list| list.len())
                .chain(name_lists.iter().map(|list| list.len()))
                .chain(field_lists.iter().map(|list| list.len()))
                // An extra "0-length" list, to get a value (start, end) offset-pair for the last list.
                .chain(std::iter::once(0))
                .scan(num_offsets, |offset, length| {
                    let current = u32::try_from(*offset).expect("checked offsets ahead of time");
                    *offset += length;
                    Some(current)
                })
                .chain(
                    // name offsets (in `chars`)
                    names
                        .iter()
                        .map(|string| string.len())
                        // An extra "0-length" string, to get a value (start, end) offset-pair for the last name.
                        .chain(std::iter::once(0))
                        .scan(0, |offset, length| {
                            let current =
                                u32::try_from(*offset).expect("checked offsets ahead of time");
                            *offset += length;
                            Some(current)
                        }),
                )
                .chain(
                    // the indices in node_lists, name_lists and field_lists
                    node_lists
                        .iter()
                        .flat_map(|list| list.iter().copied().map(u32::from))
                        .chain(
                            name_lists
                                .iter()
                                .flat_map(|list| list.iter().copied().map(u32::from)),
                        )
                        .chain(
                            field_lists
                                .iter()
                                .flat_map(|list| list.iter().copied().map(u32::from)),
                        ),
                ),
        );

        let num_chars = names.iter().copied().map(str::len).sum();
        let mut chars = String::with_capacity(num_chars);
        chars.extend(names.iter().copied());

        assert_eq!(chars.len(), chars.capacity());
        assert_eq!(integers.len(), integers.capacity());

        Ok(Self {
            root_index,
            name_lists_offset,
            field_lists_offset,
            names_offset,
            nodes: nodes.into_iter().collect(),
            integers: integers.into(),
            chars: chars.into(),
        })
    }

    #[inline]
    pub(crate) fn node(&self, index: SchemaNodeIndex) -> Result<SchemaNode, NoSuchSchemaError> {
        self.nodes
            .get(usize::from(index))
            .copied()
            .ok_or(NoSuchSchemaError(index))
    }

    #[inline]
    pub(crate) fn node_list(
        &self,
        index: SchemaNodeListIndex,
    ) -> Result<&[SchemaNodeIndex], NoSuchSchemaListError> {
        match self.integer_list(0, index.into()) {
            Some(list) => Ok(transmute_ref!(list)),
            None => Err(NoSuchSchemaListError(index)),
        }
    }

    #[inline]
    pub(crate) fn name(&self, index: NameIndex) -> Result<&str, NoSuchNameError> {
        match self.index_pair(self.names_offset, index.into()) {
            Some((start, end)) if start <= end && end <= self.chars.len() => {
                Ok(&self.chars[start..end])
            }
            _ => Err(NoSuchNameError(index)),
        }
    }

    #[inline]
    pub(crate) fn name_list(
        &self,
        index: NameListIndex,
    ) -> Result<&[NameIndex], NoSuchNameListError> {
        match self.integer_list(self.name_lists_offset, index.into()) {
            Some(list) => Ok(transmute_ref!(list)),
            None => Err(NoSuchNameListError(index)),
        }
    }

    #[inline]
    pub(crate) fn field_list(
        &self,
        index: FieldListIndex,
    ) -> Result<&[FieldIndex], NoSuchFieldListError> {
        match self.integer_list(self.field_lists_offset, index.into()) {
            Some(list) => Ok(transmute_ref!(list)),
            None => Err(NoSuchFieldListError(index)),
        }
    }

    #[inline]
    fn index_pair(&self, offset: u32, index: u32) -> Option<(usize, usize)> {
        let i_start = usize::try_from(offset + index).expect("usize is at least 32-bits");
        let i_end = i_start + 1;

        if i_end < self.integers.len() {
            let start = usize::try_from(self.integers[i_start]).expect("usize is at least 32-bits");
            let end = usize::try_from(self.integers[i_end]).expect("usize is at least 32-bits");
            if start <= end {
                return Some((start, end));
            }
        }

        None
    }

    #[inline]
    fn integer_list(&self, offset: u32, index: u32) -> Option<&[u32]> {
        match self.index_pair(offset, index) {
            Some((start, end)) if end <= self.integers.len() => Some(&self.integers[start..end]),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Error)]
#[error("no such name with index {0:?}")]
pub(crate) struct NoSuchNameError(NameIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such name list with index {0:?}")]
pub(crate) struct NoSuchNameListError(NameListIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such node with index {0:?}")]
pub(crate) struct NoSuchSchemaError(SchemaNodeIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such node list with index {0:?}")]
pub(crate) struct NoSuchSchemaListError(SchemaNodeListIndex);

#[derive(Clone, Copy, Debug, Error)]
#[error("no such field list with index {0:?}")]
pub(crate) struct NoSuchFieldListError(FieldListIndex);

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
    UnitStruct(NameIndex),
    UnitVariant(NameIndex, NameIndex),

    NewtypeStruct(NameIndex, SchemaNodeIndex),
    NewtypeVariant(NameIndex, NameIndex, SchemaNodeIndex),

    Sequence(SchemaNodeIndex),
    Map(SchemaNodeIndex, SchemaNodeIndex),

    Tuple(u32, SchemaNodeListIndex),
    TupleStruct(NameIndex, u32, SchemaNodeListIndex),
    TupleVariant(NameIndex, NameIndex, u32, SchemaNodeListIndex),

    Struct(
        NameIndex,
        NameListIndex,
        FieldListIndex,
        SchemaNodeListIndex,
    ),
    StructVariant(
        NameIndex,
        NameIndex,
        NameListIndex,
        FieldListIndex,
        SchemaNodeListIndex,
    ),

    Union(SchemaNodeListIndex),
}

#[derive(Serialize)]
#[serde(rename = "SchemaProxy")]
enum SchemaSerializeProxy<'a> {
    V0 {
        root_index: SchemaNodeIndex,
        nodes: &'a [SchemaNode],
        integers: &'a [u32],
        chars: &'a str,
    },
}

impl<'a> From<&'a Schema> for SchemaSerializeProxy<'a> {
    #[inline]
    fn from(value: &'a Schema) -> Self {
        Self::V0 {
            root_index: value.root_index,
            nodes: &value.nodes,
            integers: &value.integers,
            chars: &value.chars,
        }
    }
}

#[derive(Deserialize)]
#[serde(rename = "SchemaProxy")]
enum SchemaDeserializeProxy {
    V0 {
        root_index: SchemaNodeIndex,
        nodes: Box<[SchemaNode]>,
        integers: Box<[u32]>,
        chars: Box<str>,
    },
}

impl TryFrom<SchemaDeserializeProxy> for Schema {
    type Error = String;

    #[inline]
    fn try_from(value: SchemaDeserializeProxy) -> Result<Self, Self::Error> {
        match value {
            SchemaDeserializeProxy::V0 {
                root_index,
                nodes,
                integers,
                chars,
            } => {
                let counts = SchemaCounts::from(&*nodes);
                Ok(Self {
                    root_index,
                    name_lists_offset: counts.num_node_lists,
                    field_lists_offset: (counts.num_node_lists + counts.num_name_lists),
                    names_offset: (counts.num_node_lists
                        + counts.num_name_lists
                        + counts.num_field_lists
                        + 1),
                    nodes,
                    integers,
                    chars,
                })
            }
        }
    }
}

#[derive(Default)]
struct SchemaCounts {
    num_node_lists: u32,
    num_name_lists: u32,
    num_field_lists: u32,
}

impl SchemaCounts {
    #[inline]
    fn node_list(&mut self, node_list: SchemaNodeListIndex) {
        self.num_node_lists = self.num_node_lists.max(u32::from(node_list) + 1);
    }

    #[inline]
    fn field_list(&mut self, field_list: FieldListIndex) {
        self.num_field_lists = self.num_field_lists.max(u32::from(field_list) + 1);
    }

    #[inline]
    fn name_list(&mut self, name_list: NameListIndex) {
        self.num_name_lists = self.num_name_lists.max(u32::from(name_list) + 1);
    }
}

impl From<&[SchemaNode]> for SchemaCounts {
    #[inline]
    fn from(nodes: &[SchemaNode]) -> Self {
        let mut counts = Self::default();
        for node in nodes {
            match node {
                &SchemaNode::Bool
                | &SchemaNode::I8
                | &SchemaNode::I16
                | &SchemaNode::I32
                | &SchemaNode::I64
                | &SchemaNode::I128
                | &SchemaNode::U8
                | &SchemaNode::U16
                | &SchemaNode::U32
                | &SchemaNode::U64
                | &SchemaNode::U128
                | &SchemaNode::F32
                | &SchemaNode::F64
                | &SchemaNode::Char
                | &SchemaNode::String
                | &SchemaNode::Bytes
                | &SchemaNode::OptionNone
                | &SchemaNode::OptionSome(_)
                | &SchemaNode::Sequence(_)
                | &SchemaNode::Map(_, _)
                | &SchemaNode::Unit
                | &SchemaNode::UnitStruct(_)
                | &SchemaNode::NewtypeStruct(_, _)
                | &SchemaNode::UnitVariant(_, _)
                | &SchemaNode::NewtypeVariant(_, _, _) => {}

                &SchemaNode::Tuple(_length, node_list) => {
                    counts.node_list(node_list);
                }
                &SchemaNode::TupleStruct(_name, _length, node_list) => {
                    counts.node_list(node_list);
                }
                &SchemaNode::TupleVariant(_name, _variant, _length, node_list) => {
                    counts.node_list(node_list);
                }

                &SchemaNode::Struct(_name, name_list, field_list, node_list) => {
                    counts.name_list(name_list);
                    counts.field_list(field_list);
                    counts.node_list(node_list);
                }

                &SchemaNode::StructVariant(_name, _variant, name_list, field_list, node_list) => {
                    counts.name_list(name_list);
                    counts.field_list(field_list);
                    counts.node_list(node_list);
                }

                &SchemaNode::Union(node_list) => {
                    counts.node_list(node_list);
                }
            }
        }
        counts
    }
}

impl Serialize for Schema {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SchemaSerializeProxy::from(self).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Schema {
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SchemaDeserializeProxy::deserialize(deserializer)
            .and_then(|proxy| Schema::try_from(proxy).map_err(D::Error::custom))
    }
}
