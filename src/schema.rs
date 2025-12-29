use serde::{Deserialize, Serialize};
use std::hash::Hash;
use thiserror::Error;

use crate::indices::{
    FieldIndex, FieldListIndex, NameIndex, NameListIndex, SchemaNodeIndex, SchemaNodeListIndex,
};

#[derive(Clone, Serialize, Deserialize)]
pub struct Schema {
    pub(crate) root_index: SchemaNodeIndex,
    pub(crate) nodes: Box<[SchemaNode]>,
    pub(crate) names: Box<[Box<str>]>,
    pub(crate) name_lists: Box<[Box<[NameIndex]>]>,
    pub(crate) node_lists: Box<[Box<[SchemaNodeIndex]>]>,
    pub(crate) field_lists: Box<[Box<[FieldIndex]>]>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum SchemaNode {
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

impl Schema {
    #[inline]
    pub(crate) fn name(&self, index: NameIndex) -> Result<&str, NoSuchNameError> {
        self.names
            .get(usize::from(index))
            .map(|string| &**string)
            .ok_or(NoSuchNameError(index))
    }

    #[inline]
    pub(crate) fn name_list(
        &self,
        index: NameListIndex,
    ) -> Result<&[NameIndex], NoSuchNameListError> {
        self.name_lists
            .get(usize::from(index))
            .map(|list| &**list)
            .ok_or(NoSuchNameListError(index))
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
        self.node_lists
            .get(usize::from(index))
            .map(|list| &**list)
            .ok_or(NoSuchSchemaListError(index))
    }

    #[inline]
    pub(crate) fn field_list(
        &self,
        index: FieldListIndex,
    ) -> Result<&[FieldIndex], NoSuchFieldListError> {
        self.field_lists
            .get(usize::from(index))
            .map(|list| &**list)
            .ok_or(NoSuchFieldListError(index))
    }

    pub(crate) fn dump(
        &self,
        indent: &mut String,
        index: SchemaNodeIndex,
    ) -> Result<(), DumpError> {
        if indent.is_empty() {
            eprintln!("SCHEMA:")
        }
        indent.push_str("  ");
        let node = self.node(index)?;
        match node {
            SchemaNode::Bool
            | SchemaNode::I8
            | SchemaNode::I16
            | SchemaNode::I32
            | SchemaNode::I64
            | SchemaNode::I128
            | SchemaNode::U8
            | SchemaNode::U16
            | SchemaNode::U32
            | SchemaNode::U64
            | SchemaNode::U128
            | SchemaNode::F32
            | SchemaNode::F64
            | SchemaNode::Char
            | SchemaNode::String
            | SchemaNode::Bytes
            | SchemaNode::Unit => eprintln!("{indent}{node:?},"),
            SchemaNode::OptionNone => eprintln!("{indent}::Option::None"),

            SchemaNode::UnitStruct(name) => eprintln!("{}{},", indent, self.name(name)?),
            SchemaNode::UnitVariant(name, variant) => {
                eprintln!("{}{}::{},", indent, self.name(name)?, self.name(variant)?)
            }

            SchemaNode::OptionSome(inner) => {
                eprintln!("{}::Option::Some(", indent);
                self.dump(indent, inner)?;
                eprintln!("{indent}),")
            }
            SchemaNode::NewtypeStruct(name, inner) => {
                eprintln!("{}{}(", indent, self.name(name)?);
                self.dump(indent, inner)?;
                eprintln!("{indent}),")
            }
            SchemaNode::NewtypeVariant(name, variant, inner) => {
                eprintln!("{}{}::{}(", indent, self.name(name)?, self.name(variant)?);
                self.dump(indent, inner)?;
                eprintln!("{indent}),")
            }
            SchemaNode::Map(key, value) => {
                eprintln!("{indent}{{");
                self.dump(indent, key)?;
                self.dump(indent, value)?;
                eprintln!("{indent}}},")
            }
            SchemaNode::Sequence(item) => {
                eprintln!("{indent}[");
                self.dump(indent, item)?;
                eprintln!("{indent}],")
            }

            SchemaNode::Tuple(_, schema_list) => {
                eprintln!("{indent}(");
                for &node in self.node_list(schema_list)? {
                    self.dump(indent, node)?;
                }
                eprintln!("{indent}),")
            }

            SchemaNode::TupleStruct(name, _, schema_list) => {
                eprintln!("{}{}(", indent, self.name(name)?);
                for &node in self.node_list(schema_list)? {
                    self.dump(indent, node)?;
                }
                eprintln!("{indent}),")
            }
            SchemaNode::TupleVariant(name, variant, _, schema_list) => {
                eprintln!("{}{}::{}(", indent, self.name(name)?, self.name(variant)?);
                for &node in self.node_list(schema_list)? {
                    self.dump(indent, node)?;
                }
                eprintln!("{indent}),")
            }

            SchemaNode::Struct(name, name_list, skip_list, type_list) => {
                eprintln!("{}{} {{", indent, self.name(name)?);
                indent.push_str("  ");
                let mut skips = self.field_list(skip_list)?;
                let has_skips = !skips.is_empty();
                for (i_field, (&name, &node)) in self
                    .name_list(name_list)?
                    .iter()
                    .zip(self.node_list(type_list)?)
                    .enumerate()
                {
                    if has_skips {
                        let required = if let Some(&i_next_skip) = skips.first()
                            && usize::from(i_next_skip) == i_field
                        {
                            skips.split_off_first();
                            "?"
                        } else {
                            ""
                        };
                        eprintln!("{}{}{}:", indent, self.name(name)?, required);
                    } else {
                        eprintln!("{}{}:", indent, self.name(name)?);
                    }
                    self.dump(indent, node)?;
                }
                indent.truncate(indent.len() - 2);
                eprintln!("{indent}}},")
            }
            SchemaNode::StructVariant(name, variant, name_list, skip_list, type_list) => {
                eprintln!("{}{}::{} {{", indent, self.name(name)?, self.name(variant)?);
                indent.push_str("  ");
                let mut skips = self.field_list(skip_list)?;
                let has_skips = !skips.is_empty();
                for (i_field, (&name, &node)) in self
                    .name_list(name_list)?
                    .iter()
                    .zip(self.node_list(type_list)?)
                    .enumerate()
                {
                    if has_skips {
                        let required = if let Some(&i_next_skip) = skips.first()
                            && usize::from(i_next_skip) == i_field
                        {
                            skips.split_off_first();
                            "optional"
                        } else {
                            "required"
                        };
                        eprintln!("{}{} [{}]:", indent, self.name(name)?, required);
                    } else {
                        eprintln!("{}{}:", indent, self.name(name)?);
                    }
                    self.dump(indent, node)?;
                }
                indent.truncate(indent.len() - 2);
                eprintln!("{indent}}},")
            }

            SchemaNode::Union(type_list) => {
                eprintln!("{indent}<");
                indent.push_str("  ");
                for &node in self.node_list(type_list)? {
                    self.dump(indent, node)?;
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

#[derive(Clone, Copy, Debug, Error)]
pub(crate) enum DumpError {
    #[error("dump error: {0}")]
    NoSuchName(#[from] NoSuchNameError),

    #[error("dump error: {0}")]
    NoSuchNameList(#[from] NoSuchNameListError),

    #[error("dump error: {0}")]
    NoSuchSchema(#[from] NoSuchSchemaError),

    #[error("dump error: {0}")]
    NoSuchSchemaList(#[from] NoSuchSchemaListError),

    #[error("dump error: {0}")]
    NoSuchFieldList(#[from] NoSuchFieldListError),
}
