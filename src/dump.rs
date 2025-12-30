use std::fmt::Write;
use thiserror::Error;

use crate::{
    indices::SchemaNodeIndex,
    schema::{
        NoSuchFieldListError, NoSuchFieldNameError, NoSuchFieldNameListError, NoSuchNodeListError,
        NoSuchSchemaError, NoSuchTypeNameError, NoSuchVariantNameError, Schema, SchemaNode,
    },
};

impl Schema {
    /// Produces a large, debugging-oriented representation of the schema.
    ///
    /// The output of this method is not stable.
    pub fn dump(&self) -> String {
        let mut output = String::new();
        self.recursive_dump(&mut String::new(), self.root_index, &mut output)
            .unwrap();
        output
    }

    fn recursive_dump(
        &self,
        indent: &mut String,
        index: SchemaNodeIndex,
        output: &mut String,
    ) -> Result<(), DumpError> {
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
            | SchemaNode::Unit => {
                writeln!(output, "{indent}{node:?},").expect("string output");
            }
            SchemaNode::OptionNone => {
                writeln!(output, "{indent}::Option::None").expect("string output");
            }

            SchemaNode::UnitStruct(name) => {
                writeln!(output, "{}{},", indent, self.type_name(name)?).expect("string output");
            }
            SchemaNode::UnitVariant(name, variant) => {
                writeln!(
                    output,
                    "{}{}::{},",
                    indent,
                    self.type_name(name)?,
                    self.variant_name(variant)?
                )
                .expect("string output");
            }

            SchemaNode::OptionSome(inner) => {
                writeln!(output, "{}::Option::Some(", indent).expect("string output");
                self.recursive_dump(indent, inner, output)?;
                writeln!(output, "{indent}),").expect("string output");
            }
            SchemaNode::NewtypeStruct(name, inner) => {
                writeln!(output, "{}{}(", indent, self.type_name(name)?).expect("string output");
                self.recursive_dump(indent, inner, output)?;
                writeln!(output, "{indent}),").expect("string output");
            }
            SchemaNode::NewtypeVariant(name, variant, inner) => {
                writeln!(
                    output,
                    "{}{}::{}(",
                    indent,
                    self.type_name(name)?,
                    self.variant_name(variant)?
                )
                .expect("string output");
                self.recursive_dump(indent, inner, output)?;
                writeln!(output, "{indent}),").expect("string output");
            }
            SchemaNode::Map(key, value) => {
                writeln!(output, "{indent}{{").expect("string output");
                self.recursive_dump(indent, key, output)?;
                self.recursive_dump(indent, value, output)?;
                writeln!(output, "{indent}}},").expect("string output");
            }
            SchemaNode::Sequence(item) => {
                writeln!(output, "{indent}[").expect("string output");
                self.recursive_dump(indent, item, output)?;
                writeln!(output, "{indent}],").expect("string output")
            }

            SchemaNode::Tuple(_, schema_list) => {
                writeln!(output, "{indent}(").expect("string output");
                for &node in self.node_list(schema_list)? {
                    self.recursive_dump(indent, node, output)?;
                }
                writeln!(output, "{indent}),").expect("string output");
            }

            SchemaNode::TupleStruct(name, _, schema_list) => {
                writeln!(output, "{}{}(", indent, self.type_name(name)?).expect("string output");
                for &node in self.node_list(schema_list)? {
                    self.recursive_dump(indent, node, output)?;
                }
                writeln!(output, "{indent}),").expect("string output");
            }
            SchemaNode::TupleVariant(name, variant, _, schema_list) => {
                writeln!(
                    output,
                    "{}{}::{}(",
                    indent,
                    self.type_name(name)?,
                    self.variant_name(variant)?
                )
                .expect("string output");
                for &node in self.node_list(schema_list)? {
                    self.recursive_dump(indent, node, output)?;
                }
                writeln!(output, "{indent}),").expect("string output");
            }

            SchemaNode::Struct(name, name_list, skip_list, type_list) => {
                writeln!(output, "{}{} {{", indent, self.type_name(name)?).expect("string output");
                indent.push_str("  ");
                let mut skips = self.member_list(skip_list)?;
                let has_skips = !skips.is_empty();
                for (i_field, (&name, &node)) in self
                    .field_name_list(name_list)?
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
                        writeln!(output, "{}{}{}:", indent, self.field_name(name)?, required)
                            .expect("string output");
                    } else {
                        writeln!(output, "{}{}:", indent, self.field_name(name)?)
                            .expect("string output");
                    }
                    self.recursive_dump(indent, node, output)?;
                }
                indent.truncate(indent.len() - 2);
                writeln!(output, "{indent}}},").expect("string output");
            }
            SchemaNode::StructVariant(name, variant, name_list, skip_list, type_list) => {
                writeln!(
                    output,
                    "{}{}::{} {{",
                    indent,
                    self.type_name(name)?,
                    self.variant_name(variant)?
                )
                .expect("string output");
                indent.push_str("  ");
                let mut skips = self.member_list(skip_list)?;
                let has_skips = !skips.is_empty();
                for (i_field, (&name, &node)) in self
                    .field_name_list(name_list)?
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
                        writeln!(
                            output,
                            "{}{} [{}]:",
                            indent,
                            self.field_name(name)?,
                            required
                        )
                        .expect("string output");
                    } else {
                        writeln!(output, "{}{}:", indent, self.field_name(name)?)
                            .expect("string output");
                    }
                    self.recursive_dump(indent, node, output)?;
                }
                indent.truncate(indent.len() - 2);
                writeln!(output, "{indent}}},").expect("string output");
            }

            SchemaNode::Union(type_list) => {
                writeln!(output, "{indent}<").expect("string output");
                indent.push_str("  ");
                for &node in self.node_list(type_list)? {
                    self.recursive_dump(indent, node, output)?;
                }
                indent.truncate(indent.len() - 2);
                writeln!(output, "{indent}>,").expect("string output");
            }
        };
        indent.truncate(indent.len() - 2);
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Error)]
pub(crate) enum DumpError {
    #[error("dump error: {0}")]
    FieldName(#[from] NoSuchFieldNameError),

    #[error("dump error: {0}")]
    TypeName(#[from] NoSuchTypeNameError),

    #[error("dump error: {0}")]
    VariantName(#[from] NoSuchVariantNameError),

    #[error("dump error: {0}")]
    NameList(#[from] NoSuchFieldNameListError),

    #[error("dump error: {0}")]
    Schema(#[from] NoSuchSchemaError),

    #[error("dump error: {0}")]
    SchemaList(#[from] NoSuchNodeListError),

    #[error("dump error: {0}")]
    FieldList(#[from] NoSuchFieldListError),
}
