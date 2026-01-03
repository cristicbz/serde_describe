use serde::Serialize;
use std::fmt::{Display, Write};
use thiserror::Error;

use crate::{
    SchemaBuilder, TraceError,
    indices::{
        FieldNameListIndex, IndexIsEmpty, IsEmpty, MemberListIndex, SchemaNodeIndex,
        SchemaNodeListIndex, TypeNameIndex, VariantNameIndex,
    },
    schema::{
        NoSuchFieldListError, NoSuchFieldNameError, NoSuchFieldNameListError, NoSuchNodeListError,
        NoSuchSchemaError, NoSuchTypeNameError, NoSuchVariantNameError, Schema, SchemaNode,
    },
};

impl std::fmt::Display for Schema {
    /// Produces a large, human-readable representation of the schema.
    ///
    /// The output of this method is not stable.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut context = if formatter.alternate() {
            DumpContext::multiline()
        } else {
            DumpContext::oneline()
        };
        if let Err(error) = self.recursive_dump(&mut context, self.root_index) {
            if formatter.alternate() {
                write!(formatter, "<invalid schema: {error}; debug: {self:#?}>")
            } else {
                write!(formatter, "<invalid schema: {error}; debug: {self:?}>")
            }
        } else {
            formatter.write_str(&context.output)
        }
    }
}

impl Schema {
    /// Produces a large, human-readable representation of the schema for a given value.
    ///
    /// The output of this method is not stable.
    pub fn display_for_value<SerializeT>(value: &SerializeT) -> Result<impl Display, TraceError>
    where
        SerializeT: Serialize,
    {
        let mut builder = SchemaBuilder::new();
        let _ = builder.trace(&value)?;
        builder.build()
    }

    fn recursive_dump(
        &self,
        context: &mut DumpContext,
        index: SchemaNodeIndex,
    ) -> Result<(), DumpError> {
        let node = self.node(index)?;
        let index = usize::from(index);
        match node {
            SchemaNode::Bool => write!(context, "bool_{index}")?,
            SchemaNode::I8 => write!(context, "i8_{index}")?,
            SchemaNode::I16 => write!(context, "i16_{index}")?,
            SchemaNode::I32 => write!(context, "i32_{index}")?,
            SchemaNode::I64 => write!(context, "i64_{index}")?,
            SchemaNode::I128 => write!(context, "i128_{index}")?,
            SchemaNode::U8 => write!(context, "u8_{index}")?,
            SchemaNode::U16 => write!(context, "u16_{index}")?,
            SchemaNode::U32 => write!(context, "u32_{index}")?,
            SchemaNode::U64 => write!(context, "u64_{index}")?,
            SchemaNode::U128 => write!(context, "u128_{index}")?,
            SchemaNode::F32 => write!(context, "f32_{index}")?,
            SchemaNode::F64 => write!(context, "f64_{index}")?,
            SchemaNode::Char => write!(context, "char_{index}")?,
            SchemaNode::String => write!(context, "str_{index}")?,
            SchemaNode::Bytes => write!(context, "bytes_{index}")?,
            SchemaNode::Unit => write!(context, "unit_{index}")?,

            SchemaNode::UnitStruct(name) => write!(context, "{}_{}", self.type_name(name)?, index)?,
            SchemaNode::UnitVariant(name, variant) => write!(
                context,
                "{}::{}_{}",
                self.type_name(name)?,
                self.variant_name(variant)?,
                index,
            )?,

            SchemaNode::OptionNone => write!(context, "none_{index}")?,
            SchemaNode::OptionSome(inner) => {
                self.dump_children(context, "some", None, &[inner], index)?
            }

            SchemaNode::NewtypeStruct(name, inner) => {
                self.dump_children(context, self.type_name(name)?, None, &[inner], index)?
            }
            SchemaNode::NewtypeVariant(name, variant, inner) => {
                self.dump_children(
                    context,
                    self.type_name(name)?,
                    Some(self.variant_name(variant)?),
                    &[inner],
                    index,
                )?;
            }
            SchemaNode::Map(key, value) => {
                self.dump_children(context, "map", None, &[key, value], index)?
            }
            SchemaNode::Sequence(item) => {
                self.dump_children(context, "seq", None, &[item], index)?
            }

            SchemaNode::Tuple(type_list) => {
                self.dump_children_list(context, "tuple", None, type_list, index)?;
            }

            SchemaNode::TupleStruct(name, type_list) => {
                self.dump_children_list(context, self.type_name(name)?, None, type_list, index)?;
            }
            SchemaNode::TupleVariant(name, variant, type_list) => {
                self.dump_children_list(
                    context,
                    self.type_name(name)?,
                    Some(self.variant_name(variant)?),
                    type_list,
                    index,
                )?;
            }

            SchemaNode::Struct(name, field_names, skip_list, field_types) => self.dump_struct(
                context,
                name,
                None,
                field_names,
                field_types,
                skip_list,
                index,
            )?,
            SchemaNode::StructVariant(name, variant, field_names, skip_list, field_types) => self
                .dump_struct(
                context,
                name,
                Some(variant),
                field_names,
                field_types,
                skip_list,
                index,
            )?,

            SchemaNode::Union(SchemaNodeListIndex::EMPTY) => write!(context, "!")?,
            SchemaNode::Union(type_list) => {
                self.dump_children_list(context, "union", None, type_list, index)?
            }
        };
        Ok(())
    }

    fn dump_children_list(
        &self,
        context: &mut DumpContext,
        name: &str,
        variant: Option<&str>,
        children: SchemaNodeListIndex,
        index: usize,
    ) -> Result<(), DumpError> {
        self.dump_children(context, name, variant, self.node_list(children)?, index)
    }

    fn dump_children(
        &self,
        context: &mut DumpContext,
        name: &str,
        variant: Option<&str>,
        children: &[SchemaNodeIndex],
        index: usize,
    ) -> Result<(), DumpError> {
        context.check_wrapping(|context| {
            context.write_str(name)?;
            if let Some(variant) = variant {
                context.write_str("::")?;
                context.write_str(variant)?;
            }
            write!(context, "_{index}(")?;
            context.write_newline()?;
            context.nest();
            for (i_child, &node) in children.iter().enumerate() {
                context.write_indent()?;
                self.recursive_dump(context, node)?;
                context.write_comma(i_child == children.len() - 1)?;
            }
            context.unnest();
            context.write_indent()?;
            context.write_char(')')?;
            Ok(())
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn dump_struct(
        &self,
        context: &mut DumpContext,
        name: TypeNameIndex,
        variant: Option<VariantNameIndex>,
        field_names: FieldNameListIndex,
        field_types: SchemaNodeListIndex,
        skip_list: MemberListIndex,
        index: usize,
    ) -> Result<(), DumpError> {
        context.check_wrapping(|context| {
            context.write_str(self.type_name(name)?)?;
            if let Some(variant) = variant {
                context.write_str("::")?;
                context.write_str(self.variant_name(variant)?)?;
            }
            write!(context, "_{index} {{")?;
            context.write_newline()?;
            context.nest();
            context.write_indent_or_space()?;
            let mut skips = self.member_list(skip_list)?;
            let field_names = self.field_name_list(field_names)?;
            for (i_field, (&name, &node)) in field_names
                .iter()
                .zip(self.node_list(field_types)?)
                .enumerate()
            {
                if i_field > 0 {
                    context.write_indent()?;
                }
                context.write_str(self.field_name(name)?)?;
                if let Some(&i_next_skip) = skips.first()
                    && usize::from(i_next_skip) == i_field
                {
                    skips.split_off_first();
                    context.write_char('?')?;
                } else if node.is_empty() {
                    context.write_char('?')?;
                };
                context.write_str(": ")?;
                self.recursive_dump(context, node)?;
                context.write_comma(i_field == field_names.len() - 1)?;
            }
            context.unnest();
            context.write_indent_or_space()?;
            context.write_char('}')?;
            Ok(())
        })
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

    #[error("dump formatting error: {0}")]
    Fmt(#[from] std::fmt::Error),
}

struct DumpContext {
    newlines: bool,
    max_line_length: usize,
    indent: String,
    output: String,
    check_line_length: bool,
    line_length: usize,
}

impl DumpContext {
    pub fn multiline() -> Self {
        Self {
            newlines: true,
            max_line_length: 80,
            indent: String::new(),
            output: String::new(),
            check_line_length: false,
            line_length: 0,
        }
    }

    pub fn oneline() -> Self {
        Self {
            newlines: false,
            max_line_length: usize::MAX,
            indent: String::new(),
            output: String::new(),
            check_line_length: false,
            line_length: 0,
        }
    }

    pub fn check_wrapping<ErrorT>(
        &mut self,
        mut dump: impl FnMut(&mut Self) -> Result<(), ErrorT>,
    ) -> Result<(), ErrorT> {
        if self.max_line_length == usize::MAX || self.check_line_length || !self.newlines {
            return dump(self);
        }
        let mut child = Self {
            newlines: false,
            max_line_length: self.max_line_length,
            indent: self.indent.clone(),
            output: String::new(),
            check_line_length: true,
            line_length: self.line_length,
        };
        if dump(&mut child).is_err() {
            dump(self)
        } else {
            self.output.push_str(&child.output);
            self.line_length = child.line_length;
            self.indent = child.indent;
            Ok(())
        }
    }

    pub fn nest(&mut self) {
        self.indent.push_str("  ");
    }

    pub fn unnest(&mut self) {
        self.indent.truncate(self.indent.len() - 2);
    }

    pub fn write_indent(&mut self) -> std::fmt::Result {
        if self.newlines {
            if self.check_line_length && self.line_length + self.indent.len() > self.max_line_length
            {
                return Err(std::fmt::Error);
            }
            self.output.push_str(&self.indent);
        }
        Ok(())
    }

    pub fn write_indent_or_space(&mut self) -> std::fmt::Result {
        if self.newlines {
            self.write_indent()
        } else {
            self.write_char(' ')
        }
    }

    pub fn write_newline(&mut self) -> std::fmt::Result {
        if self.newlines {
            // Write directly to output to bypass line length logic.
            writeln!(&mut self.output)?;
            self.line_length = 0;
        }
        Ok(())
    }

    pub fn write_comma(&mut self, trailing: bool) -> std::fmt::Result {
        if self.newlines {
            self.write_char(',')?;
            self.write_newline()?;
        } else if !trailing {
            self.write_str(", ")?;
        }
        Ok(())
    }
}

impl std::fmt::Write for DumpContext {
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::fmt::Result {
        let old = self.output.len();
        self.output.write_fmt(args)?;
        self.line_length += self.output.len() - old;
        if self.check_line_length && self.line_length > self.max_line_length {
            self.output.truncate(old);
            Err(std::fmt::Error)
        } else {
            Ok(())
        }
    }

    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        if self.check_line_length && self.line_length + s.len() > self.max_line_length {
            Err(std::fmt::Error)
        } else {
            self.line_length += s.len();
            self.output.push_str(s);
            Ok(())
        }
    }

    fn write_char(&mut self, c: char) -> std::fmt::Result {
        if self.check_line_length && self.line_length + c.len_utf8() > self.max_line_length {
            Err(std::fmt::Error)
        } else {
            self.line_length += c.len_utf8();
            self.output.push(c);
            Ok(())
        }
    }
}
