use serde::Serialize;
use std::{cell::Cell, hash::Hash};

use crate::indices::{FieldNameListIndex, TypeNameIndex, VariantNameIndex};

#[derive(Copy, Debug, Clone)]
pub(crate) enum TraceNode {
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
    UnitStruct(TypeNameIndex),
    UnitVariant(TypeNameIndex, VariantNameIndex),

    NewtypeStruct(TypeNameIndex),
    NewtypeVariant(TypeNameIndex, VariantNameIndex),

    Sequence,
    Map,

    Tuple(u32),
    TupleStruct(u32, TypeNameIndex),
    TupleVariant(u32, TypeNameIndex, VariantNameIndex),

    Struct(TypeNameIndex, FieldNameListIndex),
    StructVariant(TypeNameIndex, VariantNameIndex, FieldNameListIndex),
}

/// Represents a traced serde-serialized value. Returned by
/// [`SchemaBuilder::trace`][`crate::SchemaBuilder::trace`].
///
/// Unlike e.g. `serde_json::Value`, it cannot be used by itself, it must always be used in
/// conjunction with the resulting [`Schema`][`crate::Schema`] returned by the
/// [`SchemaBuilder::build`][`crate::SchemaBuilder::build`] method of the same
/// [`SchemaBuilder`][`crate::SchemaBuilder`] used to produce the value.
#[derive(Default, Clone)]
#[must_use = "a trace is only useful if it's later serialized with the resulting schema"]
pub struct Trace(pub(crate) Vec<u8>);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize)]
#[repr(u8)]
pub enum TraceNodeKind {
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

impl TraceNodeKind {
    const ALL: [Self; 30] = [
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

impl TryFrom<u8> for TraceNodeKind {
    type Error = u8;

    #[inline]
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::ALL.get(usize::from(value)).copied().ok_or(value)
    }
}

impl From<TraceNodeKind> for u8 {
    #[inline]
    fn from(value: TraceNodeKind) -> Self {
        value as Self
    }
}

pub(crate) trait ReadTraceExt<'data> {
    fn pop_u8<ErrorT: serde::ser::Error>(&self) -> Result<u8, ErrorT>;
    fn pop_slice<ErrorT: serde::ser::Error>(&self, len: usize) -> Result<&'data [u8], ErrorT>;

    fn pop_str<ErrorT: serde::ser::Error>(&self, len: usize) -> Result<&'data str, ErrorT> {
        str::from_utf8(self.pop_slice(len)?)
            .map_err(|_| ErrorT::custom("invalid utf-8 in traced string"))
    }

    fn pop_u16<ErrorT: serde::ser::Error>(&self) -> Result<u16, ErrorT> {
        Ok(u16::from_le_bytes(
            self.pop_slice(std::mem::size_of::<u16>())?
                .try_into()
                .expect("impossible"),
        ))
    }

    fn pop_u32<ErrorT: serde::ser::Error>(&self) -> Result<u32, ErrorT> {
        Ok(u32::from_le_bytes(
            self.pop_slice(std::mem::size_of::<u32>())?
                .try_into()
                .expect("impossible"),
        ))
    }

    fn pop_u64<ErrorT: serde::ser::Error>(&self) -> Result<u64, ErrorT> {
        Ok(u64::from_le_bytes(
            self.pop_slice(std::mem::size_of::<u64>())?
                .try_into()
                .expect("impossible"),
        ))
    }

    fn pop_u128<ErrorT: serde::ser::Error>(&self) -> Result<u128, ErrorT> {
        Ok(u128::from_le_bytes(
            self.pop_slice(std::mem::size_of::<u128>())?
                .try_into()
                .expect("impossible"),
        ))
    }

    fn pop_trace_node<ErrorT: serde::ser::Error>(&self) -> Result<TraceNode, ErrorT> {
        let trace = TraceNodeKind::try_from(self.pop_u8()?)
            .map_err(|_| ErrorT::custom("bad trace node in trace"))?;
        let node = match trace {
            TraceNodeKind::Bool => TraceNode::Bool,
            TraceNodeKind::I8 => TraceNode::I8,
            TraceNodeKind::I16 => TraceNode::I16,
            TraceNodeKind::I32 => TraceNode::I32,
            TraceNodeKind::I64 => TraceNode::I64,
            TraceNodeKind::I128 => TraceNode::I128,
            TraceNodeKind::U8 => TraceNode::U8,
            TraceNodeKind::U16 => TraceNode::U16,
            TraceNodeKind::U32 => TraceNode::U32,
            TraceNodeKind::U64 => TraceNode::U64,
            TraceNodeKind::U128 => TraceNode::U128,
            TraceNodeKind::F32 => TraceNode::F32,
            TraceNodeKind::F64 => TraceNode::F64,
            TraceNodeKind::Char => TraceNode::Char,
            TraceNodeKind::String => TraceNode::String,
            TraceNodeKind::Bytes => TraceNode::Bytes,

            TraceNodeKind::OptionNone => TraceNode::None,
            TraceNodeKind::OptionSome => TraceNode::Some,

            TraceNodeKind::Unit => TraceNode::Unit,

            TraceNodeKind::UnitStruct => TraceNode::UnitStruct(self.pop_type_name()?),
            TraceNodeKind::UnitVariant => {
                TraceNode::UnitVariant(self.pop_type_name()?, self.pop_variant_name()?)
            }

            TraceNodeKind::NewtypeStruct => TraceNode::NewtypeStruct(self.pop_type_name()?),
            TraceNodeKind::NewtypeVariant => {
                TraceNode::NewtypeVariant(self.pop_type_name()?, self.pop_variant_name()?)
            }

            TraceNodeKind::Map => TraceNode::Map,
            TraceNodeKind::Sequence => TraceNode::Sequence,

            TraceNodeKind::Tuple => TraceNode::Tuple(self.pop_u32()?),
            TraceNodeKind::TupleStruct => {
                TraceNode::TupleStruct(self.pop_u32()?, self.pop_type_name()?)
            }
            TraceNodeKind::TupleVariant => TraceNode::TupleVariant(
                self.pop_u32()?,
                self.pop_type_name()?,
                self.pop_variant_name()?,
            ),

            TraceNodeKind::Struct => {
                TraceNode::Struct(self.pop_type_name()?, self.pop_field_name_list()?)
            }
            TraceNodeKind::StructVariant => TraceNode::StructVariant(
                self.pop_type_name()?,
                self.pop_variant_name()?,
                self.pop_field_name_list()?,
            ),
        };
        Ok(node)
    }

    fn pop_variant_name<ErrorT: serde::ser::Error>(&self) -> Result<VariantNameIndex, ErrorT> {
        Ok(self.pop_u32()?.into())
    }

    fn pop_type_name<ErrorT: serde::ser::Error>(&self) -> Result<TypeNameIndex, ErrorT> {
        Ok(self.pop_u32()?.into())
    }

    fn pop_field_name_list<ErrorT: serde::ser::Error>(&self) -> Result<FieldNameListIndex, ErrorT> {
        Ok(self.pop_u32()?.into())
    }

    fn pop_bool<ErrorT: serde::ser::Error>(&self) -> Result<bool, ErrorT> {
        Ok(self.pop_u8()? != 0)
    }

    fn pop_i8<ErrorT: serde::ser::Error>(&self) -> Result<i8, ErrorT> {
        Ok(self.pop_u8()? as i8)
    }

    fn pop_i16<ErrorT: serde::ser::Error>(&self) -> Result<i16, ErrorT> {
        Ok(self.pop_u16()? as i16)
    }

    fn pop_i32<ErrorT: serde::ser::Error>(&self) -> Result<i32, ErrorT> {
        Ok(self.pop_u32()? as i32)
    }

    fn pop_i64<ErrorT: serde::ser::Error>(&self) -> Result<i64, ErrorT> {
        Ok(self.pop_u64()? as i64)
    }

    fn pop_i128<ErrorT: serde::ser::Error>(&self) -> Result<i128, ErrorT> {
        Ok(self.pop_u128()? as i128)
    }

    fn pop_char<ErrorT: serde::ser::Error>(&self) -> Result<char, ErrorT> {
        char::try_from(self.pop_u32()?).map_err(|_| ErrorT::custom("bad char in trace"))
    }

    fn pop_f32<ErrorT: serde::ser::Error>(&self) -> Result<f32, ErrorT> {
        Ok(f32::from_bits(self.pop_u32()?))
    }

    fn pop_f64<ErrorT: serde::ser::Error>(&self) -> Result<f64, ErrorT> {
        Ok(f64::from_bits(self.pop_u64()?))
    }

    fn pop_length_u32<ErrorT: serde::ser::Error>(&self) -> Result<usize, ErrorT> {
        Ok(usize::try_from(self.pop_u32()?).expect("usize needs to be at least 32 bits"))
    }
}

impl<'data> ReadTraceExt<'data> for Cell<&'data [u8]> {
    fn pop_u8<ErrorT: serde::ser::Error>(&self) -> Result<u8, ErrorT> {
        let mut data = self.get();
        let byte = *data
            .split_off_first()
            .ok_or_else(|| ErrorT::custom("unexpected end of trace"))?;
        self.set(data);
        Ok(byte)
    }

    fn pop_slice<ErrorT: serde::ser::Error>(&self, len: usize) -> Result<&'data [u8], ErrorT> {
        if let Some((head, tail)) = self.get().split_at_checked(len) {
            self.set(tail);
            Ok(head)
        } else {
            Err(ErrorT::custom("unexpected end of trace"))
        }
    }
}
