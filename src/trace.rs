use serde::Serialize;
use std::{cell::Cell, hash::Hash};

use crate::indices::{NameIndex, NameListIndex};

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

pub(crate) trait ReadTraceExt<'data> {
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
