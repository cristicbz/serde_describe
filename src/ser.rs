use crate::{
    DescribedBy, Schema, Trace,
    anonymous_union::{UNION_ENUM_NAME, serialized_anonymous_variant},
    builder::SchemaBuilder,
    described::SelfDescribed,
    indices::{
        FieldNameIndex, FieldNameListIndex, MemberIndex, MemberListIndex, SchemaNodeIndex,
        SchemaNodeListIndex,
    },
    schema::SchemaNode,
    trace::{ReadTraceExt, TraceNode},
};
use serde::{
    Serialize,
    ser::{
        Error as _, SerializeMap, SerializeSeq, SerializeTuple, SerializeTupleVariant, Serializer,
    },
};
use std::{cell::Cell, fmt::Debug};

impl<T> Serialize for SelfDescribed<T>
where
    T: Serialize,
{
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut builder = SchemaBuilder::new();
        let trace = builder.trace(&self.0).map_err(S::Error::custom)?;
        let schema = builder.build().map_err(S::Error::custom)?;
        (&schema, DescribedBy(trace, &schema)).serialize(serializer)
    }
}

impl<'schema, 'trace> Serialize for DescribedBy<'schema, &'trace Trace> {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let tail = Cell::new(&*(self.0).0);
        let cursor = TraceCursor::start(self.1, &tail);
        cursor.serialize(serializer)
    }
}

impl<'schema> Serialize for DescribedBy<'schema, Trace> {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        DescribedBy(&self.0, self.1).serialize(serializer)
    }
}

#[derive(Copy, Clone)]
struct TraceCursor<'a> {
    schema: &'a Schema,
    node: SchemaNode,
    trace: TraceNode,
    data: &'a [u8],
    tail: &'a Cell<&'a [u8]>,
}

#[derive(Copy, Clone)]
enum CheckResult<'a> {
    Simple,
    Discriminated(u32, TraceCursor<'a>),
}

impl Debug for CheckResult<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple => f.debug_struct("Simple").finish(),
            Self::Discriminated(discriminator, cursor) => f
                .debug_struct("Discriminated")
                .field("discriminator", &discriminator)
                .field("node", &cursor.node)
                .finish(),
        }
    }
}

impl<'a> TraceCursor<'a> {
    #[inline]
    fn start(schema: &'a Schema, tail: &'a Cell<&'a [u8]>) -> Self {
        Self {
            schema,
            node: schema.node(schema.root_index).unwrap(),
            trace: tail.pop_trace_node(),
            tail,
            data: tail.get(),
        }
    }

    #[inline]
    fn pop_child(&self, node: SchemaNodeIndex) -> Self {
        Self {
            schema: self.schema,
            node: self.schema.node(node).unwrap(),
            trace: self.tail.pop_trace_node(),
            data: self.tail.get(),
            tail: self.tail,
        }
    }

    #[inline]
    fn traced_child(&self, node: SchemaNodeIndex, trace: TraceNode) -> Self {
        Self {
            schema: self.schema,
            node: self.schema.node(node).unwrap(),
            trace,
            data: self.tail.get(),
            tail: self.tail,
        }
    }

    #[inline]
    fn serialize_inner<S>(&self, serializer: S, inner: SchemaNodeIndex) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.pop_child(inner).serialize(serializer)
    }

    #[inline]
    fn serialize_tuple<S>(
        &self,
        serializer: S,
        length: u32,
        node_list: SchemaNodeListIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let node_list = self.schema.node_list(node_list).unwrap();
        let length = usize::try_from(length).expect("usize at least 32-bits");
        assert_eq!(node_list.len(), length);

        let mut serializer = serializer.serialize_tuple(length)?;
        for &node in node_list {
            serializer.serialize_element(&self.pop_child(node))?
        }
        serializer.end()
    }

    #[inline]
    fn serialize_map<S>(
        &self,
        serializer: S,
        length: usize,
        key: SchemaNodeIndex,
        value: SchemaNodeIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut serializer = serializer.serialize_map(Some(length))?;
        for _ in 0..length {
            serializer.serialize_key(&self.pop_child(key))?;
            serializer.serialize_value(&self.pop_child(value))?;
        }
        serializer.end()
    }

    #[inline]
    fn serialize_sequence<S>(
        &self,
        serializer: S,
        length: usize,
        item: SchemaNodeIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut serializer = serializer.serialize_seq(Some(length))?;
        for _ in 0..length {
            serializer.serialize_element(&self.pop_child(item))?;
        }
        serializer.end()
    }

    #[inline]
    fn serialize_struct<S>(
        &self,
        serializer: S,
        name_list: FieldNameListIndex,
        skip_list: MemberListIndex,
        node_list: SchemaNodeListIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let skip_list = self.schema.member_list(skip_list).unwrap();
        let node_list = self.schema.node_list(node_list).unwrap();
        let name_list = self.schema.field_name_list(name_list).unwrap();
        let length = self.tail.pop_length_u32();
        let presence = self.tail.pop_slice(length * std::mem::size_of::<u32>());
        assert_eq!(name_list.len(), node_list.len());

        if skip_list.is_empty() {
            let mut serializer = serializer.serialize_tuple(length)?;
            // Even if there are no fields in the `skip_list`, some fields are skipped because
            // they're of type `Union[]`.
            //
            // These are fields that are ALWAYS skipped, so they don't need bits in the variant,
            // but they also don't need to be emitted.
            iter_field_indices(presence).try_for_each(|field| {
                serializer.serialize_element(&self.pop_child(node_list[usize::from(field)]))
            })?;
            serializer.end()
        } else {
            SkippableStructSerializer {
                cursor: self,
                variant: variant_from_presence(skip_list, presence),
                presence,
                name_list,
                skip_list,
                node_list,
            }
            .serialize(serializer)
        }
    }

    #[inline]
    fn check(&self) -> Option<CheckResult<'a>> {
        let matches = match (self.trace, self.node) {
            (TraceNode::Bool, SchemaNode::Bool)
            | (TraceNode::I8, SchemaNode::I8)
            | (TraceNode::I16, SchemaNode::I16)
            | (TraceNode::I32, SchemaNode::I32)
            | (TraceNode::I64, SchemaNode::I64)
            | (TraceNode::I128, SchemaNode::I128)
            | (TraceNode::U8, SchemaNode::U8)
            | (TraceNode::U16, SchemaNode::U16)
            | (TraceNode::U32, SchemaNode::U32)
            | (TraceNode::U64, SchemaNode::U64)
            | (TraceNode::U128, SchemaNode::U128)
            | (TraceNode::F32, SchemaNode::F32)
            | (TraceNode::F64, SchemaNode::F64)
            | (TraceNode::Char, SchemaNode::Char)
            | (TraceNode::String, SchemaNode::String)
            | (TraceNode::Bytes, SchemaNode::Bytes)
            | (TraceNode::None, SchemaNode::OptionNone)
            | (TraceNode::Some, SchemaNode::OptionSome(_))
            | (TraceNode::Unit, SchemaNode::Unit)
            | (TraceNode::Map, SchemaNode::Map(_, _))
            | (TraceNode::Sequence, SchemaNode::Sequence(_)) => true,

            (TraceNode::UnitStruct(trace_name), SchemaNode::UnitStruct(schema_name))
            | (TraceNode::NewtypeStruct(trace_name), SchemaNode::NewtypeStruct(schema_name, _)) => {
                trace_name == schema_name
            }

            (
                TraceNode::UnitVariant(trace_name, trace_variant),
                SchemaNode::UnitVariant(schema_name, schema_variant),
            )
            | (
                TraceNode::NewtypeVariant(trace_name, trace_variant),
                SchemaNode::NewtypeVariant(schema_name, schema_variant, _),
            ) => (trace_name, trace_variant) == (schema_name, schema_variant),

            (TraceNode::Tuple(trace_length), SchemaNode::Tuple(schema_length, _)) => {
                trace_length == schema_length
            }
            (
                TraceNode::TupleStruct(trace_length, trace_name),
                SchemaNode::TupleStruct(schema_name, schema_length, _),
            ) => (trace_length, trace_name) == (schema_length, schema_name),
            (
                TraceNode::TupleVariant(trace_length, trace_name, trace_variant),
                SchemaNode::TupleVariant(schema_name, schema_variant, schema_length, _),
            ) => {
                (trace_length, trace_name, trace_variant)
                    == (schema_length, schema_name, schema_variant)
            }

            (
                TraceNode::Struct(trace_name, trace_name_list),
                SchemaNode::Struct(schema_name, schema_name_list, _, _),
            ) => (trace_name, trace_name_list) == (schema_name, schema_name_list),
            (
                TraceNode::StructVariant(trace_name, trace_variant, trace_name_list),
                SchemaNode::StructVariant(schema_name, schema_variant, schema_name_list, _, _),
            ) => {
                (trace_name, trace_variant, trace_name_list)
                    == (schema_name, schema_variant, schema_name_list)
            }

            (trace, SchemaNode::Union(schema_list)) => {
                return self
                    .schema
                    .node_list(schema_list)
                    .unwrap()
                    .iter()
                    .map(|&node| self.traced_child(node, trace))
                    .enumerate()
                    .find(|(_, child)| child.check().is_some())
                    .map(|(discriminant, child)| {
                        CheckResult::Discriminated(
                            u32::try_from(discriminant).expect("too many types in union"),
                            child,
                        )
                    });
            }

            _ => false,
        };

        matches.then_some(CheckResult::Simple)
    }

    #[inline]
    fn finish_serialize<S>(
        &self,
        serializer: S,
        checked: CheckResult<'_>,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let data = self.tail;
        if let CheckResult::Discriminated(discriminant, child) = checked {
            return serializer.serialize_newtype_variant(
                UNION_ENUM_NAME,
                discriminant,
                serialized_anonymous_variant(discriminant)?,
                &child,
            );
        }
        match self.node {
            SchemaNode::Bool => serializer.serialize_bool(data.pop_bool()),
            SchemaNode::I8 => serializer.serialize_i8(data.pop_i8()),
            SchemaNode::I16 => serializer.serialize_i16(data.pop_i16()),
            SchemaNode::I32 => serializer.serialize_i32(data.pop_i32()),
            SchemaNode::I64 => serializer.serialize_i64(data.pop_i64()),
            SchemaNode::I128 => serializer.serialize_i128(data.pop_i128()),
            SchemaNode::U8 => serializer.serialize_u8(data.pop_u8()),
            SchemaNode::U16 => serializer.serialize_u16(data.pop_u16()),
            SchemaNode::U32 => serializer.serialize_u32(data.pop_u32()),
            SchemaNode::U64 => serializer.serialize_u64(data.pop_u64()),
            SchemaNode::U128 => serializer.serialize_u128(data.pop_u128()),
            SchemaNode::F32 => serializer.serialize_f32(data.pop_f32()),
            SchemaNode::F64 => serializer.serialize_f64(data.pop_f64()),
            SchemaNode::Char => serializer.serialize_char(data.pop_char()),
            SchemaNode::String => serializer.serialize_str(data.pop_str(data.pop_length_u32())),
            SchemaNode::Bytes => serializer.serialize_bytes(data.pop_slice(data.pop_length_u32())),

            SchemaNode::Unit
            | SchemaNode::UnitStruct(_)
            | SchemaNode::UnitVariant(_, _)
            | SchemaNode::OptionNone => serializer.serialize_unit(),

            SchemaNode::OptionSome(inner)
            | SchemaNode::NewtypeStruct(_, inner)
            | SchemaNode::NewtypeVariant(_, _, inner) => self.serialize_inner(serializer, inner),

            SchemaNode::Map(key, value) => {
                self.serialize_map(serializer, data.pop_length_u32(), key, value)
            }
            SchemaNode::Sequence(item) => {
                self.serialize_sequence(serializer, data.pop_length_u32(), item)
            }

            SchemaNode::Tuple(length, type_list)
            | SchemaNode::TupleStruct(_, length, type_list)
            | SchemaNode::TupleVariant(_, _, length, type_list) => {
                self.serialize_tuple(serializer, length, type_list)
            }

            SchemaNode::Struct(_, name_list, skip_list, type_list)
            | SchemaNode::StructVariant(_, _, name_list, skip_list, type_list) => {
                self.serialize_struct(serializer, name_list, skip_list, type_list)
            }

            SchemaNode::Union(_) => unreachable!("union finish called with simple check result"),
        }
    }
}

struct SkippableStructSerializer<'a, 'v> {
    cursor: &'v TraceCursor<'a>,
    presence: &'a [u8],
    variant: u64,
    name_list: &'a [FieldNameIndex],
    skip_list: &'a [MemberIndex],
    node_list: &'a [SchemaNodeIndex],
}

impl<'a, 'v> Serialize for SkippableStructSerializer<'a, 'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        assert!(self.skip_list.len() <= 64);
        let variant = u32::from(self.variant as u8);
        if self.skip_list.len() <= 8 {
            let mut serializer = serializer.serialize_tuple_variant(
                UNION_ENUM_NAME,
                variant,
                serialized_anonymous_variant(variant)?,
                self.presence.len() / std::mem::size_of::<u32>(),
            )?;
            for field in iter_field_indices(self.presence) {
                serializer
                    .serialize_field(&self.cursor.pop_child(self.node_list[usize::from(field)]))?;
            }
            serializer.end()
        } else {
            serializer.serialize_newtype_variant(
                UNION_ENUM_NAME,
                variant,
                serialized_anonymous_variant(variant)?,
                &SkippableStructSerializer {
                    cursor: self.cursor,
                    presence: self.presence,
                    variant: self.variant >> 8,
                    name_list: self.name_list,
                    skip_list: &self.skip_list[8..],
                    node_list: self.node_list,
                },
            )
        }
    }
}

fn variant_from_presence(skip_list: &[MemberIndex], presence: &[u8]) -> u64 {
    let mut variant = 0u64;
    let mut presence = iter_field_indices(presence).rev().peekable();
    for &skip in skip_list.iter().rev() {
        variant <<= 1;
        while let Some(&present) = presence.peek() {
            if present > skip {
                presence.next();
                continue;
            }
            if present == skip {
                variant |= 1;
                presence.next();
            }
            break;
        }
    }
    variant
}

fn iter_field_indices(presence: &[u8]) -> impl DoubleEndedIterator<Item = MemberIndex> {
    presence
        .chunks_exact(std::mem::size_of::<MemberIndex>())
        .map(|chunk| u32::from_le_bytes(chunk.try_into().unwrap()))
        .map(MemberIndex::from)
}

// Any issues caused by a mismatch between the schema and the trace are considered bugs and
// therefore panic. Traces are not serializable as such, so they don't come from an untrusted
// source.
//
// The `object -> trace` part of serialization handles errors gracefully, since the object may be
// untrusted (and of course deserialization does as well).
//
// It's unclear to me whether it would be better to actually report these kinds of internal
// inconsistencies via `SerError` as well or not.
impl Serialize for TraceCursor<'_> {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.tail.set(self.data);
        self.finish_serialize(serializer, self.check().expect("schema-trace mismatch"))
    }
}
