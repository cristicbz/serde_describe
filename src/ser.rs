use crate::{
    DescribedBy, Schema, Trace,
    anonymous_union::ChunkedEnum,
    builder::SchemaBuilder,
    described::SelfDescribed,
    indices::{
        FieldNameListIndex, MemberIndex, MemberListIndex, SchemaNodeIndex, SchemaNodeListIndex,
    },
    schema::SchemaNode,
    trace::{ReadTraceExt, TraceNode},
};
use serde::{
    Serialize,
    ser::{Error as _, SerializeMap, SerializeSeq, SerializeTuple, Serializer},
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
        let cursor = TraceCursor::start(self.1, &tail)?;
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
    Discriminated(usize, usize, TraceCursor<'a>),
}

impl Debug for CheckResult<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple => f.debug_struct("Simple").finish(),
            Self::Discriminated(discriminator, num_variants, cursor) => f
                .debug_struct("Discriminated")
                .field("discriminator", &discriminator)
                .field("num_variants", &num_variants)
                .field("node", &cursor.node)
                .finish(),
        }
    }
}

impl<'a> TraceCursor<'a> {
    #[inline]
    fn start<ErrorT>(schema: &'a Schema, tail: &'a Cell<&'a [u8]>) -> Result<Self, ErrorT>
    where
        ErrorT: serde::ser::Error,
    {
        Ok(Self {
            schema,
            node: schema.node(schema.root_index).map_err(ErrorT::custom)?,
            trace: tail.pop_trace_node()?,
            tail,
            data: tail.get(),
        })
    }

    #[inline]
    fn pop_child<ErrorT>(&self, node: SchemaNodeIndex) -> Result<Self, ErrorT>
    where
        ErrorT: serde::ser::Error,
    {
        Ok(Self {
            schema: self.schema,
            node: self.schema.node(node).map_err(ErrorT::custom)?,
            trace: self.tail.pop_trace_node()?,
            data: self.tail.get(),
            tail: self.tail,
        })
    }

    #[inline]
    fn traced_child<ErrorT>(&self, node: SchemaNodeIndex, trace: TraceNode) -> Result<Self, ErrorT>
    where
        ErrorT: serde::ser::Error,
    {
        Ok(Self {
            schema: self.schema,
            node: self.schema.node(node).map_err(ErrorT::custom)?,
            trace,
            data: self.tail.get(),
            tail: self.tail,
        })
    }

    #[inline]
    fn serialize_inner<S>(&self, serializer: S, inner: SchemaNodeIndex) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.pop_child(inner)?.serialize(serializer)
    }

    #[inline]
    fn serialize_tuple<S>(
        &self,
        serializer: S,
        length: usize,
        node_list: SchemaNodeListIndex,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let node_list = self.schema.node_list(node_list).map_err(S::Error::custom)?;

        let mut serializer = serializer.serialize_tuple(length)?;
        for &node in node_list {
            serializer.serialize_element(&self.pop_child(node)?)?
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
            serializer.serialize_key(&self.pop_child(key)?)?;
            serializer.serialize_value(&self.pop_child(value)?)?;
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
            serializer.serialize_element(&self.pop_child(item)?)?;
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
        let skip_list = self
            .schema
            .member_list(skip_list)
            .map_err(S::Error::custom)?;
        let node_list = self.schema.node_list(node_list).map_err(S::Error::custom)?;
        let name_list = self
            .schema
            .field_name_list(name_list)
            .map_err(S::Error::custom)?;
        let length = self.tail.pop_length_u32()?;
        let presence = self.tail.pop_slice(length * std::mem::size_of::<u32>())?;
        if name_list.len() != node_list.len() {
            return Err(S::Error::custom(
                "field name - field type length mismatch in schema",
            ));
        }

        ChunkedEnum::serializable(
            skip_list.len(),
            discriminant_from_presence(skip_list, presence),
            &SkippableStructSerializer {
                cursor: self,
                presence,
                node_list,
            },
        )?
        .serialize(serializer)
    }

    // Checks whether the trace matches the schema node.
    //
    // Note that this check is shallow, tightly coupled with the logic in
    // `SchemaBuilderNode::unify`. The assumption is that, within a union there is:
    //  * At most one record type (incl. units and newtypes) with a given (name, variant, field_names, length).
    //  * At most one `Some(_)`, `Sequence[_]`, `Map[_, _]`
    #[inline]
    fn check<ErrorT>(&self) -> Result<Option<CheckResult<'a>>, ErrorT>
    where
        ErrorT: serde::ser::Error,
    {
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

            (TraceNode::Tuple(trace_length), SchemaNode::Tuple(schema_type_list)) => {
                self.matches_length(trace_length, schema_type_list)?
            }
            (
                TraceNode::TupleStruct(trace_length, trace_name),
                SchemaNode::TupleStruct(schema_name, schema_type_list),
            ) => {
                trace_name == schema_name && self.matches_length(trace_length, schema_type_list)?
            }
            (
                TraceNode::TupleVariant(trace_length, trace_name, trace_variant),
                SchemaNode::TupleVariant(schema_name, schema_variant, schema_type_list),
            ) => {
                (trace_name, trace_variant) == (schema_name, schema_variant)
                    && self.matches_length(trace_length, schema_type_list)?
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
                let variants = self.schema.node_list(schema_list).map_err(ErrorT::custom)?;
                for (discriminant, &node) in variants.iter().enumerate() {
                    let child = self.traced_child(node, trace)?;
                    if child.check()?.is_some() {
                        return Ok(Some(CheckResult::Discriminated(
                            discriminant,
                            variants.len(),
                            child,
                        )));
                    }
                }
                return Ok(None);
            }

            _ => false,
        };

        Ok(matches.then_some(CheckResult::Simple))
    }

    #[inline]
    fn matches_length<ErrorT>(
        &self,
        trace_length: u32,
        schema_type_list: SchemaNodeListIndex,
    ) -> Result<bool, ErrorT>
    where
        ErrorT: serde::ser::Error,
    {
        Ok(
            usize::try_from(trace_length).expect("usize must be at least 32-bits")
                == self
                    .schema
                    .node_list(schema_type_list)
                    .map_err(ErrorT::custom)?
                    .len(),
        )
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
        if let CheckResult::Discriminated(discriminant, num_variants, child) = checked {
            assert!(
                discriminant < num_variants,
                "out of bounds discriminant: {discriminant} >= {num_variants}"
            );
            return ChunkedEnum::serializable(
                usize::try_from(usize::BITS - (num_variants - 1).leading_zeros())
                    .expect("usize must be at least 32 bits"),
                u64::try_from(discriminant)
                    .map_err(|_| S::Error::custom("too many discriminants"))?,
                &child,
            )?
            .serialize(serializer);
        }
        match self.node {
            SchemaNode::Bool => serializer.serialize_bool(data.pop_bool()?),
            SchemaNode::I8 => serializer.serialize_i8(data.pop_i8()?),
            SchemaNode::I16 => serializer.serialize_i16(data.pop_i16()?),
            SchemaNode::I32 => serializer.serialize_i32(data.pop_i32()?),
            SchemaNode::I64 => serializer.serialize_i64(data.pop_i64()?),
            SchemaNode::I128 => serializer.serialize_i128(data.pop_i128()?),
            SchemaNode::U8 => serializer.serialize_u8(data.pop_u8()?),
            SchemaNode::U16 => serializer.serialize_u16(data.pop_u16()?),
            SchemaNode::U32 => serializer.serialize_u32(data.pop_u32()?),
            SchemaNode::U64 => serializer.serialize_u64(data.pop_u64()?),
            SchemaNode::U128 => serializer.serialize_u128(data.pop_u128()?),
            SchemaNode::F32 => serializer.serialize_f32(data.pop_f32()?),
            SchemaNode::F64 => serializer.serialize_f64(data.pop_f64()?),
            SchemaNode::Char => serializer.serialize_char(data.pop_char()?),
            SchemaNode::String => serializer.serialize_str(data.pop_str(data.pop_length_u32()?)?),
            SchemaNode::Bytes => {
                serializer.serialize_bytes(data.pop_slice(data.pop_length_u32()?)?)
            }

            SchemaNode::Unit
            | SchemaNode::UnitStruct(_)
            | SchemaNode::UnitVariant(_, _)
            | SchemaNode::OptionNone => serializer.serialize_unit(),

            SchemaNode::OptionSome(inner)
            | SchemaNode::NewtypeStruct(_, inner)
            | SchemaNode::NewtypeVariant(_, _, inner) => self.serialize_inner(serializer, inner),

            SchemaNode::Map(key, value) => {
                self.serialize_map(serializer, data.pop_length_u32()?, key, value)
            }
            SchemaNode::Sequence(item) => {
                self.serialize_sequence(serializer, data.pop_length_u32()?, item)
            }

            SchemaNode::Tuple(type_list)
            | SchemaNode::TupleStruct(_, type_list)
            | SchemaNode::TupleVariant(_, _, type_list) => self.serialize_tuple(
                serializer,
                self.schema
                    .node_list(type_list)
                    .map_err(S::Error::custom)?
                    .len(),
                type_list,
            ),

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
    node_list: &'a [SchemaNodeIndex],
}

impl<'a, 'v> Serialize for SkippableStructSerializer<'a, 'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut serializer =
            serializer.serialize_tuple(self.presence.len() / std::mem::size_of::<u32>())?;
        for field in iter_field_indices(self.presence) {
            serializer.serialize_element(&self.cursor.pop_child(
                *self.node_list.get(usize::from(field)).ok_or_else(|| {
                    S::Error::custom("member index out of bounds for struct in schema")
                })?,
            )?)?;
        }
        serializer.end()
    }
}

fn discriminant_from_presence(skip_list: &[MemberIndex], presence: &[u8]) -> u64 {
    let mut discriminant = 0u64;
    let mut presence = iter_field_indices(presence).rev().peekable();
    for &skip in skip_list.iter().rev() {
        discriminant <<= 1;
        while let Some(&present) = presence.peek() {
            if present > skip {
                presence.next();
                continue;
            }
            if present == skip {
                discriminant |= 1;
                presence.next();
            }
            break;
        }
    }
    discriminant
}

fn iter_field_indices(presence: &[u8]) -> impl DoubleEndedIterator<Item = MemberIndex> {
    presence
        .chunks_exact(std::mem::size_of::<MemberIndex>())
        .map(|chunk| u32::from_le_bytes(chunk.try_into().expect("impossible")))
        .map(MemberIndex::from)
}

// Any issues caused by a mismatch between the schema and the trace are technically bugs but
// instead of panicking, we still propagate errors as `S::Error::custom`. Traces are not
// serializable as such, so they don't come from an untrusted source.
//
// The `object -> trace` part of serialization handles errors gracefully, since the object may be
// untrusted (and of course deserialization does as well).
//
// It's unclear to me whether this is actually better than panicking.
impl Serialize for TraceCursor<'_> {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.tail.set(self.data);
        self.finish_serialize(
            serializer,
            self.check()?
                .ok_or_else(|| S::Error::custom("schema-trace mismatch"))?,
        )
    }
}
