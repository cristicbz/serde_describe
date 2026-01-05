use serde::{
    Deserialize,
    de::{
        DeserializeSeed, Deserializer, EnumAccess, Error as _, Expected, IgnoredAny, MapAccess,
        SeqAccess, Unexpected, VariantAccess,
    },
};
use std::marker::PhantomData;

use crate::{
    Schema,
    anonymous_union::ChunkedEnum,
    deferred::{self, CallResult, CanonicalVisit, DeferredDeserialize},
    described::{DescribedBy, SelfDescribed},
    indices::{
        FieldNameIndex, FieldNameListIndex, IsEmpty, MemberIndex, MemberListIndex, SchemaNodeIndex,
        SchemaNodeListIndex, VariantNameIndex,
    },
    lossless_cast::LosslessCast,
    schema::SchemaNode,
};

impl<'de, T> Deserialize<'de> for SelfDescribed<T>
where
    T: Deserialize<'de>,
{
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        SelfDescribed(PhantomData).deserialize(deserializer)
    }
}

impl<'de, SeedT> DeserializeSeed<'de> for SelfDescribed<SeedT>
where
    SeedT: DeserializeSeed<'de>,
{
    type Value = SelfDescribed<SeedT::Value>;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct SchemaPairVisitor<SeedT>(SeedT);
        impl<'de, SeedT> serde::de::Visitor<'de> for SchemaPairVisitor<SeedT>
        where
            SeedT: DeserializeSeed<'de>,
        {
            type Value = SelfDescribed<SeedT::Value>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(
                    formatter,
                    "a described object: (schema, described-elsewhere object) pair"
                )
            }

            #[inline]
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let schema = seq
                    .next_element::<Schema>()?
                    .ok_or_else(|| A::Error::invalid_length(0, &self))?;

                Ok(SelfDescribed(
                    seq.next_element_seed(DescribedBy(self.0, &schema))?
                        .ok_or_else(|| {
                            A::Error::custom(
                                "missing described-elsewhere pair in described object pair",
                            )
                        })?
                        .0,
                ))
            }
        }

        deserializer.deserialize_tuple(2, SchemaPairVisitor(self.0))
    }
}

impl<'de, 'schema, SeedT> DeserializeSeed<'de> for DescribedBy<'schema, SeedT>
where
    SeedT: DeserializeSeed<'de>,
{
    type Value = DescribedBy<'schema, SeedT::Value>;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        SchemaDeserializer {
            schema: self.1,
            node: self.1.node(self.1.root_index).map_err(D::Error::custom)?,
            inner: self.0,
        }
        .deserialize(deserializer)
        .map(|value| DescribedBy(value, self.1))
    }
}

#[derive(Copy, Clone)]
pub(crate) struct SchemaDeserializer<'schema, InnerT> {
    schema: &'schema Schema,
    node: SchemaNode,
    inner: InnerT,
}

impl<'schema, InnerT> SchemaDeserializer<'schema, InnerT> {
    #[inline]
    fn variant_name_deserializer<ErrorT>(
        &self,
        variant: VariantNameIndex,
    ) -> Result<NameDeserializer<'schema, ErrorT>, ErrorT>
    where
        ErrorT: serde::de::Error,
    {
        Ok(NameDeserializer {
            name: self.schema.variant_name(variant).map_err(ErrorT::custom)?,
            phantom: PhantomData,
        })
    }

    #[inline]
    fn forward<ErrorT>(self, node: SchemaNodeIndex) -> Result<Self, ErrorT>
    where
        ErrorT: serde::de::Error,
    {
        Ok(Self {
            schema: self.schema,
            node: self.schema.node(node).map_err(ErrorT::custom)?,
            inner: self.inner,
        })
    }
}

impl<'de, 'schema, SeedT> DeserializeSeed<'de> for SchemaDeserializer<'schema, SeedT>
where
    SeedT: DeserializeSeed<'de>,
{
    type Value = <SeedT as DeserializeSeed<'de>>::Value;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        self.inner.deserialize(SchemaDeserializer {
            schema: self.schema,
            node: self.node,
            inner: deserializer,
        })
    }
}

impl<'s, 'de, DeserializerT> SchemaDeserializer<'s, DeserializerT>
where
    DeserializerT: Deserializer<'de>,
{
    #[inline]
    fn do_deserialize_seq<VisitorT>(
        self,
        item: SchemaNodeIndex,
        visitor: VisitorT,
    ) -> Result<VisitorT::Value, DeserializerT::Error>
    where
        VisitorT: serde::de::Visitor<'de>,
    {
        self.inner.deserialize_seq(SchemaSeqDeserializer {
            schema: self.schema,
            item: self
                .schema
                .node(item)
                .map_err(DeserializerT::Error::custom)?,
            inner: visitor,
        })
    }

    #[inline]
    fn do_deserialize_tuple<VisitorT>(
        self,
        field_types: SchemaNodeListIndex,
        visitor: VisitorT,
    ) -> Result<VisitorT::Value, DeserializerT::Error>
    where
        VisitorT: serde::de::Visitor<'de>,
    {
        let items = self
            .schema
            .node_list(field_types)
            .map_err(DeserializerT::Error::custom)?;
        self.inner.deserialize_tuple(
            items.len(),
            SchemaTupleDeserializer {
                schema: self.schema,
                items,
                inner: visitor,
            },
        )
    }

    #[inline]
    fn do_deserialize_map<VisitorT>(
        self,
        key: SchemaNodeIndex,
        value: SchemaNodeIndex,
        visitor: VisitorT,
    ) -> Result<VisitorT::Value, DeserializerT::Error>
    where
        VisitorT: serde::de::Visitor<'de>,
    {
        self.inner.deserialize_map(SchemaMapDeserializer {
            schema: self.schema,
            key: self
                .schema
                .node(key)
                .map_err(DeserializerT::Error::custom)?,
            value: self
                .schema
                .node(value)
                .map_err(DeserializerT::Error::custom)?,
            inner: visitor,
        })
    }

    #[inline]
    fn do_deserialize_struct<VisitorT>(
        self,
        field_names: FieldNameListIndex,
        skip_list: MemberListIndex,
        field_types: SchemaNodeListIndex,
        visitor: VisitorT,
    ) -> Result<VisitorT::Value, DeserializerT::Error>
    where
        VisitorT: serde::de::Visitor<'de>,
    {
        SchemaStructDeserializer::seed(self.schema, field_names, skip_list, field_types, visitor)?
            .deserialize(self.inner)
    }

    #[inline]
    fn deserialize_if<CallT>(
        self,
        condition: impl FnOnce(SchemaNode) -> bool,
        call: CallT,
    ) -> CallResult<'de, CallT, DeserializerT>
    where
        CallT: DeferredDeserialize<'de>,
    {
        match self.node {
            SchemaNode::Union(variants) => self.deserialize_union(variants, call),
            SchemaNode::OptionSome(inner) | SchemaNode::NewtypeStruct(_, inner) => {
                call.call(self.forward(inner)?)
            }
            actual if condition(actual) => call.call(self.inner),
            _ => self.invalid_type_error(&call),
        }
    }

    #[inline]
    fn deserialize_number<CallT>(self, call: CallT) -> CallResult<'de, CallT, DeserializerT>
    where
        CallT: CanonicalVisit<'de>,
        u8: LosslessCast<CallT::CanonicalInput>,
        u16: LosslessCast<CallT::CanonicalInput>,
        u32: LosslessCast<CallT::CanonicalInput>,
        u64: LosslessCast<CallT::CanonicalInput>,
        u128: LosslessCast<CallT::CanonicalInput>,
        i8: LosslessCast<CallT::CanonicalInput>,
        i16: LosslessCast<CallT::CanonicalInput>,
        i32: LosslessCast<CallT::CanonicalInput>,
        i64: LosslessCast<CallT::CanonicalInput>,
        i128: LosslessCast<CallT::CanonicalInput>,
        f32: LosslessCast<CallT::CanonicalInput>,
        f64: LosslessCast<CallT::CanonicalInput>,
    {
        macro_rules! number_conversion {
            ($type:ident, $visit_fn:ident, Other($text:expr)) => {
                number_conversion!(@common, $type, $visit_fn, number, {Unexpected::Other($text)})
            };
            ($type:ident, $visit_fn:ident, $unexpected:ident) => {
                number_conversion!(@common, $type, $visit_fn, number, Unexpected::$unexpected(number.into()))
            };
            (@common, $type:ident, $visit_fn:ident, $int:ident, $unexpected:expr) => {
                {
                    let $int = $type::deserialize((self).inner)?;
                    match $int.lossless_cast() {
                        Some(number) => call.canonical_visit(number),
                        None => <CallT::Visitor as serde::de::Visitor<'de>>::$visit_fn(call.into_visitor(), $int),
                    }
                }
            };
        }

        match self.node {
            SchemaNode::Union(variants) => self.deserialize_union(variants, call),
            SchemaNode::OptionSome(inner)
            | SchemaNode::NewtypeStruct(_, inner)
            | SchemaNode::NewtypeVariant(_, _, inner) => call.call(self.forward(inner)?),

            SchemaNode::I8 => number_conversion!(i8, visit_i8, Signed),
            SchemaNode::I16 => number_conversion!(i16, visit_i16, Signed),
            SchemaNode::I32 => number_conversion!(i32, visit_i32, Signed),
            SchemaNode::I64 => number_conversion!(i64, visit_i64, Signed),
            SchemaNode::I128 => {
                number_conversion!(i128, visit_i128, Other("128-bit signed integer"))
            }

            SchemaNode::U8 => number_conversion!(u8, visit_u8, Unsigned),
            SchemaNode::U16 => number_conversion!(u16, visit_u16, Unsigned),
            SchemaNode::U32 => number_conversion!(u32, visit_u32, Unsigned),
            SchemaNode::U64 => number_conversion!(u64, visit_u64, Unsigned),
            SchemaNode::U128 => {
                number_conversion!(u128, visit_u128, Other("128-bit unsigned integer"))
            }

            SchemaNode::F32 => number_conversion!(f32, visit_f32, Float),
            SchemaNode::F64 => number_conversion!(f64, visit_f64, Float),

            _ => self.invalid_type_error(&call),
        }
    }

    #[inline]
    fn deserialize_union<CallT>(
        self,
        variants: SchemaNodeListIndex,
        call: CallT,
    ) -> CallResult<'de, CallT, DeserializerT>
    where
        CallT: DeferredDeserialize<'de>,
    {
        struct ResolvedUnion<'schema, CallT> {
            schema: &'schema Schema,
            variants: &'schema [SchemaNodeIndex],
            discriminant: u64,
            deferred: CallT,
        }

        impl<'de, 'schema, CallT> DeserializeSeed<'de> for ResolvedUnion<'schema, CallT>
        where
            CallT: DeferredDeserialize<'de>,
        {
            type Value = <CallT::Visitor as serde::de::Visitor<'de>>::Value;

            #[inline]
            fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: Deserializer<'de>,
            {
                let node_index = usize::try_from(self.discriminant)
                    .ok()
                    .and_then(|discriminant| self.variants.get(discriminant).copied())
                    .ok_or_else(|| {
                        D::Error::invalid_value(Unexpected::Unsigned(self.discriminant), &"variant")
                    })?;
                self.deferred.call(SchemaDeserializer {
                    schema: self.schema,
                    node: self.schema.node(node_index).map_err(D::Error::custom)?,
                    inner: deserializer,
                })
            }
        }

        let variants = self
            .schema
            .node_list(variants)
            .map_err(DeserializerT::Error::custom)?;
        if variants.is_empty() {
            return Err(DeserializerT::Error::custom(
                "attempted to deserialize a bottom type / empty union",
            ));
        }

        ChunkedEnum::deserializable(
            usize::try_from(usize::BITS - (variants.len() - 1).leading_zeros())
                .expect("usize should be at least 32 bits"),
            move |discriminant| ResolvedUnion {
                schema: self.schema,
                discriminant,
                variants,
                deferred: call,
            },
        )?
        .deserialize(self.inner)
    }

    fn unexpected(self) -> Result<Unexpected<'de>, DeserializerT::Error> {
        Ok(match self.node {
            SchemaNode::Bool => Unexpected::Bool(bool::deserialize(self.inner)?),

            SchemaNode::I8 => Unexpected::Signed(i64::from(i8::deserialize(self.inner)?)),
            SchemaNode::I16 => Unexpected::Signed(i64::from(i16::deserialize(self.inner)?)),
            SchemaNode::I32 => Unexpected::Signed(i64::from(i32::deserialize(self.inner)?)),
            SchemaNode::I64 => Unexpected::Signed(i64::deserialize(self.inner)?),
            SchemaNode::I128 => Unexpected::Other("128-bit signed integer"),

            SchemaNode::U8 => Unexpected::Unsigned(u64::from(u8::deserialize(self.inner)?)),
            SchemaNode::U16 => Unexpected::Unsigned(u64::from(u16::deserialize(self.inner)?)),
            SchemaNode::U32 => Unexpected::Unsigned(u64::from(u32::deserialize(self.inner)?)),
            SchemaNode::U64 => Unexpected::Unsigned(u64::deserialize(self.inner)?),
            SchemaNode::U128 => Unexpected::Other("128-bit unsigned integer"),

            SchemaNode::F32 => Unexpected::Float(f64::from(f32::deserialize(self.inner)?)),
            SchemaNode::F64 => Unexpected::Float(f64::deserialize(self.inner)?),
            SchemaNode::Char => Unexpected::Char(char::deserialize(self.inner)?),

            SchemaNode::String => <&'de str>::deserialize(self.inner)
                .map(Unexpected::Str)
                .unwrap_or(Unexpected::Other("string")),
            SchemaNode::Bytes => <&'de [u8]>::deserialize(self.inner)
                .map(Unexpected::Bytes)
                .unwrap_or(Unexpected::Other("bytes")),

            SchemaNode::OptionNone | SchemaNode::OptionSome(_) => Unexpected::Option,

            SchemaNode::Unit => Unexpected::Unit,
            SchemaNode::UnitStruct(_) => Unexpected::Other("unit struct"),
            SchemaNode::UnitVariant(_, _) => Unexpected::UnitVariant,

            SchemaNode::NewtypeStruct(_, _) => Unexpected::NewtypeStruct,
            SchemaNode::NewtypeVariant(_, _, _) => Unexpected::NewtypeVariant,

            SchemaNode::Sequence(_) => Unexpected::Seq,
            SchemaNode::Map(_, _) => Unexpected::Map,

            SchemaNode::Tuple(_) => Unexpected::Other("tuple"),
            SchemaNode::TupleStruct(_, _) => Unexpected::Other("tuple struct"),
            SchemaNode::TupleVariant(_, _, _) => Unexpected::TupleVariant,

            SchemaNode::Struct(_, _, _, _) => Unexpected::Other("struct"),
            SchemaNode::StructVariant(_, _, _, _, _) => Unexpected::StructVariant,

            SchemaNode::Union(_) => Unexpected::Other("enum, skippable field or untagged union"),
        })
    }

    fn invalid_type_error<ExpectedT>(
        self,
        expected: &dyn Expected,
    ) -> Result<ExpectedT, DeserializerT::Error> {
        Err(DeserializerT::Error::invalid_type(
            self.unexpected()?,
            expected,
        ))
    }
}

macro_rules! deserialize_simple {
    ($fn_name:ident, $node:pat) => {
        deserialize_simple!{@helper, $fn_name, self, visitor, (self.deserialize_if(|node| matches!(node, $node), deferred::$fn_name { visitor }))}
    };

    ($fn_name:ident, @number) => {
        deserialize_simple!{@helper, $fn_name, self, visitor, (self.deserialize_number(deferred::$fn_name { visitor }))}
    };

    (@helper, $fn_name:ident, $self:ident, $visitor:ident, $with:tt) => {
        fn $fn_name<V>($self, $visitor: V) -> Result<V::Value, Self::Error>
        where
            V: serde::de::Visitor<'de>,
        {
            $with
        }
    };
}

impl<'s, 'de, DeserializerT> Deserializer<'de> for SchemaDeserializer<'s, DeserializerT>
where
    DeserializerT: Deserializer<'de>,
{
    type Error = DeserializerT::Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.node {
            SchemaNode::Bool => self.inner.deserialize_bool(visitor),

            SchemaNode::I8 => self.inner.deserialize_i8(visitor),
            SchemaNode::I16 => self.inner.deserialize_i16(visitor),
            SchemaNode::I32 => self.inner.deserialize_i32(visitor),
            SchemaNode::I64 => self.inner.deserialize_i64(visitor),
            SchemaNode::I128 => self.inner.deserialize_i128(visitor),

            SchemaNode::U8 => self.inner.deserialize_u8(visitor),
            SchemaNode::U16 => self.inner.deserialize_u16(visitor),
            SchemaNode::U32 => self.inner.deserialize_u32(visitor),
            SchemaNode::U64 => self.inner.deserialize_u64(visitor),
            SchemaNode::U128 => self.inner.deserialize_u128(visitor),

            SchemaNode::F32 => self.inner.deserialize_f32(visitor),
            SchemaNode::F64 => self.inner.deserialize_f64(visitor),
            SchemaNode::Char => self.inner.deserialize_char(visitor),

            SchemaNode::String => self.inner.deserialize_str(visitor),
            SchemaNode::Bytes => self.inner.deserialize_bytes(visitor),

            SchemaNode::OptionNone => self
                .inner
                .deserialize_unit(serde::de::IgnoredAny)
                .and_then(|_| visitor.visit_none()),
            SchemaNode::OptionSome(inner) => visitor.visit_some(self.forward(inner)?),
            SchemaNode::Unit | SchemaNode::UnitStruct(_) | SchemaNode::UnitVariant(_, _) => {
                self.deserialize_unit(visitor)
            }

            SchemaNode::NewtypeStruct(_, inner) | SchemaNode::NewtypeVariant(_, _, inner) => {
                visitor.visit_newtype_struct(self.forward(inner)?)
            }
            SchemaNode::Sequence(item) => self.do_deserialize_seq(item, visitor),
            SchemaNode::Map(key, value) => self.do_deserialize_map(key, value, visitor),
            SchemaNode::Tuple(field_types)
            | SchemaNode::TupleStruct(_, field_types)
            | SchemaNode::TupleVariant(_, _, field_types) => {
                self.do_deserialize_tuple(field_types, visitor)
            }
            SchemaNode::Struct(_, field_names, skip_list, field_types)
            | SchemaNode::StructVariant(_, _, field_names, skip_list, field_types) => {
                self.do_deserialize_struct(field_names, skip_list, field_types, visitor)
            }
            SchemaNode::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_any { visitor })
            }
        }
    }

    deserialize_simple! { deserialize_bool, SchemaNode::Bool }
    deserialize_simple! { deserialize_i8, @number }
    deserialize_simple! { deserialize_i16, @number }
    deserialize_simple! { deserialize_i32, @number }
    deserialize_simple! { deserialize_i64, @number }
    deserialize_simple! { deserialize_i128, @number }
    deserialize_simple! { deserialize_u8, @number }
    deserialize_simple! { deserialize_u16, @number }
    deserialize_simple! { deserialize_u32, @number }
    deserialize_simple! { deserialize_u64, @number }
    deserialize_simple! { deserialize_u128, @number }
    deserialize_simple! { deserialize_f32, @number }
    deserialize_simple! { deserialize_f64, @number }
    deserialize_simple! { deserialize_char, SchemaNode::Char }

    deserialize_simple! { deserialize_str, SchemaNode::String }
    deserialize_simple! { deserialize_string, SchemaNode::String }
    deserialize_simple! { deserialize_bytes, SchemaNode::Bytes }
    deserialize_simple! { deserialize_byte_buf, SchemaNode::Bytes }
    deserialize_simple! { deserialize_unit, SchemaNode::Unit | SchemaNode::UnitStruct(_) | SchemaNode::UnitVariant(_, _) }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.node {
            SchemaNode::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_option { visitor })
            }
            SchemaNode::OptionNone => self
                .inner
                .deserialize_unit(IgnoredAny)
                .and_then(|_| visitor.visit_none()),
            SchemaNode::OptionSome(inner) => visitor.visit_some(self.forward(inner)?),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.node {
            SchemaNode::Union(variants) => self.deserialize_union(
                variants,
                deferred::deserialize_newtype_struct { name, visitor },
            ),
            SchemaNode::OptionSome(inner) => self
                .forward(inner)?
                .deserialize_newtype_struct(name, visitor),
            SchemaNode::NewtypeStruct(_, inner) | SchemaNode::NewtypeVariant(_, _, inner) => {
                visitor.visit_newtype_struct(self.forward(inner)?)
            }
            _ => visitor.visit_newtype_struct(self),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.node {
            SchemaNode::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_seq { visitor })
            }

            SchemaNode::NewtypeStruct(_, inner)
            | SchemaNode::NewtypeVariant(_, _, inner)
            | SchemaNode::OptionSome(inner) => self.forward(inner)?.deserialize_seq(visitor),

            SchemaNode::Sequence(item) => self.do_deserialize_seq(item, visitor),
            SchemaNode::Tuple(field_types)
            | SchemaNode::TupleStruct(_, field_types)
            | SchemaNode::TupleVariant(_, _, field_types) => {
                self.do_deserialize_tuple(field_types, visitor)
            }
            _ => self.invalid_type_error(&visitor),
        }
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.node {
            SchemaNode::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_tuple { len, visitor })
            }

            SchemaNode::NewtypeStruct(_, inner)
            | SchemaNode::NewtypeVariant(_, _, inner)
            | SchemaNode::OptionSome(inner) => self.forward(inner)?.deserialize_tuple(len, visitor),

            SchemaNode::Tuple(field_types)
            | SchemaNode::TupleStruct(_, field_types)
            | SchemaNode::TupleVariant(_, _, field_types) => {
                self.do_deserialize_tuple(field_types, visitor)
            }

            _ => self.invalid_type_error(&visitor),
        }
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.node {
            SchemaNode::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_map { visitor })
            }

            SchemaNode::NewtypeStruct(_, inner)
            | SchemaNode::NewtypeVariant(_, _, inner)
            | SchemaNode::OptionSome(inner) => self.forward(inner)?.deserialize_map(visitor),

            SchemaNode::Map(key, value) => self.do_deserialize_map(key, value, visitor),
            SchemaNode::Struct(_, field_names, skip_list, field_types)
            | SchemaNode::StructVariant(_, _, field_names, skip_list, field_types) => {
                self.do_deserialize_struct(field_names, skip_list, field_types, visitor)
            }

            _ => self.invalid_type_error(&visitor),
        }
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.node {
            SchemaNode::Union(variants) => self.deserialize_union(
                variants,
                deferred::deserialize_struct {
                    name,
                    fields,
                    visitor,
                },
            ),

            SchemaNode::NewtypeStruct(_, inner)
            | SchemaNode::NewtypeVariant(_, _, inner)
            | SchemaNode::OptionSome(inner) => self
                .forward(inner)?
                .deserialize_struct(name, fields, visitor),

            SchemaNode::Struct(_, field_names, skip_list, field_types)
            | SchemaNode::StructVariant(_, _, field_names, skip_list, field_types) => {
                self.do_deserialize_struct(field_names, skip_list, field_types, visitor)
            }
            SchemaNode::Map(key, value) => self.do_deserialize_map(key, value, visitor),

            _ => self.invalid_type_error(&visitor),
        }
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.node {
            SchemaNode::Union(types) => self.deserialize_union(
                types,
                deferred::deserialize_enum {
                    name,
                    variants,
                    visitor,
                },
            ),

            SchemaNode::NewtypeStruct(_, inner) | SchemaNode::OptionSome(inner) => self
                .forward(inner)?
                .deserialize_enum(name, variants, visitor),

            _ => visitor.visit_enum(self),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.node {
            SchemaNode::Union(variants) => {
                self.deserialize_union(variants, deferred::deserialize_identifier { visitor })
            }
            SchemaNode::OptionSome(inner) => self.forward(inner)?.deserialize_identifier(visitor),
            SchemaNode::UnitVariant(_, variant)
            | SchemaNode::TupleVariant(_, variant, _)
            | SchemaNode::StructVariant(_, variant, _, _, _) => visitor.visit_str(
                self.schema
                    .variant_name(variant)
                    .map_err(Self::Error::custom)?,
            ),
            _ => self.invalid_type_error(&visitor),
        }
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_any(IgnoredAny)?;
        visitor.visit_unit()
    }

    #[inline]
    fn is_human_readable(&self) -> bool {
        false
    }
}

impl<'s, 'de, DeserializerT> EnumAccess<'de> for SchemaDeserializer<'s, DeserializerT>
where
    DeserializerT: Deserializer<'de>,
{
    type Error = DeserializerT::Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        match self.node {
            SchemaNode::UnitVariant(_, variant)
            | SchemaNode::TupleVariant(_, variant, _)
            | SchemaNode::StructVariant(_, variant, _, _, _) => seed
                .deserialize(self.variant_name_deserializer(variant)?)
                .map(move |value| (value, self)),
            SchemaNode::NewtypeVariant(_, variant, inner) => {
                self.forward(inner).and_then(|newtype| {
                    seed.deserialize(newtype.variant_name_deserializer(variant)?)
                        .map(|value| (value, newtype))
                })
            }
            _ => self.invalid_type_error(&"enum variant"),
        }
    }
}

impl<'s, 'de, DeserializerT> VariantAccess<'de> for SchemaDeserializer<'s, DeserializerT>
where
    DeserializerT: Deserializer<'de>,
{
    type Error = DeserializerT::Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        self.deserialize_unit(IgnoredAny).map(|_| ())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        seed.deserialize(self)
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_tuple(len, visitor)
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_struct("", fields, visitor)
    }
}

pub struct SchemaSeqDeserializer<'schema, InnerT> {
    schema: &'schema Schema,
    item: SchemaNode,
    inner: InnerT,
}

impl<'schema, 'de, VisitorT> serde::de::Visitor<'de> for SchemaSeqDeserializer<'schema, VisitorT>
where
    VisitorT: serde::de::Visitor<'de>,
{
    type Value = VisitorT::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.expecting(formatter)
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        self.inner.visit_seq(SchemaSeqDeserializer {
            schema: self.schema,
            item: self.item,
            inner: seq,
        })
    }
}

impl<'schema, 'de, SeqAccessT> SeqAccess<'de> for SchemaSeqDeserializer<'schema, SeqAccessT>
where
    SeqAccessT: SeqAccess<'de>,
{
    type Error = SeqAccessT::Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        self.inner.next_element_seed(SchemaDeserializer {
            schema: self.schema,
            node: self.item,
            inner: seed,
        })
    }
}

pub struct SchemaMapDeserializer<'schema, InnerT> {
    schema: &'schema Schema,
    key: SchemaNode,
    value: SchemaNode,
    inner: InnerT,
}

impl<'schema, 'de, VisitorT> serde::de::Visitor<'de> for SchemaMapDeserializer<'schema, VisitorT>
where
    VisitorT: serde::de::Visitor<'de>,
{
    type Value = VisitorT::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.expecting(formatter)
    }

    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        self.inner.visit_map(SchemaMapDeserializer {
            schema: self.schema,
            key: self.key,
            value: self.value,
            inner: map,
        })
    }
}

impl<'schema, 'de, MapAccessT> MapAccess<'de> for SchemaMapDeserializer<'schema, MapAccessT>
where
    MapAccessT: MapAccess<'de>,
{
    type Error = MapAccessT::Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        self.inner.next_key_seed(SchemaDeserializer {
            schema: self.schema,
            node: self.key,
            inner: seed,
        })
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        self.inner.next_value_seed(SchemaDeserializer {
            schema: self.schema,
            node: self.value,
            inner: seed,
        })
    }

    #[inline]
    fn next_entry_seed<K, V>(
        &mut self,
        kseed: K,
        vseed: V,
    ) -> Result<Option<(K::Value, V::Value)>, Self::Error>
    where
        K: DeserializeSeed<'de>,
        V: DeserializeSeed<'de>,
    {
        self.inner.next_entry_seed(
            SchemaDeserializer {
                schema: self.schema,
                node: self.key,
                inner: kseed,
            },
            SchemaDeserializer {
                schema: self.schema,
                node: self.value,
                inner: vseed,
            },
        )
    }
}

pub struct SchemaTupleDeserializer<'schema, InnerT> {
    schema: &'schema Schema,
    items: &'schema [SchemaNodeIndex],
    inner: InnerT,
}

impl<'schema, 'de, VisitorT> serde::de::Visitor<'de> for SchemaTupleDeserializer<'schema, VisitorT>
where
    VisitorT: serde::de::Visitor<'de>,
{
    type Value = VisitorT::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.expecting(formatter)
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        self.inner.visit_seq(SchemaTupleDeserializer {
            schema: self.schema,
            items: self.items,
            inner: seq,
        })
    }
}

impl<'schema, 'de, SeqAccessT> SeqAccess<'de> for SchemaTupleDeserializer<'schema, SeqAccessT>
where
    SeqAccessT: SeqAccess<'de>,
{
    type Error = SeqAccessT::Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if let Some(&node) = self.items.split_off_first() {
            self.inner.next_element_seed(SchemaDeserializer {
                schema: self.schema,
                node: self.schema.node(node).map_err(Self::Error::custom)?,
                inner: seed,
            })
        } else {
            Ok(None)
        }
    }
}

pub struct SchemaStructDeserializer<'schema, InnerT> {
    schema: &'schema Schema,
    field_names: &'schema [FieldNameIndex],
    field_types: &'schema [SchemaNodeIndex],
    skip_list: &'schema [MemberIndex],
    discriminant: u64,
    i_field: usize,
    next_value_schema: Option<SchemaNode>,
    inner: InnerT,
}

impl<'de, 'schema, InnerT> SchemaStructDeserializer<'schema, InnerT>
where
    InnerT: serde::de::Visitor<'de>,
{
    pub fn seed<ErrorT>(
        schema: &'schema Schema,
        field_names: FieldNameListIndex,
        skip_list: MemberListIndex,
        field_types: SchemaNodeListIndex,
        inner: InnerT,
    ) -> Result<impl DeserializeSeed<'de, Value = InnerT::Value>, ErrorT>
    where
        ErrorT: serde::de::Error,
    {
        let field_names = schema
            .field_name_list(field_names)
            .map_err(ErrorT::custom)?;
        let field_types = schema.node_list(field_types).map_err(ErrorT::custom)?;
        let skip_list = schema.member_list(skip_list).map_err(ErrorT::custom)?;

        if field_names.len() != field_types.len() {
            return Err(ErrorT::custom(
                "bad schema: struct field name length and type length mismatch",
            ));
        }

        ChunkedEnum::deserializable(skip_list.len(), move |discriminant| Self {
            schema,
            field_names,
            field_types,
            skip_list,
            discriminant,
            i_field: 0,
            next_value_schema: None,
            inner,
        })
    }
}

impl<'schema, InnerT> SchemaStructDeserializer<'schema, InnerT> {
    fn next<ErrorT>(&mut self) -> Result<Option<(&'schema str, SchemaNode)>, ErrorT>
    where
        ErrorT: serde::de::Error,
    {
        loop {
            let (name_index, node_index) = match (
                self.field_names.split_off_first(),
                self.field_types.split_off_first(),
            ) {
                (Some(&name_index), Some(&node_index)) => (name_index, node_index),
                (None, None) => return Ok(None),
                _ => unreachable!("types & names are verified to have the same length"),
            };

            // Skip fields marked as such in the variant.
            if let Some(&i_skip_field) = self.skip_list.first() {
                let i_field = self.i_field;
                self.i_field += 1;
                if usize::from(i_skip_field) == i_field {
                    let skipped = (self.discriminant & 1) == 0;
                    self.discriminant >>= 1;
                    self.skip_list.split_off_first();
                    if skipped {
                        continue;
                    }
                }
            }

            // Skip Union([]) fields.
            if node_index.is_empty() {
                continue;
            }

            return Ok(Some((
                self.schema.field_name(name_index).map_err(ErrorT::custom)?,
                self.schema.node(node_index).map_err(ErrorT::custom)?,
            )));
        }
    }
}

impl<'schema, 'de, VisitorT> DeserializeSeed<'de> for SchemaStructDeserializer<'schema, VisitorT>
where
    VisitorT: serde::de::Visitor<'de>,
{
    type Value = VisitorT::Value;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        let length = self.field_names.len()
            // Discount fields that are present in the `skip_list` and don't have a bit set in
            // the presence variant.
            + usize::try_from(self.discriminant.count_ones())
                .expect("usize needs to be at least 32 bits")
            - self.skip_list.len()
            // Fields that are ALWAYS skipped are not present in `skip_list`, instead
            // they're typed as `Union[]`, the bottom type. We need to subtract these
            // as well.
            - self
                .field_types
                .iter()
                .filter(|field_type| field_type.is_empty())
                .count();
        deserializer.deserialize_tuple(length, self)
    }
}

impl<'schema, 'de, VisitorT> serde::de::Visitor<'de> for SchemaStructDeserializer<'schema, VisitorT>
where
    VisitorT: serde::de::Visitor<'de>,
{
    type Value = VisitorT::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.expecting(formatter)
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        self.inner.visit_map(SchemaStructDeserializer {
            schema: self.schema,
            field_names: self.field_names,
            field_types: self.field_types,
            skip_list: self.skip_list,
            discriminant: self.discriminant,
            i_field: self.i_field,
            next_value_schema: self.next_value_schema,
            inner: seq,
        })
    }
}

impl<'schema, 'de, SeqAccessT> MapAccess<'de> for SchemaStructDeserializer<'schema, SeqAccessT>
where
    SeqAccessT: SeqAccess<'de>,
{
    type Error = SeqAccessT::Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        let Some((field_name, field_type)) = self.next()? else {
            return Ok(None);
        };
        self.next_value_schema = Some(field_type);
        seed.deserialize(NameDeserializer {
            name: field_name,
            phantom: PhantomData,
        })
        .map(Some)
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        self.inner
            .next_element_seed(SchemaDeserializer {
                schema: self.schema,
                node: self
                    .next_value_schema
                    .expect("called next_value_seed with no next_key_seed"),
                inner: seed,
            })?
            .ok_or_else(|| Self::Error::custom("more struct keys than values"))
    }

    #[inline]
    fn next_entry_seed<K, V>(
        &mut self,
        kseed: K,
        vseed: V,
    ) -> Result<Option<(K::Value, V::Value)>, Self::Error>
    where
        K: DeserializeSeed<'de>,
        V: DeserializeSeed<'de>,
    {
        let Some((field_name, field_type)) = self.next()? else {
            return Ok(None);
        };

        let key = kseed.deserialize(NameDeserializer {
            name: field_name,
            phantom: PhantomData,
        })?;

        let value = self
            .inner
            .next_element_seed(SchemaDeserializer {
                schema: self.schema,
                node: field_type,
                inner: vseed,
            })?
            .ok_or_else(|| Self::Error::custom("more struct keys than values"))?;

        Ok(Some((key, value)))
    }
}

pub struct NameDeserializer<'schema, ErrorT> {
    name: &'schema str,
    phantom: PhantomData<ErrorT>,
}

impl<'schema, 'de, ErrorT> Deserializer<'de> for NameDeserializer<'schema, ErrorT>
where
    ErrorT: serde::de::Error,
{
    type Error = ErrorT;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_str(self.name)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}
