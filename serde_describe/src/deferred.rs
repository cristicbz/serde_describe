use serde::de::{Deserialize, Deserializer, Error, Expected, Visitor};

pub(crate) trait DeferredDeserialize<'de>: Expected {
    type Visitor: Visitor<'de>;

    fn into_visitor(self) -> Self::Visitor;

    fn call<DeserializerT>(
        self,
        deserializer: DeserializerT,
    ) -> Result<<Self::Visitor as Visitor<'de>>::Value, DeserializerT::Error>
    where
        DeserializerT: Deserializer<'de>;
}

pub(crate) trait CanonicalVisit<'de>: DeferredDeserialize<'de> {
    type CanonicalInput: Deserialize<'de>;

    fn canonical_visit<ErrorT>(
        self,
        value: Self::CanonicalInput,
    ) -> Result<<Self::Visitor as Visitor<'de>>::Value, ErrorT>
    where
        ErrorT: Error;
}

pub(crate) type CallValue<'de, CallT> =
    <<CallT as DeferredDeserialize<'de>>::Visitor as Visitor<'de>>::Value;
pub(crate) type CallResult<'de, CallT, DeserializerT> =
    Result<CallValue<'de, CallT>, <DeserializerT as Deserializer<'de>>::Error>;

macro_rules! define_deferred_calls {
    ($($fn_name:ident($($arg_name:ident : $arg_type:ty),*) $(=> { $visit_name:ident($visit_type:ty) })?, )+) => {
        $(
            define_deferred_calls! {
                @single $fn_name($($arg_name: $arg_type),*) $(=> { $visit_name($visit_type) })?
            }
        )+
    };
    (@single $fn_name:ident($($arg_name:ident : $arg_type:ty),*) => { $visit_name:ident($visit_type:ty) }) => {
        define_deferred_calls! { @single $fn_name($($arg_name: $arg_type),*) }
        impl<'de, VisitorT> CanonicalVisit<'de> for $fn_name<VisitorT>
        where VisitorT: Visitor<'de> {
            type CanonicalInput = $visit_type;

            #[inline]
            fn canonical_visit<ErrorT>(
                self,
                value: Self::CanonicalInput,
            ) -> Result<<Self::Visitor as Visitor<'de>>::Value, ErrorT>
            where ErrorT: Error
            {
                self.visitor.$visit_name(value)
            }
        }
    };
    (@single $fn_name:ident($($arg_name:ident : $arg_type:ty),*)) => {
        #[allow(unused, non_camel_case_types)]
        pub(crate) struct $fn_name<VisitorT> {
            $(pub(crate) $arg_name: $arg_type,)*
            pub(crate) visitor: VisitorT,
        }

        impl<'de, VisitorT> Expected for $fn_name<VisitorT>
        where VisitorT: Visitor<'de> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.visitor.expecting(f)
            }
        }

        impl<'de, VisitorT> DeferredDeserialize<'de> for $fn_name<VisitorT>
        where VisitorT: Visitor<'de>
        {
            type Visitor = VisitorT;

            #[inline]
            fn into_visitor(self) -> VisitorT {
                self.visitor
            }

            #[inline]
            fn call<DeserializerT>(self, deserializer: DeserializerT) -> Result<VisitorT::Value, DeserializerT::Error>
            where DeserializerT: Deserializer<'de>
            {
                deserializer.$fn_name($(self.$arg_name,)* self.visitor)
            }
        }
    };
}

define_deferred_calls! {
    deserialize_any(),
    deserialize_bool() => { visit_bool(bool) },

    deserialize_i8() => { visit_i8(i8) },
    deserialize_i16() => { visit_i16(i16) },
    deserialize_i32() => { visit_i32(i32) },
    deserialize_i64() => { visit_i64(i64) },
    deserialize_i128() => { visit_i128(i128) },

    deserialize_u8() => { visit_u8(u8) },
    deserialize_u16() => { visit_u16(u16) },
    deserialize_u32() => { visit_u32(u32) },
    deserialize_u64() => { visit_u64(u64) },
    deserialize_u128() => { visit_u128(u128) },

    deserialize_f32() => { visit_f32(f32) },
    deserialize_f64() => { visit_f64(f64) },

    deserialize_char() => { visit_char(char) },
    deserialize_str(),
    deserialize_string(),
    deserialize_bytes(),
    deserialize_byte_buf(),
    deserialize_option(),
    deserialize_unit(),
    deserialize_unit_struct(name: &'static str),
    deserialize_newtype_struct(name: &'static str),
    deserialize_seq(),

    deserialize_tuple(len: usize),
    deserialize_tuple_struct(name: &'static str, len: usize),
    deserialize_map(),
    deserialize_struct(name: &'static str, fields: &'static [&'static str]),
    deserialize_enum(name: &'static str, variants: &'static [&'static str]),
    deserialize_identifier(),
    deserialize_ignored_any(),
}
