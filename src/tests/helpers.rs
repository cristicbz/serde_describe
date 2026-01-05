use crate::{Schema, described::SelfDescribed};
use serde::{
    Deserialize, Serialize,
    de::{DeserializeOwned, Error as _},
};
use std::{fmt::Debug, marker::PhantomData};

pub(crate) fn to_self_described_ron<T: Serialize>(value: T) -> ron::Result<String> {
    ron::ser::to_string_pretty(
        &SelfDescribed(value),
        ron::ser::PrettyConfig::default()
            .struct_names(true)
            .number_suffixes(true),
    )
}

pub(crate) fn from_self_described_ron<'de, T: Deserialize<'de>>(bytes: &'de str) -> ron::Result<T> {
    Ok(ron::from_str::<SelfDescribed<T>>(bytes).map(|pair| pair.0)?)
}

pub(crate) fn to_self_described_bitcode<T: Serialize>(value: T) -> Vec<u8> {
    bitcode::serialize(&SelfDescribed(value)).unwrap()
}

pub(crate) fn from_self_described_bitcode<'de, T: Deserialize<'de>>(
    bytes: &'de [u8],
) -> Result<T, bitcode::Error> {
    bitcode::deserialize::<SelfDescribed<T>>(bytes).map(|wrapper| wrapper.0)
}

pub(crate) fn to_self_described_postcard<T: Serialize>(value: T) -> Vec<u8> {
    postcard::to_stdvec(&SelfDescribed(value)).unwrap()
}

pub(crate) fn from_self_described_postcard<'de, T: Deserialize<'de>>(
    bytes: &'de [u8],
) -> Result<T, postcard::Error> {
    postcard::from_bytes::<SelfDescribed<T>>(bytes).map(|wrapper| wrapper.0)
}

pub(crate) fn check_roundtrip<T: Serialize + DeserializeOwned + PartialEq + Debug>(original: &T) {
    check_evolution_ok::<T, T>(original, T::eq);
}

pub(crate) fn check_evolution_ok<T: Serialize + Debug, U: DeserializeOwned + Debug>(
    original: &T,
    mut condition: impl FnMut(&T, &U) -> bool,
) {
    let schema = Schema::display_for_value(original)
        .map(|display| display.to_string())
        .unwrap_or_else(|error| format!("<trace error: {error}>"));

    let self_described_ron = to_self_described_ron(original);
    let ron_roundtripped = self_described_ron
        .as_ref()
        .map_err(Clone::clone)
        .and_then(|string| from_self_described_ron::<U>(string));
    let self_described_ron_str =
        self_described_ron.unwrap_or_else(|error| format!("error: {error}"));
    let ron_roundtripped_str = ron_roundtripped
        .as_ref()
        .map(|value| format!("{value:#?}"))
        .unwrap_or_else(|error| format!("error: {error}"));
    assert!(
        matches!(ron_roundtripped.as_ref(), Ok(roundtripped) if condition(original, roundtripped)),
        "ORIGINAL: {original:#?}\n\nSCHEMA: {schema:#}\n\nRON: {self_described_ron_str}\n\nRON (roundtripped): {ron_roundtripped_str}"
    );

    let bitcode_roundtripped =
        from_self_described_bitcode::<U>(&to_self_described_bitcode(original));
    assert!(
        matches!(bitcode_roundtripped.as_ref(), Ok(roundtripped) if condition(original, roundtripped)),
        "ORIGINAL: {original:#?}\n\nBITCODE: {bitcode_roundtripped:#?}\n\nSCHEMA: {schema:#}\n\nRON: {self_described_ron_str}\n\nRON (roundtripped): {ron_roundtripped_str}"
    );

    let postcard_roundtripped =
        from_self_described_postcard::<U>(&to_self_described_postcard(original));
    assert!(
        matches!(postcard_roundtripped.as_ref(), Ok(roundtripped) if condition(original, roundtripped)),
        "ORIGINAL: {original:#?}\n\nPOSTCARD: {postcard_roundtripped:#?}\n\nSCHEMA: {schema:#}\n\nRON: {self_described_ron_str}\n\nRON (roundtripped): {ron_roundtripped_str}"
    );
}

pub(crate) fn check_evolution_fails<T: Serialize + Debug, U: DeserializeOwned + Debug>(
    original: &T,
) {
    let schema = Schema::display_for_value(original)
        .map(|display| display.to_string())
        .unwrap_or_else(|error| format!("<trace error: {error}>"));

    let self_described_ron = to_self_described_ron(original);
    let ron_roundtripped = self_described_ron
        .as_ref()
        .map_err(Clone::clone)
        .and_then(|string| from_self_described_ron::<U>(string));
    let self_described_ron_str =
        self_described_ron.unwrap_or_else(|error| format!("error: {error}"));
    let ron_roundtripped_str = ron_roundtripped
        .as_ref()
        .map(|value| format!("{value:#?}"))
        .unwrap_or_else(|error| format!("error: {error}"));
    assert!(
        ron_roundtripped.is_err(),
        "ORIGINAL: {original:#?}\n\nSCHEMA: {schema:#}\n\nRON: {self_described_ron_str}\n\nRON (roundtripped): {ron_roundtripped_str}"
    );

    let bitcode_roundtripped =
        from_self_described_bitcode::<U>(&to_self_described_bitcode(original));
    assert!(
        bitcode_roundtripped.as_ref().is_err(),
        "ORIGINAL: {original:#?}\n\nBITCODE: {bitcode_roundtripped:#?}\n\nSCHEMA: {schema:#}\n\nRON: {self_described_ron_str}\n\nRON (roundtripped): {ron_roundtripped_str}"
    );

    let postcard_roundtripped =
        from_self_described_postcard::<U>(&to_self_described_postcard(original));
    assert!(
        postcard_roundtripped.as_ref().is_err(),
        "ORIGINAL: {original:#?}\n\nPOSTCARD: {postcard_roundtripped:#?}\n\nSCHEMA: {schema:#}\n\nRON: {self_described_ron_str}\n\nRON (roundtripped): {ron_roundtripped_str}"
    );
}

// For tests we want to check that a specific `visit` method is called. If we rely on default
// Deserialize impl-s for primitives, they'll happily narrow and widen values as needed, so instead
// the `StrictVisitor` is just like the `Visitor` trait, but all the default impls return errors.
//
// Then we provide specific implementations for each of the primitives.
pub(crate) trait StrictVisitor<'de>: serde::de::Expected + Sized {
    type Value;

    fn strict_visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Bool(v), &self))
    }

    fn strict_visit_i8<E>(self, _v: i8) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("i8"), &self))
    }

    fn strict_visit_i16<E>(self, _v: i16) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("i16"), &self))
    }

    fn strict_visit_i32<E>(self, _v: i32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("i32"), &self))
    }

    fn strict_visit_i64<E>(self, _v: i64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("i64"), &self))
    }

    fn strict_visit_i128<E>(self, _v: i128) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("i128"), &self))
    }

    fn strict_visit_u8<E>(self, _v: u8) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("u8"), &self))
    }

    fn strict_visit_u16<E>(self, _v: u16) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("u16"), &self))
    }

    fn strict_visit_u32<E>(self, _v: u32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("u32"), &self))
    }

    fn strict_visit_u64<E>(self, _v: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("u64"), &self))
    }

    fn strict_visit_u128<E>(self, _v: u128) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("u128"), &self))
    }

    fn strict_visit_f32<E>(self, _v: f32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("f32"), &self))
    }

    fn strict_visit_f64<E>(self, _v: f64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("f64"), &self))
    }

    fn strict_visit_char<E>(self, v: char) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Char(v), &self))
    }

    fn strict_visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Str(&v), &self))
    }

    #[inline]
    fn strict_visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Bytes(v), &self))
    }

    fn strict_visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Other("none"), &self))
    }

    fn strict_visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let _ = deserializer;
        Err(D::Error::invalid_type(
            serde::de::Unexpected::Other("some"),
            &self,
        ))
    }

    fn strict_visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Err(E::invalid_type(serde::de::Unexpected::Unit, &self))
    }

    fn strict_visit_newtype_struct<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let _ = deserializer;
        Err(D::Error::invalid_type(
            serde::de::Unexpected::NewtypeStruct,
            &self,
        ))
    }

    fn strict_visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let _ = seq;
        Err(A::Error::invalid_type(serde::de::Unexpected::Seq, &self))
    }

    fn strict_visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let _ = map;
        Err(A::Error::invalid_type(serde::de::Unexpected::Map, &self))
    }

    fn strict_visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::EnumAccess<'de>,
    {
        let _ = data;
        Err(A::Error::invalid_type(serde::de::Unexpected::Enum, &self))
    }
}

#[derive(Default)]
pub(crate) struct StrictVisitorAdapter<T>(PhantomData<T>);

impl<'de, VisitorT> serde::de::Visitor<'de> for StrictVisitorAdapter<VisitorT>
where
    Self: StrictVisitor<'de>,
{
    type Value = <Self as StrictVisitor<'de>>::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "strict {}", std::any::type_name::<Self::Value>())
    }

    fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_bool(v)
    }

    fn visit_i8<E>(self, v: i8) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_i8(v)
    }

    fn visit_i16<E>(self, v: i16) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_i16(v)
    }

    fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_i32(v)
    }

    fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_i64(v)
    }

    fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_i128(v)
    }

    fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_u8(v)
    }

    fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_u16(v)
    }

    fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_u32(v)
    }

    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_u64(v)
    }

    fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_u128(v)
    }

    fn visit_f32<E>(self, v: f32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_f32(v)
    }

    fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_f64(v)
    }

    fn visit_char<E>(self, v: char) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_char(v)
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_string(v.to_owned())
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_string(v)
    }

    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_bytes(v)
    }

    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_none()
    }

    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        self.strict_visit_some(deserializer)
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.strict_visit_unit()
    }

    fn visit_newtype_struct<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        self.strict_visit_newtype_struct(deserializer)
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        self.strict_visit_seq(seq)
    }

    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        self.strict_visit_map(map)
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::EnumAccess<'de>,
    {
        self.strict_visit_enum(data)
    }
}

pub(crate) trait StrictDeserialize<'de>: Sized {
    type Visitor: serde::de::Visitor<'de, Value = Self> + Default;

    fn strict_deserialize<D, V>(deserializer: D, visitor: V) -> Result<V::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
        V: serde::de::Visitor<'de>;
}

macro_rules! impl_strict_deserialize {
    ($(impl $type:ident => $deserialize:ident => $visit:ident;)+) => {
        $(
            impl_strict_deserialize! { @one; $type; $deserialize; $visit; }
        )+
    };
    (@one; $type:ident; $deserialize:ident; $visit:ident;) => {
        impl<'de> StrictVisitor<'de> for StrictVisitorAdapter<$type> {
            type Value = $type;

            fn $visit<E>(self, value: $type) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(value)
            }
        }

        impl<'de> StrictDeserialize<'de> for $type {
            type Visitor = StrictVisitorAdapter<$type>;

            fn strict_deserialize<D, V>(deserializer: D, visitor: V) -> Result<V::Value, D::Error>
            where
                D: serde::Deserializer<'de>,
                V: serde::de::Visitor<'de>
            {
                deserializer.$deserialize(visitor)
            }
        }
    }
}
impl_strict_deserialize! {
    impl u8 => deserialize_u8 => strict_visit_u8;
    impl u16 => deserialize_u16 => strict_visit_u16;
    impl u32 => deserialize_u32 => strict_visit_u32;
    impl u64 => deserialize_u64 => strict_visit_u64;
    impl u128 => deserialize_u128 => strict_visit_u128;

    impl i8 => deserialize_i8 => strict_visit_i8;
    impl i16 => deserialize_i16 => strict_visit_i16;
    impl i32 => deserialize_i32 => strict_visit_i32;
    impl i64 => deserialize_i64 => strict_visit_i64;
    impl i128 => deserialize_i128 => strict_visit_i128;

    impl f32 => deserialize_f32 => strict_visit_f32;
    impl f64 => deserialize_f64 => strict_visit_f64;

    impl bool => deserialize_bool => strict_visit_bool;
    impl char => deserialize_char => strict_visit_char;

    impl String => deserialize_string => strict_visit_string;
}
