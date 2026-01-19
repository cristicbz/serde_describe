use serde::{
    Serialize,
    de::{DeserializeSeed, Deserializer, EnumAccess, Unexpected, VariantAccess},
};

pub(crate) struct ChunkedEnum<InnerT> {
    num_bytes: usize,
    variants: &'static [&'static str],
    discriminant: [u8; 8],
    inner: InnerT,
}

impl<'value, ValueT> ChunkedEnum<&'value ValueT>
where
    ValueT: Serialize,
{
    #[inline]
    pub(crate) fn serializable<ErrorT>(
        num_bits: usize,
        discriminant: u64,
        value: &'value ValueT,
    ) -> Result<Self, ErrorT>
    where
        ErrorT: serde::ser::Error,
    {
        if num_bits <= 64 {
            Ok(Self {
                num_bytes: num_bits.div_ceil(8),
                variants: &[], // unused for serialization
                discriminant: discriminant.to_le_bytes(),
                inner: value,
            })
        } else {
            Err(ErrorT::custom("too many bits in chunked enum"))
        }
    }
}

impl<ValueT> Serialize for ChunkedEnum<&'_ ValueT>
where
    ValueT: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if self.num_bytes == 0 {
            return self.inner.serialize(serializer);
        }

        let i_byte = self.num_bytes - 1;
        let new_byte = self.discriminant[i_byte];
        if i_byte == 0 {
            serializer.serialize_newtype_variant(
                UNION_ENUM_NAME,
                u32::from(new_byte),
                serialized_anonymous_variant(new_byte),
                self.inner,
            )
        } else {
            serializer.serialize_newtype_variant(
                UNION_ENUM_NAME,
                u32::from(new_byte),
                serialized_anonymous_variant(new_byte),
                &ChunkedEnum {
                    num_bytes: i_byte,
                    ..*self
                },
            )
        }
    }
}

impl<'de, SeederT, SeedT> ChunkedEnum<SeederT>
where
    SeederT: FnOnce(u64) -> SeedT,
    SeedT: DeserializeSeed<'de>,
{
    #[inline]
    pub(crate) fn deserializable<ErrorT>(num_bits: usize, seeder: SeederT) -> Result<Self, ErrorT>
    where
        ErrorT: serde::de::Error,
    {
        if num_bits <= 64 {
            let (num_bytes, variants) = match (num_bits / 8, num_bits % 8) {
                (0, 0) => (0, &[][..]),
                (quotient, 0) => (quotient, &UNION_ENUM_VARIANT_NAMES[..]),
                (quotient, remainder) => {
                    (quotient + 1, &UNION_ENUM_VARIANT_NAMES[..(1 << remainder)])
                }
            };

            Ok(Self {
                num_bytes,
                variants,
                discriminant: [0u8; 8],
                inner: seeder,
            })
        } else {
            Err(ErrorT::custom("too many bits in chunked enum"))
        }
    }
}

impl<'de, SeederT, SeedT> DeserializeSeed<'de> for ChunkedEnum<SeederT>
where
    SeederT: FnOnce(u64) -> SeedT,
    SeedT: DeserializeSeed<'de>,
{
    type Value = SeedT::Value;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        if self.num_bytes > 0 {
            deserializer.deserialize_enum(UNION_ENUM_NAME, self.variants, self)
        } else {
            (self.inner)(u64::from_le_bytes(self.discriminant)).deserialize(deserializer)
        }
    }
}

impl<'de, SeederT, SeedT> serde::de::Visitor<'de> for ChunkedEnum<SeederT>
where
    SeederT: FnOnce(u64) -> SeedT,
    SeedT: DeserializeSeed<'de>,
{
    type Value = SeedT::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "chunked discriminant")
    }

    fn visit_enum<A>(mut self, data: A) -> Result<Self::Value, A::Error>
    where
        A: EnumAccess<'de>,
    {
        let i_byte = self.num_bytes - 1;
        let (new_byte, data) = data.variant_seed(AnonymousVariantSeed)?;
        self.discriminant[i_byte] = new_byte;

        if i_byte > 0 {
            self.num_bytes = i_byte;
            self.variants = UNION_ENUM_VARIANT_NAMES;
            data.newtype_variant_seed(self)
        } else {
            data.newtype_variant_seed((self.inner)(u64::from_le_bytes(self.discriminant)))
        }
    }
}

#[inline]
fn serialized_anonymous_variant(discriminant: u8) -> &'static str {
    UNION_ENUM_VARIANT_NAMES[usize::from(discriminant)]
}

struct AnonymousVariantSeed;

impl<'de> DeserializeSeed<'de> for AnonymousVariantSeed {
    type Value = u8;

    fn deserialize<D>(self, deserializer: D) -> Result<u8, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_identifier(self)
    }
}

impl<'de> serde::de::Visitor<'de> for AnonymousVariantSeed {
    type Value = u8;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "union discriminant")
    }

    #[inline]
    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        u8::try_from(value).map_err(|_| E::invalid_value(Unexpected::Unsigned(value), &self))
    }

    #[inline]
    fn visit_u8<E>(self, value: u8) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(value)
    }

    #[inline]
    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        (if let Some(hex) = value.strip_prefix('_') {
            u8::from_str_radix(hex, 16).ok()
        } else {
            None
        })
        .ok_or_else(|| E::custom(format!("bad union variant name {value}")))
    }
}

const UNION_ENUM_NAME: &str = "Union";
const UNION_ENUM_VARIANT_NAMES: &[&str; 256] = &{
    const HEX: [u8; 16] = *b"0123456789abcdef";

    // Creates the variant names `_00`, `_01`, ..., `_ff` as byte arrays.
    const AS_BYTES: [[u8; 3]; 256] = {
        let mut variants = [[0u8; 3]; 256];
        let mut high_nibble = 0;
        while high_nibble < 16 {
            let mut low_nibble = 0;
            while low_nibble < 16 {
                variants[high_nibble * 16 + low_nibble] = [b'_', HEX[high_nibble], HEX[low_nibble]];
                low_nibble += 1;
            }
            high_nibble += 1;
        }
        variants
    };

    // Convert the byte byte arrays to `str`-s.
    let mut strings = [""; 256];
    let mut i_variant = 0;
    while i_variant < 256 {
        strings[i_variant] = match str::from_utf8(&AS_BYTES[i_variant]) {
            Ok(string) => string,
            Err(_) => unreachable!(),
        };
        i_variant += 1;
    }
    strings
};
