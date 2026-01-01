//! TODO: crate level docs
//!
//! Known limitations:
//! * At most 64 "skippable" fields. This can be lifted by using a `BitVec` instead of as single
//!   u64 to keep track of them. This is a backwards compatible change that can be made in the
//!   future.
//! * Similarly, there is a limit of 256 union variants. This can be lifted by splitting the
//!   discriminator into multiple bytes (variant 0x03fe becomes `_03(_fe(data))`). This trick is
//!   already done to support more than 8 skippable fields and can be extended backwards
//!   compatibly.
#![deny(missing_docs)]

pub(crate) mod anonymous_union;
pub(crate) mod builder;
pub(crate) mod de;
pub(crate) mod deferred;
pub(crate) mod described;
pub(crate) mod dump;
pub(crate) mod errors;
pub(crate) mod indices;
pub(crate) mod pool;
pub(crate) mod schema;
pub(crate) mod ser;
pub(crate) mod trace;

pub use builder::SchemaBuilder;
pub use described::{DescribedBy, SelfDescribed};
pub use schema::Schema;
pub use trace::Trace;

#[cfg(test)]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize, de::DeserializeOwned};

    fn if_zero(value: &u32) -> bool {
        *value == 0
    }

    fn to_self_described_bitcode<T: Serialize>(value: T) -> Vec<u8> {
        bitcode::serialize(&SelfDescribed(value)).unwrap()
    }

    fn from_self_described_bitcode<'de, T: Deserialize<'de>>(bytes: &'de [u8]) -> T {
        bitcode::deserialize::<SelfDescribed<T>>(bytes).unwrap().0
    }

    fn bitcode_roundtrip<T: Serialize + DeserializeOwned>(value: &T) -> T {
        from_self_described_bitcode(&to_self_described_bitcode(value))
    }

    fn to_self_described_postcard<T: Serialize>(value: T) -> Vec<u8> {
        postcard::to_stdvec(&SelfDescribed(value)).unwrap()
    }

    fn from_self_described_postcard<'de, T: Deserialize<'de>>(bytes: &'de [u8]) -> T {
        postcard::from_bytes::<SelfDescribed<T>>(bytes).unwrap().0
    }

    fn postcard_roundtrip<T: Serialize + DeserializeOwned>(value: &T) -> T {
        from_self_described_postcard(&to_self_described_postcard(value))
    }

    #[test]
    fn test_always_skipped() {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct WithSkipped {
            never_skipped: u32,

            #[serde(default, skip_serializing_if = "if_zero")]
            always_skipped: u32,
        }

        let original = vec![
            WithSkipped {
                never_skipped: 1,
                always_skipped: 0,
            },
            WithSkipped {
                never_skipped: 2,
                always_skipped: 0,
            },
        ];
        assert_eq!(original, bitcode_roundtrip(&original));
        assert_eq!(original, postcard_roundtrip(&original));
    }
}
