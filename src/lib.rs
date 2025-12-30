//! TODO: module level docs
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

pub use builder::{SchemaBuilder, Value};
pub use described::{DescribedBy, SelfDescribed};
pub use schema::Schema;

#[cfg(test)]
mod tests {
    use super::*;
    use serde::{de::DeserializeOwned, Deserialize, Serialize};

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
        panic!("AHH");
        assert_eq!(original, postcard_roundtrip(&original));
    }
}
