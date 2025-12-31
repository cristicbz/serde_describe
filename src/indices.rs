use serde::{Deserialize, Serialize};
use std::hash::Hash;
use zerocopy::{FromBytes, Immutable};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub(crate) struct TypeName(pub(crate) NameIndex, pub(crate) Option<NameIndex>);

macro_rules! u32_indices {
    ($($index_ty:ident => $error:ident,)+) => {
        $(
            #[derive(Immutable, FromBytes, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[repr(transparent)]
            pub(crate) struct $index_ty(u32);

            impl From<$index_ty> for u32 {
                #[inline]
                fn from(index: $index_ty) -> u32 {
                    index.0
                }
            }

            impl From<$index_ty> for usize {
                #[inline]
                fn from(index: $index_ty) -> usize {
                    usize::try_from(u32::from(index.0)).expect("usize must be at least 32-bits")
                }
            }

            impl TryFrom<usize> for $index_ty {
                type Error = $crate::errors::SerError;

                #[inline]
                fn try_from(value: usize) -> Result<Self, Self::Error> {
                    match u32::try_from(value) {
                        Ok(value) => Ok($index_ty(value)),
                        Err(_) => Err($crate::errors::SerError::$error),
                    }
                }
            }

            impl From<u32> for $index_ty {
                #[inline]
                fn from(value: u32) -> Self {
                    $index_ty(value)
                }
            }

            // No need to waste time deriving these trivial implementations.
            impl Serialize for $index_ty {
                #[inline]
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    self.0.serialize(serializer)
                }
            }

            impl<'de> Deserialize<'de> for $index_ty {
                #[inline]
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: serde::Deserializer<'de>,
                {
                    u32::deserialize(deserializer).map(Self)
                }
            }
        )+
    };
}
u32_indices! {
    SchemaNodeIndex => TooManySchemaNodes,
    SchemaNodeListIndex => TooManySchemaNodeLists,
    FieldListIndex => TooManyFields,
    FieldIndex => TooManyFields,
    NameIndex => TooManyNames,
    NameListIndex => TooManyNameLists,
    TraceIndex => TooManyValues,
}
