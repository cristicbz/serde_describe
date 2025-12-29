use serde::{Deserialize, Serialize};
use std::hash::Hash;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub(crate) struct TypeName(pub(crate) NameIndex, pub(crate) Option<NameIndex>);

macro_rules! u32_indices {
    ($($index_ty:ident => $error:ident,)+) => {
        $(
            #[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub(crate) struct $index_ty(nonmax::NonMaxU32);

            impl From<$index_ty> for u32 {
                #[inline]
                fn from(index: $index_ty) -> u32 {
                    index.0.into()
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
                    u32::try_from(value)
                        .ok()
                        .and_then(nonmax::NonMaxU32::new)
                        .map($index_ty)
                        .ok_or($crate::errors::SerError::$error)
                }
            }

            impl TryFrom<u32> for $index_ty {
                type Error = $crate::errors::SerError;

                #[inline]
                fn try_from(value: u32) -> Result<Self, Self::Error> {
                    nonmax::NonMaxU32::new(value)
                        .map($index_ty)
                        .ok_or($crate::errors::SerError::$error)
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
