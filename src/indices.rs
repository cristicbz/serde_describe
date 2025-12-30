use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, hash::Hash};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub(crate) struct TypeName(
    pub(crate) TypeNameIndex,
    pub(crate) Option<VariantNameIndex>,
);

macro_rules! u32_indices {
    ($($index_ty:ident => $error:ident,)+) => {
        $(
            #[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub(crate) struct $index_ty(u32);

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
        )+
    };
}
pub(crate) trait IsEmpty
where
    Self: 'static + Borrow<Self::Borrowed> + Sized,
{
    type Borrowed: ?Sized;
    const BORROWED_EMPTY: &Self::Borrowed;

    fn is_empty(&self) -> bool;
}

impl<ValueT> IsEmpty for Box<[ValueT]>
where
    ValueT: 'static,
{
    type Borrowed = [ValueT];
    const BORROWED_EMPTY: &Self::Borrowed = &[];

    #[inline]
    fn is_empty(&self) -> bool {
        (**self).is_empty()
    }
}

impl<ValueT> IsEmpty for Vec<ValueT>
where
    ValueT: 'static,
{
    type Borrowed = [ValueT];
    const BORROWED_EMPTY: &Self::Borrowed = &[];

    #[inline]
    fn is_empty(&self) -> bool {
        (**self).is_empty()
    }
}

impl IsEmpty for Box<str> {
    type Borrowed = str;
    const BORROWED_EMPTY: &<Self as IsEmpty>::Borrowed = "";

    #[inline]
    fn is_empty(&self) -> bool {
        (**self).is_empty()
    }
}

impl IsEmpty for &'static str {
    type Borrowed = str;
    const BORROWED_EMPTY: &<Self as IsEmpty>::Borrowed = "";

    #[inline]
    fn is_empty(&self) -> bool {
        (**self).is_empty()
    }
}

pub(crate) trait IndexIsEmpty:
    Sized + Copy + PartialEq + 'static + IsEmpty<Borrowed = Self>
{
    const EMPTY: Self;
}

impl<IndexT> IndexIsEmpty for IndexT
where
    IndexT: Copy + PartialEq + 'static + IsEmpty<Borrowed = Self>,
{
    const EMPTY: Self = *Self::BORROWED_EMPTY;
}

macro_rules! impl_is_empty {
    ($($index_ty:ident,)+) => {
        $(
            #[allow(unused)]
            impl IsEmpty for $index_ty {
                type Borrowed = Self;
                const BORROWED_EMPTY: &<Self as IsEmpty>::Borrowed = &Self(0);

                #[inline]
                fn is_empty(&self) -> bool {
                    *self == Self(0)
                }
            }
        )+
    };
}

u32_indices! {
    SchemaNodeIndex => TooManySchemaNodes,
    SchemaNodeListIndex => TooManySchemaNodeLists,
    MemberListIndex => TooManyMembers,
    MemberIndex => TooManyMembers,
    TypeNameIndex => TooManyNames,
    VariantNameIndex => TooManyNames,
    FieldNameIndex => TooManyNames,
    FieldNameListIndex => TooManyFieldNameLists,
    TraceIndex => TooManyValues,
}

impl_is_empty! {
    SchemaNodeIndex,
    SchemaNodeListIndex,
    MemberListIndex,
    FieldNameListIndex,
    TypeNameIndex,
    VariantNameIndex,
    FieldNameIndex,
}
