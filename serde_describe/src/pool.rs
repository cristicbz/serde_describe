use indexmap::IndexSet;
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, hash::Hash, marker::PhantomData};

use crate::indices::{IndexIsEmpty, IsEmpty};

#[derive(Clone)]
pub(crate) struct Pool<ValueT, ValueIndexT> {
    inner: IndexSet<ValueT>,
    _dummy: PhantomData<ValueIndexT>,
}

impl<ValueT, ValueIndexT> Default for Pool<ValueT, ValueIndexT> {
    #[inline]
    fn default() -> Self {
        Self {
            inner: Default::default(),
            _dummy: PhantomData,
        }
    }
}

impl<ValueT, ValueIndexT> Pool<ValueT, ValueIndexT>
where
    ValueT: Hash + Eq + IsEmpty,
    ValueIndexT: TryFrom<usize> + IndexIsEmpty,
{
    pub(crate) fn intern(&mut self, value: ValueT) -> Result<ValueIndexT, ValueIndexT::Error> {
        if value.is_empty() {
            Ok(ValueIndexT::EMPTY)
        } else {
            ValueIndexT::try_from(self.inner.insert_full(value).0 + 1)
        }
    }

    pub(crate) fn intern_from<FromT>(
        &mut self,
        value: FromT,
    ) -> Result<ValueIndexT, ValueIndexT::Error>
    where
        ValueT: From<FromT>,
        FromT: IsEmpty,
    {
        if value.is_empty() {
            Ok(ValueIndexT::EMPTY)
        } else {
            ValueIndexT::try_from(self.inner.insert_full(value.into()).0 + 1)
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NonEmptyPool<ValueT, ValueIndexT> {
    inner: IndexSet<ValueT>,
    _dummy: PhantomData<ValueIndexT>,
}

impl<ValueT, ValueIndexT> Default for NonEmptyPool<ValueT, ValueIndexT> {
    #[inline]
    fn default() -> Self {
        Self {
            inner: Default::default(),
            _dummy: PhantomData,
        }
    }
}

impl<ValueT, ValueIndexT> NonEmptyPool<ValueT, ValueIndexT>
where
    ValueT: Hash + Eq,
    ValueIndexT: TryFrom<usize>,
{
    pub(crate) fn intern(&mut self, value: ValueT) -> Result<ValueIndexT, ValueIndexT::Error> {
        ValueIndexT::try_from(self.inner.insert_full(value).0)
    }

    pub(crate) fn intern_from<FromT>(
        &mut self,
        value: FromT,
    ) -> Result<ValueIndexT, ValueIndexT::Error>
    where
        ValueT: From<FromT>,
    {
        ValueIndexT::try_from(self.inner.insert_full(value.into()).0)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(transparent)]
pub(crate) struct ReadonlyPool<ValueT, ValueIndexT> {
    values: Box<[ValueT]>,

    #[serde(skip)]
    _dummy: PhantomData<ValueIndexT>,
}

impl<ValueT, ValueIndexT> ReadonlyPool<ValueT, ValueIndexT>
where
    ValueT: IsEmpty,
    ValueIndexT: IsEmpty + Into<usize>,
{
    #[inline]
    pub(crate) fn get(&self, index: ValueIndexT) -> Option<&ValueT::Borrowed> {
        if index.is_empty() {
            Some(ValueT::BORROWED_EMPTY)
        } else {
            self.values.get(index.into() - 1).map(Borrow::borrow)
        }
    }
}

impl<FromT, IntoT, ValueIndexT> From<Pool<FromT, ValueIndexT>> for ReadonlyPool<IntoT, ValueIndexT>
where
    FromT: Into<IntoT>,
{
    #[inline]
    fn from(value: Pool<FromT, ValueIndexT>) -> Self {
        Self {
            values: value.inner.into_iter().map(Into::into).collect(),
            _dummy: PhantomData,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(transparent)]
pub(crate) struct ReadonlyNonEmptyPool<ValueT, ValueIndexT> {
    values: Box<[ValueT]>,

    #[serde(skip)]
    _dummy: PhantomData<ValueIndexT>,
}

impl<ValueT, ValueIndexT> ReadonlyNonEmptyPool<ValueT, ValueIndexT>
where
    ValueIndexT: Into<usize>,
{
    #[inline]
    pub(crate) fn get(&self, index: ValueIndexT) -> Option<&ValueT> {
        self.values.get(index.into())
    }
}

impl<FromT, IntoT, ValueIndexT> From<NonEmptyPool<FromT, ValueIndexT>>
    for ReadonlyNonEmptyPool<IntoT, ValueIndexT>
where
    FromT: Into<IntoT>,
{
    #[inline]
    fn from(value: NonEmptyPool<FromT, ValueIndexT>) -> Self {
        Self {
            values: value.inner.into_iter().map(Into::into).collect(),
            _dummy: PhantomData,
        }
    }
}
