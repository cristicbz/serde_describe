use indexmap::IndexSet;
use std::{hash::Hash, marker::PhantomData, ops::Index};

use crate::errors::SerError;

#[derive(Debug, Clone)]
pub(crate) struct Pool<ValueT, ValueIndexT> {
    inner: IndexSet<ValueT>,
    _dummy: PhantomData<ValueIndexT>,
}

impl<ValueT, ValueIndexT> IntoIterator for Pool<ValueT, ValueIndexT> {
    type IntoIter = <IndexSet<ValueT> as IntoIterator>::IntoIter;
    type Item = ValueT;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
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

impl<ValueT, ValueIndexT> Index<ValueIndexT> for Pool<ValueT, ValueIndexT>
where
    usize: From<ValueIndexT>,
{
    type Output = ValueT;

    #[inline]
    fn index(&self, index: ValueIndexT) -> &Self::Output {
        self.inner
            .get_index(usize::from(index))
            .expect("no such index in pool")
    }
}

impl<ValueT, ValueIndexT> Pool<ValueT, ValueIndexT>
where
    ValueT: Hash + Eq,
    ValueIndexT: TryFrom<usize, Error = SerError>,
{
    // pub(crate) fn new() -> Self {
    //     Self::default()
    // }

    pub(crate) fn intern(&mut self, value: ValueT) -> Result<ValueIndexT, SerError> {
        ValueIndexT::try_from(self.inner.insert_full(value).0)
    }

    pub(crate) fn intern_from<FromT>(&mut self, value: FromT) -> Result<ValueIndexT, SerError>
    where
        ValueT: From<FromT>,
    {
        ValueIndexT::try_from(self.inner.insert_full(value.into()).0)
    }

    // pub(crate) fn intern_borrowed<'s, 'q, QueryT>(
    //     &'s mut self,
    //     query: &'q QueryT,
    // ) -> Result<ValueIndexT, SerError>
    // where
    //     QueryT: ?Sized + Hash + Equivalent<ValueT>,
    //     ValueT: From<&'q QueryT>,
    // {
    //     let index = match self.inner.get_full(query) {
    //         Some((index, _)) => index,
    //         None => self.inner.insert_full(query.into()).0,
    //     };
    //     ValueIndexT::try_from(index)
    // }
}
