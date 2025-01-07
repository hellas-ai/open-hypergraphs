use crate::array::*;
use crate::category::*;
use crate::finite_function::*;
use crate::semifinite::*;

use core::ops::Add;

// TODO: replace sources with a FiniteFunction<K> of *pointers* whose codomain is total size?
// This lets us remove a lot of trait bounds.
/// A finite coproduct of arrows of type `A`.
/// Pragmatically, it's a segmented array
pub struct IndexedCoproduct<K: ArrayKind, F> {
    sources: SemifiniteFunction<K, K::I>,
    values: F,
}

impl<K: ArrayKind, F: Clone> Clone for IndexedCoproduct<K, F>
where
    K::Type<K::I>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            sources: self.sources.clone(),
            values: self.values.clone(),
        }
    }
}

impl<K: ArrayKind, T> Add<IndexedCoproduct<K, SemifiniteFunction<K, T>>>
    for IndexedCoproduct<K, SemifiniteFunction<K, T>>
where
    K::Type<K::I>: Array<K, K::I>,
    K::Type<T>: Array<K, T>,
{
    type Output = IndexedCoproduct<K, SemifiniteFunction<K, T>>;

    fn add(
        self,
        rhs: IndexedCoproduct<K, SemifiniteFunction<K, T>>,
    ) -> IndexedCoproduct<K, SemifiniteFunction<K, T>> {
        IndexedCoproduct {
            sources: self.sources + rhs.sources,
            values: self.values + rhs.values,
        }
    }
}

impl<K: ArrayKind> Add<IndexedCoproduct<K, FiniteFunction<K>>>
    for IndexedCoproduct<K, FiniteFunction<K>>
where
    K::Type<K::I>: Array<K, K::I>,
{
    type Output = Option<IndexedCoproduct<K, FiniteFunction<K>>>;

    fn add(
        self,
        rhs: IndexedCoproduct<K, FiniteFunction<K>>,
    ) -> Option<IndexedCoproduct<K, FiniteFunction<K>>> {
        Some(IndexedCoproduct {
            sources: self.sources + rhs.sources,
            values: (self.values.coproduct(&rhs.values))?,
        })
    }
}

impl<K: ArrayKind, F: Clone> IndexedCoproduct<K, F>
// TODO: delete this trait bound; use sources: FiniteFunction<K> instead?
where
    K::Type<K::I>: AsRef<K::Index>,
{
    pub fn len(&self) -> K::I {
        self.sources.0.as_ref().len()
    }

    pub fn singleton(_values: F) -> Self {
        todo!()
        //let sources: SemifiniteFunction<K, K::I> = todo!();
        //IndexedCoproduct { sources, values }
    }

    // TODO: check for correctness! Implement me.
    pub fn flatmap(&self, other: &IndexedCoproduct<K, F>) -> IndexedCoproduct<K, F> {
        let x: &K::Index = self.sources.0.as_ref();
        let y: &K::Index = other.sources.0.as_ref();
        let sources: SemifiniteFunction<K, K::I> = SemifiniteFunction(x.segmented_sum(y).into());
        let values = other.values.clone();
        IndexedCoproduct { sources, values }
    }
}

// Special case methods where the values are a "segmented finite function".
impl<K: ArrayKind> IndexedCoproduct<K, FiniteFunction<K>>
where
    K::Type<K::I>: Array<K, K::I>,
{
    // This could generalise to any Tensor type, but we only need it for finite functions
    pub fn tensor(
        &self,
        _other: &IndexedCoproduct<K, FiniteFunction<K>>,
    ) -> IndexedCoproduct<K, FiniteFunction<K>> {
        todo!()
    }

    pub fn indexed_values(&self, _x: &FiniteFunction<K>) -> FiniteFunction<K> {
        todo!()
    }

    pub fn map_values(&self, _x: &FiniteFunction<K>) -> FiniteFunction<K> {
        todo!()
    }

    pub fn map_indexes(&self, _x: &FiniteFunction<K>) -> FiniteFunction<K> {
        todo!()
    }
}
