//! [`IndexedCoproduct`] as collections of [`FiniteFunction`]s.
//! NOTE: the implementations here are not optimized.
use crate::array::*;
#[allow(unused_imports)] // for docs
use crate::finite_function::*;
use crate::indexed_coproduct::*;
use crate::semifinite::*;
use core::iter::IntoIterator;
use num_traits::{One, Zero};

/// Iterator for IndexedCoproduct that yields each element
pub struct IndexedCoproductSemifiniteFunctionIterator<K: ArrayKind, T> {
    /// Cumulative sum of sources of an indexed coproduct
    pointers: K::Type<K::I>,

    /// Unchanged values array
    values: SemifiniteFunction<K, T>,

    /// index of next slice.
    index: K::I,
}

impl<K: ArrayKind, T> Iterator for IndexedCoproductSemifiniteFunctionIterator<K, T>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<T>: Array<K, T>,
    K::I: Into<usize>,
{
    type Item = SemifiniteFunction<K, T>;

    fn next(&mut self) -> Option<Self::Item> {
        // Check if we've reached the end of the iterator
        if self.index >= self.pointers.len() - K::I::one() {
            return None;
        }

        // Get the start and end indices for this slice
        let start = self.pointers.get(self.index.clone());
        let end = self.pointers.get(self.index.clone() + K::I::one());

        // Create a SemifiniteFunction from the slice of values between start and end
        let values = self.values.0.get_range(start.clone()..end.clone());

        // Create a new SemifiniteFunction with the extracted values
        let ff = SemifiniteFunction::new(K::Type::<T>::from_slice(values));

        // Increment the index for the next call
        self.index = self.index.clone() + K::I::one();

        Some(ff)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = (self.pointers.len() - K::I::one()).into();
        (n, Some(n)) // exact size is known
    }
}

impl<K: ArrayKind, T> ExactSizeIterator for IndexedCoproductSemifiniteFunctionIterator<K, T>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<T>: Array<K, T>,
    K::I: Into<usize>,
{
    fn len(&self) -> usize {
        (self.pointers.len() - K::I::one()).into()
    }
}

impl<K: ArrayKind, T> IntoIterator for IndexedCoproduct<K, SemifiniteFunction<K, T>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<T>: Array<K, T>,
    K::I: Into<usize>,
{
    type Item = SemifiniteFunction<K, T>;
    type IntoIter = IndexedCoproductSemifiniteFunctionIterator<K, T>;

    fn into_iter(self) -> Self::IntoIter {
        IndexedCoproductSemifiniteFunctionIterator {
            pointers: self.sources.table.into().cumulative_sum(),
            values: self.values,
            index: K::I::zero(),
        }
    }
}

use crate::array::vec::VecKind;

impl<T> IndexedCoproduct<VecKind, SemifiniteFunction<VecKind, T>> {
    pub fn iter(&self) -> impl Iterator<Item = &[T]> {
        let pointers = self.sources.table.cumulative_sum();
        (0..pointers.len() - 1).map(move |i| {
            let start = pointers.get(i);
            let end = pointers.get(i + 1);
            &self.values.0[start..end]
        })
    }
}
