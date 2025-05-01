//! [`IndexedCoproduct`] as collections of [`FiniteFunction`]s.
//! NOTE: the implementations here are not optimized.
use {
    crate::{array::*, finite_function::*, indexed_coproduct::*},
    core::iter::IntoIterator,
    num_traits::{One, Zero},
};

/// Iterator for IndexedCoproduct that yields each element
pub struct IndexedCoproductFiniteFunctionIterator<K: ArrayKind> {
    /// Cumulative sum of sources of an indexed coproduct
    pointers: K::Type<K::I>,

    /// Unchanged values array
    values: FiniteFunction<K>,

    /// index of next slice.
    index: K::I,
}

impl<K: ArrayKind> Iterator for IndexedCoproductFiniteFunctionIterator<K>
where
    K::Type<K::I>: NaturalArray<K>,
    K::I: Into<usize>,
{
    type Item = FiniteFunction<K>;

    fn next(&mut self) -> Option<Self::Item> {
        // Check if we've reached the end of the iterator
        if self.index >= self.pointers.len() - K::I::one() {
            return None;
        }

        // Get the start and end indices for this slice
        let start = self.pointers.get(self.index.clone());
        let end = self.pointers.get(self.index.clone() + K::I::one());

        // Create a FiniteFunction from the slice of values between start and end
        let values = self.values.table.get_range(start.clone()..end.clone());

        // Create a new FiniteFunction with the extracted values
        let ff = FiniteFunction {
            table: K::Index::from_slice(values),
            target: self.values.target.clone(),
        };

        // Increment the index for the next call
        self.index = self.index.clone() + K::I::one();

        Some(ff)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = (self.pointers.len() - K::I::one()).into();
        (n, Some(n)) // exact size is known
    }
}

impl<K: ArrayKind> ExactSizeIterator for IndexedCoproductFiniteFunctionIterator<K>
where
    K::Type<K::I>: NaturalArray<K>,
    K::I: Into<usize>,
{
    fn len(&self) -> usize {
        (self.pointers.len() - K::I::one()).into()
    }
}

impl<K: ArrayKind> IntoIterator for IndexedCoproduct<K, FiniteFunction<K>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::I: Into<usize>,
{
    type Item = FiniteFunction<K>;
    type IntoIter = IndexedCoproductFiniteFunctionIterator<K>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            pointers: self.sources.table.into().cumulative_sum(),
            values: self.values,
            index: K::I::zero(),
        }
    }
}
