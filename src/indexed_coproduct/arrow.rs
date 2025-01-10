use crate::array::*;
use crate::category::*;
use crate::finite_function::*;
use crate::semifinite::*;

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

impl<K: ArrayKind, F: Clone> IndexedCoproduct<K, F>
where
    K::Type<K::I>: NaturalArray<K>,
{
    pub fn len(&self) -> K::I {
        // NOTE: this forces the NaturalArray bound on `K::Type<K::I>` since we can't witness these
        // types as equal.
        self.sources.0.as_ref().len()
    }

    pub fn initial(_target: K::I) -> Self {
        todo!()
    }

    pub fn is_empty(&self) -> bool {
        todo!()
    }

    pub fn values(self) -> F {
        self.values
    }

    /// Compose two `IndexedCoproduct` thought of as lists-of-lists.
    ///
    /// An indexed (finite) coproduct `c` consists of a mapping
    /// `s : A → K`
    /// and
    /// of arrows `f : s(a) is a map `x : Σ_{a ∈ A} s(a) → B`,
    /// where `s(a)
    ///
    ///
    /// ```text
    /// x : Σ_{a ∈ A} s(a) → B      aka A → B*
    /// y : Σ_{b ∈ B} s(b) → C      aka B → C*
    /// z : Σ_{a ∈ A} s'(a) → C     aka A → C*
    /// ```
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
    K::Type<K::I>: NaturalArray<K>,
{
    pub fn singleton(values: FiniteFunction<K>) -> Self {
        let n = values.source();
        let sources = SemifiniteFunction::singleton(n);
        IndexedCoproduct { sources, values }
    }

    // This could generalise to any type with a tensor product, but we only need it for finite functions
    pub fn tensor(
        &self,
        other: &IndexedCoproduct<K, FiniteFunction<K>>,
    ) -> IndexedCoproduct<K, FiniteFunction<K>> {
        IndexedCoproduct {
            sources: self.sources.coproduct(&other.sources),
            values: &self.values | &other.values,
        }
    }

    pub fn indexed_values(&self, _x: &FiniteFunction<K>) -> FiniteFunction<K> {
        todo!()
    }

    pub fn map_values(&self, _x: &FiniteFunction<K>) -> Self {
        todo!()
    }

    pub fn map_indexes(&self, _x: &FiniteFunction<K>) -> Self {
        todo!()
    }
}

// NOTE: this (unfortunately) reproduces the code in the FiniteFunction case
impl<K: ArrayKind, T> IndexedCoproduct<K, SemifiniteFunction<K, T>>
where
    K::Type<T>: Array<K, T>,
    K::Type<K::I>: NaturalArray<K>,
{
    pub fn singleton(values: SemifiniteFunction<K, T>) -> Self {
        let n = values.len();
        let sources = SemifiniteFunction::singleton(n);
        IndexedCoproduct { sources, values }
    }
}
