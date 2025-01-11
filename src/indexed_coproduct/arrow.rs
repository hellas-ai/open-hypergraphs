use crate::array::*;
use crate::category::*;
use crate::finite_function::*;
use crate::semifinite::*;

use num_traits::Zero;

// The minimum set of operations some arrows must have in order to define an [`IndexedCoproduct`]
// over them.
pub trait HasLen<K: ArrayKind> {
    fn len(&self) -> K::I;
    fn is_empty(&self) -> bool {
        self.len() == K::I::zero()
    }
}

impl<K: ArrayKind> HasLen<K> for FiniteFunction<K> {
    fn len(&self) -> K::I {
        self.source()
    }
}

impl<K: ArrayKind, T> HasLen<K> for SemifiniteFunction<K, T>
where
    K::Type<T>: Array<K, T>,
{
    fn len(&self) -> K::I {
        self.0.len()
    }
}

// TODO: replace sources with a FiniteFunction<K> of *pointers* whose codomain is total size?
// This lets us remove a lot of trait bounds.
/// A finite coproduct of arrows of type `A`.
/// Pragmatically, it's a segmented array
#[non_exhaustive] // force construction via new.
pub struct IndexedCoproduct<K: ArrayKind, F> {
    pub sources: SemifiniteFunction<K, K::I>,
    pub values: F,
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

impl<K: ArrayKind, F: Clone + HasLen<K>> IndexedCoproduct<K, F>
where
    K::Type<K::I>: NaturalArray<K>,
{
    // TODO: need to know a length of values!
    pub fn new(sources: SemifiniteFunction<K, K::I>, values: F) -> Option<Self> {
        if sources.0.as_ref().sum() > values.len() {
            return None;
        }

        // TODO: check "length" of values!
        Some(IndexedCoproduct { sources, values })
    }

    pub fn singleton(values: F) -> Self {
        let n = values.len();
        let sources = SemifiniteFunction::singleton(n);
        IndexedCoproduct { sources, values }
    }

    pub fn len(&self) -> K::I {
        // NOTE: this forces the NaturalArray bound on `K::Type<K::I>` since we can't witness these
        // types as equal.
        self.sources.0.as_ref().len()
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

impl<K: ArrayKind, F> HasLen<K> for IndexedCoproduct<K, F>
where
    K::Type<K::I>: NaturalArray<K>,
{
    fn len(&self) -> K::I {
        self.sources.len()
    }
}

// Special case methods where the values are finite functions.
impl<K: ArrayKind> IndexedCoproduct<K, FiniteFunction<K>>
where
    K::Type<K::I>: NaturalArray<K>,
{
    /// The initial object, i.e., the finite coproduct indexed by the empty set
    pub fn initial(target: K::I) -> Self {
        let sources = SemifiniteFunction::zero();
        let values = FiniteFunction::initial(target);
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
