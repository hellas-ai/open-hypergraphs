use crate::array::*;
use crate::category::*;
use crate::finite_function::*;
use crate::semifinite::*;

use core::fmt::Debug;
use core::ops::{Add, Shr};
use num_traits::{One, Zero};

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
    /// A ['FiniteFunction'] whose sum is the length of `values`, and whose target is `sum + 1`.
    pub sources: FiniteFunction<K>,

    /// The concatenation of all arrays in the coproduct.
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
    /// Create a new IndexedCoproduct from a FiniteFunction whose target is the sum of its
    /// elements. This condition is checked by summing the array.
    pub fn new(sources: FiniteFunction<K>, values: F) -> Option<Self> {
        IndexedCoproduct { sources, values }.validate()
    }

    pub fn from_semifinite(sources: SemifiniteFunction<K, K::I>, values: F) -> Option<Self> {
        let sources = FiniteFunction::new(sources.0.into(), values.len() + K::I::one())?;
        IndexedCoproduct { sources, values }.validate()
    }

    fn validate(self) -> Option<Self> {
        let sum = self.sources.table.sum();

        // Target of 'sources' must equal its sum + 1
        if self.sources.target != sum.clone() + K::I::one() {
            return None;
        }

        if sum != self.values.len() {
            return None;
        }

        Some(self)
    }
}

impl<K: ArrayKind, F: Clone + HasLen<K>> IndexedCoproduct<K, F>
where
    K::Type<K::I>: NaturalArray<K>,
{
    /// Construct a segmented array with a single segment containing values.
    pub fn singleton(values: F) -> Self {
        let n = values.len();
        let sources = FiniteFunction::constant(K::I::one(), n, K::I::zero());
        IndexedCoproduct { sources, values }
    }

    /// Construct a segmented array with `values.len()` segments, each containing a single element.
    pub fn elements(values: F) -> Self {
        let n = values.len();

        // Construct the sources array directly: an array of constant 1s with target n+1.
        let sources =
            FiniteFunction::new(K::Index::fill(K::I::one(), n.clone()), n + K::I::one()).unwrap();

        //let sources = FiniteFunction::terminal(n.clone()).inject1(n);
        IndexedCoproduct::new(sources, values).expect("by construction")
    }

    pub fn len(&self) -> K::I {
        self.sources.source()
    }

    /// Compose two [`IndexedCoproduct`] thought of as lists-of-lists.
    ///
    /// Given
    ///
    /// ```text
    /// x : Σ_{a ∈ A} s(a) → B      aka A → B*
    /// y : Σ_{b ∈ B} s(b) → C      aka B → C*
    /// ```
    ///
    /// we obtain
    ///
    /// ```text
    /// x.flatmap(y) : Σ_{a ∈ A} s(a) → C     aka A → C*
    /// ```
    pub fn flatmap<G: Clone>(&self, other: &IndexedCoproduct<K, G>) -> IndexedCoproduct<K, G> {
        // The number of ...?
        assert_eq!(self.values.len(), other.len());

        let sources = FiniteFunction {
            table: self.sources.table.segmented_sum(&other.sources.table),
            target: other.sources.target.clone(), // TODO: write a test for this
        };
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
    /// Note that the target of `sources` must be zero for laws to work here.
    pub fn initial(target: K::I) -> Self {
        let sources = FiniteFunction::initial(K::I::one());
        let values = FiniteFunction::initial(target);
        IndexedCoproduct { sources, values }
    }

    // This could generalise to any type with a tensor product, but we only need it for finite functions
    pub fn tensor(
        &self,
        other: &IndexedCoproduct<K, FiniteFunction<K>>,
    ) -> IndexedCoproduct<K, FiniteFunction<K>> {
        // build a new finite function for 'sources'. it consists of:
        //  - concatenated segment sizes
        //  - target equal to *total sum* (sum of targets)
        let table = self.sources.table.concatenate(&other.sources.table);
        let target = (self.sources.target.clone() + other.sources.target.clone()) - K::I::one();

        IndexedCoproduct {
            sources: FiniteFunction { table, target },
            values: &self.values | &other.values,
        }
    }

    /// Map the *values* array of an indexed coproduct, leaving the sources unchanged.
    ///
    /// Given an indexed coproduct
    ///
    /// ```text
    /// Σ_{i ∈ I} f_i : Σ_{i ∈ I} A_i → B
    /// ```
    ///
    /// and a finite function `x : B → C`,
    /// return a new [`IndexedCoproduct`] representing
    ///
    /// ```text
    /// Σ_{i ∈ I} (f_i ; x) : Σ_{i ∈ I} A_i → C
    /// ```
    ///
    /// Returns `None` if `x.source() != B`.
    pub fn map_values(&self, x: &FiniteFunction<K>) -> Option<Self> {
        Some(Self {
            sources: self.sources.clone(),
            values: (&self.values >> x)?,
        })
    }

    // TODO: FIXME: including this is annoying. Can we roll map_values and map_semifinite into one
    // function by just requiring the `F` parameter to be post-composable with FiniteFunction?
    /// Same as `map_values`, but for `SemifiniteFunction`.
    pub fn map_semifinite<T>(
        &self,
        x: &SemifiniteFunction<K, T>,
    ) -> Option<IndexedCoproduct<K, SemifiniteFunction<K, T>>>
    where
        K::Type<T>: Array<K, T>,
    {
        Some(IndexedCoproduct::<K, SemifiniteFunction<K, T>> {
            sources: self.sources.clone(),
            values: (&self.values >> x)?,
        })
    }
}

// Special case methods for SemifiniteFunction
// NOTE: this is a bit of a hack: it requires Shr and Add instances for the values type (F)
// so this impl works for both SemifiniteFunction and FiniteFunction values arrays.
impl<K: ArrayKind, F> IndexedCoproduct<K, F>
where
    K::Type<K::I>: NaturalArray<K>,
    F: HasLen<K> + Clone,
    for<'a, 'b> &'a FiniteFunction<K>: Shr<&'b F, Output = Option<F>>, // compose
    for<'a, 'b> &'a F: Add<&'b F, Output = Option<F>>,                 // coproduct
{
    pub fn coproduct(&self, other: &Self) -> Option<Self> {
        // build a new finite function for 'sources'. it consists of:
        //  - concatenated segment sizes
        //  - target equal to *total sum* (sum of targets)
        let table = self.sources.table.concatenate(&other.sources.table);
        let target = (self.sources.target.clone() + other.sources.target.clone()) - K::I::one();

        // NOTE: this might fail if the two underlying FiniteFunctions do not share a codomain.
        Some(IndexedCoproduct {
            sources: FiniteFunction { table, target },
            values: (&self.values + &other.values)?,
        })
    }

    /// Given an [`IndexedCoproduct`] of [`SemifiniteFunction`]:
    ///
    /// ```text
    /// Σ_{i ∈ X} f_i : Σ_{i ∈ X} A_i → B
    /// ```
    ///
    /// and a finite function `x : W → X`
    ///
    /// Return a new [`IndexedCoproduct`] representing
    ///
    /// ```text
    /// Σ_{i ∈ W} f_{x(i)} : Σ_{i ∈ W} A_{x(i)} → B
    /// ```
    pub fn map_indexes(&self, x: &FiniteFunction<K>) -> Option<Self> {
        let sources = x.compose(&self.sources)?;
        let values = self.indexed_values(x)?;
        IndexedCoproduct::from_semifinite(SemifiniteFunction(sources.table.into()), values)
    }

    /// Like [`Self::map_indexes`] but only returns the values array.
    pub fn indexed_values(&self, x: &FiniteFunction<K>) -> Option<F> {
        &self.sources.injections(x)? >> &self.values
    }
}

impl<K: ArrayKind, F: Debug> Debug for IndexedCoproduct<K, F>
where
    K::Index: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IndexedCoproduct")
            .field("sources", &self.sources)
            .field("values", &self.values)
            .finish()
    }
}
