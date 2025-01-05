use crate::array::*;
use crate::category::*;

use core::ops::{BitOr, Shr};
use num_traits::{One, Zero};

/// A finite function is an array of indices in a range `{0..N}` for some `N ∈ Nat`
#[derive(PartialEq, Eq, Debug)]
pub struct FiniteFunction<K: ArrayKind> {
    pub table: K::Index,
    pub target: K::I,
}

// Ad-hoc methods for finite functions
impl<K: ArrayKind> FiniteFunction<K> {
    /// Construct a FiniteFunction from a table of indices
    pub fn new(table: K::Index, target: K::I) -> Option<FiniteFunction<K>> {
        // If table was nonempty and had a value larger or equal to codomain, this is invalid.
        if let Some(true) = table.max().map(|m| m >= target) {
            return None;
        }
        Some(FiniteFunction { table, target })
    }

    /// Directly compute `f ; ι₀` instead of by composition.
    ///
    /// ```rust
    /// # use open_hypergraphs::array::vec::*;
    /// # use open_hypergraphs::category::*;
    /// # use open_hypergraphs::finite_function::*;
    /// # let f = FiniteFunction::<VecKind>::identity(5);
    /// # let b = 3;
    /// # let i0 = FiniteFunction::<VecKind>::inj0(f.target(), b);
    /// assert_eq!(Some(f.inject0(b)), &f >> &i0);
    /// ```
    pub fn inject0(&self, b: K::I) -> FiniteFunction<K> {
        FiniteFunction {
            table: self.table.clone(),
            target: b + self.target(),
        }
    }

    /// Directly compute `f ; ι₁` instead of by composition.
    ///
    /// ```rust
    /// # use open_hypergraphs::array::vec::*;
    /// # use open_hypergraphs::category::*;
    /// # use open_hypergraphs::finite_function::*;
    /// # let f = FiniteFunction::<VecKind>::identity(5);
    /// # let a = 3;
    /// # let i1 = FiniteFunction::<VecKind>::inj1(a, f.target());
    /// assert_eq!(Some(f.inject1(a)), &f >> &i1);
    /// ```
    pub fn inject1(&self, a: K::I) -> FiniteFunction<K> {
        FiniteFunction {
            table: a.clone() + &self.table,
            target: a + self.target.clone(),
        }
    }

    /// Given a finite function `f : A → B`, return the initial map `initial : 0 → B`.
    pub fn to_initial(&self) -> FiniteFunction<K> {
        Self::initial(self.target.clone())
    }

    pub fn coequalizer(&self, other: &Self) -> FiniteFunction<K> {
        todo!();
    }

    pub fn coequalizer_universal(&self, other: &Self) -> FiniteFunction<K> {
        todo!();
    }

    /// Given a finite function `s : N → K`
    /// representing the objects of the finite coproduct
    /// `Σ_{n ∈ N} s(n)`
    /// whose injections have the type
    /// `ι_x : s(x) → Σ_{n ∈ N} s(n)`,
    /// and given a finite map
    /// `a : A → N`,
    /// compute the coproduct of injections
    /// ```text
    /// injections(s, a) : Σ_{x ∈ A} s(x) → Σ_{n ∈ N} s(n)
    /// injections(s, a) = Σ_{x ∈ A} ι_a(x)
    /// ```
    /// So that `injections(s, id) == id`
    ///
    /// Note that when a is a permutation,
    /// injections(s, a) is a "blockwise" version of that permutation with block
    /// sizes equal to s.
    /// """
    pub fn injections(&self, a: &FiniteFunction<K>) -> Option<Self> {
        let s = self;
        let p = self.table.cumulative_sum();

        // TODO: better errors!
        let k = (a >> s)?;
        let r = k.table.segmented_arange();

        let repeats = k.table;
        let values = p.gather(a.table.get_range(..));
        let z = repeats.repeat(values.get_range(..));

        Some(FiniteFunction {
            table: r + z,
            target: p.get(p.len() - K::I::one()),
        })
    }
}

impl<K: ArrayKind> Arrow for FiniteFunction<K> {
    type Object = K::I;

    fn source(&self) -> Self::Object {
        self.table.len()
    }

    fn target(&self) -> Self::Object {
        self.target.clone()
    }

    fn identity(a: Self::Object) -> Self {
        let table = K::Index::arange(&K::I::zero(), &a);
        let target = a.clone();
        FiniteFunction { table, target }
    }

    fn compose(&self, other: &Self) -> Option<Self> {
        if self.target == other.source() {
            let table = other.table.gather(self.table.get_range(..));
            let target = other.target.clone();
            Some(FiniteFunction { table, target })
        } else {
            None
        }
    }
}

impl<K: ArrayKind> Coproduct for FiniteFunction<K> {
    fn initial_object() -> Self::Object {
        K::I::zero()
    }

    fn initial(a: Self::Object) -> Self {
        Self {
            table: K::Index::empty(),
            target: a.clone(),
        }
    }

    fn coproduct(&self, other: &Self) -> Option<Self> {
        if self.target != other.target {
            return None;
        }

        Some(Self {
            table: self.table.concatenate(&other.table),
            target: self.target.clone(),
        })
    }

    /// Coproduct injection 0.
    ///
    /// As an array, the indices `0..a`
    fn inj0(a: Self::Object, b: Self::Object) -> Self {
        let table = K::Index::arange(&K::I::zero(), &a);
        let target = a.clone() + b.clone();
        Self { table, target }
    }

    /// Coproduct injection 1.
    ///
    /// As an array, the indices `a..(a+b)`
    ///
    /// ```rust
    /// # use open_hypergraphs::category::*;
    /// # use open_hypergraphs::finite_function::*;
    /// # use open_hypergraphs::array::vec::*;
    /// assert_eq!(
    ///     FiniteFunction::<VecKind>::inj1(3, 5).table,
    ///     VecArray(vec![3,4,5,6,7]),
    ///     )
    /// ```
    fn inj1(a: Self::Object, b: Self::Object) -> Self {
        let target = a.clone() + b.clone();
        let table = K::Index::arange(&a, &target);
        Self { table, target }
    }
}

impl<K: ArrayKind> Monoidal for FiniteFunction<K> {
    // the unit object
    fn unit() -> Self::Object {
        K::I::zero()
    }

    fn tensor(&self, other: &Self) -> Self {
        // NOTE: this uses the `Add<&K::Index>` bound on `K::I` to compute offset piece without
        // unnecessary cloning.
        let table = self
            .table
            .concatenate(&(self.target.clone() + &other.table));
        let target = self.target.clone() + other.target.clone();
        Self { table, target }
    }
}

impl<K: ArrayKind> SymmetricMonoidal for FiniteFunction<K> {
    fn twist(a: K::I, b: K::I) -> Self {
        // This is more efficiently expressed as arange + b `mod` target,
        // but this would require adding an operation add_mod(a, b, n) to the array trait.
        let target = a.clone() + b.clone();
        let lhs = K::Index::arange(&b, &target);
        let rhs = K::Index::arange(&a, &b);
        let table = lhs.concatenate(&rhs);
        Self { table, target }
    }
}

// Syntactic sugar for finite function composition
impl<K: ArrayKind> Shr<&FiniteFunction<K>> for &FiniteFunction<K> {
    type Output = Option<FiniteFunction<K>>;

    fn shr(self, rhs: &FiniteFunction<K>) -> Option<FiniteFunction<K>> {
        self.compose(rhs)
    }
}

// Parallel composition
impl<K: ArrayKind> BitOr<&FiniteFunction<K>> for &FiniteFunction<K> {
    type Output = FiniteFunction<K>;
    fn bitor(self, rhs: &FiniteFunction<K>) -> FiniteFunction<K> {
        self.tensor(rhs)
    }
}

impl<K: ArrayKind> Clone for FiniteFunction<K> {
    fn clone(&self) -> Self {
        Self {
            table: self.table.clone(),
            target: self.target.clone(),
        }
    }
}
