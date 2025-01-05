use crate::array::*;
use crate::category::*;

use core::ops::{BitOr, Shr};
use num_traits::Zero;

/// A finite function is an array of indices in a range `{0..N}` for some `N ∈ Nat`
#[derive(PartialEq, Eq, Debug)]
pub struct FiniteFunction<K: ArrayKind> {
    pub table: K::Index,
    pub target: K::I,
}

// Ad-hoc methods for finite functions
impl<K: ArrayKind> FiniteFunction<K> {
    pub fn new(table: K::Index, target: K::I) -> Option<FiniteFunction<K>> {
        // If table was nonempty and had a value larger or equal to codomain, this is invalid.
        if let Some(true) = table.max().map(|m| m >= target) {
            return None;
        }
        Some(FiniteFunction { table, target })
    }

    pub fn inject0(&self, b: K::I) -> FiniteFunction<K> {
        todo!();
    }

    pub fn inject1(&self, a: K::I) -> FiniteFunction<K> {
        todo!();
    }

    pub fn to_initial(&self) -> FiniteFunction<K> {
        Self::initial(self.target.clone())
    }

    pub fn coequalizer(&self, other: &Self) -> FiniteFunction<K> {
        todo!();
    }

    pub fn coequalizer_universal(&self, other: &Self) -> FiniteFunction<K> {
        todo!();
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

    fn inj0(a: Self::Object, b: Self::Object) -> Self {
        let table = K::Index::arange(&a, &b);
        let target = a.clone() + b.clone();
        Self { table, target }
    }

    fn inj1(a: Self::Object, b: Self::Object) -> Self {
        let target = a.clone() + b.clone();
        let table = K::Index::arange(&b, &target);
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
