use crate::array::*;
use crate::category::*;

use core::ops::{BitOr, Shr};
use num_traits::Zero;

/// A finite function is an array of indices in a range `{0..N}` for some `N ∈ Nat`
#[derive(PartialEq, Eq)]
pub struct FiniteFunction<K: ArrayKind> {
    pub table: K::Index,
    pub target: K::I,
}

impl<K: ArrayKind> FiniteFunction<K> {
    pub fn inject0(&self, b: K::I) -> FiniteFunction<K> {
        todo!();
    }

    pub fn inject1(&self, a: K::I) -> FiniteFunction<K> {
        todo!();
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
            let target = self.target.clone();
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
    // the 0 → 0 map: the empty array going to 0-ary indices
    fn unit() -> Self::Object {
        K::I::zero()
    }

    // Problem: need to compute other + offset
    // Solutions:
    //  - change other to mut or not borrowing (CHANGES TENSOR)
    //  - clone other (EXPENSIVE; add Clone to trait; perf improves if we wrap vecs in e.g., Arc/Cow)
    //  - custom add_constant method taking a ref, returning new array (AD-HOC)
    //  - mutable add_constant_to_range?
    //  - require constant + &K::I
    fn tensor(&self, other: &Self) -> Self {
        // Write in the obvious way, clones others.
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
