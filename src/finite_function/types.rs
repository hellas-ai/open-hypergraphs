use crate::array::*;
use crate::category::*;

use std::ops::Shr;

/// A finite function is an array of indices in a range `{0..N}` for some `N ∈ Nat`
#[derive(PartialEq, Eq)]
pub struct FiniteFunction<K: ArrayKind> {
    pub table: K::Type<K::I>,
    pub target: K::I,
}

impl<K: ArrayKind> Arrow for FiniteFunction<K>
where
    K::Type<K::I>: NaturalArray<K>,
{
    type Object = K::I;

    fn source(&self) -> Self::Object {
        // TODO: doing .len() then .into() basically assumes K::I is isomorphic to usize.
        // Either return a usize from len, or make the isomorphism explicit?
        self.table.len().into()
    }

    fn target(&self) -> Self::Object {
        self.target.clone()
    }

    fn identity(a: &Self::Object) -> Self {
        let table = K::Type::arange(&0.into(), a);
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

// Syntactic sugar for finite function composition
impl<K: ArrayKind> Shr<&FiniteFunction<K>> for &FiniteFunction<K>
where
    K::Type<K::I>: NaturalArray<K>,
{
    type Output = Option<FiniteFunction<K>>;

    fn shr(self, rhs: &FiniteFunction<K>) -> Option<FiniteFunction<K>> {
        self.compose(rhs)
    }
}
