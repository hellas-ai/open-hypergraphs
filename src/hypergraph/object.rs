use crate::array::ArrayKind;
use crate::finite_function::FiniteFunction;
use crate::indexed_coproduct::*;
use crate::semifinite::*;

use core::ops::Add;

pub struct Hypergraph<K: ArrayKind, O, A> {
    pub s: IndexedCoproduct<K, FiniteFunction<K>>,
    pub t: IndexedCoproduct<K, FiniteFunction<K>>,
    pub w: SemifiniteFunction<K, O>,
    pub x: SemifiniteFunction<K, A>,
}

impl<K: ArrayKind, O, A> Hypergraph<K, O, A> {
    // TODO: This is the unit object - put inside category interface
    pub fn empty() -> Hypergraph<K, O, A> {
        todo!()
    }

    pub fn discrete(_w: SemifiniteFunction<K, O>) -> Hypergraph<K, O, A> {
        todo!();
    }

    pub fn coequalize_vertices(&self, q: &FiniteFunction<K>) -> Option<Hypergraph<K, O, A>> {
        todo!();
    }
}

impl<K: ArrayKind, O, A> Add<&Hypergraph<K, O, A>> for &Hypergraph<K, O, A> {
    type Output = Hypergraph<K, O, A>;

    fn add(self, rhs: &Hypergraph<K, O, A>) -> Self::Output {
        todo!()
    }
}

impl<K: ArrayKind, O, A> Clone for Hypergraph<K, O, A>
where
    K::Type<K::I>: Clone,
    K::Type<O>: Clone,
    K::Type<A>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            s: self.s.clone(),
            t: self.t.clone(),
            w: self.w.clone(),
            x: self.x.clone(),
        }
    }
}
