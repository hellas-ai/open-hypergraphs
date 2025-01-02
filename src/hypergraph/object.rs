use crate::array::ArrayKind;
use crate::finite_function::FiniteFunction;
use crate::indexed_coproduct::*;
use crate::semifinite::*;

pub struct Hypergraph<K: ArrayKind, O, A> {
    pub s: IndexedCoproduct<K, FiniteFunction<K>>,
    pub t: IndexedCoproduct<K, FiniteFunction<K>>,
    pub w: SemifiniteFunction<K, O>,
    pub x: SemifiniteFunction<K, A>,
}

impl<K: ArrayKind, O, A> Hypergraph<K, O, A> {
    pub fn discrete(_w: SemifiniteFunction<K, O>) -> Hypergraph<K, O, A> {
        todo!();
    }

    pub fn coequalize_vertices(&self, q: &FiniteFunction<K>) -> Option<Hypergraph<K, O, A>> {
        todo!();
    }
}
