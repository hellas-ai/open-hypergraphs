use crate::array::{Array, ArrayKind, NaturalArray};
use crate::finite_function::{coequalizer_universal, FiniteFunction};
use crate::indexed_coproduct::*;
use crate::operations::Operations;
use crate::semifinite::*;

use core::ops::Add;
use num_traits::Zero;

pub enum InvalidHypergraph {
    Todo,
}

pub struct Hypergraph<K: ArrayKind, O, A> {
    pub s: IndexedCoproduct<K, FiniteFunction<K>>,
    pub t: IndexedCoproduct<K, FiniteFunction<K>>,
    pub w: SemifiniteFunction<K, O>,
    pub x: SemifiniteFunction<K, A>,
}

impl<K: ArrayKind, O, A> Hypergraph<K, O, A>
where
    K::Type<K::I>: AsRef<K::Index>,
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    pub fn new(
        s: IndexedCoproduct<K, FiniteFunction<K>>,
        t: IndexedCoproduct<K, FiniteFunction<K>>,
        w: SemifiniteFunction<K, O>,
        x: SemifiniteFunction<K, A>,
    ) -> Result<Hypergraph<K, O, A>, InvalidHypergraph> {
        let h = Hypergraph { s, t, w, x };
        h.validate()
    }

    pub fn validate(self) -> Result<Self, InvalidHypergraph> {
        Ok(self)
    }

    // TODO: This is the unit object - put inside category interface?
    /// Construct the empty hypergraph with no nodes and no hyperedges.
    pub fn empty() -> Hypergraph<K, O, A> {
        Hypergraph {
            s: IndexedCoproduct::initial(K::I::zero()),
            t: IndexedCoproduct::initial(K::I::zero()),
            w: SemifiniteFunction::zero(),
            x: SemifiniteFunction::zero(),
        }
    }

    /// The discrete hypergraph, consisting of hypernodes labeled in `O`.
    pub fn discrete(w: SemifiniteFunction<K, O>) -> Hypergraph<K, O, A> {
        Hypergraph {
            s: IndexedCoproduct::initial(w.len()),
            t: IndexedCoproduct::initial(w.len()),
            w,
            x: SemifiniteFunction::zero(),
        }
    }

    pub fn is_discrete(&self) -> bool {
        self.s.is_empty() && self.t.is_empty() && self.x.0.is_empty()
    }

    pub fn coequalize_vertices(&self, q: &FiniteFunction<K>) -> Option<Hypergraph<K, O, A>> {
        let s = self.s.map_values(q);
        let t = self.t.map_values(q);
        let w = SemifiniteFunction(coequalizer_universal(q, &self.w.0)?);
        let x = self.x.clone();
        Some(Hypergraph { s, t, w, x })
    }

    pub fn coproduct(&self, other: &Self) -> Self {
        Hypergraph {
            s: self.s.tensor(&other.s),
            t: self.t.tensor(&other.t),
            w: &self.w + &other.w,
            x: &self.x + &other.x,
        }
    }

    pub fn tensor_operations(_operations: Operations<K, O, A>) -> Hypergraph<K, O, A> {
        todo!()
    }
}

impl<K: ArrayKind, O, A> Add<&Hypergraph<K, O, A>> for &Hypergraph<K, O, A>
where
    K::Type<K::I>: AsRef<K::Index>,
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    type Output = Hypergraph<K, O, A>;

    fn add(self, rhs: &Hypergraph<K, O, A>) -> Self::Output {
        self.coproduct(rhs)
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
