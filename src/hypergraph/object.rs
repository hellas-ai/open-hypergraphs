use crate::array::{Array, ArrayKind, NaturalArray};
use crate::category::*;
use crate::finite_function::{coequalizer_universal, FiniteFunction};
use crate::indexed_coproduct::*;
use crate::operations::Operations;
use crate::semifinite::*;

use core::fmt::Debug;
use core::ops::Add;
use num_traits::Zero;

#[derive(Debug)]
pub enum InvalidHypergraph<K: ArrayKind> {
    SourcesCount(K::I, K::I),
    TargetsCount(K::I, K::I),
    SourcesSet(K::I, K::I),
    TargetsSet(K::I, K::I),
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
    /// Safely create a Hypergraph, ensuring its data is valid.
    pub fn new(
        s: IndexedCoproduct<K, FiniteFunction<K>>,
        t: IndexedCoproduct<K, FiniteFunction<K>>,
        w: SemifiniteFunction<K, O>,
        x: SemifiniteFunction<K, A>,
    ) -> Result<Hypergraph<K, O, A>, InvalidHypergraph<K>> {
        let h = Hypergraph { s, t, w, x };
        h.validate()
    }

    /// A hypergraph is valid when for both sources and targets segmented arrays:
    ///
    /// 1. Number of segments is equal to number of operations (`x.len()`)
    /// 2. Values of segmented array are indices in set `w.source()`
    ///
    pub fn validate(self) -> Result<Self, InvalidHypergraph<K>> {
        // num ops, wires
        let n_x = self.x.len();
        let n_w = self.w.len();

        // Sources segmented array has as many segments as operations
        if self.s.len() != self.x.len() {
            return Err(InvalidHypergraph::SourcesCount(self.s.len(), n_x));
        }

        // Targets segmented array has as many segments as operations
        if self.t.len() != self.x.len() {
            return Err(InvalidHypergraph::TargetsCount(self.t.len(), n_x));
        }

        // *values* of sources segmented array should index into operations
        let n_s = self.s.values.target();
        if n_s != n_w {
            return Err(InvalidHypergraph::SourcesSet(n_s, n_w));
        }

        // *values* of targets segmented array should index into operations
        let n_t = self.t.values.target();
        if n_t != n_w {
            return Err(InvalidHypergraph::TargetsSet(n_t, n_w));
        }

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
            w: self.w.coproduct(&other.w),
            x: self.x.coproduct(&other.x),
        }
    }

    pub fn tensor_operations(operations: Operations<K, O, A>) -> Hypergraph<K, O, A> {
        let Operations { x, a, b } = operations;
        // NOTE: the validity of the result assumes validity of `operations`.
        let inj0 = FiniteFunction::inj0(a.values.len(), b.values.len());
        let inj1 = FiniteFunction::inj0(a.values.len(), b.values.len());
        let s = IndexedCoproduct::new(a.sources, inj0).expect("By construction");
        let t = IndexedCoproduct::new(b.sources, inj1).expect("By construction");
        let w = a.values.coproduct(&b.values);
        Hypergraph { s, t, w, x }
    }
}

impl<K: ArrayKind, O, A> Add<&Hypergraph<K, O, A>> for &Hypergraph<K, O, A>
where
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

// NOTE: manual Debug required because we need to specify array bounds.
impl<K: ArrayKind, O: Debug, A: Debug> Debug for Hypergraph<K, O, A>
where
    K::Index: Debug,
    K::Type<K::I>: Debug,
    K::Type<O>: Debug,
    K::Type<A>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Hypergraph")
            .field("s", &self.s)
            .field("t", &self.t)
            .field("w", &self.w)
            .field("x", &self.x)
            .finish()
    }
}
