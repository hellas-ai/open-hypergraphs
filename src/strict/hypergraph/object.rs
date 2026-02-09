use crate::array::{Array, ArrayKind, NaturalArray};
use crate::category::*;
use crate::finite_function::{coequalizer_universal, FiniteFunction};
use crate::indexed_coproduct::*;
use crate::operations::Operations;
use crate::semifinite::*;
use crate::strict::hypergraph::arrow::HypergraphArrow;
use crate::strict::hypergraph::subobject::SubgraphMorphism;

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

    pub fn coproduct(&self, other: &Self) -> Self {
        Hypergraph {
            s: self.s.tensor(&other.s),
            t: self.t.tensor(&other.t),
            w: self.w.coproduct(&other.w),
            x: self.x.coproduct(&other.x),
        }
    }

    pub fn tensor_operations(Operations { x, a, b }: Operations<K, O, A>) -> Hypergraph<K, O, A> {
        // NOTE: the validity of the result assumes validity of `operations`.
        let inj0 = FiniteFunction::inj0(a.values.len(), b.values.len());
        let inj1 = FiniteFunction::inj1(a.values.len(), b.values.len());
        let s = IndexedCoproduct::new(a.sources, inj0).expect("invalid Operations?");
        let t = IndexedCoproduct::new(b.sources, inj1).expect("invalid Operations?");
        let w = a.values + b.values;
        Hypergraph { s, t, w, x }
    }
}

impl<K: ArrayKind, O, A> Hypergraph<K, O, A>
where
    K::Type<K::I>: AsRef<K::Index>,
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A>,
{
    pub fn coequalize_vertices(&self, q: &FiniteFunction<K>) -> Option<Hypergraph<K, O, A>> {
        // TODO: wrap coequalizers in a newtype!
        let s = self.s.map_values(q)?;
        let t = self.t.map_values(q)?;
        let w = SemifiniteFunction(coequalizer_universal(q, &self.w.0)?);
        let x = self.x.clone();
        Some(Hypergraph { s, t, w, x })
    }

    pub fn remainder_with_injection(
        &self,
        w_map: &FiniteFunction<K>,
        x_map: &FiniteFunction<K>,
    ) -> Option<(Hypergraph<K, O, A>, FiniteFunction<K>, FiniteFunction<K>)>
    where
        K::Type<bool>: Array<K, bool>,
        for<'a> K::Slice<'a, K::I>: From<&'a [K::I]>,
    {
        if w_map.target() != self.w.len() || x_map.target() != self.x.len() {
            return None;
        }

        let remainder = SubgraphMorphism::from_maps(self, w_map, x_map);
        remainder.as_hypergraph_with_injections()
    }

    pub(crate) fn remainder_from_masks(
        &self,
        remove_node_mask: K::Type<bool>,
        remove_edge_mask: K::Type<bool>,
    ) -> Option<(Hypergraph<K, O, A>, FiniteFunction<K>, FiniteFunction<K>)>
    where
        K::Type<bool>: Array<K, bool>,
        for<'a> K::Slice<'a, K::I>: From<&'a [K::I]>,
    {
        let remainder = SubgraphMorphism::from_masks(self, remove_node_mask, remove_edge_mask);
        remainder.as_hypergraph_with_injections()
    }

    // Compute the pushout of a span of wire maps into hypergraphs.
    pub(crate) fn pushout_along_span(
        left: &Hypergraph<K, O, A>,
        right: &Hypergraph<K, O, A>,
        f: &FiniteFunction<K>,
        g: &FiniteFunction<K>,
    ) -> Option<(
        Hypergraph<K, O, A>,
        HypergraphArrow<K, O, A>,
        HypergraphArrow<K, O, A>,
    )>
    where
        K::Type<K::I>: NaturalArray<K>,
        K::Type<O>: Array<K, O> + PartialEq,
        K::Type<A>: Array<K, A> + PartialEq,
    {
        let q = f.coequalizer(g)?;
        let coproduct = left + right;
        let target = coproduct.coequalize_vertices(&q)?;

        let w_left = FiniteFunction::inj0(left.w.len(), right.w.len()).compose(&q)?;
        let w_right = FiniteFunction::inj1(left.w.len(), right.w.len()).compose(&q)?;
        let x_left = FiniteFunction::inj0(left.x.len(), right.x.len());
        let x_right = FiniteFunction::inj1(left.x.len(), right.x.len());

        let left_arrow = HypergraphArrow::new(left.clone(), target.clone(), w_left, x_left).ok()?;
        let right_arrow =
            HypergraphArrow::new(right.clone(), target.clone(), w_right, x_right).ok()?;

        Some((target, left_arrow, right_arrow))
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
