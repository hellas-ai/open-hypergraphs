use {
    super::object::Hypergraph,
    crate::{
        array::{Array, ArrayKind},
        finite_function::FiniteFunction,
    },
};

use core::fmt::Debug;

#[derive(Debug)]
pub enum InvalidHypergraphArrow {
    NotNaturalW,
    NotNaturalX,
}

pub struct HypergraphArrow<K: ArrayKind, O, A> {
    /// Source hypergraph
    pub source: Hypergraph<K, O, A>,

    /// target hypergraph
    pub target: Hypergraph<K, O, A>,

    /// Natural transformation on wires
    pub w: FiniteFunction<K>,

    /// Natural transformation on operations
    pub x: FiniteFunction<K>,
}

impl<K: ArrayKind, O, A> HypergraphArrow<K, O, A>
where
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    /// Safely create a new HypergraphArrow by checking naturality of `w` and `x`.
    pub fn new(
        source: Hypergraph<K, O, A>,
        target: Hypergraph<K, O, A>,
        w: FiniteFunction<K>,
        x: FiniteFunction<K>,
    ) -> Result<Self, InvalidHypergraphArrow> {
        Self {
            source,
            target,
            w,
            x,
        }
        .validate()
    }

    /// Check validity of a HypergraphArrow.
    pub fn validate(self) -> Result<Self, InvalidHypergraphArrow> {
        // for self : g → h
        let g = &self.source;
        let h = &self.target;

        // Naturality checks:
        // wire labels, operation labels, and operation types should be preserved under the natural
        // transformations w and x.

        // Check naturality of w
        if g.w != (&self.w >> &h.w).unwrap() {
            return Err(InvalidHypergraphArrow::NotNaturalW);
        }

        // Check naturality of x
        if g.x != (&self.x >> &h.x).unwrap() {
            return Err(InvalidHypergraphArrow::NotNaturalX);
        }

        Ok(self)

        // TODO: add this check.
        // Types of operations are also preserved under w and x.
        //assert_eq!(g.s.values >> g.w, h.s.indexed_values(f.x) >> h.w);
        //assert_eq!(g.t.values >> g.w, h.t.indexed_values(f.x) >> h.w);
    }
}

impl<K: ArrayKind, O: Debug, A: Debug> Debug for HypergraphArrow<K, O, A>
where
    K::Index: Debug,
    K::Type<K::I>: Debug,
    K::Type<A>: Debug,
    K::Type<O>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HypergraphArrow")
            .field("source", &self.source)
            .field("target", &self.target)
            .field("w", &self.w)
            .field("x", &self.x)
            .finish()
    }
}

impl<K: ArrayKind, O: Debug, A: Debug> Clone for HypergraphArrow<K, O, A>
where
    K::Type<K::I>: Clone,
    K::Type<A>: Clone,
    K::Type<O>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            source: self.source.clone(),
            target: self.target.clone(),
            w: self.w.clone(),
            x: self.x.clone(),
        }
    }
}
