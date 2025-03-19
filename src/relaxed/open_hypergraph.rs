//! Cospans of Hypergraphs.
use super::hypergraph::*;

/// A relaxed OpenHypergraph is a cospan of relaxed hypergraphs:
/// a hypergraph equipped with two finite maps representing the *interfaces*.
#[derive(Debug, Clone)]
pub struct OpenHypergraph<O, A> {
    pub sources: Vec<NodeId>,
    pub targets: Vec<NodeId>,
    pub hypergraph: Hypergraph<O, A>,
}

// Imperative-specific methods
impl<O, A> OpenHypergraph<O, A> {
    /// the empty (unit) OpenHypergraph - equal to the identity map at the unit object.
    pub fn empty() -> Self {
        OpenHypergraph {
            sources: vec![],
            targets: vec![],
            hypergraph: Hypergraph::empty(),
        }
    }

    /// Convenience wrapper for [`Hypergraph::new_operation`]
    pub fn new_operation(
        &mut self,
        x: A,
        source_type: Vec<O>,
        target_type: Vec<O>,
    ) -> (EdgeId, Interface) {
        self.hypergraph.new_operation(x, source_type, target_type)
    }

    /// Compute an open hypergraph by calling `to_hypergraph` on the internal `Hypergraph`.
    pub fn unify(&mut self, v: NodeId, w: NodeId) {
        self.hypergraph.unify(v, w);
    }
}

impl<O: Clone + PartialEq, A: Clone + PartialEq> OpenHypergraph<O, A> {
    /// Quotient this OpenHypergraph in-place
    pub fn quotient(&mut self) {
        // mutably quotient self.hypergraph, returning the coequalizer q
        let q = self.hypergraph.quotient();

        // note: this is composition of finite functions `q >> self.sources`,
        // but we do it mutably in-place.
        self.sources
            .iter_mut()
            .for_each(|x| *x = NodeId(q.table[x.0]));
        self.targets
            .iter_mut()
            .for_each(|x| *x = NodeId(q.table[x.0]));
    }

    pub fn to_open_hypergraph(mut self) -> crate::prelude::OpenHypergraph<O, A> {
        use crate::array::vec::VecArray;
        use crate::finite_function::FiniteFunction;
        use crate::open_hypergraph::OpenHypergraph;

        self.quotient();

        let target = self.hypergraph.nodes.len();

        let s = {
            let table = self.sources.iter().map(|x| x.0).collect();
            FiniteFunction::new(VecArray(table), target).expect("Valid by construction")
        };

        let t = {
            let table = self.targets.iter().map(|x| x.0).collect();
            FiniteFunction::new(VecArray(table), target).expect("Valid by construction")
        };

        let h = self.hypergraph.to_hypergraph();

        OpenHypergraph::new(s, t, h).expect("any valid relaxed::Hypergraph must be quotientable!")
    }
}
