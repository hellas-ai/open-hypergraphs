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
    /// The empty OpenHypergraph with no nodes and no edges.
    ///
    /// In categorical terms, this is the identity map at the unit object.
    pub fn empty() -> Self {
        OpenHypergraph {
            sources: vec![],
            targets: vec![],
            hypergraph: Hypergraph::empty(),
        }
    }

    /// Create a new node in the hypergraph labeled `w`.
    pub fn new_node(&mut self, w: O) -> NodeId {
        self.hypergraph.new_node(w)
    }

    /// Create a new "operation" in the hypergraph.
    /// Concretely, `f.new_operation(x, s, t)` mutates `f` by adding:
    ///
    /// 1. a new hyperedge labeled `x`
    /// 2. `len(s)` new nodes, with the `i`th node labeled `s[i]`
    /// 3. `len(t)` new nodes, with the `i`th node labeled `t[i]`
    ///
    /// Returns the new hyperedge ID and the [`NodeId`]s of the source/target nodes.
    ///
    /// This is a convenience wrapper for [`Hypergraph::new_operation`]
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

    pub fn add_edge_source(&mut self, edge_id: EdgeId, w: O) -> NodeId {
        self.hypergraph.add_edge_source(edge_id, w)
    }

    pub fn add_edge_target(&mut self, edge_id: EdgeId, w: O) -> NodeId {
        self.hypergraph.add_edge_target(edge_id, w)
    }
}

impl<O: Clone + PartialEq, A: Clone + PartialEq> OpenHypergraph<O, A> {
    /// Apply the quotient map to identify nodes in the internal [`Hypergraph`].
    /// This deletes the internal quotient map, resulting in a *strict* [`OpenHypergraph`].
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

    /// Convert this *relaxed* [`OpenHypergraph`] to a strict [`crate::prelude::OpenHypergraph`] by
    /// quotienting.
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
