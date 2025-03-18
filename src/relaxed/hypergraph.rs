use crate::array::vec::{VecArray, VecKind};
use crate::hypergraph;
use core::fmt::Debug;

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct NodeId(pub usize);

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct EdgeId(pub usize);

#[derive(Debug, Clone)]
pub struct Hyperedge {
    pub sources: Vec<NodeId>,
    pub targets: Vec<NodeId>,
}

pub type Interface = (Vec<NodeId>, Vec<NodeId>);

/// A [`crate::relaxed::Hypergraph`] represents an "un-quotiented" hypergraph.
///
/// It can be thought of as a collection of disconnected operations and wires along with a
/// *quotient map* which can be used with connected components to produce a `Hypergraph`.
#[derive(Debug, Clone)]
pub struct Hypergraph<O, A> {
    /// Node labels. Defines a finite map from [`NodeId`] to node label
    pub nodes: Vec<O>,

    /// Edge labels. Defines a finite map from [`EdgeId`] to edge label
    pub edges: Vec<A>,

    /// Hyperedges map an *ordered list* of source nodes to an ordered list of target nodes.
    pub adjacency: Vec<Hyperedge>,

    // A finite endofunction on the set of nodes, identifying nodes to be quotiented.
    // NOTE: this is a *graph* on the set of nodes.
    pub quotient: (Vec<NodeId>, Vec<NodeId>),
}

impl<O, A> Hypergraph<O, A> {
    /// The empty Hypergraph with no nodes or edges.
    pub fn empty() -> Self {
        Hypergraph {
            nodes: vec![],
            edges: vec![],
            adjacency: vec![],
            quotient: (vec![], vec![]),
        }
    }

    /// Add a single node labeled `w` to the [`Hypergraph`]
    pub fn new_node(&mut self, w: O) -> NodeId {
        let index = self.nodes.len();
        self.nodes.push(w);
        NodeId(index)
    }

    /// Add a single hyperedge labeled `a` to the [`Hypergraph`]
    /// it has sources and targets specified by `interface`
    /// return which `EdgeId` corresponds to that new hyperedge
    pub fn new_edge(&mut self, x: A, interface: Hyperedge) -> EdgeId {
        let edge_idx = self.edges.len();
        self.edges.push(x);
        self.adjacency.push(interface);
        EdgeId(edge_idx)
    }

    /// Append a "singleton" operation to the Hypergraph.
    ///
    /// 1. For each element t of `source_type` (resp. `target_type`), creates a node labeled t
    /// 2. creates An edge labeled `x`, and sets its source/target nodes to those from step (1)
    ///
    /// Returns the index [`EdgeId`] of the operation in the [`Hypergraph`], and its source and
    /// target nodes.
    pub fn new_operation(
        &mut self,
        x: A,
        source_type: Vec<O>,
        target_type: Vec<O>,
    ) -> (EdgeId, Interface) {
        let sources: Vec<NodeId> = source_type.into_iter().map(|t| self.new_node(t)).collect();
        let targets: Vec<NodeId> = target_type.into_iter().map(|t| self.new_node(t)).collect();
        let interface = (sources.clone(), targets.clone());
        let edge_id = self.new_edge(x, Hyperedge { sources, targets });
        (edge_id, interface)
    }

    /// Identify a pair of nodes `(v, w)` by adding them to the quotient map.
    ///
    /// Note that if the labels of `v` and `w` are not equal, then this will not represent a valid
    /// `Hypergraph`.
    /// This is intentional so that typechecking and type inference can be deferred until after
    /// construction of the `Hypergraph`.
    pub fn unify(&mut self, v: NodeId, w: NodeId) {
        // add nodes to the quotient graph
        self.quotient.0.push(v);
        self.quotient.1.push(w);
    }
}

impl<O: Clone + PartialEq, A: Clone + PartialEq> Hypergraph<O, A> {
    /// Construct a [`Hypergraph`] by identifying nodes in the quotient map.
    ///
    /// NOTE: this operation is unchecked; you should verify quotiented nodes have the exact same
    /// type first, or this operation is undefined.
    pub fn quotient(&self) -> hypergraph::Hypergraph<VecKind, O, A> {
        use crate::finite_function::*;

        // Compute the coequalizer of the quotient map
        let q = {
            let s: FiniteFunction<VecKind> = FiniteFunction {
                table: VecArray(self.quotient.0.iter().map(|x| x.0).collect()),
                target: self.nodes.len(),
            };

            let t: FiniteFunction<VecKind> = FiniteFunction {
                table: VecArray(self.quotient.1.iter().map(|x| x.0).collect()),
                target: self.nodes.len(),
            };
            s.coequalizer(&t)
        }
        .expect("coequalizer must exist for any graph");

        // Create a crate::hypergraph::Hypergraph, then coequalize its vertices.
        // TODO: faster to coequalize here?
        make_hypergraph(self)
            .coequalize_vertices(&q)
            .expect("coequalize_vertices should work for any coequalizer")
    }
}

/// Create a [`crate::hypergraph::Hypergraph`] by forgetting about the quotient map.
fn make_hypergraph<O: Clone, A: Clone>(
    h: &Hypergraph<O, A>,
) -> crate::hypergraph::Hypergraph<VecKind, O, A> {
    use crate::finite_function::*;
    use crate::indexed_coproduct::*;
    use crate::semifinite::*;

    let s = {
        let mut lengths = Vec::<usize>::with_capacity(h.edges.len());
        let mut values = Vec::<usize>::new();
        for e in h.adjacency.iter() {
            lengths.push(e.sources.len());
            values.extend(e.sources.iter().map(|x| x.0));
        }

        let sources = SemifiniteFunction(VecArray(lengths));
        let values = FiniteFunction::new(VecArray(values), h.nodes.len())
            .expect("invalid relaxed::Hypergraph!");
        IndexedCoproduct::from_semifinite(sources, values).expect("valid IndexedCoproduct")
    };

    let t = {
        let mut lengths = Vec::<usize>::with_capacity(h.edges.len());
        let mut values = Vec::<usize>::new();
        for e in h.adjacency.iter() {
            lengths.push(e.targets.len());
            values.extend(e.targets.iter().map(|x| x.0));
        }

        let sources = SemifiniteFunction(VecArray(lengths));
        let values = FiniteFunction::new(VecArray(values), h.nodes.len())
            .expect("invalid relaxed::Hypergraph!");
        IndexedCoproduct::from_semifinite(sources, values).expect("valid IndexedCoproduct")
    };

    let w = SemifiniteFunction(VecArray(h.nodes.clone()));
    let x = SemifiniteFunction(VecArray(h.edges.clone()));

    crate::hypergraph::Hypergraph { s, t, w, x }
}
