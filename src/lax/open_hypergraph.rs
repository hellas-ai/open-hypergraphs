//! Cospans of Hypergraphs.
use super::hypergraph::*;
use crate::array::vec::VecKind;

/// A lax OpenHypergraph is a cospan of lax hypergraphs:
/// a hypergraph equipped with two finite maps representing the *interfaces*.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(
        bound = "O: serde::Serialize + serde::de::DeserializeOwned, A: serde::Serialize + serde::de::DeserializeOwned"
    )
)]
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

    pub fn from_strict(f: crate::strict::open_hypergraph::OpenHypergraph<VecKind, O, A>) -> Self {
        let sources = f.s.table.0.into_iter().map(NodeId).collect();
        let targets = f.t.table.0.into_iter().map(NodeId).collect();
        let hypergraph = Hypergraph::from_strict(f.h);
        OpenHypergraph {
            sources,
            targets,
            hypergraph,
        }
    }

    /// Create a new node in the hypergraph labeled `w`.
    pub fn new_node(&mut self, w: O) -> NodeId {
        self.hypergraph.new_node(w)
    }

    pub fn new_edge(&mut self, x: A, interface: impl Into<Hyperedge>) -> EdgeId {
        self.hypergraph.new_edge(x, interface)
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

    /// An [`OpenHypergraph`] consisting of a single operation.
    pub fn singleton(x: A, source_type: Vec<O>, target_type: Vec<O>) -> Self {
        let mut f = Self::empty();
        let (_, (s, t)) = f.new_operation(x, source_type, target_type);
        f.sources = s;
        f.targets = t;
        f
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

    /// Set the nodes of the OpenHypergraph, possibly changing types.
    /// Returns None if new nodes array had different length.
    pub fn with_nodes<T, F: FnOnce(Vec<O>) -> Vec<T>>(self, f: F) -> Option<OpenHypergraph<T, A>> {
        self.hypergraph
            .with_nodes(f)
            .map(|hypergraph| OpenHypergraph {
                sources: self.sources,
                targets: self.targets,
                hypergraph,
            })
    }

    /// Map the node labels of this OpenHypergraph, possibly changing their type
    pub fn map_nodes<F: Fn(O) -> T, T>(self, f: F) -> OpenHypergraph<T, A> {
        OpenHypergraph {
            sources: self.sources,
            targets: self.targets,
            hypergraph: self.hypergraph.map_nodes(f),
        }
    }

    /// Set the edges of the OpenHypergraph, possibly changing types.
    /// Returns None if new edges array had different length.
    pub fn with_edges<T, F: FnOnce(Vec<A>) -> Vec<T>>(self, f: F) -> Option<OpenHypergraph<O, T>> {
        self.hypergraph
            .with_edges(f)
            .map(|hypergraph| OpenHypergraph {
                sources: self.sources,
                targets: self.targets,
                hypergraph,
            })
    }

    /// Map the edge labels of this OpenHypergraph, possibly changing their type
    pub fn map_edges<F: Fn(A) -> T, T>(self, f: F) -> OpenHypergraph<O, T> {
        OpenHypergraph {
            sources: self.sources,
            targets: self.targets,
            hypergraph: self.hypergraph.map_edges(f),
        }
    }
}

impl<O: Clone, A> OpenHypergraph<O, A> {
    pub fn identity(a: Vec<O>) -> Self {
        let mut f = OpenHypergraph::empty();
        f.sources = (0..a.len()).map(NodeId).collect();
        f.targets = (0..a.len()).map(NodeId).collect();
        f.hypergraph.nodes = a;
        f
    }
}

impl<O: Clone + PartialEq, A: Clone> OpenHypergraph<O, A> {
    /// Apply the quotient map to identify nodes in the internal [`Hypergraph`].
    /// This deletes the internal quotient map, resulting in a *strict* [`OpenHypergraph`].
    pub fn quotient(&mut self) {
        self.quotient_witness();
    }

    /// Like [`Self::quotient`], but also returns the coequalizer [`crate::finite_function::FiniteFunction`]
    /// mapping pre-quotient node indices to post-quotient node indices.
    pub fn quotient_witness(&mut self) -> crate::finite_function::FiniteFunction<VecKind> {
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

        q
    }

    /// Convert this *lax* [`OpenHypergraph`] to a strict [`crate::strict::OpenHypergraph`] by
    /// quotienting.
    pub fn to_strict(mut self) -> crate::strict::OpenHypergraph<VecKind, O, A> {
        use crate::array::vec::VecArray;
        use crate::finite_function::FiniteFunction;
        use crate::strict::open_hypergraph::OpenHypergraph;

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

        OpenHypergraph::new(s, t, h).expect("any valid lax::Hypergraph must be quotientable!")
    }

    // Old name for `to_strict`. Provided for backwards compatibility
    #[deprecated(since = "0.2.4", note = "renamed to_strict")]
    pub fn to_open_hypergraph(self) -> crate::strict::OpenHypergraph<VecKind, O, A> {
        self.to_strict()
    }
}
