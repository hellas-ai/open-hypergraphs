use crate::array::vec::{VecArray, VecKind};
use crate::category::Arrow;
use crate::finite_function::*;

use core::fmt::Debug;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NodeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct EdgeId(pub usize);

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Hyperedge {
    pub sources: Vec<NodeId>,
    pub targets: Vec<NodeId>,
}

/// A map on nodes and edges without structure preservation guarantees.
#[derive(Clone)]
pub struct NodeEdgeMap {
    pub nodes: FiniteFunction<VecKind>,
    pub edges: FiniteFunction<VecKind>,
}

#[derive(Clone)]
pub struct LaxSpan<O, A> {
    pub apex: Hypergraph<O, A>,
    pub left: Hypergraph<O, A>,
    pub right: Hypergraph<O, A>,
    pub left_map: NodeEdgeMap,
    pub right_map: NodeEdgeMap,
}

impl<O, A> LaxSpan<O, A> {
    /// Construct a lax span and validate its structural properties.
    pub fn new(
        apex: Hypergraph<O, A>,
        left: Hypergraph<O, A>,
        right: Hypergraph<O, A>,
        left_map: NodeEdgeMap,
        right_map: NodeEdgeMap,
    ) -> Self {
        LaxSpan {
            apex,
            left,
            right,
            left_map,
            right_map,
        }
        .validate()
    }

    /// Validate that maps are compatible with their hypergraphs.
    pub fn validate(self) -> Self {
        if !self.apex.edges.is_empty() {
            panic!(
                "apex must be discrete (no edges), got {} edge(s)",
                self.apex.edges.len()
            );
        }
        if self.left_map.nodes.source() != self.apex.nodes.len() {
            panic!(
                "left map node source size mismatch: got {}, expected {}",
                self.left_map.nodes.source(),
                self.apex.nodes.len()
            );
        }
        if self.left_map.nodes.target() != self.left.nodes.len() {
            panic!(
                "left map node target size mismatch: got {}, expected {}",
                self.left_map.nodes.target(),
                self.left.nodes.len()
            );
        }
        if self.left_map.edges.source() != self.apex.edges.len() {
            panic!(
                "left map edge source size mismatch: got {}, expected {}",
                self.left_map.edges.source(),
                self.apex.edges.len()
            );
        }
        if self.left_map.edges.target() != self.left.edges.len() {
            panic!(
                "left map edge target size mismatch: got {}, expected {}",
                self.left_map.edges.target(),
                self.left.edges.len()
            );
        }
        if self.right_map.nodes.source() != self.apex.nodes.len() {
            panic!(
                "right map node source size mismatch: got {}, expected {}",
                self.right_map.nodes.source(),
                self.apex.nodes.len()
            );
        }
        if self.right_map.nodes.target() != self.right.nodes.len() {
            panic!(
                "right map node target size mismatch: got {}, expected {}",
                self.right_map.nodes.target(),
                self.right.nodes.len()
            );
        }
        if self.right_map.edges.source() != self.apex.edges.len() {
            panic!(
                "right map edge source size mismatch: got {}, expected {}",
                self.right_map.edges.source(),
                self.apex.edges.len()
            );
        }
        if self.right_map.edges.target() != self.right.edges.len() {
            panic!(
                "right map edge target size mismatch: got {}, expected {}",
                self.right_map.edges.target(),
                self.right.edges.len()
            );
        }

        self
    }

    /// Compute the pushout of the span, identifying only nodes.
    ///
    /// NOTE: this assumes the apex is discrete (no edges), so edge identifications are ignored.
    pub fn pushout(&self) -> Hypergraph<O, A>
    where
        O: Clone + PartialEq,
        A: Clone + PartialEq,
    {
        debug_assert!(
            self.apex.edges.is_empty(),
            "pushout assumes discrete apex (no edge identifications)"
        );

        let mut pushout = self.left.coproduct(&self.right);
        let left_nodes = self.left.nodes.len();
        for (k_idx, &l_idx) in self.left_map.nodes.table.iter().enumerate() {
            let r_idx = self.right_map.nodes.table[k_idx] + left_nodes;
            pushout.quotient.0.push(NodeId(l_idx));
            pushout.quotient.1.push(NodeId(r_idx));
        }
        pushout.quotient();
        pushout
    }
}

/// Create a [`Hyperedge`] from a tuple of `(sources, targets)`.
///
/// This allows convenient creation of hyperedges from various collection types:
/// ```
/// # use open_hypergraphs::lax::{Hyperedge, NodeId};
/// let edge: Hyperedge = (vec![NodeId(0), NodeId(1)], vec![NodeId(2)]).into();
/// let edge: Hyperedge = ([NodeId(0), NodeId(1)], [NodeId(2)]).into();
/// ```
impl<S, T> From<(S, T)> for Hyperedge
where
    S: Into<Vec<NodeId>>,
    T: Into<Vec<NodeId>>,
{
    fn from((sources, targets): (S, T)) -> Self {
        Hyperedge {
            sources: sources.into(),
            targets: targets.into(),
        }
    }
}

pub type Interface = (Vec<NodeId>, Vec<NodeId>);

/// A [`crate::lax::Hypergraph`] represents an "un-quotiented" hypergraph.
///
/// It can be thought of as a collection of disconnected operations and wires along with a
/// *quotient map* which can be used with connected components to produce a `Hypergraph`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(
        bound = "O: serde::Serialize + serde::de::DeserializeOwned, A: serde::Serialize + serde::de::DeserializeOwned"
    )
)]
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

/// A lax morphism of hypergraphs that preserves structure up to the target quotient.
pub struct LaxHypergraphArrow<O, A> {
    /// Source hypergraph
    pub source: Hypergraph<O, A>,

    /// Target hypergraph
    pub target: Hypergraph<O, A>,

    /// Map on nodes
    pub w: FiniteFunction<VecKind>,

    /// Map on edges
    pub x: FiniteFunction<VecKind>,
}

fn class_labels<O: Clone + PartialEq>(
    nodes: &[O],
    q: &FiniteFunction<VecKind>,
) -> Result<Vec<O>, (NodeId, NodeId)> {
    let mut classes: Vec<Option<(O, NodeId)>> = vec![None; q.target()];
    for (i, label) in nodes.iter().enumerate() {
        let class = q.table[i];
        match &classes[class] {
            None => classes[class] = Some((label.clone(), NodeId(i))),
            Some((existing, existing_id)) => {
                if existing != label {
                    return Err((NodeId(i), *existing_id));
                }
            }
        }
    }
    Ok(classes
        .into_iter()
        .map(|entry| entry.expect("each class has at least one node").0)
        .collect())
}

impl<O: PartialEq + Clone, A: PartialEq + Clone> LaxHypergraphArrow<O, A> {
    pub fn new(
        source: Hypergraph<O, A>,
        target: Hypergraph<O, A>,
        w: FiniteFunction<VecKind>,
        x: FiniteFunction<VecKind>,
    ) -> Self {
        LaxHypergraphArrow {
            source,
            target,
            w,
            x,
        }
    }

    /// Convert this lax morphism to a strict one by quotienting source/target.
    ///
    /// This panics if the morphism is invalid
    pub fn to_strict(self) -> crate::strict::hypergraph::HypergraphArrow<VecKind, O, A> {
        if self.w.source() != self.source.nodes.len() {
            panic!(
                "node map source size mismatch: got {}, expected {}",
                self.w.source(),
                self.source.nodes.len()
            );
        }
        if self.w.target() != self.target.nodes.len() {
            panic!(
                "node map target size mismatch: got {}, expected {}",
                self.w.target(),
                self.target.nodes.len()
            );
        }
        if self.x.source() != self.source.edges.len() {
            panic!(
                "edge map source size mismatch: got {}, expected {}",
                self.x.source(),
                self.source.edges.len()
            );
        }
        if self.x.target() != self.target.edges.len() {
            panic!(
                "edge map target size mismatch: got {}, expected {}",
                self.x.target(),
                self.target.edges.len()
            );
        }

        let q_src = self.source.coequalizer();
        let q_tgt = self.target.coequalizer();

        let _source_labels = class_labels(&self.source.nodes, &q_src).unwrap_or_else(|(a, b)| {
            panic!(
                "source quotient has conflicting node labels between {:?} and {:?}",
                a, b
            )
        });
        let target_labels = class_labels(&self.target.nodes, &q_tgt).unwrap_or_else(|(a, b)| {
            panic!(
                "target quotient has conflicting node labels between {:?} and {:?}",
                a, b
            )
        });

        for (i, src_label) in self.source.nodes.iter().enumerate() {
            let tgt_node = self.w.table[i];
            let class = q_tgt.table[tgt_node];
            if *src_label != target_labels[class] {
                panic!(
                    "node label mismatch: source node {:?} does not match target class {}",
                    NodeId(i),
                    class
                );
            }
        }

        let mut class_image: Vec<Option<(usize, NodeId)>> = vec![None; q_src.target()];
        for i in 0..self.source.nodes.len() {
            let class = q_src.table[i];
            let image_class = q_tgt.table[self.w.table[i]];
            match class_image[class] {
                None => class_image[class] = Some((image_class, NodeId(i))),
                Some((existing_class, existing_id)) => {
                    if existing_class != image_class {
                        panic!(
                            "node map not constant on source quotient: {:?}->{}, {:?}->{}",
                            NodeId(i),
                            image_class,
                            existing_id,
                            existing_class
                        );
                    }
                }
            }
        }

        for (i, src_label) in self.source.edges.iter().enumerate() {
            let tgt_edge = self.x.table[i];
            if *src_label != self.target.edges[tgt_edge] {
                panic!(
                    "edge label mismatch: source edge {:?} does not match target edge {:?}",
                    EdgeId(i),
                    EdgeId(tgt_edge)
                );
            }

            let src_edge = &self.source.adjacency[i];
            let tgt_edge_adj = &self.target.adjacency[tgt_edge];

            if src_edge.sources.len() != tgt_edge_adj.sources.len() {
                panic!(
                    "source arity mismatch: source edge {:?} vs target edge {:?}",
                    EdgeId(i),
                    EdgeId(tgt_edge)
                );
            }
            if src_edge.targets.len() != tgt_edge_adj.targets.len() {
                panic!(
                    "target arity mismatch: source edge {:?} vs target edge {:?}",
                    EdgeId(i),
                    EdgeId(tgt_edge)
                );
            }

            let src_sources_classes: Vec<usize> = src_edge
                .sources
                .iter()
                .map(|n| q_tgt.table[self.w.table[n.0]])
                .collect();
            let tgt_sources_classes: Vec<usize> = tgt_edge_adj
                .sources
                .iter()
                .map(|n| q_tgt.table[n.0])
                .collect();
            if src_sources_classes != tgt_sources_classes {
                panic!(
                    "source incidence mismatch: source edge {:?} vs target edge {:?}",
                    EdgeId(i),
                    EdgeId(tgt_edge)
                );
            }

            let src_targets_classes: Vec<usize> = src_edge
                .targets
                .iter()
                .map(|n| q_tgt.table[self.w.table[n.0]])
                .collect();
            let tgt_targets_classes: Vec<usize> = tgt_edge_adj
                .targets
                .iter()
                .map(|n| q_tgt.table[n.0])
                .collect();
            if src_targets_classes != tgt_targets_classes {
                panic!(
                    "target incidence mismatch: source edge {:?} vs target edge {:?}",
                    EdgeId(i),
                    EdgeId(tgt_edge)
                );
            }
        }

        let mut source_h = self.source.clone();
        source_h.quotient();
        let mut target_h = self.target.clone();
        target_h.quotient();

        let source_strict = make_hypergraph(&source_h)
            .validate()
            .unwrap_or_else(|err| panic!("invalid source hypergraph after quotient: {:?}", err));
        let target_strict = make_hypergraph(&target_h)
            .validate()
            .unwrap_or_else(|err| panic!("invalid target hypergraph after quotient: {:?}", err));

        let w_composed = (&self.w >> &q_tgt).expect("node map already checked");
        let w_table =
            coequalizer_universal(&q_src, &w_composed.table).expect("compatibility checked");
        let w_strict = FiniteFunction::new(w_table, q_tgt.target()).expect("by construction");

        crate::strict::hypergraph::HypergraphArrow::new(
            source_strict,
            target_strict,
            w_strict,
            self.x,
        )
        .unwrap_or_else(|err| panic!("strict morphism naturality failed: {:?}", err))
    }
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

    /// Check if the quotient map is empty: if so, then this is already a strict OpenHypergraph
    pub fn is_strict(&self) -> bool {
        self.quotient.0.is_empty()
    }

    pub fn from_strict(h: crate::strict::hypergraph::Hypergraph<VecKind, O, A>) -> Self {
        let mut adjacency = Vec::with_capacity(h.x.0.len());
        for (sources, targets) in h.s.into_iter().zip(h.t.into_iter()) {
            adjacency.push(Hyperedge {
                sources: sources.table.iter().map(|i| NodeId(*i)).collect(),
                targets: targets.table.iter().map(|i| NodeId(*i)).collect(),
            })
        }

        Hypergraph {
            nodes: h.w.0 .0,
            edges: h.x.0 .0,
            adjacency,
            quotient: (vec![], vec![]),
        }
    }

    pub fn discrete(nodes: Vec<O>) -> Self {
        let mut h = Self::empty();
        h.nodes = nodes;
        h
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
    pub fn new_edge(&mut self, x: A, interface: impl Into<Hyperedge>) -> EdgeId {
        let edge_idx = self.edges.len();
        self.edges.push(x);
        self.adjacency.push(interface.into());
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

    /// Add a new *source* node labeled `w` to edge `edge_id`.
    pub fn add_edge_source(&mut self, edge_id: EdgeId, w: O) -> NodeId {
        let node_id = self.new_node(w);
        self.adjacency[edge_id.0].sources.push(node_id);
        node_id
    }

    /// Add a new *target* node labeled `w` to edge `edge_id`
    pub fn add_edge_target(&mut self, edge_id: EdgeId, w: O) -> NodeId {
        let node_id = self.new_node(w);
        self.adjacency[edge_id.0].targets.push(node_id);
        node_id
    }

    /// Set the nodes of a Hypergraph, possibly changing types.
    /// Returns None if new nodes array had different length.
    pub fn with_nodes<T, F: FnOnce(Vec<O>) -> Vec<T>>(self, f: F) -> Option<Hypergraph<T, A>> {
        let n = self.nodes.len();
        let nodes = f(self.nodes);
        if nodes.len() != n {
            return None;
        }

        Some(Hypergraph {
            nodes,
            edges: self.edges,
            adjacency: self.adjacency,
            quotient: self.quotient,
        })
    }

    /// Map the node labels of this Hypergraph, possibly changing their type
    pub fn map_nodes<F: Fn(O) -> T, T>(self, f: F) -> Hypergraph<T, A> {
        // note: unwrap is safe because length is preserved
        self.with_nodes(|nodes| nodes.into_iter().map(f).collect())
            .unwrap()
    }

    /// Set the edges of a Hypergraph, possibly changing types.
    /// Returns None if new edges array had different length.
    pub fn with_edges<T, F: FnOnce(Vec<A>) -> Vec<T>>(self, f: F) -> Option<Hypergraph<O, T>> {
        let n = self.edges.len();
        let edges = f(self.edges);
        if edges.len() != n {
            return None;
        }

        Some(Hypergraph {
            nodes: self.nodes,
            edges,
            adjacency: self.adjacency,
            quotient: self.quotient,
        })
    }

    /// Map the edge labels of this Hypergraph, possibly changing their type
    pub fn map_edges<F: Fn(A) -> T, T>(self, f: F) -> Hypergraph<O, T> {
        // note: unwrap is safe because length is preserved
        self.with_edges(|edges| edges.into_iter().map(f).collect())
            .unwrap()
    }

    /// Delete the specified nodes, remapping remaining node indices in adjacency and quotient.
    ///
    /// Out-of-bounds node ids are ignored.
    pub fn delete_nodes(&mut self, node_ids: &[NodeId]) {
        if node_ids.is_empty() {
            return;
        }

        let node_count = self.nodes.len();
        let mut remove = vec![false; node_count];
        let mut any_removed = false;
        let mut remove_count = 0usize;
        for node_id in node_ids {
            if node_id.0 < node_count {
                if !remove[node_id.0] {
                    remove[node_id.0] = true;
                    any_removed = true;
                    remove_count += 1;
                }
            }
        }

        if !any_removed {
            return;
        }

        let mut new_index = vec![None; node_count];
        let mut nodes = Vec::with_capacity(node_count - remove_count);
        for (i, node) in self.nodes.drain(..).enumerate() {
            if !remove[i] {
                let next = nodes.len();
                new_index[i] = Some(next);
                nodes.push(node);
            }
        }
        self.nodes = nodes;

        for edge in &mut self.adjacency {
            edge.sources = edge
                .sources
                .iter()
                .filter_map(|node| new_index[node.0].map(NodeId))
                .collect();
            edge.targets = edge
                .targets
                .iter()
                .filter_map(|node| new_index[node.0].map(NodeId))
                .collect();
        }

        let mut quotient_left = Vec::with_capacity(self.quotient.0.len());
        let mut quotient_right = Vec::with_capacity(self.quotient.1.len());
        for (v, w) in self.quotient.0.iter().zip(self.quotient.1.iter()) {
            if let (Some(v_new), Some(w_new)) = (new_index[v.0], new_index[w.0]) {
                quotient_left.push(NodeId(v_new));
                quotient_right.push(NodeId(w_new));
            }
        }
        self.quotient = (quotient_left, quotient_right);
    }
}

impl<O: Clone + PartialEq, A: Clone> Hypergraph<O, A> {
    /// Construct a [`Hypergraph`] by identifying nodes in the quotient map.
    /// Mutably quotient this [`Hypergraph`], returning the coequalizer calculated from `self.quotient`.
    ///
    /// NOTE: this operation is unchecked; you should verify quotiented nodes have the exact same
    /// type first, or this operation is undefined.
    pub fn quotient(&mut self) -> FiniteFunction<VecKind> {
        use std::mem::take;
        let q = self.coequalizer();

        self.nodes = coequalizer_universal(&q, &VecArray(take(&mut self.nodes)))
            .unwrap()
            .0;

        // map hyperedges
        for e in &mut self.adjacency {
            e.sources.iter_mut().for_each(|x| *x = NodeId(q.table[x.0]));
            e.targets.iter_mut().for_each(|x| *x = NodeId(q.table[x.0]));
        }

        // clear the quotient map (we just used it)
        self.quotient = (vec![], vec![]); // empty

        q // return the coequalizer used to quotient the hypergraph
    }
}

impl<O: Clone, A: Clone> Hypergraph<O, A> {
    pub fn to_hypergraph(&self) -> crate::strict::Hypergraph<VecKind, O, A> {
        make_hypergraph(self)
    }

    pub fn coequalizer(&self) -> FiniteFunction<VecKind> {
        // Compute the coequalizer (connected components) of the quotient graph
        let s: FiniteFunction<VecKind> = FiniteFunction {
            table: VecArray(self.quotient.0.iter().map(|x| x.0).collect()),
            target: self.nodes.len(),
        };

        let t: FiniteFunction<VecKind> = FiniteFunction {
            table: VecArray(self.quotient.1.iter().map(|x| x.0).collect()),
            target: self.nodes.len(),
        };

        s.coequalizer(&t)
            .expect("coequalizer must exist for any graph")
    }
}

pub(crate) fn finite_function_coproduct(
    v1: &[NodeId],
    v2: &[NodeId],
    target: usize,
) -> Vec<NodeId> {
    v1.iter()
        .cloned()
        .chain(v2.iter().map(|&s| NodeId(s.0 + target)))
        .collect()
}

pub(crate) fn concat<T: Clone>(v1: &[T], v2: &[T]) -> Vec<T> {
    v1.iter().cloned().chain(v2.iter().cloned()).collect()
}

impl<O: Clone, A: Clone> Hypergraph<O, A> {
    pub(crate) fn coproduct(&self, other: &Hypergraph<O, A>) -> Hypergraph<O, A> {
        let n = self.nodes.len();

        let adjacency = self
            .adjacency
            .iter()
            .cloned()
            .chain(other.adjacency.iter().map(|edge| Hyperedge {
                sources: edge.sources.iter().map(|&s| NodeId(s.0 + n)).collect(),
                targets: edge.targets.iter().map(|&t| NodeId(t.0 + n)).collect(),
            }))
            .collect();

        let quotient = (
            finite_function_coproduct(&self.quotient.0, &other.quotient.0, n),
            finite_function_coproduct(&self.quotient.1, &other.quotient.1, n),
        );

        Hypergraph {
            nodes: concat(&self.nodes, &other.nodes),
            edges: concat(&self.edges, &other.edges),
            adjacency,
            quotient,
        }
    }
}

/// Create a [`crate::strict::hypergraph::Hypergraph`] by forgetting about the quotient map.
fn make_hypergraph<O: Clone, A: Clone>(
    h: &Hypergraph<O, A>,
) -> crate::strict::hypergraph::Hypergraph<VecKind, O, A> {
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
        let values =
            FiniteFunction::new(VecArray(values), h.nodes.len()).expect("invalid lax::Hypergraph!");
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
        let values =
            FiniteFunction::new(VecArray(values), h.nodes.len()).expect("invalid lax::Hypergraph!");
        IndexedCoproduct::from_semifinite(sources, values).expect("valid IndexedCoproduct")
    };

    let w = SemifiniteFunction(VecArray(h.nodes.clone()));
    let x = SemifiniteFunction(VecArray(h.edges.clone()));

    crate::strict::hypergraph::Hypergraph { s, t, w, x }
}
