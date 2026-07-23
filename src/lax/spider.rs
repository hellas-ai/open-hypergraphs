//! Make the Frobenius structure of an open hypergraph explicit.

use super::{Hyperedge, NodeId, OpenHypergraph};
use crate::strict::vec::FiniteFunction;

/// An operation from `A`, or an explicitly represented spider.
///
/// A spider's arity is determined by its corresponding [`Hyperedge`].
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum WithSpider<A> {
    Operation(A),
    Spider,
}

impl<O: Clone + PartialEq, A: Clone> OpenHypergraph<O, A> {
    /// Replace implicit wiring with explicit spider operations.
    ///
    /// The result is strict (has no pending quotient), monogamous, and
    /// acyclic. Each original operation is retained as
    /// [`WithSpider::Operation`].
    ///
    /// For every original node, the construction inserts two spiders:
    ///
    /// - `p -> 1 + m` on the left, where `p` is the number of global source
    ///   occurrences and `m` is the number of operation-source occurrences;
    /// - `1 + n -> q` on the right, where `n` is the number of
    ///   operation-target occurrences and `q` is the number of global target
    ///   occurrences.
    ///
    /// The extra leg connects the two spiders. Every other occurrence gets a
    /// distinct node, making every node occur exactly once as a source and
    /// exactly once as a target (counting the global interfaces).
    ///
    /// Pending identifications in the lax quotient are applied first. An error
    /// means that the quotient attempted to identify nodes with different
    /// labels; the returned finite function is the quotient witness, as for
    /// [`OpenHypergraph::quotient`].
    pub fn spiderize(self) -> Result<OpenHypergraph<O, WithSpider<A>>, FiniteFunction> {
        let nodes: Vec<NodeId> = (0..self.hypergraph.nodes.len()).map(NodeId).collect();
        self.spiderize_nodes(&nodes)
    }

    /// Replace the chosen nodes' implicit wiring with explicit spider
    /// operations.
    ///
    /// Each selected node is replaced by the same pair of spiders used by
    /// [`Self::spiderize`]. Unselected nodes retain their implicit wiring.
    /// Consequently, unlike [`Self::spiderize`], this operation does not by
    /// itself guarantee that the result is acyclic or monogamous.
    ///
    /// `nodes` contains IDs from the input hypergraph. Pending identifications
    /// are applied first, and IDs in `nodes` are mapped through the resulting
    /// quotient. Selecting any representative therefore selects its complete
    /// equivalence class. Duplicate selections are ignored.
    ///
    /// # Panics
    ///
    /// Panics if a selected node ID is out of bounds.
    pub fn spiderize_nodes(
        mut self,
        nodes: &[NodeId],
    ) -> Result<OpenHypergraph<O, WithSpider<A>>, FiniteFunction> {
        let input_node_count = self.hypergraph.nodes.len();
        for node in nodes {
            assert!(
                node.0 < input_node_count,
                "node id {:?} is out of bounds",
                node
            );
        }

        // Quotient the input if necessary and remap the selected node IDs.
        let remapped_nodes;
        let nodes = if self.hypergraph.is_strict() {
            nodes
        } else {
            let quotient = self.quotient()?;
            remapped_nodes = nodes
                .iter()
                .map(|node| NodeId(quotient.table[node.0]))
                .collect::<Vec<_>>();
            remapped_nodes.as_slice()
        };

        assert_eq!(
            self.hypergraph.edges.len(),
            self.hypergraph.adjacency.len(),
            "malformed hypergraph: edges and adjacency lengths differ"
        );

        // Split selected-node occurrences and record the spiders that reconnect them.
        let spiders = rewrite_occurrences(
            nodes,
            &mut self.hypergraph.nodes,
            &mut self.hypergraph.adjacency,
            &mut self.sources,
            &mut self.targets,
        );

        // Reuse the input graph, wrapping its existing operation labels.
        let mut result = self.map_edges(WithSpider::Operation);

        // Append the two explicit operations for each selected node.
        for (left_spider, right_spider) in spiders {
            result.new_edge(WithSpider::Spider, left_spider);
            result.new_edge(WithSpider::Spider, right_spider);
        }

        Ok(result)
    }
}

/// Append a fresh occurrence of `node`, carrying the same label.
fn new_occurrence<O: Clone>(nodes: &mut Vec<O>, node: NodeId) -> NodeId {
    let occurrence = NodeId(nodes.len());
    nodes.push(nodes[node.0].clone());
    occurrence
}

/// Replace selected node occurrences and build their spider interfaces.
///
/// For every selected node, the returned vector contains a pair consisting of:
///
/// - a left spider whose sources are global-source occurrences and whose
///   targets are the original node followed by operation-source occurrences;
/// - a right spider whose sources are the original node followed by
///   operation-target occurrences and whose targets are global-target
///   occurrences.
///
/// Each selected occurrence is replaced in-place with a fresh node carrying
/// the original label. Unselected occurrences remain unchanged.
fn rewrite_occurrences<O: Clone>(
    selected: &[NodeId],
    nodes: &mut Vec<O>,
    adjacency: &mut [Hyperedge],
    sources: &mut [NodeId],
    targets: &mut [NodeId],
) -> Vec<(Hyperedge, Hyperedge)> {
    let node_count = nodes.len();

    // Index spiders by original node while rewriting, but only create selected pairs.
    let mut spiders: Vec<Option<(Hyperedge, Hyperedge)>> = (0..node_count).map(|_| None).collect();
    for &node in selected {
        spiders[node.0].get_or_insert_with(|| {
            (
                Hyperedge {
                    sources: vec![],
                    targets: vec![node],
                },
                Hyperedge {
                    sources: vec![node],
                    targets: vec![],
                },
            )
        });
    }

    // Operation sources leave the left spider; operation targets enter the
    // right spider.
    for adjacency in adjacency {
        for node in &mut adjacency.sources {
            let original = *node;
            if let Some((left_spider, _)) = spiders[original.0].as_mut() {
                let occurrence = new_occurrence(nodes, original);
                left_spider.targets.push(occurrence);
                *node = occurrence;
            }
        }

        for node in &mut adjacency.targets {
            let original = *node;
            if let Some((_, right_spider)) = spiders[original.0].as_mut() {
                let occurrence = new_occurrence(nodes, original);
                right_spider.sources.push(occurrence);
                *node = occurrence;
            }
        }
    }

    // Global sources enter the left spider; global targets leave the right.
    for node in sources {
        let original = *node;
        if let Some((left_spider, _)) = spiders[original.0].as_mut() {
            let occurrence = new_occurrence(nodes, original);
            left_spider.sources.push(occurrence);
            *node = occurrence;
        }
    }

    for node in targets {
        let original = *node;
        if let Some((_, right_spider)) = spiders[original.0].as_mut() {
            let occurrence = new_occurrence(nodes, original);
            right_spider.targets.push(occurrence);
            *node = occurrence;
        }
    }

    spiders.into_iter().flatten().collect()
}
