//! Make the Frobenius structure of an open hypergraph explicit.

use super::{Hyperedge, NodeId, OpenHypergraph};
use crate::strict::vec::FiniteFunction;

/// An operation from `A`, or an explicitly represented spider.
///
/// A spider records its number of sources and targets. Its incident nodes still
/// carry the object labels, just like those of an ordinary operation.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum WithSpider<A> {
    Operation(A),
    Spider { sources: usize, targets: usize },
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
    pub fn spiderize(mut self) -> Result<OpenHypergraph<O, WithSpider<A>>, FiniteFunction> {
        self.quotient()?;

        let OpenHypergraph {
            sources,
            targets,
            hypergraph,
        } = self;

        assert_eq!(
            hypergraph.edges.len(),
            hypergraph.adjacency.len(),
            "malformed hypergraph: edges and adjacency lengths differ"
        );

        let mut result = OpenHypergraph::empty();

        // Keeping these first and in the original order makes forgetting the
        // spiders recover the original node ordering after quotienting.
        let central: Vec<NodeId> = hypergraph
            .nodes
            .iter()
            .cloned()
            .map(|label| result.new_node(label))
            .collect();

        let mut left_sources = vec![Vec::new(); central.len()];
        let mut left_targets: Vec<Vec<NodeId>> =
            central.iter().copied().map(|node| vec![node]).collect();
        let mut right_sources: Vec<Vec<NodeId>> =
            central.iter().copied().map(|node| vec![node]).collect();
        let mut right_targets = vec![Vec::new(); central.len()];

        for (operation, adjacency) in hypergraph.edges.into_iter().zip(hypergraph.adjacency) {
            let operation_sources = adjacency
                .sources
                .into_iter()
                .map(|node| {
                    let occurrence = result.new_node(hypergraph.nodes[node.0].clone());
                    left_targets[node.0].push(occurrence);
                    occurrence
                })
                .collect();

            let operation_targets = adjacency
                .targets
                .into_iter()
                .map(|node| {
                    let occurrence = result.new_node(hypergraph.nodes[node.0].clone());
                    right_sources[node.0].push(occurrence);
                    occurrence
                })
                .collect();

            result.new_edge(
                WithSpider::Operation(operation),
                Hyperedge {
                    sources: operation_sources,
                    targets: operation_targets,
                },
            );
        }

        result.sources = sources
            .into_iter()
            .map(|node| {
                let occurrence = result.new_node(hypergraph.nodes[node.0].clone());
                left_sources[node.0].push(occurrence);
                occurrence
            })
            .collect();

        result.targets = targets
            .into_iter()
            .map(|node| {
                let occurrence = result.new_node(hypergraph.nodes[node.0].clone());
                right_targets[node.0].push(occurrence);
                occurrence
            })
            .collect();

        for (((left_sources, left_targets), right_sources), right_targets) in left_sources
            .into_iter()
            .zip(left_targets)
            .zip(right_sources)
            .zip(right_targets)
        {
            result.new_edge(
                WithSpider::Spider {
                    sources: left_sources.len(),
                    targets: left_targets.len(),
                },
                Hyperedge {
                    sources: left_sources,
                    targets: left_targets,
                },
            );

            result.new_edge(
                WithSpider::Spider {
                    sources: right_sources.len(),
                    targets: right_targets.len(),
                },
                Hyperedge {
                    sources: right_sources,
                    targets: right_targets,
                },
            );
        }

        Ok(result)
    }
}
