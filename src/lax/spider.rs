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
    /// - `0 -> 1 + m`, where `m` is the number of occurrences as an operation
    ///   source or global target;
    /// - `1 + n -> 0`, where `n` is the number of occurrences as an operation
    ///   target or global source.
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

        let mut splitter_targets: Vec<Vec<NodeId>> =
            central.iter().copied().map(|node| vec![node]).collect();
        let mut merger_sources: Vec<Vec<NodeId>> =
            central.iter().copied().map(|node| vec![node]).collect();

        for (operation, adjacency) in hypergraph.edges.into_iter().zip(hypergraph.adjacency) {
            let operation_sources = adjacency
                .sources
                .into_iter()
                .map(|node| {
                    let occurrence = result.new_node(hypergraph.nodes[node.0].clone());
                    splitter_targets[node.0].push(occurrence);
                    occurrence
                })
                .collect();

            let operation_targets = adjacency
                .targets
                .into_iter()
                .map(|node| {
                    let occurrence = result.new_node(hypergraph.nodes[node.0].clone());
                    merger_sources[node.0].push(occurrence);
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
                merger_sources[node.0].push(occurrence);
                occurrence
            })
            .collect();

        result.targets = targets
            .into_iter()
            .map(|node| {
                let occurrence = result.new_node(hypergraph.nodes[node.0].clone());
                splitter_targets[node.0].push(occurrence);
                occurrence
            })
            .collect();

        for (splitter, merger) in splitter_targets.into_iter().zip(merger_sources) {
            let splitter_outputs = splitter.len();
            result.new_edge(
                WithSpider::Spider {
                    sources: 0,
                    targets: splitter_outputs,
                },
                Hyperedge {
                    sources: vec![],
                    targets: splitter,
                },
            );

            let merger_inputs = merger.len();
            result.new_edge(
                WithSpider::Spider {
                    sources: merger_inputs,
                    targets: 0,
                },
                Hyperedge {
                    sources: merger,
                    targets: vec![],
                },
            );
        }

        Ok(result)
    }
}
