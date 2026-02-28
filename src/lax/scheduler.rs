//! # Scheduling (dynamic ordering) of acyclic lax open hypergraph operations
use crate::array::vec::{VecArray, VecKind};
use crate::array::NaturalArray;
use crate::finite_function::FiniteFunction;
use crate::indexed_coproduct::IndexedCoproduct;
use crate::lax::{EdgeId, OpenHypergraph};
use crate::strict::graph;
use std::collections::HashSet;

/// Errors when constructing or advancing a topological ordering.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TopologicalSchedulerError {
    /// The hypergraph has a directed cycle, so no topological order exists.
    Cycle,
    /// The requested edge id is outside the operation set.
    UnknownEdge(EdgeId),
    /// The requested edge is not currently available to pop.
    NotAvailable(EdgeId),
    /// The requested pop subset contains the same edge more than once.
    Duplicate(EdgeId),
}

/// Stateful topological scheduler over operation edges.
///
/// At each step, [`available`](Self::available) returns operations with zero current indegree.
/// Calling [`pop_subset`](Self::pop_subset) marks any subset of those operations as processed.
pub struct TopologicalScheduler {
    adjacency: IndexedCoproduct<VecKind, FiniteFunction<VecKind>>,
    indegree: Vec<usize>,
    available: HashSet<EdgeId>,
    popped: HashSet<EdgeId>,
    remaining: usize,
}

impl TopologicalScheduler {
    /// Build a scheduler for all operations in `f`.
    ///
    /// Returns [`TopologicalSchedulerError::Cycle`] if `f` has a directed cycle.
    pub fn new<O, A>(f: &OpenHypergraph<O, A>) -> Result<Self, TopologicalSchedulerError> {
        let adjacency = operation_adjacency(f);

        let (_, unvisited) = graph::kahn(&adjacency);
        if unvisited.0.contains(&1) {
            return Err(TopologicalSchedulerError::Cycle);
        }

        let indegree_ff = graph::indegree(&adjacency);
        let mut available = HashSet::new();
        for &edge_ix in indegree_ff.table.zero().iter() {
            available.insert(EdgeId(edge_ix));
        }

        let edge_count = indegree_ff.table.len();
        Ok(Self {
            adjacency,
            indegree: indegree_ff.table.0,
            available,
            popped: HashSet::new(),
            remaining: edge_count,
        })
    }

    /// Operations currently available to be popped.
    pub fn available(&self) -> &HashSet<EdgeId> {
        &self.available
    }

    /// Number of operations that have not yet been popped.
    pub fn remaining(&self) -> usize {
        self.remaining
    }

    /// Whether all operations have been popped.
    pub fn is_complete(&self) -> bool {
        self.remaining == 0
    }

    /// Pop any subset of currently available operations.
    ///
    /// This advances the scheduler by updating indegrees and frontier according to Kahn's
    /// algorithm.
    pub fn pop_subset(&mut self, edges: &[EdgeId]) -> Result<(), TopologicalSchedulerError> {
        if edges.is_empty() {
            return Ok(());
        }

        let mut seen = HashSet::new();
        let edge_count = self.indegree.len();
        for &edge in edges {
            if edge.0 >= edge_count {
                return Err(TopologicalSchedulerError::UnknownEdge(edge));
            }
            if !seen.insert(edge.0) {
                return Err(TopologicalSchedulerError::Duplicate(edge));
            }
            if !self.available.contains(&edge) {
                return Err(TopologicalSchedulerError::NotAvailable(edge));
            }
        }

        for &edge in edges {
            self.available.remove(&edge);
            self.popped.insert(edge);
            self.remaining -= 1;
        }

        let frontier = FiniteFunction::new(
            VecArray(edges.iter().map(|e| e.0).collect()),
            self.adjacency.len(),
        )
        .expect("subset validated to be in range");

        let (reachable_ix, reachable_count) =
            graph::sparse_relative_indegree(&self.adjacency, &frontier);
        for (&edge_ix, &count) in reachable_ix.table.iter().zip(reachable_count.table.iter()) {
            self.indegree[edge_ix] -= count;
            let edge = EdgeId(edge_ix);
            if self.indegree[edge_ix] == 0 && !self.popped.contains(&edge) {
                self.available.insert(edge);
            }
        }

        Ok(())
    }
}

fn operation_adjacency<O, A>(
    f: &OpenHypergraph<O, A>,
) -> IndexedCoproduct<VecKind, FiniteFunction<VecKind>> {
    let edge_count = f.hypergraph.edges.len();
    let quotient = f.hypergraph.coequalizer();
    let class_count = quotient.target;

    // For each quotiented node-class, all source incidences (consumers) at that class.
    let mut consumers: Vec<Vec<usize>> = vec![Vec::new(); class_count];
    for (edge_ix, edge) in f.hypergraph.adjacency.iter().enumerate() {
        for source in &edge.sources {
            let class_ix = quotient.table[source.0];
            consumers[class_ix].push(edge_ix);
        }
    }

    let mut lengths = Vec::with_capacity(edge_count);
    let mut values = Vec::<usize>::new();
    for edge in &f.hypergraph.adjacency {
        let start = values.len();
        for target in &edge.targets {
            let class_ix = quotient.table[target.0];
            values.extend_from_slice(&consumers[class_ix]);
        }
        lengths.push(values.len() - start);
    }

    IndexedCoproduct::new(
        FiniteFunction::new(VecArray(lengths), values.len() + 1).expect("valid by construction"),
        FiniteFunction::new(VecArray(values), edge_count).expect("valid by construction"),
    )
    .expect("valid by construction")
}
