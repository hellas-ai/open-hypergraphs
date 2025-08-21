//! Isomorphisms of strict open hypergraphs
use crate::strict::*;

/// Morphisms of open hypergraphs
pub struct OpenHypergraphArrow<K: ArrayKind, O, A> {
    pub source: OpenHypergraph<K, O, A>,
    pub target: OpenHypergraph<K, O, A>,

    /// Natural transformation on nodes (i.e., a map preserving labels, sources, and targets)
    pub w: FiniteFunction<K>,

    /// Natural transformation on edges (i.e., a map preserving labels)
    pub x: FiniteFunction<K>,
}

impl<K: ArrayKind, O, A> OpenHypergraph<K, O, A> {
    /// Try to find an isomorphism between OpenHypergraphs,
    /// returning `None` if the graphs are not isomorphic.
    pub fn isomorphism(
        &self,
        _target: &OpenHypergraph<K, O, A>,
    ) -> Option<OpenHypergraphArrow<K, O, A>> {
        todo!()
    }
}
