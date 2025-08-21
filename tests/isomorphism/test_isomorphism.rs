// use an example theory (with no meaning) to test
use crate::theory::meaningless::*;
use open_hypergraphs::strict::vec::*;
use proptest::prelude::*;

// TODO: *randomly* permute nodes - use proptest to generate a random permutation.
fn permute_nodes(f: &OpenHypergraph<Obj, Arr>) -> OpenHypergraph<Obj, Arr> {
    todo!()
}

// TODO: *randomly* permute edges - use proptest to generate a random permutation.
fn permute_edges(f: &OpenHypergraph<Obj, Arr>) -> OpenHypergraph<Obj, Arr> {
    todo!()
}

proptest! {
    /// Every open hypergraph should be isomorphic to itself
    #[test]
    fn test_refl_isomorphism(oh in arb_open_hypergraph()) {
        oh.isomorphism(&oh).unwrap();
    }

    /// Permuting the nodes of an open hypergraph should give an isomorphic open hypergraph
    #[test]
    fn test_permute_nodes_isomorphic(oh in arb_open_hypergraph()) {
        // Generate a permutation of the nodes and apply it
        let permuted = permute_nodes(&oh);
        oh.isomorphism(&permuted).unwrap();
    }

    /// Permuting the edges of an open hypergraph should give an isomorphic open hypergraph
    #[test]
    fn test_permute_edges_isomorphic(oh in arb_open_hypergraph()) {
        // Generate a permutation of the edges and apply it
        let permuted = permute_edges(&oh);
        oh.isomorphism(&permuted).unwrap();
    }

    /// Permuting both nodes and edges of an open hypergraph should give an isomorphic open hypergraph
    #[test]
    fn test_permute_nodes_and_edges_isomorphic(oh in arb_open_hypergraph()) {
        // Generate permutations of both nodes and edges and apply them
        let permuted = permute_edges(&oh);
        let permuted = permute_nodes(&oh);
        oh.isomorphism(&permuted).unwrap();
    }
}
