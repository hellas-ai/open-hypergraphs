use open_hypergraphs::array::vec::{VecArray, VecKind};
use open_hypergraphs::category::SymmetricMonoidal;
use open_hypergraphs::strict::hypergraph::Hypergraph;
use open_hypergraphs::strict::open_hypergraph::OpenHypergraph;
use open_hypergraphs::strict::vec::{FiniteFunction, IndexedCoproduct, SemifiniteFunction};

use crate::theory::meaningless::{Arr, Obj};

fn build_hypergraph(
    node_count: usize,
    edges: &[(Vec<usize>, Vec<usize>)],
) -> Hypergraph<VecKind, Obj, Arr> {
    let w = SemifiniteFunction(VecArray(vec![0; node_count]));
    let x = SemifiniteFunction(VecArray(vec![0; edges.len()]));

    let mut source_lengths = Vec::with_capacity(edges.len());
    let mut source_values = Vec::new();
    let mut target_lengths = Vec::with_capacity(edges.len());
    let mut target_values = Vec::new();

    for (sources, targets) in edges {
        source_lengths.push(sources.len());
        source_values.extend_from_slice(sources);
        target_lengths.push(targets.len());
        target_values.extend_from_slice(targets);
    }

    let s = IndexedCoproduct::from_semifinite(
        SemifiniteFunction(VecArray(source_lengths)),
        FiniteFunction::new(VecArray(source_values), node_count).unwrap(),
    )
    .unwrap();

    let t = IndexedCoproduct::from_semifinite(
        SemifiniteFunction(VecArray(target_lengths)),
        FiniteFunction::new(VecArray(target_values), node_count).unwrap(),
    )
    .unwrap();

    Hypergraph::new(s, t, w, x).unwrap()
}

#[test]
fn test_identity_is_acyclic() {
    let w = SemifiniteFunction(VecArray(vec![0, 1]));
    let f: OpenHypergraph<VecKind, Obj, Arr> = OpenHypergraph::identity(w);
    assert!(f.is_acyclic());
}

#[test]
fn test_symmetry_is_acyclic() {
    let a = SemifiniteFunction(VecArray(vec![0]));
    let b = SemifiniteFunction(VecArray(vec![1]));
    let f: OpenHypergraph<VecKind, Obj, Arr> = OpenHypergraph::twist(a, b);
    assert!(f.is_acyclic());
}

#[test]
fn test_composition_of_acyclic_can_be_cyclic() {
    // f has targets t1,t2 and a single edge t1 -> t2.
    let f_h = build_hypergraph(2, &[(vec![0], vec![1])]);
    let f = OpenHypergraph::new(
        FiniteFunction::initial(2),
        FiniteFunction::new(VecArray(vec![0, 1]), 2).unwrap(),
        f_h,
    )
    .unwrap();

    // g has sources s1,s2 and a single edge s2 -> s1.
    let g_h = build_hypergraph(2, &[(vec![1], vec![0])]);
    let g = OpenHypergraph::new(
        FiniteFunction::new(VecArray(vec![0, 1]), 2).unwrap(),
        FiniteFunction::initial(2),
        g_h,
    )
    .unwrap();

    assert!(f.is_acyclic());
    assert!(g.is_acyclic());

    // After composition, t1=s1 and t2=s2, so we have t1 -> t2 (from f)
    // and t2 -> t1 (from g), creating a 2-cycle.
    let composed = (&f >> &g).unwrap();
    assert!(!composed.is_acyclic());
}
