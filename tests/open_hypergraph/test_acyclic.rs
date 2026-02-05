use open_hypergraphs::category::SymmetricMonoidal;
use open_hypergraphs::lax::{Hyperedge, Hypergraph as LaxHypergraph, NodeId, OpenHypergraph};

use crate::theory::meaningless::{Arr, Obj};

fn build_lax_hypergraph(node_count: usize, edges: &[(Vec<usize>, Vec<usize>)]) -> LaxHypergraph<Obj, Arr> {
    let mut h = LaxHypergraph::empty();
    h.nodes = vec![0; node_count];
    for (sources, targets) in edges {
        let edge = Hyperedge {
            sources: sources.iter().map(|&i| NodeId(i)).collect(),
            targets: targets.iter().map(|&i| NodeId(i)).collect(),
        };
        h.new_edge(0, edge);
    }
    h
}

#[test]
fn test_identity_is_acyclic() {
    let lax = OpenHypergraph::<Obj, Arr>::identity(vec![0, 1]);
    assert!(lax.to_strict().is_acyclic());
}

#[test]
fn test_symmetry_is_acyclic() {
    let lax = OpenHypergraph::<Obj, Arr>::twist(vec![0], vec![1]);
    assert!(lax.to_strict().is_acyclic());
}

#[test]
fn test_composition_of_acyclic_can_be_cyclic() {
    // f has targets t1,t2 and a single edge t1 -> t2.
    let f_h = build_lax_hypergraph(2, &[(vec![0], vec![1])]);
    let f = OpenHypergraph {
        sources: vec![],
        targets: vec![NodeId(0), NodeId(1)],
        hypergraph: f_h,
    };

    // g has sources s1,s2 and a single edge s2 -> s1.
    let g_h = build_lax_hypergraph(2, &[(vec![1], vec![0])]);
    let g = OpenHypergraph {
        sources: vec![NodeId(0), NodeId(1)],
        targets: vec![],
        hypergraph: g_h,
    };

    assert!(f.to_strict().is_acyclic());
    assert!(g.to_strict().is_acyclic());

    // After composition, t1=s1 and t2=s2, so we have t1 -> t2 (from f)
    // and t2 -> t1 (from g), creating a 2-cycle.
    let composed = (&f >> &g).unwrap().to_strict();
    assert!(!composed.is_acyclic());
}
