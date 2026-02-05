use open_hypergraphs::category::{Arrow, SymmetricMonoidal};
use open_hypergraphs::lax::{Hyperedge, Hypergraph, NodeId, OpenHypergraph};

#[derive(Clone, Debug, PartialEq)]
enum Obj {
    A,
    B,
    C,
}

#[derive(Clone, Debug, PartialEq)]
enum Op {
    F,
    G,
}

#[test]
fn test_compose_type_mismatch() {
    let f = OpenHypergraph::singleton(Op::F, vec![Obj::A], vec![Obj::B]);
    let g = OpenHypergraph::singleton(Op::G, vec![Obj::C], vec![Obj::A]);

    assert!(Arrow::compose(&f, &g).is_none());
}

#[test]
fn test_lax_compose_allows_mismatch_and_unifies_boundary() {
    let f = OpenHypergraph::singleton(Op::F, vec![Obj::A], vec![Obj::B]);
    let g = OpenHypergraph::singleton(Op::G, vec![Obj::C], vec![Obj::A]);

    let composed = f.lax_compose(&g).expect("arity matches");

    assert_eq!(composed.sources, vec![NodeId(0)]);
    assert_eq!(composed.targets, vec![NodeId(3)]);
    assert_eq!(composed.hypergraph.quotient.0, vec![NodeId(1)]);
    assert_eq!(composed.hypergraph.quotient.1, vec![NodeId(2)]);
}

#[test]
fn test_compose_matches_lax_compose_on_equal_boundaries() {
    let f = OpenHypergraph::singleton(Op::F, vec![Obj::A], vec![Obj::B]);
    let g = OpenHypergraph::singleton(Op::G, vec![Obj::B], vec![Obj::C]);

    let strict = Arrow::compose(&f, &g).expect("labels match");
    let lax = f.lax_compose(&g).expect("arity matches");

    assert_eq!(strict, lax);
}

#[test]
fn test_identity_is_acyclic() {
    let f = OpenHypergraph::<Obj, Op>::identity(vec![Obj::A, Obj::B]);
    assert!(f.is_acyclic());
}

#[test]
fn test_symmetry_is_acyclic() {
    let f = OpenHypergraph::<Obj, Op>::twist(vec![Obj::A], vec![Obj::B]);
    assert!(f.is_acyclic());
}

#[test]
fn test_composition_of_acyclic_can_be_cyclic() {
    // f has targets t1,t2 and a single edge t1 -> t2.
    let mut f_h = Hypergraph::empty();
    f_h.nodes = vec![Obj::A, Obj::A];
    f_h.edges = vec![Op::F];
    f_h.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(1)],
    }];
    let f = OpenHypergraph {
        sources: vec![],
        targets: vec![NodeId(0), NodeId(1)],
        hypergraph: f_h,
    };

    // g has sources s1,s2 and a single edge s2 -> s1.
    let mut g_h = Hypergraph::empty();
    g_h.nodes = vec![Obj::A, Obj::A];
    g_h.edges = vec![Op::G];
    g_h.adjacency = vec![Hyperedge {
        sources: vec![NodeId(1)],
        targets: vec![NodeId(0)],
    }];
    let g = OpenHypergraph {
        sources: vec![NodeId(0), NodeId(1)],
        targets: vec![],
        hypergraph: g_h,
    };

    assert!(f.is_acyclic());
    assert!(g.is_acyclic());

    let mut composed = f.lax_compose(&g).expect("arity matches");
    composed.quotient();

    // After quotienting, t1=s1 and t2=s2, so we have t1 -> t2 (from f)
    // and t2 -> t1 (from g), creating a 2-cycle.
    assert!(!composed.is_acyclic());
}
