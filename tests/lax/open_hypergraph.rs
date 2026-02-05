use open_hypergraphs::category::Arrow;
use open_hypergraphs::lax::{NodeId, OpenHypergraph};

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
fn test_in_out_nodes_are_images() {
    let mut h: OpenHypergraph<Obj, Op> = OpenHypergraph::empty();
    h.hypergraph.nodes = vec![Obj::A, Obj::B, Obj::C];
    h.sources = vec![NodeId(0), NodeId(2), NodeId(2)];
    h.targets = vec![NodeId(1), NodeId(1)];

    assert_eq!(h.in_nodes(), vec![NodeId(0), NodeId(2)]);
    assert_eq!(h.out_nodes(), vec![NodeId(1)]);
}

#[test]
fn test_is_monogamous_true() {
    let mut h: OpenHypergraph<Obj, Op> = OpenHypergraph::empty();
    h.hypergraph.nodes = vec![Obj::A, Obj::B];
    h.hypergraph.adjacency = vec![open_hypergraphs::lax::Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(1)],
    }];
    h.sources = vec![NodeId(0)];
    h.targets = vec![NodeId(1)];

    assert!(h.is_monogamous());
}

#[test]
fn test_is_monogamous_false_non_mono_interface() {
    let mut h: OpenHypergraph<Obj, Op> = OpenHypergraph::empty();
    h.hypergraph.nodes = vec![Obj::A, Obj::B];
    h.sources = vec![NodeId(0), NodeId(0)];
    h.targets = vec![NodeId(1)];

    assert!(!h.is_monogamous());
}

#[test]
fn test_is_monogamous_false_boundary_has_degree() {
    let mut h: OpenHypergraph<Obj, Op> = OpenHypergraph::empty();
    h.hypergraph.nodes = vec![Obj::A, Obj::B];
    h.hypergraph.adjacency = vec![open_hypergraphs::lax::Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(1)],
    }];
    h.sources = vec![NodeId(0), NodeId(1)];
    h.targets = vec![NodeId(1)];

    assert!(!h.is_monogamous());
}
