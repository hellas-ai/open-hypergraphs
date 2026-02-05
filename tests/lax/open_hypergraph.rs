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
