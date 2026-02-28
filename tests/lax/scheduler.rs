use open_hypergraphs::category::Arrow;
use open_hypergraphs::lax::scheduler::{TopologicalScheduler, TopologicalSchedulerError};
use open_hypergraphs::lax::{EdgeId, OpenHypergraph};

#[derive(Clone, Debug, PartialEq, Eq)]
enum Obj {
    A,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Op {
    Copy,
    Mul,
    F,
    G,
}

#[test]
fn test_topological_order_sequential() {
    let copy = OpenHypergraph::singleton(Op::Copy, vec![Obj::A], vec![Obj::A, Obj::A]);
    let mul = OpenHypergraph::singleton(Op::Mul, vec![Obj::A, Obj::A], vec![Obj::A]);
    let composed = copy.compose(&mul).expect("copy ; mul should typecheck");

    let mut order = TopologicalScheduler::new(&composed).expect("acyclic");
    assert_eq!(order.available().len(), 1);
    assert!(order.available().contains(&EdgeId(0)));

    order.pop_subset(&[EdgeId(0)]).expect("edge 0 available");
    assert_eq!(order.available().len(), 1);
    assert!(order.available().contains(&EdgeId(1)));
    assert_eq!(order.remaining(), 1);

    order.pop_subset(&[EdgeId(1)]).expect("edge 1 available");
    assert!(order.is_complete());
    assert!(order.available().is_empty());
}

#[test]
fn test_topological_order_parallel_pop_subset() {
    let f = OpenHypergraph::singleton(Op::Copy, vec![Obj::A], vec![Obj::A, Obj::A]);
    let g = OpenHypergraph::singleton(Op::Mul, vec![Obj::A, Obj::A], vec![Obj::A]);
    let parallel = f.tensor(&g);

    let mut order = TopologicalScheduler::new(&parallel).expect("acyclic");
    assert_eq!(order.available().len(), 2);
    assert!(order.available().contains(&EdgeId(0)));
    assert!(order.available().contains(&EdgeId(1)));

    order.pop_subset(&[EdgeId(1)]).expect("edge 1 available");
    assert_eq!(order.available().len(), 1);
    assert!(order.available().contains(&EdgeId(0)));
    assert_eq!(order.remaining(), 1);

    order.pop_subset(&[EdgeId(0)]).expect("edge 0 available");
    assert!(order.is_complete());
}

#[test]
fn test_topological_order_pop_rejects_unavailable() {
    let copy = OpenHypergraph::singleton(Op::Copy, vec![Obj::A], vec![Obj::A, Obj::A]);
    let mul = OpenHypergraph::singleton(Op::Mul, vec![Obj::A, Obj::A], vec![Obj::A]);
    let composed = copy.compose(&mul).expect("copy ; mul should typecheck");

    let mut order = TopologicalScheduler::new(&composed).expect("acyclic");
    let err = order
        .pop_subset(&[EdgeId(1)])
        .expect_err("edge 1 is not initially available");
    assert_eq!(err, TopologicalSchedulerError::NotAvailable(EdgeId(1)));
}

#[test]
fn test_topological_order_unquotiented_respects_quotient_dependency() {
    let mut f = OpenHypergraph::empty();
    let (_, (_, f_targets)) = f.new_operation(Op::F, vec![Obj::A], vec![Obj::A]);
    let (_, (g_sources, _)) = f.new_operation(Op::G, vec![Obj::A], vec![Obj::A]);

    // Keep the graph unquotiented, but record that output(F) and input(G) are the same node.
    f.unify(f_targets[0], g_sources[0]);
    assert_eq!(f.hypergraph.quotient.0.len(), 1);

    let mut order = TopologicalScheduler::new(&f).expect("acyclic");
    assert_eq!(order.available().len(), 1);
    assert!(order.available().contains(&EdgeId(0)));

    order.pop_subset(&[EdgeId(0)]).expect("edge 0 available");
    assert_eq!(order.available().len(), 1);
    assert!(order.available().contains(&EdgeId(1)));
}
