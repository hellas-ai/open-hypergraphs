use open_hypergraphs::lax::{cycle_breaking_nodes, NodeId, OpenHypergraph};
use proptest::proptest;

use crate::theory::meaningless::{arb_open_hypergraph, Arr, Obj};

#[test]
fn acyclic_graph_needs_no_cycle_breakers() {
    let f = OpenHypergraph::singleton("edge", vec![()], vec![()]);

    assert!(cycle_breaking_nodes(&f).unwrap().is_empty());
}

#[test]
fn self_loop_selects_its_node() {
    let mut f = OpenHypergraph::empty();
    let node = f.new_node(());
    f.new_edge("loop", ([node], [node]));

    let selected = cycle_breaking_nodes(&f).unwrap();

    assert_eq!(selected, vec![node]);
    assert!(f
        .spiderize_nodes(&selected)
        .unwrap()
        .to_strict()
        .is_acyclic());
}

#[test]
fn one_node_breaks_a_two_node_cycle() {
    let mut f = OpenHypergraph::empty();
    let x = f.new_node(());
    let y = f.new_node(());
    f.new_edge("forward", ([x], [y]));
    f.new_edge("backward", ([y], [x]));

    let selected = cycle_breaking_nodes(&f).unwrap();

    assert_eq!(selected.len(), 1);
    assert!(f
        .spiderize_nodes(&selected)
        .unwrap()
        .to_strict()
        .is_acyclic());
}

#[test]
fn shared_high_degree_node_breaks_multiple_cycles() {
    let mut f = OpenHypergraph::empty();
    let hub = f.new_node(());
    let x = f.new_node(());
    let y = f.new_node(());
    f.new_edge("hub-x", ([hub], [x]));
    f.new_edge("x-hub", ([x], [hub]));
    f.new_edge("hub-y", ([hub], [y]));
    f.new_edge("y-hub", ([y], [hub]));

    assert_eq!(cycle_breaking_nodes(&f).unwrap(), vec![hub]);
}

#[test]
fn pending_quotient_is_rejected_then_its_cycle_is_broken() {
    let mut f = OpenHypergraph::empty();
    let x = f.new_node(());
    let y = f.new_node(());
    f.new_edge("edge", ([x], [y]));
    f.unify(x, y);

    assert!(cycle_breaking_nodes(&f).is_none());

    f.quotient().unwrap();
    let selected = cycle_breaking_nodes(&f).unwrap();

    assert_eq!(selected, vec![NodeId(0)]);
    assert!(f
        .spiderize_nodes(&selected)
        .unwrap()
        .to_strict()
        .is_acyclic());
}

proptest! {
    #[test]
    fn selected_nodes_always_break_cycles(
        strict_input in arb_open_hypergraph()
    ) {
        let input: OpenHypergraph<Obj, Arr> =
            OpenHypergraph::from_strict(strict_input);
        let selected = cycle_breaking_nodes(&input).unwrap();
        let spiderized = input.spiderize_nodes(&selected).unwrap();

        assert!(spiderized.to_strict().is_acyclic());
    }
}
