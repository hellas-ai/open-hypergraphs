use open_hypergraphs::lax::{Hyperedge, Hypergraph as LaxHypergraph, NodeId};
use open_hypergraphs::strict::graph;

#[test]
fn test_node_adjacency_simple_example() {
    // Nodes: 0, 1, 2
    // Edge e0: sources [0, 1] -> targets [2]
    let mut lax = LaxHypergraph::empty();
    lax.nodes = vec![0, 1, 2];
    lax.new_edge(
        0,
        Hyperedge {
            sources: vec![NodeId(0), NodeId(1)],
            targets: vec![NodeId(2)],
        },
    );

    let strict = lax.to_hypergraph();
    let adjacency = graph::node_adjacency(&strict);

    let got: Vec<Vec<usize>> = adjacency.into_iter().map(|ff| ff.table.0).collect();

    let expected = vec![vec![2], vec![2], vec![]];
    assert_eq!(got, expected);
}

#[test]
fn test_operation_adjacency_simple_example() {
    // Nodes: 0, 1, 2
    // Edge e0: sources [0] -> targets [1]
    // Edge e1: sources [1] -> targets [2]
    let mut lax = LaxHypergraph::empty();
    lax.nodes = vec![0, 1, 2];
    lax.new_edge(
        0,
        Hyperedge {
            sources: vec![NodeId(0)],
            targets: vec![NodeId(1)],
        },
    );
    lax.new_edge(
        0,
        Hyperedge {
            sources: vec![NodeId(1)],
            targets: vec![NodeId(2)],
        },
    );

    let strict = lax.to_hypergraph();
    let adjacency = graph::operation_adjacency(&strict);

    let got: Vec<Vec<usize>> = adjacency.into_iter().map(|ff| ff.table.0).collect();
    let expected = vec![vec![1], vec![]];
    assert_eq!(got, expected);
}

#[test]
fn test_kahn_acyclic_chain() {
    // 0 -> 1 -> 2
    let mut lax = LaxHypergraph::empty();
    lax.nodes = vec![0, 1, 2];
    lax.new_edge(
        0,
        Hyperedge {
            sources: vec![NodeId(0)],
            targets: vec![NodeId(1)],
        },
    );
    lax.new_edge(
        0,
        Hyperedge {
            sources: vec![NodeId(1)],
            targets: vec![NodeId(2)],
        },
    );

    let strict = lax.to_hypergraph();
    let adjacency = graph::node_adjacency(&strict);
    let (_order, unvisited) = graph::kahn(&adjacency);

    assert_eq!(unvisited.0, vec![0, 0, 0]);
}

#[test]
fn test_kahn_cyclic() {
    // 0 -> 1 -> 0
    let mut lax = LaxHypergraph::empty();
    lax.nodes = vec![0, 1];
    lax.new_edge(
        0,
        Hyperedge {
            sources: vec![NodeId(0)],
            targets: vec![NodeId(1)],
        },
    );
    lax.new_edge(
        0,
        Hyperedge {
            sources: vec![NodeId(1)],
            targets: vec![NodeId(0)],
        },
    );

    let strict = lax.to_hypergraph();
    let adjacency = graph::node_adjacency(&strict);
    let (_order, unvisited) = graph::kahn(&adjacency);

    assert_eq!(unvisited.0, vec![1, 1]);
}

#[test]
fn test_kahn_disconnected_acyclic() {
    // 0 -> 1   and   2 isolated
    let mut lax = LaxHypergraph::empty();
    lax.nodes = vec![0, 1, 2];
    lax.new_edge(
        0,
        Hyperedge {
            sources: vec![NodeId(0)],
            targets: vec![NodeId(1)],
        },
    );

    let strict = lax.to_hypergraph();
    let adjacency = graph::node_adjacency(&strict);
    let (_order, unvisited) = graph::kahn(&adjacency);

    assert_eq!(unvisited.0, vec![0, 0, 0]);
}

#[test]
fn test_kahn_disconnected_with_cycle() {
    // 0 -> 1 -> 0   and   2 isolated
    let mut lax = LaxHypergraph::empty();
    lax.nodes = vec![0, 1, 2];
    lax.new_edge(
        0,
        Hyperedge {
            sources: vec![NodeId(0)],
            targets: vec![NodeId(1)],
        },
    );
    lax.new_edge(
        0,
        Hyperedge {
            sources: vec![NodeId(1)],
            targets: vec![NodeId(0)],
        },
    );

    let strict = lax.to_hypergraph();
    let adjacency = graph::node_adjacency(&strict);
    let (_order, unvisited) = graph::kahn(&adjacency);

    assert_eq!(unvisited.0, vec![1, 1, 0]);
}
