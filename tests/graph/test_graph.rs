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
