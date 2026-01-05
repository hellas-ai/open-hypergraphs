use open_hypergraphs::lax::{Hyperedge, Hypergraph, NodeId};

#[test]
fn test_delete_nodes_remap_and_quotient() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![10, 20, 30, 40];
    h.edges = vec![0];
    h.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0), NodeId(1), NodeId(2), NodeId(3)],
        targets: vec![NodeId(3), NodeId(1)],
    }];
    h.quotient = (vec![NodeId(0), NodeId(1)], vec![NodeId(2), NodeId(3)]);

    h.delete_nodes(&[NodeId(1), NodeId(3)]);

    assert_eq!(h.nodes, vec![10, 30]);
    assert_eq!(h.adjacency.len(), 1);
    assert_eq!(h.adjacency[0].sources, vec![NodeId(0), NodeId(1)]);
    assert!(h.adjacency[0].targets.is_empty());
    assert_eq!(h.quotient.0, vec![NodeId(0)]);
    assert_eq!(h.quotient.1, vec![NodeId(1)]);
}
