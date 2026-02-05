use open_hypergraphs::lax::{EdgeId, Hyperedge, Hypergraph, NodeId};

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

#[test]
fn test_delete_nodes_empty_input_no_change() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![1, 2];
    h.edges = vec![7];
    h.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(1)],
    }];
    h.quotient = (vec![NodeId(0)], vec![NodeId(1)]);

    h.delete_nodes(&[]);

    assert_eq!(h.nodes, vec![1, 2]);
    assert_eq!(h.edges, vec![7]);
    assert_eq!(h.adjacency[0].sources, vec![NodeId(0)]);
    assert_eq!(h.adjacency[0].targets, vec![NodeId(1)]);
    assert_eq!(h.quotient.0, vec![NodeId(0)]);
    assert_eq!(h.quotient.1, vec![NodeId(1)]);
}

#[test]
fn test_delete_nodes_all_nodes_removed() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![1, 2, 3];
    h.edges = vec![0];
    h.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0), NodeId(2)],
        targets: vec![NodeId(1)],
    }];
    h.quotient = (vec![NodeId(0), NodeId(1)], vec![NodeId(2), NodeId(0)]);

    h.delete_nodes(&[NodeId(0), NodeId(1), NodeId(2)]);

    assert!(h.nodes.is_empty());
    assert_eq!(h.edges, vec![0]);
    assert!(h.adjacency[0].sources.is_empty());
    assert!(h.adjacency[0].targets.is_empty());
    assert!(h.quotient.0.is_empty());
    assert!(h.quotient.1.is_empty());
}

#[test]
#[should_panic]
fn test_delete_nodes_panics_on_out_of_range() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![5, 6];
    h.edges = vec![1];
    h.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(1)],
    }];
    h.quotient = (vec![NodeId(0)], vec![NodeId(1)]);

    h.delete_nodes(&[NodeId(99)]);
}

#[test]
fn test_delete_edge_single() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![10, 20, 30];
    h.edges = vec![1, 2, 3];
    h.adjacency = vec![
        Hyperedge {
            sources: vec![NodeId(0)],
            targets: vec![NodeId(1)],
        },
        Hyperedge {
            sources: vec![NodeId(2)],
            targets: vec![NodeId(0), NodeId(1)],
        },
        Hyperedge {
            sources: vec![],
            targets: vec![NodeId(2)],
        },
    ];
    h.quotient = (vec![NodeId(0)], vec![NodeId(2)]);

    h.delete_edge(&[EdgeId(1)]);

    assert_eq!(h.nodes, vec![10, 20, 30]);
    assert_eq!(h.edges, vec![1, 3]);
    assert_eq!(h.adjacency.len(), 2);
    assert_eq!(h.adjacency[0].sources, vec![NodeId(0)]);
    assert_eq!(h.adjacency[0].targets, vec![NodeId(1)]);
    assert_eq!(h.adjacency[1].sources, vec![]);
    assert_eq!(h.adjacency[1].targets, vec![NodeId(2)]);
    assert_eq!(h.quotient.0, vec![NodeId(0)]);
    assert_eq!(h.quotient.1, vec![NodeId(2)]);
}

#[test]
fn test_delete_edge_multiple_with_duplicates() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![1, 2];
    h.edges = vec![11, 22, 33, 44];
    h.adjacency = vec![
        Hyperedge {
            sources: vec![NodeId(0)],
            targets: vec![NodeId(1)],
        },
        Hyperedge {
            sources: vec![NodeId(1)],
            targets: vec![NodeId(0)],
        },
        Hyperedge {
            sources: vec![],
            targets: vec![NodeId(0)],
        },
        Hyperedge {
            sources: vec![NodeId(0), NodeId(1)],
            targets: vec![],
        },
    ];

    h.delete_edge(&[EdgeId(3), EdgeId(1), EdgeId(1)]);

    assert_eq!(h.edges, vec![11, 33]);
    assert_eq!(h.adjacency.len(), 2);
    assert_eq!(h.adjacency[0].sources, vec![NodeId(0)]);
    assert_eq!(h.adjacency[0].targets, vec![NodeId(1)]);
    assert_eq!(h.adjacency[1].sources, vec![]);
    assert_eq!(h.adjacency[1].targets, vec![NodeId(0)]);
}

#[test]
fn test_delete_edge_all_edges_removed() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![7, 8, 9];
    h.edges = vec![0, 1];
    h.adjacency = vec![
        Hyperedge {
            sources: vec![NodeId(0)],
            targets: vec![NodeId(2)],
        },
        Hyperedge {
            sources: vec![NodeId(1)],
            targets: vec![NodeId(0)],
        },
    ];

    h.delete_edge(&[EdgeId(0), EdgeId(1)]);

    assert!(h.edges.is_empty());
    assert!(h.adjacency.is_empty());
    assert_eq!(h.nodes, vec![7, 8, 9]);
}

#[test]
fn test_delete_edge_empty_input_no_change() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![1];
    h.edges = vec![99];
    h.adjacency = vec![Hyperedge {
        sources: vec![],
        targets: vec![NodeId(0)],
    }];

    h.delete_edge(&[]);

    assert_eq!(h.edges, vec![99]);
    assert_eq!(h.adjacency.len(), 1);
    assert_eq!(h.adjacency[0].targets, vec![NodeId(0)]);
}

#[test]
#[should_panic]
fn test_delete_edge_panics_on_out_of_bounds() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![1];
    h.edges = vec![5];
    h.adjacency = vec![Hyperedge {
        sources: vec![],
        targets: vec![NodeId(0)],
    }];

    h.delete_edge(&[EdgeId(1)]);
}

#[test]
fn test_is_acyclic_true() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![0, 1, 2];
    h.edges = vec![10, 11];
    h.adjacency = vec![
        Hyperedge {
            sources: vec![NodeId(0)],
            targets: vec![NodeId(1)],
        },
        Hyperedge {
            sources: vec![NodeId(1)],
            targets: vec![NodeId(2)],
        },
    ];

    assert!(h.is_acyclic());
}

#[test]
fn test_is_acyclic_false_cycle() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![0, 1];
    h.edges = vec![10, 11];
    h.adjacency = vec![
        Hyperedge {
            sources: vec![NodeId(0)],
            targets: vec![NodeId(1)],
        },
        Hyperedge {
            sources: vec![NodeId(1)],
            targets: vec![NodeId(0)],
        },
    ];

    assert!(!h.is_acyclic());
}

#[test]
fn test_is_acyclic_false_self_loop() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![0];
    h.edges = vec![10];
    h.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(0)],
    }];

    assert!(!h.is_acyclic());
}
