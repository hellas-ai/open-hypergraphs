use open_hypergraphs::array::vec::{VecArray, VecKind};
use open_hypergraphs::finite_function::FiniteFunction;
use open_hypergraphs::lax::{Coproduct as _, Hyperedge, Hypergraph, NodeEdgeMap, NodeId, EdgeId};

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
fn test_remainder_with_injection_splits_excluded_node_occurrences() {
    let mut host = Hypergraph::empty();
    let w = host.new_node(1);
    host.new_edge(
        10,
        Hyperedge {
            sources: vec![w],
            targets: vec![w],
        },
    );
    host.new_edge(
        20,
        Hyperedge {
            sources: vec![w],
            targets: vec![w],
        },
    );

    let excluded = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::new(VecArray(vec![w.0]), host.nodes.len()).unwrap(),
        edges: FiniteFunction::<VecKind>::initial(host.edges.len()),
    };

    let (remainder, remainder_in_host) = host.remainder_with_injection(&excluded);

    assert_eq!(remainder.nodes.len(), 4);
    assert_eq!(remainder.edges.len(), 2);
    assert_eq!(remainder.adjacency.len(), 2);
    assert_eq!(
        remainder_in_host.nodes.table,
        VecArray(vec![w.0, w.0, w.0, w.0])
    );
    assert_eq!(remainder.adjacency[0].sources.len(), 1);
    assert_eq!(remainder.adjacency[0].targets.len(), 1);
    assert_eq!(remainder.adjacency[1].sources.len(), 1);
    assert_eq!(remainder.adjacency[1].targets.len(), 1);
    let a0s = remainder.adjacency[0].sources[0];
    let a0t = remainder.adjacency[0].targets[0];
    let a1s = remainder.adjacency[1].sources[0];
    let a1t = remainder.adjacency[1].targets[0];
    assert_ne!(a0s, a0t);
    assert_ne!(a1s, a1t);
    assert_ne!(a0s, a1s);
    assert_ne!(a0s, a1t);
    assert_ne!(a0t, a1s);
    assert_ne!(a0t, a1t);
}

#[test]
fn test_remainder_with_injection_empty_excluded_is_identity() {
    let mut host = Hypergraph::empty();
    let w0 = host.new_node(1);
    let w1 = host.new_node(2);
    host.new_edge(
        10,
        Hyperedge {
            sources: vec![w0],
            targets: vec![w1],
        },
    );

    let excluded = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::initial(host.nodes.len()),
        edges: FiniteFunction::<VecKind>::initial(host.edges.len()),
    };

    let (remainder, remainder_in_host) = host.remainder_with_injection(&excluded);

    assert_eq!(remainder, host);
    assert_eq!(remainder_in_host.nodes.table, VecArray(vec![0, 1]));
    assert_eq!(remainder_in_host.edges.table, VecArray(vec![0]));
}

#[test]
fn test_remainder_with_injection_excluded_edge_drops_edge_only() {
    let mut host = Hypergraph::empty();
    let w0 = host.new_node(1);
    let w1 = host.new_node(2);
    host.new_edge(
        10,
        Hyperedge {
            sources: vec![w0],
            targets: vec![w1],
        },
    );

    let excluded = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::initial(host.nodes.len()),
        edges: FiniteFunction::<VecKind>::new(VecArray(vec![0]), host.edges.len()).unwrap(),
    };

    let (remainder, remainder_in_host) = host.remainder_with_injection(&excluded);

    assert_eq!(remainder.nodes, host.nodes);
    assert!(remainder.edges.is_empty());
    assert!(remainder.adjacency.is_empty());
    assert_eq!(remainder_in_host.nodes.table, VecArray(vec![0, 1]));
    assert!(remainder_in_host.edges.table.is_empty());
}
