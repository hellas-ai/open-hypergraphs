use open_hypergraphs::array::vec::{VecArray, VecKind};
use open_hypergraphs::finite_function::FiniteFunction;
use open_hypergraphs::lax::{Arrow, Hyperedge, Hypergraph, LaxHypergraphArrow, NodeId};

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
fn test_delete_nodes_ignores_out_of_range() {
    let mut h = Hypergraph::empty();
    h.nodes = vec![5, 6];
    h.edges = vec![1];
    h.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(1)],
    }];
    h.quotient = (vec![NodeId(0)], vec![NodeId(1)]);

    h.delete_nodes(&[NodeId(99)]);

    assert_eq!(h.nodes, vec![5, 6]);
    assert_eq!(h.adjacency[0].sources, vec![NodeId(0)]);
    assert_eq!(h.adjacency[0].targets, vec![NodeId(1)]);
    assert_eq!(h.quotient.0, vec![NodeId(0)]);
    assert_eq!(h.quotient.1, vec![NodeId(1)]);
}

#[test]
fn test_lax_hypergraph_arrow_to_strict_identity() {
    let mut source = Hypergraph::empty();
    source.nodes = vec![1, 2];
    source.edges = vec![10];
    source.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(1)],
    }];

    let mut target = Hypergraph::empty();
    target.nodes = vec![1, 2];
    target.edges = vec![10];
    target.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(1)],
    }];

    let w = FiniteFunction::<VecKind>::new(VecArray(vec![0, 1]), 2).unwrap();
    let x = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 1).unwrap();

    let arrow = LaxHypergraphArrow::new(source, target, w, x).to_strict();

    assert_eq!(arrow.w.table, VecArray(vec![0, 1]));
    assert_eq!(arrow.w.target(), 2);
    assert_eq!(arrow.x.table, VecArray(vec![0]));
    assert_eq!(arrow.x.target(), 1);
}

#[test]
#[should_panic(expected = "node map not constant on source quotient")]
fn test_lax_hypergraph_arrow_to_strict_panics_on_nonconstant_w() {
    let mut source = Hypergraph::empty();
    source.nodes = vec![1, 1];
    source.edges = vec![10];
    source.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(1)],
    }];
    source.quotient = (vec![NodeId(0)], vec![NodeId(1)]);

    let mut target = Hypergraph::empty();
    target.nodes = vec![1, 1];
    target.edges = vec![10];
    target.adjacency = vec![Hyperedge {
        sources: vec![NodeId(0)],
        targets: vec![NodeId(1)],
    }];

    let w = FiniteFunction::<VecKind>::new(VecArray(vec![0, 1]), 2).unwrap();
    let x = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 1).unwrap();

    let _ = LaxHypergraphArrow::new(source, target, w, x).to_strict();
}
