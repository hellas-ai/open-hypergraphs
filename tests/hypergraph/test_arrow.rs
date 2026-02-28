use open_hypergraphs::array::vec::*;
use open_hypergraphs::finite_function::FiniteFunction;
use open_hypergraphs::lax::{Hyperedge, Hypergraph as LaxHypergraph, NodeId};
use open_hypergraphs::strict::hypergraph::arrow::{HypergraphArrow, InvalidHypergraphArrow};
use open_hypergraphs::strict::hypergraph::Hypergraph;

type Obj = usize;
type Arr = usize;

fn build_hypergraph(
    node_labels: Vec<usize>,
    edges: &[(usize, Vec<usize>, Vec<usize>)],
) -> Hypergraph<VecKind, Obj, Arr> {
    let mut h = LaxHypergraph::empty();
    h.nodes = node_labels;
    for (label, sources, targets) in edges {
        let edge = Hyperedge {
            sources: sources.iter().map(|&i| NodeId(i)).collect(),
            targets: targets.iter().map(|&i| NodeId(i)).collect(),
        };
        h.new_edge(*label, edge);
    }
    h.to_hypergraph()
}

fn ff(table: Vec<usize>, target: usize) -> FiniteFunction<VecKind> {
    FiniteFunction::new(VecArray(table), target).unwrap()
}

#[test]
fn test_validate_rejects_non_natural_w() {
    let source = build_hypergraph(vec![0], &[]);
    let target = build_hypergraph(vec![1], &[]);
    let w = ff(vec![0], 1);
    let x = ff(vec![], 0);
    let err = HypergraphArrow::new(source, target, w, x).unwrap_err();
    assert!(matches!(err, InvalidHypergraphArrow::NotNaturalW));
}

#[test]
fn test_validate_rejects_non_natural_x() {
    let source = build_hypergraph(vec![0, 0], &[(0, vec![0], vec![1])]);
    let target = build_hypergraph(vec![0, 0], &[(1, vec![0], vec![1])]);
    let w = ff(vec![0, 1], 2);
    let x = ff(vec![0], 1);
    let err = HypergraphArrow::new(source, target, w, x).unwrap_err();
    assert!(matches!(err, InvalidHypergraphArrow::NotNaturalX));
}

#[test]
fn test_validate_accepts_incidence_natural_arrow() {
    let source = build_hypergraph(vec![0, 0], &[(0, vec![0], vec![1])]);
    let target = build_hypergraph(vec![0, 0], &[(0, vec![1], vec![1])]);

    let w = ff(vec![1, 1], 2);
    let x = ff(vec![0], 1);
    assert!(HypergraphArrow::new(source, target, w, x).is_ok());
}

#[test]
fn test_validate_rejects_non_natural_s() {
    let source = build_hypergraph(vec![0, 0], &[(0, vec![0], vec![1])]);
    let target = build_hypergraph(vec![0, 0], &[(0, vec![0], vec![1])]);

    let w = ff(vec![1, 1], 2);
    let x = ff(vec![0], 1);
    let err = HypergraphArrow::new(source, target, w, x).unwrap_err();
    assert!(matches!(err, InvalidHypergraphArrow::NotNaturalS));
}

#[test]
fn test_validate_rejects_non_natural_t() {
    let source = build_hypergraph(vec![0, 0], &[(0, vec![0], vec![1])]);
    let target = build_hypergraph(vec![0, 0], &[(0, vec![0], vec![1])]);

    let w = ff(vec![0, 0], 2);
    let x = ff(vec![0], 1);
    let err = HypergraphArrow::new(source, target, w, x).unwrap_err();
    assert!(matches!(err, InvalidHypergraphArrow::NotNaturalT));
}
