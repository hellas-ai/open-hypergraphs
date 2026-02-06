use open_hypergraphs::array::vec::*;
use open_hypergraphs::finite_function::FiniteFunction;
use open_hypergraphs::lax::{Hyperedge, Hypergraph as LaxHypergraph, NodeId};
use open_hypergraphs::strict::hypergraph::arrow::HypergraphArrow;
use open_hypergraphs::strict::hypergraph::Hypergraph;

type Obj = usize;
type Arr = usize;

fn build_hypergraph(
    node_count: usize,
    edges: &[(Vec<usize>, Vec<usize>)],
) -> Hypergraph<VecKind, Obj, Arr> {
    let mut h = LaxHypergraph::empty();
    h.nodes = vec![0; node_count];
    for (sources, targets) in edges {
        let edge = Hyperedge {
            sources: sources.iter().map(|&i| NodeId(i)).collect(),
            targets: targets.iter().map(|&i| NodeId(i)).collect(),
        };
        h.new_edge(0, edge);
    }
    h.to_hypergraph()
}

fn ff(table: Vec<usize>, target: usize) -> FiniteFunction<VecKind> {
    FiniteFunction::new(VecArray(table), target).unwrap()
}

#[test]
fn test_is_monomorphism_false_for_non_injective_w() {
    let h = build_hypergraph(2, &[]);
    let g = build_hypergraph(1, &[]);
    let w = ff(vec![0, 0], 1);
    let x = ff(vec![], 0);
    let arrow = HypergraphArrow::new(h, g, w, x).unwrap();
    assert!(!arrow.is_monomorphism());
}

#[test]
fn test_convex_subgraph_false_with_shortcut() {
    let g = build_hypergraph(
        3,
        &[(vec![0], vec![1]), (vec![1], vec![2]), (vec![0], vec![2])],
    );
    let h = build_hypergraph(3, &[(vec![0], vec![1]), (vec![1], vec![2])]);

    let w = ff(vec![0, 1, 2], 3);
    let x = ff(vec![0, 1], 3);
    let arrow = HypergraphArrow::new(h, g, w, x).unwrap();
    assert!(!arrow.is_convex_subgraph());
}

#[test]
fn test_convex_subgraph_true_identity() {
    let g = build_hypergraph(
        3,
        &[(vec![0], vec![1]), (vec![1], vec![2]), (vec![0], vec![2])],
    );
    let w = ff(vec![0, 1, 2], 3);
    let x = ff(vec![0, 1, 2], 3);
    let arrow = HypergraphArrow::new(g.clone(), g, w, x).unwrap();
    assert!(arrow.is_convex_subgraph());
}

#[test]
fn test_convex_subgraph_true_small() {
    let g = build_hypergraph(
        3,
        &[(vec![0], vec![1]), (vec![1], vec![2]), (vec![0], vec![2])],
    );
    let h = build_hypergraph(2, &[(vec![0], vec![1])]);

    let w = ff(vec![0, 1], 3);
    let x = ff(vec![0], 3);
    let arrow = HypergraphArrow::new(h, g, w, x).unwrap();
    assert!(arrow.is_convex_subgraph());
}
