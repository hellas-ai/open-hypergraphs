use open_hypergraphs::array::vec::{VecArray, VecKind};
use open_hypergraphs::finite_function::FiniteFunction;
use open_hypergraphs::lax::{rewrite, Hyperedge, Hypergraph, LaxSpan, NodeEdgeMap};

fn empty_map(target: usize) -> FiniteFunction<VecKind> {
    FiniteFunction::<VecKind>::new(VecArray(vec![]), target).unwrap()
}

fn span_with_empty_apex(
    left: Hypergraph<i32, i32>,
    right: Hypergraph<i32, i32>,
) -> LaxSpan<i32, i32> {
    let apex: Hypergraph<i32, i32> = Hypergraph::empty();
    let left_map = NodeEdgeMap {
        nodes: empty_map(left.nodes.len()),
        edges: empty_map(left.edges.len()),
    };
    let right_map = NodeEdgeMap {
        nodes: empty_map(right.nodes.len()),
        edges: empty_map(right.edges.len()),
    };
    LaxSpan::new(apex, left, right, left_map, right_map)
}

#[test]
fn test_rewrite_identification_fails() {
    // K = ∅, L = {a, b}. G has one node w and m(a) = m(b) = w.
    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.new_node(1);
    l.new_node(2);

    let r: Hypergraph<i32, i32> = Hypergraph::empty();
    let rule = span_with_empty_apex(l.clone(), r);

    let mut g: Hypergraph<i32, i32> = Hypergraph::empty();
    g.new_node(1);

    let candidate = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0, 0]), g.nodes.len()).unwrap(),
        edges: empty_map(g.edges.len()),
    };

    assert!(rewrite(&g, &rule, &candidate).is_none());
}

#[test]
fn test_rewrite_dangling_fails() {
    // K = ∅, L = {u}. G = {v} with a loop edge e: v -> v, and m(u) = v.
    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.new_node(1);

    let r: Hypergraph<i32, i32> = Hypergraph::empty();
    let rule = span_with_empty_apex(l.clone(), r);

    let mut g: Hypergraph<i32, i32> = Hypergraph::empty();
    let v = g.new_node(1);
    g.new_edge(
        10,
        Hyperedge {
            sources: vec![v],
            targets: vec![v],
        },
    );

    let candidate = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0]), g.nodes.len()).unwrap(),
        edges: empty_map(g.edges.len()),
    };

    assert!(rewrite(&g, &rule, &candidate).is_none());
}

#[test]
fn test_rewrite_gluing_ok() {
    // K = ∅, L = {u}. G = {v} with no edges, and m(u) = v.
    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.new_node(1);

    let r: Hypergraph<i32, i32> = Hypergraph::empty();
    let rule = span_with_empty_apex(l.clone(), r);

    let mut g: Hypergraph<i32, i32> = Hypergraph::empty();
    g.new_node(1);

    let candidate = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0]), g.nodes.len()).unwrap(),
        edges: empty_map(g.edges.len()),
    };

    let complements = rewrite(&g, &rule, &candidate).expect("expected complements");
    assert!(!complements.is_empty());
}
