use open_hypergraphs::array::vec::{VecArray, VecKind};
use open_hypergraphs::finite_function::FiniteFunction;
use open_hypergraphs::lax::{rewrite, Hyperedge, Hypergraph, NodeEdgeMap, Span};

fn empty_map(target: usize) -> FiniteFunction<VecKind> {
    FiniteFunction::<VecKind>::new(VecArray(vec![]), target).unwrap()
}

fn span_with_empty_apex(
    left: &Hypergraph<i32, i32>,
    right: &Hypergraph<i32, i32>,
) -> (Hypergraph<i32, i32>, NodeEdgeMap, NodeEdgeMap) {
    let apex: Hypergraph<i32, i32> = Hypergraph::empty();
    let left_map = NodeEdgeMap {
        nodes: empty_map(left.nodes.len()),
        edges: empty_map(left.edges.len()),
    };
    let right_map = NodeEdgeMap {
        nodes: empty_map(right.nodes.len()),
        edges: empty_map(right.edges.len()),
    };
    (apex, left_map, right_map)
}

#[test]
fn test_rewrite_identification_fails() {
    // K = ∅, L = {a, b}. G has one node w and m(a) = m(b) = w.
    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.new_node(1);
    l.new_node(1);

    let r: Hypergraph<i32, i32> = Hypergraph::empty();
    let (apex, left_map, right_map) = span_with_empty_apex(&l, &r);
    let rule = Span::new(&apex, &l, &r, &left_map, &right_map);

    let mut g: Hypergraph<i32, i32> = Hypergraph::empty();
    g.new_node(1);

    let candidate = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0, 0]), g.nodes.len()).unwrap(),
        edges: empty_map(g.edges.len()),
    };

    assert!(rewrite(&g, &rule, &candidate).is_empty());
}

#[test]
fn test_rewrite_dangling_fails() {
    // K = ∅, L = {u}. G = {v} with a loop edge e: v -> v, and m(u) = v.
    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.new_node(1);

    let r: Hypergraph<i32, i32> = Hypergraph::empty();
    let (apex, left_map, right_map) = span_with_empty_apex(&l, &r);
    let rule = Span::new(&apex, &l, &r, &left_map, &right_map);

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

    assert!(rewrite(&g, &rule, &candidate).is_empty());
}

#[test]
fn test_rewrite_gluing_ok() {
    // K = ∅, L = {u}. G = {v} with no edges, and m(u) = v.
    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.new_node(1);

    let r: Hypergraph<i32, i32> = Hypergraph::empty();
    let (apex, left_map, right_map) = span_with_empty_apex(&l, &r);
    let rule = Span::new(&apex, &l, &r, &left_map, &right_map);

    let mut g: Hypergraph<i32, i32> = Hypergraph::empty();
    g.new_node(1);

    let candidate = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0]), g.nodes.len()).unwrap(),
        edges: empty_map(g.edges.len()),
    };

    let complements = rewrite(&g, &rule, &candidate);
    assert!(!complements.is_empty());
}

#[test]
fn test_rewrite_left_linear_unique_complement() {
    // Pattern L: three nodes with a single edge 100: l0 -> (l1, l2).
    // Target G: four nodes with edge 100: g0 -> (g1, g2) matched by L,
    // plus extra context edge 300: g0 -> g1.
    // K -> L is injective (left-linear), and the match satisfies gluing,
    // so there is a unique complement and thus a unique rewrite.
    let mut apex: Hypergraph<i32, i32> = Hypergraph::empty();
    apex.new_node(1);
    apex.new_node(1);

    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    let l0 = l.new_node(1);
    let l1 = l.new_node(1);
    let l2 = l.new_node(1);
    l.new_edge(
        100,
        Hyperedge {
            sources: vec![l0],
            targets: vec![l1, l2],
        },
    );

    let mut r: Hypergraph<i32, i32> = Hypergraph::empty();
    let r0 = r.new_node(1);
    let r1 = r.new_node(1);
    let r2 = r.new_node(1);
    r.new_edge(
        200,
        Hyperedge {
            sources: vec![r0, r2],
            targets: vec![r1],
        },
    );

    let left_map = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0, 1]), l.nodes.len()).unwrap(),
        edges: empty_map(l.edges.len()),
    };
    let right_map = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0, 1]), r.nodes.len()).unwrap(),
        edges: empty_map(r.edges.len()),
    };
    let rule = Span::new(&apex, &l, &r, &left_map, &right_map);

    let mut g: Hypergraph<i32, i32> = Hypergraph::empty();
    let g0 = g.new_node(1);
    let g1 = g.new_node(1);
    let g2 = g.new_node(1);
    g.new_node(1);
    g.new_edge(
        100,
        Hyperedge {
            sources: vec![g0],
            targets: vec![g1, g2],
        },
    );
    g.new_edge(
        300,
        Hyperedge {
            sources: vec![g0],
            targets: vec![g1],
        },
    );

    let candidate = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0, 1, 2]), g.nodes.len()).unwrap(),
        edges: FiniteFunction::<VecKind>::new(VecArray(vec![0]), g.edges.len()).unwrap(),
    };

    let rewrites = rewrite(&g, &rule, &candidate);
    assert_eq!(rewrites.len(), 1);
    assert_eq!(rewrites[0].nodes.len(), 4);
    assert_eq!(rewrites[0].edges.len(), 2);
    assert_eq!(rewrites[0].adjacency.len(), 2);
    assert!(rewrites[0].edges.contains(&200));
    assert!(rewrites[0].edges.contains(&300));
    assert!(!rewrites[0].edges.contains(&100));
}
