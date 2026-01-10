use open_hypergraphs::array::vec::{VecArray, VecKind};
use open_hypergraphs::finite_function::FiniteFunction;
use open_hypergraphs::lax::{Hyperedge, Hypergraph, LaxHypergraphArrow, Match, NodeId, Rule};

fn empty_edges() -> FiniteFunction<VecKind> {
    FiniteFunction::<VecKind>::new(VecArray(vec![]), 0).unwrap()
}

#[test]
fn test_rule_new_ok() {
    let mut k: Hypergraph<i32, i32> = Hypergraph::empty();
    k.nodes = vec![1];

    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.nodes = vec![1, 2];

    let mut r: Hypergraph<i32, i32> = Hypergraph::empty();
    r.nodes = vec![1, 2];

    let left_w = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 2).unwrap();
    let right_w = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 2).unwrap();

    let left = LaxHypergraphArrow::new(k.clone(), l.clone(), left_w, empty_edges());
    let right = LaxHypergraphArrow::new(k.clone(), r.clone(), right_w, empty_edges());

    let _rule = Rule::new(l, k, r, left, right);
}

#[test]
#[should_panic(expected = "rule left morphism target does not match lhs")]
fn test_rule_new_panics_on_mismatch() {
    let mut k: Hypergraph<i32, i32> = Hypergraph::empty();
    k.nodes = vec![1];

    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.nodes = vec![1, 2];

    let mut r: Hypergraph<i32, i32> = Hypergraph::empty();
    r.nodes = vec![1, 2];

    let mut bad_lhs: Hypergraph<i32, i32> = Hypergraph::empty();
    bad_lhs.nodes = vec![9];

    let left_w = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 2).unwrap();
    let right_w = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 2).unwrap();

    let left = LaxHypergraphArrow::new(k.clone(), l, left_w, empty_edges());
    let right = LaxHypergraphArrow::new(k.clone(), r.clone(), right_w, empty_edges());

    let _rule = Rule::new(bad_lhs, k, r, left, right);
}

#[test]
fn test_match_validate_ok() {
    let mut k: Hypergraph<i32, i32> = Hypergraph::empty();
    k.nodes = vec![1];

    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.nodes = vec![1, 2];

    let mut r: Hypergraph<i32, i32> = Hypergraph::empty();
    r.nodes = vec![1, 2];

    let left_w = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 2).unwrap();
    let right_w = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 2).unwrap();
    let left = LaxHypergraphArrow::new(k.clone(), l.clone(), left_w, empty_edges());
    let right = LaxHypergraphArrow::new(k.clone(), r.clone(), right_w, empty_edges());

    let rule = Rule::new(l.clone(), k, r.clone(), left, right);

    let m_w = FiniteFunction::<VecKind>::new(VecArray(vec![0, 1]), 2).unwrap();
    let morphism = LaxHypergraphArrow::new(l, r.clone(), m_w, empty_edges());

    let _match = Match::new(rule, morphism).validate();
}

#[test]
#[should_panic(expected = "match violates the identification condition")]
fn test_match_validate_identification_fails() {
    let mut k: Hypergraph<i32, i32> = Hypergraph::empty();
    k.nodes = vec![1];

    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.nodes = vec![1, 2, 3];

    let mut r: Hypergraph<i32, i32> = Hypergraph::empty();
    r.nodes = vec![1, 2, 3];

    let left_w = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 3).unwrap();
    let right_w = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 3).unwrap();
    let left = LaxHypergraphArrow::new(k.clone(), l.clone(), left_w, empty_edges());
    let right = LaxHypergraphArrow::new(k.clone(), r.clone(), right_w, empty_edges());

    let rule = Rule::new(l.clone(), k, r.clone(), left, right);

    let m_w = FiniteFunction::<VecKind>::new(VecArray(vec![0, 0, 0]), 3).unwrap();
    let morphism = LaxHypergraphArrow::new(l, r, m_w, empty_edges());

    let _match = Match::new(rule, morphism).validate();
}

#[test]
#[should_panic(expected = "match violates the dangling condition")]
fn test_match_validate_dangling_fails() {
    let mut k: Hypergraph<i32, i32> = Hypergraph::empty();
    k.nodes = vec![1];

    let mut l: Hypergraph<i32, i32> = Hypergraph::empty();
    l.nodes = vec![1, 2];

    let mut r: Hypergraph<i32, i32> = Hypergraph::empty();
    r.nodes = vec![1, 2];

    let left_w = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 2).unwrap();
    let right_w = FiniteFunction::<VecKind>::new(VecArray(vec![0]), 2).unwrap();
    let left = LaxHypergraphArrow::new(k.clone(), l.clone(), left_w, empty_edges());
    let right = LaxHypergraphArrow::new(k.clone(), r.clone(), right_w, empty_edges());

    let rule = Rule::new(l.clone(), k, r.clone(), left, right);

    let mut g: Hypergraph<i32, i32> = Hypergraph::empty();
    g.nodes = vec![1, 2];
    g.edges = vec![10];
    g.adjacency = vec![Hyperedge {
        sources: vec![NodeId(1)],
        targets: vec![],
    }];

    let m_w = FiniteFunction::<VecKind>::new(VecArray(vec![0, 1]), 2).unwrap();
    let m_x = FiniteFunction::<VecKind>::new(VecArray(vec![]), 1).unwrap();
    let morphism = LaxHypergraphArrow::new(l, g, m_w, m_x);

    let _match = Match::new(rule, morphism).validate();
}
