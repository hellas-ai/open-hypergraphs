use crate::array::vec::{VecArray, VecKind};
use crate::category::Coproduct;
use crate::finite_function::FiniteFunction;
use crate::indexed_coproduct::IndexedCoproduct;
use crate::semifinite::SemifiniteFunction;
use crate::strict::hypergraph::Hypergraph;
use crate::strict::open_hypergraph::{
    apply_rewrite, ConvexMatchWitness, MonogamousAcyclicHost, OpenHypergraph, RewriteRule,
};

fn make_indexed_coproduct(
    segments: &[Vec<usize>],
    target: usize,
) -> IndexedCoproduct<VecKind, FiniteFunction<VecKind>> {
    let mut lengths = Vec::with_capacity(segments.len());
    let mut values = Vec::new();
    for seg in segments {
        lengths.push(seg.len());
        values.extend_from_slice(seg);
    }
    let sources = SemifiniteFunction::new(VecArray(lengths));
    let values = FiniteFunction::new(VecArray(values), target).unwrap();
    IndexedCoproduct::from_semifinite(sources, values).unwrap()
}

fn make_hypergraph(
    sources: &[Vec<usize>],
    targets: &[Vec<usize>],
    w_labels: Vec<i32>,
    x_labels: Vec<i32>,
) -> Hypergraph<VecKind, i32, i32> {
    let w_len = w_labels.len();
    let s = make_indexed_coproduct(sources, w_len);
    let t = make_indexed_coproduct(targets, w_len);
    let w = SemifiniteFunction::new(VecArray(w_labels));
    let x = SemifiniteFunction::new(VecArray(x_labels));
    Hypergraph::new(s, t, w, x).unwrap()
}

fn make_open_hypergraph(
    sources: &[Vec<usize>],
    targets: &[Vec<usize>],
    w_labels: Vec<i32>,
    x_labels: Vec<i32>,
    s_map: &[usize],
    t_map: &[usize],
) -> OpenHypergraph<VecKind, i32, i32> {
    let h = make_hypergraph(sources, targets, w_labels, x_labels);
    let s = FiniteFunction::new(VecArray(s_map.to_vec()), h.w.len()).unwrap();
    let t = FiniteFunction::new(VecArray(t_map.to_vec()), h.w.len()).unwrap();
    OpenHypergraph::new(s, t, h).unwrap()
}

fn make_open_hypergraph_empty_boundary(
    sources: &[Vec<usize>],
    targets: &[Vec<usize>],
    w_labels: Vec<i32>,
    x_labels: Vec<i32>,
) -> OpenHypergraph<VecKind, i32, i32> {
    let h = make_hypergraph(sources, targets, w_labels, x_labels);
    let s = FiniteFunction::initial(h.w.len());
    let t = FiniteFunction::initial(h.w.len());
    OpenHypergraph::new(s, t, h).unwrap()
}

fn make_map(indices: &[usize], target: usize) -> FiniteFunction<VecKind> {
    FiniteFunction::new(VecArray(indices.to_vec()), target).unwrap()
}

#[test]
fn apply_rewrite_replaces_matched_edge() {
    // Host: edge 0 labeled 20 from wire 0 -> 1, with boundary 0 input, 1 output.
    let host = make_open_hypergraph(
        &[vec![0]],
        &[vec![1]],
        vec![10, 11],
        vec![20],
        &[0],
        &[1],
    );

    // LHS matches the single edge, RHS replaces it with label 21 (same boundary).
    let lhs = make_open_hypergraph(
        &[vec![0]],
        &[vec![1]],
        vec![10, 11],
        vec![20],
        &[0],
        &[1],
    );
    let rhs = make_open_hypergraph(
        &[vec![0]],
        &[vec![1]],
        vec![10, 11],
        vec![21],
        &[0],
        &[1],
    );
    let rule = RewriteRule::new(lhs, rhs).unwrap();

    let host_w_map = make_map(&[0, 1], host.h.w.len());
    let host_x_map = make_map(&[0], host.h.x.len());
    let host = MonogamousAcyclicHost::new(&host).unwrap();
    let m = ConvexMatchWitness::new(rule.lhs(), &host, host_w_map, host_x_map).unwrap();

    let out = apply_rewrite(&rule, &host, &m).unwrap();
    assert_eq!(out.s.table, VecArray(vec![0]));
    assert_eq!(out.t.table, VecArray(vec![1]));
    assert_eq!(out.h.w.0, VecArray(vec![10, 11]));
    assert_eq!(out.h.x.0, VecArray(vec![21]));
    assert_eq!(out.h.s.values.table, VecArray(vec![0]));
    assert_eq!(out.h.t.values.table, VecArray(vec![1]));
}

#[test]
fn apply_rewrite_removes_matched_subgraph_with_empty_rhs() {
    // Host: edge 0 labeled 20 from wire 0 -> 1, no boundary.
    let host = make_open_hypergraph_empty_boundary(
        &[vec![0]],
        &[vec![1]],
        vec![10, 11],
        vec![20],
    );

    // LHS matches the whole host (empty boundary), RHS is empty.
    let lhs = make_open_hypergraph_empty_boundary(
        &[vec![0]],
        &[vec![1]],
        vec![10, 11],
        vec![20],
    );
    let rhs = make_open_hypergraph_empty_boundary(&[], &[], vec![], vec![]);
    let rule = RewriteRule::new(lhs, rhs).unwrap();

    let host_w_map = make_map(&[0, 1], host.h.w.len());
    let host_x_map = make_map(&[0], host.h.x.len());
    let host = MonogamousAcyclicHost::new(&host).unwrap();
    let m = ConvexMatchWitness::new(rule.lhs(), &host, host_w_map, host_x_map).unwrap();

    let out = apply_rewrite(&rule, &host, &m).unwrap();
    assert_eq!(out.s.table.len(), 0);
    assert_eq!(out.t.table.len(), 0);
    assert_eq!(out.h.w.len(), 0);
    assert_eq!(out.h.x.len(), 0);
}
