use super::subobject::SubgraphMorphism;
use super::Hypergraph;
use crate::array::vec::{VecArray, VecKind};
use crate::finite_function::FiniteFunction;
use crate::indexed_coproduct::IndexedCoproduct;
use crate::semifinite::SemifiniteFunction;

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

fn make_map(indices: &[usize], target: usize) -> FiniteFunction<VecKind> {
    FiniteFunction::new(VecArray(indices.to_vec()), target).unwrap()
}

#[test]
fn subgraph_from_masks_rejects_dangling() {
    // Graph: single node 0 with a single self-loop edge 0 -> 0.
    // Remove node 0 but keep the edge, so the subgraph is dangling and rejected.
    let host = make_hypergraph(&[vec![0]], &[vec![0]], vec![10], vec![20]);
    let remove_node_mask = VecArray(vec![true]);
    let remove_edge_mask = VecArray(vec![false]);
    assert!(SubgraphMorphism::from_masks(&host, remove_node_mask, remove_edge_mask).is_none());
}

#[test]
fn subgraph_from_masks_accepts_removing_incident_edge() {
    // Graph: single node 0 with a single self-loop edge 0 -> 0.
    // Remove both the node and the edge, so the remaining subgraph is empty.
    let host = make_hypergraph(&[vec![0]], &[vec![0]], vec![10], vec![20]);
    let remove_node_mask = VecArray(vec![true]);
    let remove_edge_mask = VecArray(vec![true]);
    let subgraph = SubgraphMorphism::from_masks(&host, remove_node_mask, remove_edge_mask).unwrap();
    let (kept, w_inj, x_inj) = subgraph.as_hypergraph_with_injections().unwrap();
    assert_eq!(kept.w.len(), 0);
    assert_eq!(kept.x.len(), 0);
    assert_eq!(w_inj.table.len(), 0);
    assert_eq!(x_inj.table.len(), 0);
}

#[test]
fn subgraph_keep_all_preserves_sizes() {
    // Graph: two nodes {0,1} with two edges:
    // edge 0: [0,1] -> [1]
    // edge 1: [1] -> [0,1]
    // Keep everything, so sizes and injections match the host.
    let host = make_hypergraph(
        &[vec![0, 1], vec![1]],
        &[vec![1], vec![0, 1]],
        vec![10, 11],
        vec![20, 21],
    );
    let remove_node_mask = VecArray(vec![false, false]);
    let remove_edge_mask = VecArray(vec![false, false]);
    let subgraph = SubgraphMorphism::from_masks(&host, remove_node_mask, remove_edge_mask).unwrap();
    let (kept, w_inj, x_inj) = subgraph.as_hypergraph_with_injections().unwrap();
    assert_eq!(kept.w.len(), host.w.len());
    assert_eq!(kept.x.len(), host.x.len());
    assert_eq!(w_inj.table.len(), host.w.len());
    assert_eq!(x_inj.table.len(), host.x.len());
}

#[test]
fn subgraph_remove_edge_only_keeps_nodes() {
    // Graph: two nodes {0,1} and two edges:
    // edge 0: 0 -> 1
    // edge 1: 1 -> 0
    // Remove edge 0 but keep all nodes, so only the edge count changes.
    let host = make_hypergraph(
        &[vec![0], vec![1]],
        &[vec![1], vec![0]],
        vec![10, 11],
        vec![20, 21],
    );
    let remove_node_mask = VecArray(vec![false, false]);
    let remove_edge_mask = VecArray(vec![true, false]);
    let subgraph = SubgraphMorphism::from_masks(&host, remove_node_mask, remove_edge_mask).unwrap();
    let (kept, w_inj, x_inj) = subgraph.as_hypergraph_with_injections().unwrap();
    assert_eq!(kept.w.len(), host.w.len());
    assert_eq!(kept.x.len(), 1);
    assert_eq!(w_inj.table.len(), host.w.len());
    assert_eq!(x_inj.table.len(), 1);
}

#[test]
fn subgraph_remove_node_and_incident_edge() {
    // Graph: two nodes {0,1} and two edges.
    // Edge 0: 0 -> 1
    // Edge 1: 1 -> 0
    // We remove node 0 and also remove edge 0 (the one incident to node 0 on the source side).
    // If edge 1 is kept, it still targets node 0, so the subgraph is dangling.
    let host = make_hypergraph(
        &[vec![0], vec![1]],
        &[vec![1], vec![0]],
        vec![10, 11],
        vec![20, 21],
    );
    let remove_node_mask = VecArray(vec![true, false]);
    let remove_edge_mask = VecArray(vec![true, false]);
    assert!(SubgraphMorphism::from_masks(&host, remove_node_mask, remove_edge_mask).is_none());
}

#[test]
fn subgraph_dangling_when_removed_node_in_source() {
    // Graph: two nodes {0,1} and one edge 0 -> 1.
    // Remove node 0 but keep the edge; the source endpoint dangles.
    let host = make_hypergraph(&[vec![0]], &[vec![1]], vec![10, 11], vec![20]);
    let remove_node_mask = VecArray(vec![true, false]);
    let remove_edge_mask = VecArray(vec![false]);
    assert!(SubgraphMorphism::from_masks(&host, remove_node_mask, remove_edge_mask).is_none());
}

#[test]
fn subgraph_dangling_when_removed_node_in_target() {
    // Graph: two nodes {0,1} and one edge 1 -> 0.
    // Remove node 0 but keep the edge; the target endpoint dangles.
    let host = make_hypergraph(&[vec![1]], &[vec![0]], vec![10, 11], vec![20]);
    let remove_node_mask = VecArray(vec![true, false]);
    let remove_edge_mask = VecArray(vec![false]);
    assert!(SubgraphMorphism::from_masks(&host, remove_node_mask, remove_edge_mask).is_none());
}

#[test]
fn subgraph_dangling_with_multi_source() {
    // Graph: three nodes {0,1,2} and one edge [0,1] -> [2].
    // Remove node 1 but keep the edge; one source endpoint dangles.
    let host = make_hypergraph(&[vec![0, 1]], &[vec![2]], vec![10, 11, 12], vec![20]);
    let remove_node_mask = VecArray(vec![false, true, false]);
    let remove_edge_mask = VecArray(vec![false]);
    assert!(SubgraphMorphism::from_masks(&host, remove_node_mask, remove_edge_mask).is_none());
}

#[test]
fn subgraph_dangling_with_multi_target() {
    // Graph: three nodes {0,1,2} and one edge [0] -> [1,2].
    // Remove node 2 but keep the edge; one target endpoint dangles.
    let host = make_hypergraph(&[vec![0]], &[vec![1, 2]], vec![10, 11, 12], vec![20]);
    let remove_node_mask = VecArray(vec![false, false, true]);
    let remove_edge_mask = VecArray(vec![false]);
    assert!(SubgraphMorphism::from_masks(&host, remove_node_mask, remove_edge_mask).is_none());
}

#[test]
fn remainder_with_injection_removes_selected_node_and_edge() {
    // Graph: nodes {0,1} with labels [10,11], edges {0,1} with labels [20,21].
    // edge 0: 0 -> 1
    // edge 1: 1 -> 1
    // Remove node 0 and edge 0; remainder should keep node 1 and edge 1,
    // with indices remapped so node 1 becomes 0.
    let host = make_hypergraph(
        &[vec![0], vec![1]],
        &[vec![1], vec![1]],
        vec![10, 11],
        vec![20, 21],
    );
    let w_map = make_map(&[0], host.w.len());
    let x_map = make_map(&[0], host.x.len());
    let (rem, w_inj, x_inj) = host.remainder_with_injection(&w_map, &x_map).unwrap();
    assert_eq!(rem.w.0, VecArray(vec![11]));
    assert_eq!(rem.x.0, VecArray(vec![21]));
    assert_eq!(rem.s.values.table, VecArray(vec![0]));
    assert_eq!(rem.t.values.table, VecArray(vec![0]));
    assert_eq!(w_inj.table, VecArray(vec![1]));
    assert_eq!(x_inj.table, VecArray(vec![1]));
}

#[test]
fn remainder_with_injection_keep_all_is_identity() {
    // Graph: nodes {0,1} with edges 0:0->1 and 1:1->0. Removing nothing keeps everything.
    let host = make_hypergraph(
        &[vec![0], vec![1]],
        &[vec![1], vec![0]],
        vec![10, 11],
        vec![20, 21],
    );
    let w_map = make_map(&[], host.w.len());
    let x_map = make_map(&[], host.x.len());
    let (rem, w_inj, x_inj) = host.remainder_with_injection(&w_map, &x_map).unwrap();
    assert_eq!(rem.w.0, host.w.0);
    assert_eq!(rem.x.0, host.x.0);
    assert_eq!(rem.s.values.table, host.s.values.table);
    assert_eq!(rem.t.values.table, host.t.values.table);
    assert_eq!(w_inj.table, VecArray(vec![0, 1]));
    assert_eq!(x_inj.table, VecArray(vec![0, 1]));
}

#[test]
fn remainder_with_injection_remove_all_is_empty() {
    // Graph: single node with a self-loop edge. Removing all yields the empty hypergraph.
    let host = make_hypergraph(&[vec![0]], &[vec![0]], vec![10], vec![20]);
    let w_map = make_map(&[0], host.w.len());
    let x_map = make_map(&[0], host.x.len());
    let (rem, w_inj, x_inj) = host.remainder_with_injection(&w_map, &x_map).unwrap();
    assert_eq!(rem.w.len(), 0);
    assert_eq!(rem.x.len(), 0);
    assert_eq!(rem.s.values.table.len(), 0);
    assert_eq!(rem.t.values.table.len(), 0);
    assert_eq!(w_inj.table.len(), 0);
    assert_eq!(x_inj.table.len(), 0);
}
