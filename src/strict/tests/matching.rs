use crate::array::vec::VecKind;
use crate::strict::hypergraph::matching::{find_subgraph_matches, MatchOptions};
use crate::strict::Hypergraph;

fn make_hypergraph(
    sources: &[Vec<usize>],
    targets: &[Vec<usize>],
    w_labels: Vec<i32>,
    x_labels: Vec<i32>,
) -> Hypergraph<VecKind, i32, i32> {
    use crate::array::vec::VecArray;
    use crate::finite_function::FiniteFunction;
    use crate::indexed_coproduct::IndexedCoproduct;
    use crate::semifinite::SemifiniteFunction;

    let mut lengths = Vec::with_capacity(sources.len());
    let mut source_values = Vec::new();
    for segment in sources {
        lengths.push(segment.len());
        source_values.extend_from_slice(segment);
    }

    let source_sizes = SemifiniteFunction::new(VecArray(lengths.clone()));
    let source_values = FiniteFunction::new(VecArray(source_values), w_labels.len()).unwrap();
    let s = IndexedCoproduct::from_semifinite(source_sizes, source_values).unwrap();

    let mut target_lengths = Vec::with_capacity(targets.len());
    let mut target_values = Vec::new();
    for segment in targets {
        target_lengths.push(segment.len());
        target_values.extend_from_slice(segment);
    }

    let target_sizes = SemifiniteFunction::new(VecArray(target_lengths));
    let target_values = FiniteFunction::new(VecArray(target_values), w_labels.len()).unwrap();
    let t = IndexedCoproduct::from_semifinite(target_sizes, target_values).unwrap();

    let w = SemifiniteFunction::new(VecArray(w_labels));
    let x = SemifiniteFunction::new(VecArray(x_labels));
    Hypergraph::new(s, t, w, x).unwrap()
}

#[test]
fn strict_match_finds_expected_single_embedding() {
    let pattern = make_hypergraph(&[vec![0]], &[vec![1]], vec![0, 0], vec![7]);
    let host = make_hypergraph(
        &[vec![0], vec![1]],
        &[vec![1], vec![2]],
        vec![0, 0, 0],
        vec![7, 8],
    );

    let matches = find_subgraph_matches(&pattern, &host, &MatchOptions::default());
    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0].x.table.0, vec![0]);
    assert_eq!(matches[0].w.table.0, vec![0, 1]);
}

#[test]
fn strict_match_can_filter_nonconvex_embeddings() {
    let pattern = make_hypergraph(&[vec![0], vec![1]], &[vec![1], vec![2]], vec![0, 0, 0], vec![1, 2]);
    let host = make_hypergraph(
        &[vec![0], vec![1], vec![0]],
        &[vec![1], vec![2], vec![2]],
        vec![0, 0, 0],
        vec![1, 2, 3],
    );

    let plain = find_subgraph_matches(&pattern, &host, &MatchOptions::default());
    assert_eq!(plain.len(), 1);

    let convex_only = find_subgraph_matches(
        &pattern,
        &host,
        &MatchOptions {
            require_convex: true,
            stop_after_first: false,
        },
    );
    assert!(convex_only.is_empty());
}
