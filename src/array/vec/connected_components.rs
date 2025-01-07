/// Find connected components of a graph stored as a pair of arrays of nodes,
/// interpreted as a list of edges `sources[i] → targets[i]`
///
/// # Panics
///
/// * `sources.len() != targets.len()`
/// * When any `sources[i] >= n` or `targets[i] >= n`
pub fn connected_components(sources: &[usize], targets: &[usize], n: usize) -> (Vec<usize>, usize) {
    // Must have equal sized arrays
    assert_eq!(sources.len(), targets.len());
    // Arrays must be empty if graph has no nodes.
    assert!(n > 0 || sources.is_empty());
    todo!();
}
