use crate::lax::{Hyperedge, Hypergraph};

pub(crate) fn build_hypergraph<'a>(
    node_types: Vec<&'a str>,
    edges: Vec<(&'a str, Vec<usize>, Vec<usize>)>,
) -> Hypergraph<String, String> {
    let mut graph: Hypergraph<String, String> = Hypergraph::empty();
    let node_ids = node_types
        .into_iter()
        .map(|label| graph.new_node(label.to_string()))
        .collect::<Vec<_>>();

    for (label, sources, targets) in edges {
        let sources = sources
            .into_iter()
            .map(|idx| {
                *node_ids
                    .get(idx)
                    .unwrap_or_else(|| panic!("edge source index {} out of range", idx))
            })
            .collect::<Vec<_>>();
        let targets = targets
            .into_iter()
            .map(|idx| {
                *node_ids
                    .get(idx)
                    .unwrap_or_else(|| panic!("edge target index {} out of range", idx))
            })
            .collect::<Vec<_>>();
        graph.new_edge(label.to_string(), Hyperedge { sources, targets });
    }

    graph
}

#[macro_export]
macro_rules! hg {
    (
        nodes: { $($node:ident : $node_ty:expr),* $(,)? },
        edges: [
            $(
                ($edge_label:expr, [$($src:ident),* $(,)?], [$($tgt:ident),* $(,)?])
            ),* $(,)?
        ] $(,)?
    ) => {{
        // Node identifiers are only for readability/debugging; they do not persist in the graph.
        // Future: consider adding logical node names to Hypergraph for improved diagnostics.
        let node_types = vec![$($node_ty),*];
        let node_names = vec![$(stringify!($node)),*];
        let mut node_index = std::collections::HashMap::new();
        for (idx, name) in node_names.iter().enumerate() {
            node_index.insert(*name, idx);
        }
        let edges = vec![
            $(
                (
                    $edge_label,
                    vec![
                        $( *node_index.get(stringify!($src)).expect("unknown source node") ),*
                    ],
                    vec![
                        $( *node_index.get(stringify!($tgt)).expect("unknown target node") ),*
                    ],
                )
            ),*
        ];
        $crate::lax::test_utils::build_hypergraph(node_types, edges)
    }};
}
