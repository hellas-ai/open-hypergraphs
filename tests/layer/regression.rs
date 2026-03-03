// build using lax, it's easier
use open_hypergraphs::lax::OpenHypergraph;
use open_hypergraphs::strict::layer::layered_operations;

// We cannot
#[test]
fn single_node_multiple_edges_layered() {
    let mut term = OpenHypergraph::empty();
    let x = term.new_node(());
    term.new_edge("f", ([x], [x]));
    term.new_edge("g", ([x, x], [x]));
    term.sources = vec![x, x, x];
    term.targets = vec![x, x];

    let strict = term.to_strict();
    layered_operations(&strict);
}
