use open_hypergraphs::lax::{EdgeId, Hyperedge, Hypergraph, NodeId};

#[test]
fn test_subgraph_isomorphisms_single_edge() {
    let mut target = Hypergraph::empty();
    let t0 = target.new_node(0);
    let t1 = target.new_node(1);
    let t2 = target.new_node(0);
    target.new_edge('f', (vec![t0], vec![t1]));
    target.new_edge('f', (vec![t2], vec![t1]));

    let mut pattern = Hypergraph::empty();
    let p0 = pattern.new_node(0);
    let p1 = pattern.new_node(1);
    pattern.new_edge('f', (vec![p0], vec![p1]));

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert_eq!(matches.len(), 2);
    assert!(matches.iter().all(|m| m.node_map()[1] == NodeId(1)));

    let mut sources = matches.iter().map(|m| m.node_map()[0].0).collect::<Vec<_>>();
    sources.sort();
    assert_eq!(sources, vec![0, 2]);

    for m in matches {
        if m.node_map()[0] == NodeId(0) {
            assert_eq!(m.edge_map()[0], EdgeId(0));
        } else {
            assert_eq!(m.edge_map()[0], EdgeId(1));
        }
    }
}

#[test]
fn test_subgraph_isomorphisms_order_sensitive() {
    let mut target = Hypergraph::empty();
    let t0 = target.new_node(0);
    let t1 = target.new_node(1);
    target.new_edge('f', (vec![t0, t1], vec![]));

    let mut pattern = Hypergraph::empty();
    let p0 = pattern.new_node(0);
    let p1 = pattern.new_node(1);
    pattern.new_edge('f', (vec![p1, p0], vec![]));

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert!(matches.is_empty());
}

#[test]
fn test_subgraph_isomorphisms_isolated_nodes() {
    let mut target: Hypergraph<i32, ()> = Hypergraph::empty();
    target.new_node(1);
    target.new_node(2);
    target.new_node(1);

    let mut pattern: Hypergraph<i32, ()> = Hypergraph::empty();
    pattern.new_node(1);
    pattern.new_node(2);

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert_eq!(matches.len(), 2);
    assert!(matches.iter().all(|m| m.node_map()[1] == NodeId(1)));

    let mut sources = matches.iter().map(|m| m.node_map()[0].0).collect::<Vec<_>>();
    sources.sort();
    assert_eq!(sources, vec![0, 2]);
}
