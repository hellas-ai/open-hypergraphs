use open_hypergraphs::lax::{EdgeId, Hypergraph, NodeId};

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

    let mut sources = matches
        .iter()
        .map(|m| m.node_map()[0].0)
        .collect::<Vec<_>>();
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

    let mut sources = matches
        .iter()
        .map(|m| m.node_map()[0].0)
        .collect::<Vec<_>>();
    sources.sort();
    assert_eq!(sources, vec![0, 2]);
}

#[test]
fn test_subgraph_isomorphisms_shared_nodes() {
    let mut target = Hypergraph::empty();
    let n0 = target.new_node(0);
    let n1 = target.new_node(1);
    let n2 = target.new_node(2);
    target.new_edge('g', (vec![n0], vec![n1]));
    target.new_edge('h', (vec![n1], vec![n2]));

    let mut pattern = Hypergraph::empty();
    let p0 = pattern.new_node(0);
    let p1 = pattern.new_node(1);
    let p2 = pattern.new_node(2);
    pattern.new_edge('g', (vec![p0], vec![p1]));
    pattern.new_edge('h', (vec![p1], vec![p2]));

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert_eq!(matches.len(), 1);
}

#[test]
fn test_subgraph_isomorphisms_arity_mismatch() {
    let mut target = Hypergraph::empty();
    let n0 = target.new_node(0);
    let n1 = target.new_node(1);
    target.new_edge('f', (vec![n0], vec![n1]));

    let mut pattern = Hypergraph::empty();
    let p0 = pattern.new_node(0);
    let p1 = pattern.new_node(1);
    let p2 = pattern.new_node(2);
    pattern.new_edge('f', (vec![p0, p1], vec![p2]));

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert!(matches.is_empty());
}

#[test]
fn test_subgraph_isomorphisms_degree_feasible_prune() {
    let mut target = Hypergraph::empty();
    let n0 = target.new_node(0);
    let n1 = target.new_node(1);
    target.new_edge('a', (vec![n0], vec![n1]));

    let mut pattern = Hypergraph::empty();
    let p0 = pattern.new_node(0);
    let p1 = pattern.new_node(1);
    let p2 = pattern.new_node(2);
    pattern.new_edge('a', (vec![p0], vec![p1]));
    pattern.new_edge('b', (vec![p0], vec![p2]));

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert!(matches.is_empty());
}

#[test]
fn test_subgraph_isomorphisms_empty_pattern() {
    let mut target = Hypergraph::empty();
    target.new_node(1);
    target.new_node(2);

    let pattern: Hypergraph<i32, char> = Hypergraph::empty();
    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert_eq!(matches.len(), 1);
    assert!(matches[0].node_map().is_empty());
    assert!(matches[0].edge_map().is_empty());
}

#[test]
fn test_subgraph_isomorphisms_multi_incidence_sources() {
    let mut target = Hypergraph::empty();
    let n0 = target.new_node(0);
    let n1 = target.new_node(1);
    target.new_edge('f', (vec![n0, n0], vec![n1]));

    let mut pattern = Hypergraph::empty();
    let p0 = pattern.new_node(0);
    let p1 = pattern.new_node(1);
    pattern.new_edge('f', (vec![p0, p0], vec![p1]));

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0].node_map()[0], n0);
    assert_eq!(matches[0].node_map()[1], n1);
    assert_eq!(matches[0].edge_map()[0], EdgeId(0));
}

#[test]
fn test_subgraph_isomorphisms_node_in_sources_and_targets() {
    let mut target = Hypergraph::empty();
    let n0 = target.new_node(0);
    target.new_edge('g', (vec![n0], vec![n0]));

    let mut pattern = Hypergraph::empty();
    let p0 = pattern.new_node(0);
    pattern.new_edge('g', (vec![p0], vec![p0]));

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0].node_map()[0], n0);
    assert_eq!(matches[0].edge_map()[0], EdgeId(0));
}

#[test]
fn test_subgraph_isomorphisms_identical_edges_injective() {
    let mut target = Hypergraph::empty();
    let n0 = target.new_node(0);
    let n1 = target.new_node(1);
    target.new_edge('h', (vec![n0], vec![n1]));
    target.new_edge('h', (vec![n0], vec![n1]));

    let mut pattern = Hypergraph::empty();
    let p0 = pattern.new_node(0);
    let p1 = pattern.new_node(1);
    pattern.new_edge('h', (vec![p0], vec![p1]));

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert_eq!(matches.len(), 2);
    let mut edge_ids = matches.iter().map(|m| m.edge_map()[0].0).collect::<Vec<_>>();
    edge_ids.sort();
    assert_eq!(edge_ids, vec![0, 1]);
}

#[test]
fn test_subgraph_isomorphisms_two_identical_edges_bijective() {
    let mut target = Hypergraph::empty();
    let n0 = target.new_node(0);
    let n1 = target.new_node(1);
    target.new_edge('h', (vec![n0], vec![n1]));
    target.new_edge('h', (vec![n0], vec![n1]));

    let mut pattern = Hypergraph::empty();
    let p0 = pattern.new_node(0);
    let p1 = pattern.new_node(1);
    pattern.new_edge('h', (vec![p0], vec![p1]));
    pattern.new_edge('h', (vec![p0], vec![p1]));

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert_eq!(matches.len(), 2);
    let mut edge_maps = matches
        .iter()
        .map(|m| (m.edge_map()[0].0, m.edge_map()[1].0))
        .collect::<Vec<_>>();
    edge_maps.sort();
    assert_eq!(edge_maps, vec![(0, 1), (1, 0)]);
}

#[test]
fn test_subgraph_isomorphisms_isolated_nodes_duplicate_labels() {
    let mut target: Hypergraph<i32, ()> = Hypergraph::empty();
    target.new_node(1);
    target.new_node(1);
    target.new_node(1);

    let mut pattern: Hypergraph<i32, ()> = Hypergraph::empty();
    pattern.new_node(1);
    pattern.new_node(1);

    let matches = target.find_subgraph_isomorphisms(&pattern);
    assert_eq!(matches.len(), 6);
}
