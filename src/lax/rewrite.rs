use crate::lax::{Arrow, Hyperedge, Hypergraph, LaxSpan, NodeEdgeMap, NodeId};

/// Rewrite a lax hypergraph using a rule span and candidate map.
pub fn rewrite<O: Clone + PartialEq, A: Clone + PartialEq>(
    g: &Hypergraph<O, A>,
    rule: &LaxSpan<O, A>,
    candidate: &NodeEdgeMap,
) -> Option<Hypergraph<O, A>> {
    validate_candidate_map(rule, g, candidate);
    if !identification_condition(rule, candidate) || !dangling_condition(rule, candidate, g) {
        return None;
    }
    let exploded = exploded_context(g, rule, candidate);
    Some(exploded)
}

fn exploded_context<O: Clone, A: Clone>(
    g: &Hypergraph<O, A>,
    rule: &LaxSpan<O, A>,
    candidate: &NodeEdgeMap,
) -> Hypergraph<O, A> {
    let mut in_image_nodes = vec![false; g.nodes.len()];
    for i in 0..candidate.nodes.source() {
        let idx = candidate.nodes.table[i];
        in_image_nodes[idx] = true;
    }

    let mut in_image_edges = vec![false; g.edges.len()];
    for i in 0..candidate.edges.source() {
        let idx = candidate.edges.table[i];
        in_image_edges[idx] = true;
    }

    let mut h = Hypergraph::empty();
    let mut node_map: Vec<Option<usize>> = vec![None; g.nodes.len()];
    for (idx, label) in g.nodes.iter().enumerate() {
        if in_image_nodes[idx] {
            continue;
        }
        let new_id = h.new_node(label.clone());
        node_map[idx] = Some(new_id.0);
    }

    for (edge_id, edge) in g.adjacency.iter().enumerate() {
        if in_image_edges[edge_id] {
            continue;
        }

        let mut sources = Vec::with_capacity(edge.sources.len());
        for node in &edge.sources {
            let new_id = match node_map[node.0] {
                Some(existing) => NodeId(existing),
                None => h.new_node(g.nodes[node.0].clone()),
            };
            sources.push(new_id);
        }

        let mut targets = Vec::with_capacity(edge.targets.len());
        for node in &edge.targets {
            let new_id = match node_map[node.0] {
                Some(existing) => NodeId(existing),
                None => h.new_node(g.nodes[node.0].clone()),
            };
            targets.push(new_id);
        }

        h.new_edge(g.edges[edge_id].clone(), Hyperedge { sources, targets });
    }

    h.coproduct(&rule.apex)
}

fn validate_candidate_map<O, A>(
    rule: &LaxSpan<O, A>,
    g: &Hypergraph<O, A>,
    candidate: &NodeEdgeMap,
) {
    if candidate.nodes.source() != rule.left.nodes.len() {
        panic!(
            "candidate map node source size mismatch: got {}, expected {}",
            candidate.nodes.source(),
            rule.left.nodes.len()
        );
    }
    if candidate.nodes.target() != g.nodes.len() {
        panic!(
            "candidate map node target size mismatch: got {}, expected {}",
            candidate.nodes.target(),
            g.nodes.len()
        );
    }
    if candidate.edges.source() != rule.left.edges.len() {
        panic!(
            "candidate map edge source size mismatch: got {}, expected {}",
            candidate.edges.source(),
            rule.left.edges.len()
        );
    }
    if candidate.edges.target() != g.edges.len() {
        panic!(
            "candidate map edge target size mismatch: got {}, expected {}",
            candidate.edges.target(),
            g.edges.len()
        );
    }
}

fn identification_condition<O, A>(rule: &LaxSpan<O, A>, candidate: &NodeEdgeMap) -> bool {
    let mut in_image = vec![false; rule.left.nodes.len()];
    for i in 0..rule.left_map.nodes.source() {
        let idx = rule.left_map.nodes.table[i];
        in_image[idx] = true;
    }

    let mut seen = vec![None; candidate.nodes.target()];
    for i in 0..rule.left.nodes.len() {
        if in_image[i] {
            continue;
        }
        let img = candidate.nodes.table[i];
        if let Some(existing) = seen[img] {
            if existing != i {
                return false;
            }
        } else {
            seen[img] = Some(i);
        }
    }

    true
}

fn dangling_condition<O, A>(
    rule: &LaxSpan<O, A>,
    candidate: &NodeEdgeMap,
    g: &Hypergraph<O, A>,
) -> bool {
    let mut in_l_image = vec![false; rule.left.nodes.len()];
    for i in 0..rule.left_map.nodes.source() {
        let idx = rule.left_map.nodes.table[i];
        in_l_image[idx] = true;
    }

    let mut forbidden_nodes = vec![false; g.nodes.len()];
    for i in 0..rule.left.nodes.len() {
        if in_l_image[i] {
            continue;
        }
        let img = candidate.nodes.table[i];
        forbidden_nodes[img] = true;
    }

    let mut edge_in_image = vec![false; g.edges.len()];
    for i in 0..candidate.edges.source() {
        let idx = candidate.edges.table[i];
        edge_in_image[idx] = true;
    }

    for (edge_id, edge) in g.adjacency.iter().enumerate() {
        if edge_in_image[edge_id] {
            continue;
        }
        let touches_forbidden = edge
            .sources
            .iter()
            .chain(edge.targets.iter())
            .any(|n| forbidden_nodes[n.0]);
        if touches_forbidden {
            return false;
        }
    }

    true
}
