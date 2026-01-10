use crate::lax::{Arrow, Hypergraph, LaxSpan, NodeEdgeMap};

/// Rewrite a lax hypergraph using a rule span and candidate map.
pub fn rewrite<O: Clone + PartialEq, A: Clone + PartialEq>(
    g: &Hypergraph<O, A>,
    rule: &LaxSpan<O, A>,
    candidate: &NodeEdgeMap,
) -> Option<Hypergraph<O, A>> {
    let rule = rule.clone().validate();

    validate_candidate_map(&rule.left, g, candidate);
    if !identification_condition(&rule, candidate) {
        return None;
    }
    if !dangling_condition(&rule, candidate, g) {
        return None;
    }

    Some(g.clone())
}

fn validate_candidate_map<O, A>(
    left: &Hypergraph<O, A>,
    g: &Hypergraph<O, A>,
    candidate: &NodeEdgeMap,
) {
    if candidate.nodes.source() != left.nodes.len() {
        panic!(
            "candidate map node source size mismatch: got {}, expected {}",
            candidate.nodes.source(),
            left.nodes.len()
        );
    }
    if candidate.nodes.target() != g.nodes.len() {
        panic!(
            "candidate map node target size mismatch: got {}, expected {}",
            candidate.nodes.target(),
            g.nodes.len()
        );
    }
    if candidate.edges.source() != left.edges.len() {
        panic!(
            "candidate map edge source size mismatch: got {}, expected {}",
            candidate.edges.source(),
            left.edges.len()
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
