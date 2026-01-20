use super::hypergraph::{EdgeId, Hypergraph, NodeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubgraphIsomorphism {
    node_map: Vec<NodeId>,
    edge_map: Vec<EdgeId>,
}

impl SubgraphIsomorphism {
    pub fn node_map(&self) -> &[NodeId] {
        &self.node_map
    }

    pub fn edge_map(&self) -> &[EdgeId] {
        &self.edge_map
    }
}

impl<O, A> Hypergraph<O, A> {
    /// Find all subgraph isomorphisms from `pattern` into `self`.
    ///
    /// This uses a VF2-style backtracking search over edges, then assigns any isolated nodes.
    /// The quotient map is ignored; run `quotient` first if you want strict matching.
    pub fn find_subgraph_isomorphisms_by<OP, AP, FN, FE>(
        &self,
        pattern: &Hypergraph<OP, AP>,
        node_eq: FN,
        edge_eq: FE,
    ) -> Vec<SubgraphIsomorphism>
    where
        FN: Fn(&OP, &O) -> bool,
        FE: Fn(&AP, &A) -> bool,
    {
        find_subgraph_isomorphisms_impl(self, pattern, &node_eq, &edge_eq)
    }
}

impl<O: PartialEq, A: PartialEq> Hypergraph<O, A> {
    /// Find all subgraph isomorphisms from `pattern` into `self` by label equality.
    pub fn find_subgraph_isomorphisms(
        &self,
        pattern: &Hypergraph<O, A>,
    ) -> Vec<SubgraphIsomorphism> {
        self.find_subgraph_isomorphisms_by(pattern, |a, b| a == b, |a, b| a == b)
    }
}

fn find_subgraph_isomorphisms_impl<OP, AP, O, A, FN, FE>(
    target: &Hypergraph<O, A>,
    pattern: &Hypergraph<OP, AP>,
    node_eq: &FN,
    edge_eq: &FE,
) -> Vec<SubgraphIsomorphism>
where
    FN: Fn(&OP, &O) -> bool,
    FE: Fn(&AP, &A) -> bool,
{
    use std::cmp::Reverse;

    // Quick cardinality check before doing any work.
    if pattern.nodes.len() > target.nodes.len() || pattern.edges.len() > target.edges.len() {
        return Vec::new();
    }

    // Precompute candidate target edges for each pattern edge.
    // `edge_candidates[p]` is the list of target edge indices that match `p` by label and arity.
    // Rationale: pruning by label/arity early shrinks the search tree dramatically, which is
    // the key idea behind VF2-style matching.
    let mut edge_candidates = Vec::with_capacity(pattern.edges.len());
    for (p_edge_idx, p_label) in pattern.edges.iter().enumerate() {
        let p_adj = &pattern.adjacency[p_edge_idx];
        let mut candidates = Vec::new();
        for (t_edge_idx, t_label) in target.edges.iter().enumerate() {
            if !edge_eq(p_label, t_label) {
                continue;
            }
            let t_adj = &target.adjacency[t_edge_idx];
            if p_adj.sources.len() != t_adj.sources.len()
                || p_adj.targets.len() != t_adj.targets.len()
            {
                continue;
            }
            candidates.push(t_edge_idx);
        }
        if candidates.is_empty() && !pattern.edges.is_empty() {
            return Vec::new();
        }
        edge_candidates.push(candidates);
    }

    // Explore edges with fewer candidates first (and higher arity as a tie-breaker).
    // Rationale: "fail fast" ordering reduces backtracking when constraints are tight.
    let mut edge_order: Vec<usize> = (0..pattern.edges.len()).collect();
    edge_order.sort_by_key(|&edge_idx| {
        let arity =
            pattern.adjacency[edge_idx].sources.len() + pattern.adjacency[edge_idx].targets.len();
        (edge_candidates[edge_idx].len(), Reverse(arity))
    });

    // Track isolated nodes so we can assign them after edge mapping.
    // Rationale: edge constraints are strongest; assigning isolated nodes earlier just
    // multiplies possibilities without adding pruning power.
    let mut node_in_edge = vec![false; pattern.nodes.len()];
    for edge in &pattern.adjacency {
        for node in edge.sources.iter().chain(edge.targets.iter()) {
            node_in_edge[node.0] = true;
        }
    }
    let isolated_nodes: Vec<usize> = node_in_edge
        .iter()
        .enumerate()
        .filter_map(|(idx, used)| if *used { None } else { Some(idx) })
        .collect();

    // Mutable state for the backtracking search.
    // Rationale: we mutate maps/used flags in-place and roll back to avoid repeated allocation.
    let mut matches = Vec::new();
    let mut node_map = vec![None; pattern.nodes.len()];
    let mut edge_map = vec![None; pattern.edges.len()];
    let mut used_target_nodes = vec![false; target.nodes.len()];
    let mut used_target_edges = vec![false; target.edges.len()];

    backtrack_edges(
        target,
        pattern,
        node_eq,
        &edge_order,
        &edge_candidates,
        0,
        &isolated_nodes,
        &mut node_map,
        &mut edge_map,
        &mut used_target_nodes,
        &mut used_target_edges,
        &mut matches,
    );

    matches
}

#[allow(clippy::too_many_arguments)]
fn backtrack_edges<OP, AP, O, A, FN>(
    target: &Hypergraph<O, A>,
    pattern: &Hypergraph<OP, AP>,
    node_eq: &FN,
    edge_order: &[usize],
    edge_candidates: &[Vec<usize>],
    edge_index: usize,
    isolated_nodes: &[usize],
    node_map: &mut Vec<Option<NodeId>>,
    edge_map: &mut Vec<Option<EdgeId>>,
    used_target_nodes: &mut Vec<bool>,
    used_target_edges: &mut Vec<bool>,
    matches: &mut Vec<SubgraphIsomorphism>,
) where
    FN: Fn(&OP, &O) -> bool,
{
    // If all edges are mapped, fill in remaining isolated nodes.
    // Rationale: at this point only label/injectivity constraints remain.
    if edge_index == edge_order.len() {
        backtrack_isolated_nodes(
            target,
            pattern,
            node_eq,
            isolated_nodes,
            0,
            node_map,
            edge_map,
            used_target_nodes,
            matches,
        );
        return;
    }

    let p_edge_idx = edge_order[edge_index];
    let p_adj = &pattern.adjacency[p_edge_idx];

    for &t_edge_idx in &edge_candidates[p_edge_idx] {
        if used_target_edges[t_edge_idx] {
            continue;
        }
        let t_adj = &target.adjacency[t_edge_idx];

        // Optimistically map nodes along this edge; rollback on failure.
        // Rationale: we try to extend the partial mapping and undo if any constraint breaks.
        let mut newly_mapped = Vec::new();
        let mut ok = true;

        for (p_node, t_node) in p_adj.sources.iter().zip(t_adj.sources.iter()) {
            if !try_map_node(
                target,
                pattern,
                node_eq,
                p_node.0,
                t_node.0,
                node_map,
                used_target_nodes,
                &mut newly_mapped,
            ) {
                ok = false;
                break;
            }
        }

        if ok {
            for (p_node, t_node) in p_adj.targets.iter().zip(t_adj.targets.iter()) {
                if !try_map_node(
                    target,
                    pattern,
                    node_eq,
                    p_node.0,
                    t_node.0,
                    node_map,
                    used_target_nodes,
                    &mut newly_mapped,
                ) {
                    ok = false;
                    break;
                }
            }
        }

        if ok {
            used_target_edges[t_edge_idx] = true;
            edge_map[p_edge_idx] = Some(EdgeId(t_edge_idx));

            backtrack_edges(
                target,
                pattern,
                node_eq,
                edge_order,
                edge_candidates,
                edge_index + 1,
                isolated_nodes,
                node_map,
                edge_map,
                used_target_nodes,
                used_target_edges,
                matches,
            );

            edge_map[p_edge_idx] = None;
            used_target_edges[t_edge_idx] = false;
        }

        // Revert any provisional node mappings for this candidate edge.
        // Rationale: keep the search state consistent for the next candidate.
        for p_node_idx in newly_mapped.drain(..) {
            let t_node_idx = node_map[p_node_idx].unwrap().0;
            node_map[p_node_idx] = None;
            used_target_nodes[t_node_idx] = false;
        }
    }
}

fn backtrack_isolated_nodes<OP, AP, O, A, FN>(
    target: &Hypergraph<O, A>,
    pattern: &Hypergraph<OP, AP>,
    node_eq: &FN,
    isolated_nodes: &[usize],
    idx: usize,
    node_map: &mut Vec<Option<NodeId>>,
    edge_map: &mut Vec<Option<EdgeId>>,
    used_target_nodes: &mut Vec<bool>,
    matches: &mut Vec<SubgraphIsomorphism>,
) where
    FN: Fn(&OP, &O) -> bool,
{
    // All isolated nodes assigned; record a complete match.
    // Rationale: both edge and node constraints are satisfied at this point.
    if idx == isolated_nodes.len() {
        let node_map = node_map
            .iter()
            .map(|node| node.expect("pattern nodes must be mapped"))
            .collect();
        let edge_map = edge_map
            .iter()
            .map(|edge| edge.expect("pattern edges must be mapped"))
            .collect();
        matches.push(SubgraphIsomorphism { node_map, edge_map });
        return;
    }

    let p_node_idx = isolated_nodes[idx];
    for t_node_idx in 0..target.nodes.len() {
        if used_target_nodes[t_node_idx] {
            continue;
        }
        if !node_eq(&pattern.nodes[p_node_idx], &target.nodes[t_node_idx]) {
            continue;
        }

        node_map[p_node_idx] = Some(NodeId(t_node_idx));
        used_target_nodes[t_node_idx] = true;

        backtrack_isolated_nodes(
            target,
            pattern,
            node_eq,
            isolated_nodes,
            idx + 1,
            node_map,
            edge_map,
            used_target_nodes,
            matches,
        );

        used_target_nodes[t_node_idx] = false;
        node_map[p_node_idx] = None;
    }
}

fn try_map_node<OP, AP, O, A, FN>(
    target: &Hypergraph<O, A>,
    pattern: &Hypergraph<OP, AP>,
    node_eq: &FN,
    p_node_idx: usize,
    t_node_idx: usize,
    node_map: &mut Vec<Option<NodeId>>,
    used_target_nodes: &mut Vec<bool>,
    newly_mapped: &mut Vec<usize>,
) -> bool
where
    FN: Fn(&OP, &O) -> bool,
{
    // Try to extend the node mapping with (pattern_node -> target_node).
    // If the pattern node is already mapped, this only succeeds when it maps to the same target.
    // Otherwise, it checks injectivity and label compatibility before recording the mapping.
    if let Some(existing) = node_map[p_node_idx] {
        return existing.0 == t_node_idx;
    }
    // injectivity: a target node can only be used once
    if used_target_nodes[t_node_idx] {
        return false;
    }
    // label compatibility: we only allow mapping when the node labels match
    if !node_eq(&pattern.nodes[p_node_idx], &target.nodes[t_node_idx]) {
        return false;
    }

    node_map[p_node_idx] = Some(NodeId(t_node_idx));
    used_target_nodes[t_node_idx] = true;
    newly_mapped.push(p_node_idx);
    true
}
