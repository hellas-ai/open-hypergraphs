use super::csp::{Csp, VarId};
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
    /// This encodes the matching problem as a small CSP and enumerates solutions.
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

    /// Find all subgraph homomorphisms from `pattern` into `self`.
    ///
    /// This encodes the matching problem as a small CSP and enumerates solutions,
    /// but does not enforce injectivity (mono) on nodes or edges.
    /// The quotient map is ignored; run `quotient` first if you want strict matching.
    pub fn find_subgraph_homomorphisms_by<OP, AP, FN, FE>(
        &self,
        pattern: &Hypergraph<OP, AP>,
        node_eq: FN,
        edge_eq: FE,
    ) -> Vec<SubgraphIsomorphism>
    where
        FN: Fn(&OP, &O) -> bool,
        FE: Fn(&AP, &A) -> bool,
    {
        find_subgraph_homomorphisms_impl(self, pattern, &node_eq, &edge_eq)
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

    /// Find all subgraph homomorphisms from `pattern` into `self` by label equality.
    pub fn find_subgraph_homomorphisms(
        &self,
        pattern: &Hypergraph<O, A>,
    ) -> Vec<SubgraphIsomorphism> {
        self.find_subgraph_homomorphisms_by(pattern, |a, b| a == b, |a, b| a == b)
    }
}

fn find_subgraph_homomorphisms_impl<OP, AP, O, A, FN, FE>(
    target: &Hypergraph<O, A>,
    pattern: &Hypergraph<OP, AP>,
    node_eq: &FN,
    edge_eq: &FE,
) -> Vec<SubgraphIsomorphism>
where
    FN: Fn(&OP, &O) -> bool,
    FE: Fn(&AP, &A) -> bool,
{
    find_subgraph_matches_impl(target, pattern, node_eq, edge_eq, false)
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
    find_subgraph_matches_impl(target, pattern, node_eq, edge_eq, true)
}

fn find_subgraph_matches_impl<OP, AP, O, A, FN, FE>(
    target: &Hypergraph<O, A>,
    pattern: &Hypergraph<OP, AP>,
    node_eq: &FN,
    edge_eq: &FE,
    injective: bool,
) -> Vec<SubgraphIsomorphism>
where
    FN: Fn(&OP, &O) -> bool,
    FE: Fn(&AP, &A) -> bool,
{
    // Quick cardinality check before doing any work.
    if pattern.nodes.len() > target.nodes.len() || pattern.edges.len() > target.edges.len() {
        return Vec::new();
    }

    // Precompute candidate target edges for each pattern edge.
    // `edge_candidates[p]` is the list of target edge indices that match `p` by label and arity.
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

    // Feasibility checks used here:
    // - Degree compatibility: in/out degree of each pattern node must not exceed the target's.
    let (pattern_in, pattern_out) = node_degrees(pattern);
    let (target_in, target_out) = node_degrees(target);

    // Build a small CSP: variables are pattern nodes and pattern edges.
    // Constraints encode label/arity matching, incidence, and injectivity.
    let mut csp = Csp::new();

    let mut node_vars = Vec::with_capacity(pattern.nodes.len());
    for (p_idx, p_label) in pattern.nodes.iter().enumerate() {
        let mut domain = Vec::new();
        for (t_idx, t_label) in target.nodes.iter().enumerate() {
            // Allow only label-compatible target nodes with sufficient degree.
            if !node_eq(p_label, t_label) {
                continue;
            }
            if !degree_compatible(
                p_idx,
                t_idx,
                &pattern_in,
                &pattern_out,
                &target_in,
                &target_out,
            ) {
                continue;
            }
            domain.push(t_idx);
        }
        // Empty domain means no match for this pattern node.
        if domain.is_empty() {
            return Vec::new();
        }
        // Create a variable for this pattern node.
        let v = csp.add_var(domain);
        node_vars.push(v);
    }

    let mut edge_vars = Vec::with_capacity(pattern.edges.len());
    for candidates in &edge_candidates {
        // Edge variables are restricted to target edges with compatible label + arity.
        if candidates.is_empty() && !pattern.edges.is_empty() {
            return Vec::new();
        }
        let e = csp.add_var(candidates.clone());
        edge_vars.push(e);
    }

    // Injective node/edge mapping for isomorphisms.
    if injective {
        csp.add_all_different(node_vars.clone());
        csp.add_all_different(edge_vars.clone());
    }

    // Edge incidence constraints: edge variables and the nodes they touch must align.
    for (p_edge_idx, p_adj) in pattern.adjacency.iter().enumerate() {
        let edge_var = edge_vars[p_edge_idx];
        let source_vars: Vec<VarId> = p_adj.sources.iter().map(|n| node_vars[n.0]).collect();
        let target_vars: Vec<VarId> = p_adj.targets.iter().map(|n| node_vars[n.0]).collect();
        let candidates = &edge_candidates[p_edge_idx];
        let target_adjacency = &target.adjacency;

        // Constraint spans one edge var plus all of its incident node vars.
        let mut vars = Vec::with_capacity(1 + source_vars.len() + target_vars.len());
        vars.push(edge_var);
        vars.extend(source_vars.iter().copied());
        vars.extend(target_vars.iter().copied());

        // Feasible if the chosen target edge (or some candidate, if unassigned)
        // matches all assigned incident node vars positionally.
        csp.add_predicate(vars, move |assignment| {
            let edge_value = assignment[edge_var];

            let compatible = |t_edge_idx: usize, assignment: &[Option<usize>]| -> bool {
                let t_adj = &target_adjacency[t_edge_idx];
                for (var_id, t_node) in source_vars.iter().zip(t_adj.sources.iter()) {
                    if let Some(value) = assignment[*var_id] {
                        if value != t_node.0 {
                            return false;
                        }
                    }
                }
                for (var_id, t_node) in target_vars.iter().zip(t_adj.targets.iter()) {
                    if let Some(value) = assignment[*var_id] {
                        if value != t_node.0 {
                            return false;
                        }
                    }
                }
                true
            };

            match edge_value {
                Some(t_edge_idx) => compatible(t_edge_idx, assignment),
                None => candidates
                    .iter()
                    .copied()
                    .any(|t_edge_idx| compatible(t_edge_idx, assignment)),
            }
        });
    }

    let solutions = csp.solve_all();
    solutions
        .into_iter()
        .map(|solution| {
            // Convert a CSP assignment into explicit node/edge maps.
            let node_map = node_vars.iter().map(|&var| NodeId(solution[var])).collect();
            let edge_map = edge_vars.iter().map(|&var| EdgeId(solution[var])).collect();
            SubgraphIsomorphism { node_map, edge_map }
        })
        .collect()
}

fn node_degrees<O, A>(graph: &Hypergraph<O, A>) -> (Vec<usize>, Vec<usize>) {
    let mut in_deg = vec![0usize; graph.nodes.len()];
    let mut out_deg = vec![0usize; graph.nodes.len()];
    for edge in &graph.adjacency {
        for node in &edge.sources {
            out_deg[node.0] += 1;
        }
        for node in &edge.targets {
            in_deg[node.0] += 1;
        }
    }
    (in_deg, out_deg)
}

fn degree_compatible(
    p_node_idx: usize,
    t_node_idx: usize,
    pattern_in: &[usize],
    pattern_out: &[usize],
    target_in: &[usize],
    target_out: &[usize],
) -> bool {
    if pattern_in[p_node_idx] > target_in[t_node_idx]
        || pattern_out[p_node_idx] > target_out[t_node_idx]
    {
        return false;
    }
    true
}
