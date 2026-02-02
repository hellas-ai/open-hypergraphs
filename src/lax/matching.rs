use super::hypergraph::{EdgeId, Hypergraph, NodeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Morphism {
    node_map: Vec<NodeId>,
    edge_map: Vec<EdgeId>,
}

impl Morphism {
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
    /// This uses an edge-first backtracking search specialized to hypergraphs.
    /// The quotient map is ignored; run `quotient` first if you want strict matching.
    pub fn find_subgraph_isomorphisms_by<OP, AP, FN, FE>(
        &self,
        pattern: &Hypergraph<OP, AP>,
        node_eq: FN,
        edge_eq: FE,
    ) -> Vec<Morphism>
    where
        FN: Fn(&OP, &O) -> bool,
        FE: Fn(&AP, &A) -> bool,
    {
        find_subgraph_isomorphisms_impl(self, pattern, &node_eq, &edge_eq)
    }

    /// Find all subgraph homomorphisms from `pattern` into `self`.
    ///
    /// This uses an edge-first backtracking search specialized to hypergraphs,
    /// but does not enforce injectivity (mono) on nodes or edges.
    /// The quotient map is ignored; run `quotient` first if you want strict matching.
    pub fn find_subgraph_homomorphisms_by<OP, AP, FN, FE>(
        &self,
        pattern: &Hypergraph<OP, AP>,
        node_eq: FN,
        edge_eq: FE,
    ) -> Vec<Morphism>
    where
        FN: Fn(&OP, &O) -> bool,
        FE: Fn(&AP, &A) -> bool,
    {
        find_subgraph_homomorphisms_impl(self, pattern, &node_eq, &edge_eq)
    }
}

impl<O: PartialEq, A: PartialEq> Hypergraph<O, A> {
    /// Find all subgraph isomorphisms from `pattern` into `self` by label equality.
    pub fn find_subgraph_isomorphisms(&self, pattern: &Hypergraph<O, A>) -> Vec<Morphism> {
        self.find_subgraph_isomorphisms_by(pattern, |a, b| a == b, |a, b| a == b)
    }

    /// Find all subgraph homomorphisms from `pattern` into `self` by label equality.
    pub fn find_subgraph_homomorphisms(&self, pattern: &Hypergraph<O, A>) -> Vec<Morphism> {
        self.find_subgraph_homomorphisms_by(pattern, |a, b| a == b, |a, b| a == b)
    }
}

fn find_subgraph_homomorphisms_impl<OP, AP, O, A, FN, FE>(
    target: &Hypergraph<O, A>,
    pattern: &Hypergraph<OP, AP>,
    node_eq: &FN,
    edge_eq: &FE,
) -> Vec<Morphism>
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
) -> Vec<Morphism>
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
) -> Vec<Morphism>
where
    FN: Fn(&OP, &O) -> bool,
    FE: Fn(&AP, &A) -> bool,
{
    let options = MatchOptions { injective };
    if !cardinality_feasible(pattern, target, &options) {
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

    // Precompute degrees for pruning in the injective case.
    let (pattern_in, pattern_out) = node_degrees(pattern);
    let (target_in, target_out) = node_degrees(target);

    // Explore edges with fewer candidates first (and higher arity as a tie-breaker).
    // Rationale: "fail fast" ordering reduces backtracking when constraints are tight.
    let mut edge_order: Vec<usize> = (0..pattern.edges.len()).collect();
    edge_order.sort_by_key(|&edge_idx| {
        let arity =
            pattern.adjacency[edge_idx].sources.len() + pattern.adjacency[edge_idx].targets.len();
        (edge_candidates[edge_idx].len(), std::cmp::Reverse(arity))
    });

    // Track isolated nodes so we can assign them after edge mapping.
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

    let mut state = MatchState::new(pattern, target);
    let context = MatchContext::new(
        target,
        pattern,
        node_eq,
        &edge_order,
        &edge_candidates,
        &isolated_nodes,
        &pattern_in,
        &pattern_out,
        &target_in,
        &target_out,
        &options,
    );
    let mut matches = Vec::new();

    backtrack_edges(&context, 0, &mut state, &mut matches);

    matches
}

fn cardinality_feasible<OP, AP, O, A>(
    pattern: &Hypergraph<OP, AP>,
    target: &Hypergraph<O, A>,
    options: &MatchOptions,
) -> bool {
    if !options.injective {
        return true;
    }
    pattern.nodes.len() <= target.nodes.len() && pattern.edges.len() <= target.edges.len()
}

struct MatchOptions {
    injective: bool,
}

struct MatchContext<'a, OP, AP, O, A, FN>
where
    FN: Fn(&OP, &O) -> bool,
{
    target: &'a Hypergraph<O, A>,
    pattern: &'a Hypergraph<OP, AP>,
    node_eq: &'a FN,
    edge_order: &'a [usize],
    edge_candidates: &'a [Vec<usize>],
    isolated_nodes: &'a [usize],
    pattern_in: &'a [usize],
    pattern_out: &'a [usize],
    target_in: &'a [usize],
    target_out: &'a [usize],
    options: &'a MatchOptions,
}

impl<'a, OP, AP, O, A, FN> MatchContext<'a, OP, AP, O, A, FN>
where
    FN: Fn(&OP, &O) -> bool,
{
    fn new(
        target: &'a Hypergraph<O, A>,
        pattern: &'a Hypergraph<OP, AP>,
        node_eq: &'a FN,
        edge_order: &'a [usize],
        edge_candidates: &'a [Vec<usize>],
        isolated_nodes: &'a [usize],
        pattern_in: &'a [usize],
        pattern_out: &'a [usize],
        target_in: &'a [usize],
        target_out: &'a [usize],
        options: &'a MatchOptions,
    ) -> Self {
        Self {
            target,
            pattern,
            node_eq,
            edge_order,
            edge_candidates,
            isolated_nodes,
            pattern_in,
            pattern_out,
            target_in,
            target_out,
            options,
        }
    }
}

struct MatchState {
    node_map: Vec<Option<NodeId>>,
    edge_map: Vec<Option<EdgeId>>,
    used_target_nodes: Vec<bool>,
    used_target_edges: Vec<bool>,
    pattern_mapped_in: Vec<usize>,
    pattern_mapped_out: Vec<usize>,
    target_mapped_in: Vec<usize>,
    target_mapped_out: Vec<usize>,
}

impl MatchState {
    fn new<OP, AP, O, A>(pattern: &Hypergraph<OP, AP>, target: &Hypergraph<O, A>) -> Self {
        Self {
            node_map: vec![None; pattern.nodes.len()],
            edge_map: vec![None; pattern.edges.len()],
            used_target_nodes: vec![false; target.nodes.len()],
            used_target_edges: vec![false; target.edges.len()],
            pattern_mapped_in: vec![0usize; pattern.nodes.len()],
            pattern_mapped_out: vec![0usize; pattern.nodes.len()],
            target_mapped_in: vec![0usize; target.nodes.len()],
            target_mapped_out: vec![0usize; target.nodes.len()],
        }
    }

    fn commit_edge_mapping(
        &mut self,
        p_edge_idx: usize,
        t_edge_idx: usize,
        p_sources: &[NodeId],
        p_targets: &[NodeId],
        t_sources: &[NodeId],
        t_targets: &[NodeId],
        options: &MatchOptions,
    ) {
        // Record the edge mapping and update incremental counters if injective.
        self.edge_map[p_edge_idx] = Some(EdgeId(t_edge_idx));
        if options.injective {
            self.used_target_edges[t_edge_idx] = true;
            add_edge_incidence(
                p_sources,
                p_targets,
                &mut self.pattern_mapped_in,
                &mut self.pattern_mapped_out,
            );
            add_edge_incidence(
                t_sources,
                t_targets,
                &mut self.target_mapped_in,
                &mut self.target_mapped_out,
            );
        }
    }

    fn rollback_edge_mapping(
        &mut self,
        p_edge_idx: usize,
        t_edge_idx: usize,
        p_sources: &[NodeId],
        p_targets: &[NodeId],
        t_sources: &[NodeId],
        t_targets: &[NodeId],
        options: &MatchOptions,
    ) {
        // Undo the edge mapping and counters.
        self.edge_map[p_edge_idx] = None;
        if options.injective {
            self.used_target_edges[t_edge_idx] = false;
            remove_edge_incidence(
                p_sources,
                p_targets,
                &mut self.pattern_mapped_in,
                &mut self.pattern_mapped_out,
            );
            remove_edge_incidence(
                t_sources,
                t_targets,
                &mut self.target_mapped_in,
                &mut self.target_mapped_out,
            );
        }
    }

    fn rollback_new_nodes(&mut self, newly_mapped: Vec<usize>, options: &MatchOptions) {
        // Undo node bindings created while exploring a candidate edge.
        for p_node_idx in newly_mapped {
            let t_node_idx = self.node_map[p_node_idx].unwrap().0;
            self.node_map[p_node_idx] = None;
            if options.injective {
                self.used_target_nodes[t_node_idx] = false;
            }
        }
    }

    fn commit_edge_nodes<OP, AP, O, A, FN>(
        &mut self,
        context: &MatchContext<'_, OP, AP, O, A, FN>,
        p_adj: &super::hypergraph::Hyperedge,
        t_adj: &super::hypergraph::Hyperedge,
    ) -> Option<Vec<usize>>
    where
        FN: Fn(&OP, &O) -> bool,
    {
        let mut newly_mapped = Vec::new();
        for (p_node, t_node) in p_adj.sources.iter().zip(t_adj.sources.iter()) {
            if !try_map_node(context, p_node.0, t_node.0, 0, 1, self, &mut newly_mapped) {
                self.rollback_new_nodes(newly_mapped, context.options);
                return None;
            }
        }
        for (p_node, t_node) in p_adj.targets.iter().zip(t_adj.targets.iter()) {
            if !try_map_node(context, p_node.0, t_node.0, 1, 0, self, &mut newly_mapped) {
                self.rollback_new_nodes(newly_mapped, context.options);
                return None;
            }
        }
        Some(newly_mapped)
    }
}

fn backtrack_edges<OP, AP, O, A, FN>(
    context: &MatchContext<'_, OP, AP, O, A, FN>,
    edge_index: usize,
    state: &mut MatchState,
    matches: &mut Vec<Morphism>,
) where
    FN: Fn(&OP, &O) -> bool,
{
    // If all edges are mapped, fill in remaining isolated nodes.
    if edge_index == context.edge_order.len() {
        backtrack_isolated_nodes(context, 0, state, matches);
        return;
    }

    let p_edge_idx = context.edge_order[edge_index];
    let p_adj = &context.pattern.adjacency[p_edge_idx];

    for &t_edge_idx in &context.edge_candidates[p_edge_idx] {
        if context.options.injective && state.used_target_edges[t_edge_idx] {
            continue;
        }
        let t_adj = &context.target.adjacency[t_edge_idx];

        let Some(newly_mapped) = state.commit_edge_nodes(context, p_adj, t_adj) else {
            continue;
        };

        state.commit_edge_mapping(
            p_edge_idx,
            t_edge_idx,
            &p_adj.sources,
            &p_adj.targets,
            &t_adj.sources,
            &t_adj.targets,
            context.options,
        );

        backtrack_edges(context, edge_index + 1, state, matches);

        state.rollback_edge_mapping(
            p_edge_idx,
            t_edge_idx,
            &p_adj.sources,
            &p_adj.targets,
            &t_adj.sources,
            &t_adj.targets,
            context.options,
        );

        // Roll back any provisional node bindings from this edge attempt.
        state.rollback_new_nodes(newly_mapped, context.options);
    }
}

fn backtrack_isolated_nodes<OP, AP, O, A, FN>(
    context: &MatchContext<'_, OP, AP, O, A, FN>,
    idx: usize,
    state: &mut MatchState,
    matches: &mut Vec<Morphism>,
) where
    FN: Fn(&OP, &O) -> bool,
{
    if idx == context.isolated_nodes.len() {
        let node_map = state
            .node_map
            .iter()
            .map(|node| node.expect("pattern nodes must be mapped"))
            .collect();
        let edge_map = state
            .edge_map
            .iter()
            .map(|edge| edge.expect("pattern edges must be mapped"))
            .collect();
        matches.push(Morphism { node_map, edge_map });
        return;
    }

    let p_node_idx = context.isolated_nodes[idx];
    for t_node_idx in 0..context.target.nodes.len() {
        if context.options.injective && state.used_target_nodes[t_node_idx] {
            continue;
        }
        if !degree_feasible(
            p_node_idx,
            t_node_idx,
            0,
            0,
            context.pattern_in,
            context.pattern_out,
            context.target_in,
            context.target_out,
            &state.pattern_mapped_in,
            &state.pattern_mapped_out,
            &state.target_mapped_in,
            &state.target_mapped_out,
            context.options.injective,
        ) {
            continue;
        }
        if !(context.node_eq)(
            &context.pattern.nodes[p_node_idx],
            &context.target.nodes[t_node_idx],
        ) {
            continue;
        }

        state.node_map[p_node_idx] = Some(NodeId(t_node_idx));
        if context.options.injective {
            state.used_target_nodes[t_node_idx] = true;
        }

        backtrack_isolated_nodes(context, idx + 1, state, matches);

        if context.options.injective {
            state.used_target_nodes[t_node_idx] = false;
        }
        state.node_map[p_node_idx] = None;
    }
}

#[allow(clippy::too_many_arguments)]
fn try_map_node<OP, AP, O, A, FN>(
    context: &MatchContext<'_, OP, AP, O, A, FN>,
    p_node_idx: usize,
    t_node_idx: usize,
    add_in: usize,
    add_out: usize,
    state: &mut MatchState,
    newly_mapped: &mut Vec<usize>,
) -> bool
where
    FN: Fn(&OP, &O) -> bool,
{
    if let Some(existing) = state.node_map[p_node_idx] {
        if existing.0 != t_node_idx {
            return false;
        }
        if context.options.injective {
            return degree_feasible(
                p_node_idx,
                t_node_idx,
                add_in,
                add_out,
                context.pattern_in,
                context.pattern_out,
                context.target_in,
                context.target_out,
                &state.pattern_mapped_in,
                &state.pattern_mapped_out,
                &state.target_mapped_in,
                &state.target_mapped_out,
                context.options.injective,
            );
        }
        return true;
    }
    if context.options.injective && state.used_target_nodes[t_node_idx] {
        return false;
    }
    if !(context.node_eq)(
        &context.pattern.nodes[p_node_idx],
        &context.target.nodes[t_node_idx],
    ) {
        return false;
    }
    if !degree_feasible(
        p_node_idx,
        t_node_idx,
        add_in,
        add_out,
        context.pattern_in,
        context.pattern_out,
        context.target_in,
        context.target_out,
        &state.pattern_mapped_in,
        &state.pattern_mapped_out,
        &state.target_mapped_in,
        &state.target_mapped_out,
        context.options.injective,
    ) {
        return false;
    }

    state.node_map[p_node_idx] = Some(NodeId(t_node_idx));
    if context.options.injective {
        state.used_target_nodes[t_node_idx] = true;
    }
    newly_mapped.push(p_node_idx);
    true
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

fn add_edge_incidence(
    sources: &[NodeId],
    targets: &[NodeId],
    mapped_in: &mut [usize],
    mapped_out: &mut [usize],
) {
    for node in sources {
        mapped_out[node.0] += 1;
    }
    for node in targets {
        mapped_in[node.0] += 1;
    }
}

fn remove_edge_incidence(
    sources: &[NodeId],
    targets: &[NodeId],
    mapped_in: &mut [usize],
    mapped_out: &mut [usize],
) {
    for node in sources {
        mapped_out[node.0] -= 1;
    }
    for node in targets {
        mapped_in[node.0] -= 1;
    }
}

fn degree_feasible(
    p_node_idx: usize,
    t_node_idx: usize,
    add_in: usize,
    add_out: usize,
    pattern_in: &[usize],
    pattern_out: &[usize],
    target_in: &[usize],
    target_out: &[usize],
    pattern_mapped_in: &[usize],
    pattern_mapped_out: &[usize],
    target_mapped_in: &[usize],
    target_mapped_out: &[usize],
    injective: bool,
) -> bool {
    if !injective {
        return true;
    }
    // Basic degree bound: a pattern node cannot map to a target node with fewer in/out edges.
    if pattern_in[p_node_idx] > target_in[t_node_idx]
        || pattern_out[p_node_idx] > target_out[t_node_idx]
    {
        return false;
    }

    // Remaining incident edges on the pattern node after this tentative assignment.
    let pattern_remaining_in =
        pattern_in[p_node_idx].saturating_sub(pattern_mapped_in[p_node_idx] + add_in);
    let pattern_remaining_out =
        pattern_out[p_node_idx].saturating_sub(pattern_mapped_out[p_node_idx] + add_out);
    // Remaining capacity on the target node to host those edges.
    let target_remaining_in =
        target_in[t_node_idx].saturating_sub(target_mapped_in[t_node_idx] + add_in);
    let target_remaining_out =
        target_out[t_node_idx].saturating_sub(target_mapped_out[t_node_idx] + add_out);

    // Feasible if the target has enough unused incident capacity to fit the pattern.
    pattern_remaining_in <= target_remaining_in && pattern_remaining_out <= target_remaining_out
}
