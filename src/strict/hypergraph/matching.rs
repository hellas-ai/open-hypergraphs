use crate::array::{Array, ArrayKind, NaturalArray};
use crate::finite_function::FiniteFunction;
use crate::strict::hypergraph::arrow::{
    is_convex_subgraph_morphism, validate_hypergraph_morphism, InvalidHypergraphArrow,
};
use crate::strict::hypergraph::Hypergraph;

use core::convert::TryFrom;

type EdgeId = usize;
type NodeId = usize;

#[derive(Clone, Debug)]
pub struct MatchOptions {
    pub mono: bool,
    pub require_convex: bool,
    pub stop_after_first: bool,
}

impl Default for MatchOptions {
    fn default() -> Self {
        Self {
            mono: true,
            require_convex: false,
            stop_after_first: false,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct HypergraphMatch<K: ArrayKind> {
    pub w: FiniteFunction<K>,
    pub x: FiniteFunction<K>,
}

#[derive(Clone)]
struct SearchState {
    wire_candidates: CandidateMap,
    op_candidates: CandidateMap,
}

#[derive(Clone)]
struct OrderedIncidence {
    by_edge: Vec<Vec<NodeId>>,
}

impl OrderedIncidence {
    fn edge(&self, edge: EdgeId) -> &[NodeId] {
        &self.by_edge[edge]
    }

    fn edge_count(&self) -> EdgeId {
        self.by_edge.len()
    }
}

#[derive(Clone)]
struct CandidateMap {
    by_pattern: Vec<Vec<bool>>,
}

impl CandidateMap {
    fn new(by_pattern: Vec<Vec<bool>>) -> Self {
        Self { by_pattern }
    }

    fn row(&self, pattern_id: usize) -> &[bool] {
        &self.by_pattern[pattern_id]
    }

    fn row_mut(&mut self, pattern_id: usize) -> &mut [bool] {
        &mut self.by_pattern[pattern_id]
    }

    fn pattern_count(&self) -> usize {
        self.by_pattern.len()
    }

    fn host_count(&self, pattern_id: usize) -> usize {
        self.by_pattern[pattern_id].len()
    }

    fn allows(&self, pattern_id: usize, host_id: usize) -> bool {
        self.by_pattern[pattern_id][host_id]
    }

    fn remove(&mut self, pattern_id: usize, host_id: usize) {
        self.by_pattern[pattern_id][host_id] = false;
    }

    fn retain_only(&mut self, pattern_id: usize, host_id: usize) {
        let row = self.row_mut(pattern_id);
        for allowed in row.iter_mut() {
            *allowed = false;
        }
        row[host_id] = true;
    }

    fn has_empty_row(&self) -> bool {
        self.by_pattern
            .iter()
            .any(|row| row.iter().all(|allowed| !allowed))
    }

    fn singleton_assignment_table<K: ArrayKind>(&self) -> Option<Vec<K::I>>
    where
        K::I: TryFrom<usize>,
    {
        let mut out = Vec::with_capacity(self.by_pattern.len());
        for row in &self.by_pattern {
            let mut candidate = None;
            for (ix, allowed) in row.iter().enumerate() {
                if *allowed {
                    if candidate.is_some() {
                        return None;
                    }
                    candidate = Some(ix);
                }
            }
            let ix = candidate?;
            out.push(K::I::try_from(ix).ok()?);
        }
        Some(out)
    }

    fn best_nontrivial_row(&self) -> Option<(usize, Vec<usize>)> {
        let mut best: Option<(usize, Vec<usize>)> = None;
        for (pattern_id, row) in self.by_pattern.iter().enumerate() {
            let candidates = allowed_indices(row);
            if candidates.len() <= 1 {
                continue;
            }
            let replace = best
                .as_ref()
                .is_none_or(|(_, current)| candidates.len() < current.len());
            if replace {
                best = Some((pattern_id, candidates));
            }
        }
        best
    }

    fn enforce_injective_singletons(&mut self) -> bool {
        let singleton_columns: Vec<_> = self
            .by_pattern
            .iter()
            .filter_map(|row| {
                let allowed = allowed_indices(row);
                if allowed.len() == 1 {
                    Some(allowed[0])
                } else {
                    None
                }
            })
            .collect();

        let mut changed = false;
        for row in &mut self.by_pattern {
            let allowed = allowed_indices(row);
            if allowed.len() == 1 {
                continue;
            }

            for &column in &singleton_columns {
                if row[column] {
                    row[column] = false;
                    changed = true;
                }
            }
        }

        changed
    }
}

pub fn find_subgraph_matches<K: ArrayKind, O, A>(
    pattern: &Hypergraph<K, O, A>,
    host: &Hypergraph<K, O, A>,
    options: &MatchOptions,
) -> Vec<HypergraphMatch<K>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
    O: PartialEq,
    A: PartialEq,
    K::I: Into<usize> + TryFrom<usize>,
    for<'a> K::Slice<'a, K::I>: From<&'a [K::I]>,
{
    let pattern_wire_count: usize = pattern.w.len().into();
    let pattern_op_count: usize = pattern.x.len().into();
    let host_wire_count: usize = host.w.len().into();
    let host_op_count: usize = host.x.len().into();

    if (options.mono && pattern_wire_count > host_wire_count) || pattern_op_count > host_op_count {
        return Vec::new();
    }

    let pattern_sources = incidence_lists(&pattern.s);
    let pattern_targets = incidence_lists(&pattern.t);
    let host_sources = incidence_lists(&host.s);
    let host_targets = incidence_lists(&host.t);

    let initial = SearchState {
        wire_candidates: CandidateMap::new(initial_wire_candidates(pattern, host, options)),
        op_candidates: CandidateMap::new(initial_op_candidates(
            pattern,
            host,
            &pattern_sources,
            &pattern_targets,
            &host_sources,
            &host_targets,
        )),
    };

    let mut matches = Vec::new();
    search(
        pattern,
        host,
        &pattern_sources,
        &pattern_targets,
        &host_sources,
        &host_targets,
        options,
        initial,
        &mut matches,
    );
    matches
}

// This implements an Ullmann-style subgraph search specialized to strict
// directed hypergraphs with ordered source/target ports:
// 1. Build initial candidate maps for wires and operations from cheap local tests.
// 2. Refine those candidates to a fixpoint using incidence consistency between
//    wire candidates and operation candidates.
// 3. Branch on the smallest remaining non-singleton candidate row and recurse.
// 4. Validate each singleton assignment with the existing hypergraph morphism
//    predicates, optionally requiring convexity.
fn search<K: ArrayKind, O, A>(
    pattern: &Hypergraph<K, O, A>,
    host: &Hypergraph<K, O, A>,
    pattern_sources: &OrderedIncidence,
    pattern_targets: &OrderedIncidence,
    host_sources: &OrderedIncidence,
    host_targets: &OrderedIncidence,
    options: &MatchOptions,
    mut state: SearchState,
    matches: &mut Vec<HypergraphMatch<K>>,
) where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
    K::I: Into<usize> + TryFrom<usize>,
    for<'a> K::Slice<'a, K::I>: From<&'a [K::I]>,
{
    if !refine_domains(
        pattern_sources,
        pattern_targets,
        host_sources,
        host_targets,
        options,
        &mut state,
    ) {
        return;
    }

    if let Some(m) = build_match(pattern, host, options, &state) {
        matches.push(m);
        return;
    }

    let choice = pick_branch_row(&state);
    let Some((branch_on_ops, row, candidates)) = choice else {
        return;
    };

    for candidate in candidates {
        if options.stop_after_first && !matches.is_empty() {
            return;
        }

        let mut next = state.clone();
        let candidates = if branch_on_ops {
            &mut next.op_candidates
        } else {
            &mut next.wire_candidates
        };
        candidates.retain_only(row, candidate);

        search(
            pattern,
            host,
            pattern_sources,
            pattern_targets,
            host_sources,
            host_targets,
            options,
            next,
            matches,
        );
    }
}

fn build_match<K: ArrayKind, O, A>(
    pattern: &Hypergraph<K, O, A>,
    host: &Hypergraph<K, O, A>,
    options: &MatchOptions,
    state: &SearchState,
) -> Option<HypergraphMatch<K>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
    K::I: Into<usize> + TryFrom<usize>,
    for<'a> K::Slice<'a, K::I>: From<&'a [K::I]>,
{
    let w_table = state.wire_candidates.singleton_assignment_table::<K>()?;
    let x_table = state.op_candidates.singleton_assignment_table::<K>()?;

    let w = FiniteFunction::new(
        K::Index::from_slice(K::Slice::from(w_table.as_slice())),
        host.w.len(),
    )?;
    let x = FiniteFunction::new(
        K::Index::from_slice(K::Slice::from(x_table.as_slice())),
        host.x.len(),
    )?;

    match validate_hypergraph_morphism(pattern, host, &w, &x) {
        Ok(()) => {}
        Err(
            InvalidHypergraphArrow::TypeMismatchW
            | InvalidHypergraphArrow::TypeMismatchX
            | InvalidHypergraphArrow::NotNaturalW
            | InvalidHypergraphArrow::NotNaturalX
            | InvalidHypergraphArrow::NotNaturalS
            | InvalidHypergraphArrow::NotNaturalT,
        ) => return None,
    }

    if !x.is_injective() || (options.mono && !w.is_injective()) {
        return None;
    }

    if options.require_convex && !is_convex_subgraph_morphism(pattern, host, &w, &x) {
        return None;
    }

    Some(HypergraphMatch { w, x })
}

fn pick_branch_row(state: &SearchState) -> Option<(bool, usize, Vec<usize>)> {
    let op_choice = state.op_candidates.best_nontrivial_row();
    let wire_choice = state.wire_candidates.best_nontrivial_row();

    match (op_choice, wire_choice) {
        (Some((op_row, op_candidates)), Some((wire_row, wire_candidates))) => {
            if op_candidates.len() <= wire_candidates.len() {
                Some((true, op_row, op_candidates))
            } else {
                Some((false, wire_row, wire_candidates))
            }
        }
        (Some((op_row, op_candidates)), None) => Some((true, op_row, op_candidates)),
        (None, Some((wire_row, wire_candidates))) => Some((false, wire_row, wire_candidates)),
        (None, None) => None,
    }
}

fn refine_domains(
    pattern_sources: &OrderedIncidence,
    pattern_targets: &OrderedIncidence,
    host_sources: &OrderedIncidence,
    host_targets: &OrderedIncidence,
    options: &MatchOptions,
    state: &mut SearchState,
) -> bool {
    loop {
        // Any empty candidate row means the partial match is inconsistent:
        // some pattern wire/op has no possible image left in the host.
        if state.wire_candidates.has_empty_row() || state.op_candidates.has_empty_row() {
            return false;
        }

        let mut changed = false;
        // Edge matches remain injective even when wire matches are allowed to
        // fold, so singleton edge columns are always removed elsewhere.
        changed |= state.op_candidates.enforce_injective_singletons();
        if options.mono {
            // When wire matches are monic, singleton wire columns can also be
            // removed from every other non-singleton row.
            changed |= state.wire_candidates.enforce_injective_singletons();
        }

        if state.wire_candidates.has_empty_row() || state.op_candidates.has_empty_row() {
            return false;
        }

        // Alternate between edge-level and wire-level consistency checks until
        // no candidate set changes. This is the Ullmann-style refinement step.
        changed |= prune_op_candidates(
            pattern_sources,
            pattern_targets,
            host_sources,
            host_targets,
            &state.wire_candidates,
            &mut state.op_candidates,
        );
        changed |= prune_wire_candidates(
            pattern_sources,
            pattern_targets,
            host_sources,
            host_targets,
            &state.op_candidates,
            &mut state.wire_candidates,
        );

        if state.wire_candidates.has_empty_row() || state.op_candidates.has_empty_row() {
            return false;
        }

        if !changed {
            return true;
        }
    }
}

fn prune_op_candidates(
    pattern_sources: &OrderedIncidence,
    pattern_targets: &OrderedIncidence,
    host_sources: &OrderedIncidence,
    host_targets: &OrderedIncidence,
    wire_candidates: &CandidateMap,
    op_candidates: &mut CandidateMap,
) -> bool {
    let mut changed = false;

    for pattern_op in 0..op_candidates.pattern_count() {
        for host_op in 0..op_candidates.host_count(pattern_op) {
            if !op_candidates.allows(pattern_op, host_op) {
                continue;
            }

            // A candidate edge match survives only if each source/target port
            // of the pattern edge can still land on the corresponding port of
            // the host edge via the current wire candidate map.
            let src_ok = pattern_sources
                .edge(pattern_op)
                .iter()
                .zip(host_sources.edge(host_op).iter())
                .all(|(&pattern_wire, &host_wire)| wire_candidates.allows(pattern_wire, host_wire));
            let tgt_ok = pattern_targets
                .edge(pattern_op)
                .iter()
                .zip(host_targets.edge(host_op).iter())
                .all(|(&pattern_wire, &host_wire)| wire_candidates.allows(pattern_wire, host_wire));

            if !(src_ok && tgt_ok) {
                op_candidates.remove(pattern_op, host_op);
                changed = true;
            }
        }
    }

    changed
}

fn prune_wire_candidates(
    pattern_sources: &OrderedIncidence,
    pattern_targets: &OrderedIncidence,
    host_sources: &OrderedIncidence,
    host_targets: &OrderedIncidence,
    op_candidates: &CandidateMap,
    wire_candidates: &mut CandidateMap,
) -> bool {
    let mut changed = false;

    for pattern_wire in 0..wire_candidates.pattern_count() {
        for host_wire in 0..wire_candidates.host_count(pattern_wire) {
            if !wire_candidates.allows(pattern_wire, host_wire) {
                continue;
            }

            // A candidate wire match survives only if every occurrence of the
            // pattern wire as an input/output port is supported by at least one
            // still-allowed candidate edge match at that same port position.
            let src_ok = (0..pattern_sources.edge_count()).all(|pattern_op| {
                pattern_sources
                    .edge(pattern_op)
                    .iter()
                    .enumerate()
                    .filter(|(_, w)| **w == pattern_wire)
                    .all(|(position, _)| {
                        op_candidates.row(pattern_op).iter().enumerate().any(
                            |(host_op, allowed)| {
                                *allowed && host_sources.edge(host_op)[position] == host_wire
                            },
                        )
                    })
            });

            let tgt_ok = (0..pattern_targets.edge_count()).all(|pattern_op| {
                pattern_targets
                    .edge(pattern_op)
                    .iter()
                    .enumerate()
                    .filter(|(_, w)| **w == pattern_wire)
                    .all(|(position, _)| {
                        op_candidates.row(pattern_op).iter().enumerate().any(
                            |(host_op, allowed)| {
                                *allowed && host_targets.edge(host_op)[position] == host_wire
                            },
                        )
                    })
            });

            if !(src_ok && tgt_ok) {
                wire_candidates.remove(pattern_wire, host_wire);
                changed = true;
            }
        }
    }

    changed
}

fn allowed_indices(domain: &[bool]) -> Vec<usize> {
    domain
        .iter()
        .enumerate()
        .filter_map(|(ix, allowed)| allowed.then_some(ix))
        .collect()
}

fn incidence_lists<K: ArrayKind>(
    incidence: &crate::indexed_coproduct::IndexedCoproduct<K, FiniteFunction<K>>,
) -> OrderedIncidence
where
    K::Type<K::I>: NaturalArray<K>,
    K::I: Into<usize> + TryFrom<usize>,
{
    OrderedIncidence {
        by_edge: incidence
            .clone()
            .into_iter()
            .map(|f| {
                (0..f.table.len().into())
                    .map(|ix| {
                        f.table
                            .get(K::I::try_from(ix).ok().expect("index conversion failed"))
                            .into()
                    })
                    .collect()
            })
            .collect(),
    }
}

fn initial_wire_candidates<K: ArrayKind, O, A>(
    pattern: &Hypergraph<K, O, A>,
    host: &Hypergraph<K, O, A>,
    options: &MatchOptions,
) -> Vec<Vec<bool>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    O: PartialEq,
    K::I: Into<usize> + TryFrom<usize>,
{
    let pattern_wire_count: usize = pattern.w.len().into();
    let host_wire_count: usize = host.w.len().into();

    (0..pattern_wire_count)
        .map(|pattern_wire| {
            let pattern_wire_ix = K::I::try_from(pattern_wire)
                .ok()
                .expect("pattern wire index conversion failed");
            let pattern_label = pattern.w.0.get(pattern_wire_ix.clone());
            let pattern_in_degree = pattern.in_degree(pattern_wire_ix.clone());
            let pattern_out_degree = pattern.out_degree(pattern_wire_ix);

            (0..host_wire_count)
                .map(|host_wire| {
                    let host_wire_ix = K::I::try_from(host_wire)
                        .ok()
                        .expect("host wire index conversion failed");
                    pattern_label == host.w.0.get(host_wire_ix.clone())
                        && (!options.mono
                            || (pattern_in_degree <= host.in_degree(host_wire_ix.clone())
                                && pattern_out_degree <= host.out_degree(host_wire_ix)))
                })
                .collect()
        })
        .collect()
}

fn initial_op_candidates<K: ArrayKind, O, A>(
    pattern: &Hypergraph<K, O, A>,
    host: &Hypergraph<K, O, A>,
    pattern_sources: &OrderedIncidence,
    pattern_targets: &OrderedIncidence,
    host_sources: &OrderedIncidence,
    host_targets: &OrderedIncidence,
) -> Vec<Vec<bool>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
    O: PartialEq,
    A: PartialEq,
    K::I: Into<usize> + TryFrom<usize>,
{
    let pattern_op_count: usize = pattern.x.len().into();
    let host_op_count: usize = host.x.len().into();

    (0..pattern_op_count)
        .map(|pattern_op| {
            let pattern_op_ix = K::I::try_from(pattern_op)
                .ok()
                .expect("pattern op index conversion failed");
            let pattern_label = pattern.x.0.get(pattern_op_ix);

            (0..host_op_count)
                .map(|host_op| {
                    let host_op_ix = K::I::try_from(host_op)
                        .ok()
                        .expect("host op index conversion failed");
                    let same_label = pattern_label == host.x.0.get(host_op_ix);
                    let same_arity = pattern_sources.edge(pattern_op).len()
                        == host_sources.edge(host_op).len()
                        && pattern_targets.edge(pattern_op).len()
                            == host_targets.edge(host_op).len();

                    same_label
                        && same_arity
                        && endpoints_have_matching_labels(
                            pattern,
                            host,
                            pattern_sources.edge(pattern_op),
                            host_sources.edge(host_op),
                        )
                        && endpoints_have_matching_labels(
                            pattern,
                            host,
                            pattern_targets.edge(pattern_op),
                            host_targets.edge(host_op),
                        )
                })
                .collect()
        })
        .collect()
}

fn endpoints_have_matching_labels<K: ArrayKind, O, A>(
    pattern: &Hypergraph<K, O, A>,
    host: &Hypergraph<K, O, A>,
    pattern_wires: &[usize],
    host_wires: &[usize],
) -> bool
where
    K::Type<O>: Array<K, O> + PartialEq,
    O: PartialEq,
    K::I: TryFrom<usize>,
{
    pattern_wires
        .iter()
        .zip(host_wires.iter())
        .all(|(&pattern_wire, &host_wire)| {
            let pattern_ix = K::I::try_from(pattern_wire)
                .ok()
                .expect("pattern endpoint conversion failed");
            let host_ix = K::I::try_from(host_wire)
                .ok()
                .expect("host endpoint conversion failed");
            pattern.w.0.get(pattern_ix) == host.w.0.get(host_ix)
        })
}
