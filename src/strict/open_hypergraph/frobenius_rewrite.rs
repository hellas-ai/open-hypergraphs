use crate::array::vec::{VecArray, VecKind};
use crate::array::*;
use crate::category::{Arrow, Coproduct};
use crate::finite_function::FiniteFunction;
use crate::partition::{enumerate_partitions, Partition, PartitionInput};
use crate::strict::hypergraph::arrow::validate_hypergraph_morphism;
use crate::strict::hypergraph::Hypergraph;
use crate::strict::open_hypergraph::OpenHypergraph;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrobeniusRewriteRuleError {
    BoundaryMismatch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrobeniusRewriteMatchError {
    InvalidHypergraphMorphism,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrobeniusRewriteApplyError {
    IdentificationConditionFailed,
    DanglingConditionFailed,
    NoValidPushoutComplement,
}

/// A rewrite rule for strict open hypergraphs under rewriting modulo
/// Frobenius structure.
pub struct FrobeniusRewriteRule<O, A> {
    lhs: OpenHypergraph<VecKind, O, A>,
    rhs: OpenHypergraph<VecKind, O, A>,
}

impl<O, A> FrobeniusRewriteRule<O, A>
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    pub fn new(
        lhs: OpenHypergraph<VecKind, O, A>,
        rhs: OpenHypergraph<VecKind, O, A>,
    ) -> Option<Self> {
        Self::try_new(lhs, rhs).ok()
    }

    pub fn try_new(
        lhs: OpenHypergraph<VecKind, O, A>,
        rhs: OpenHypergraph<VecKind, O, A>,
    ) -> Result<Self, FrobeniusRewriteRuleError> {
        if lhs.source() == rhs.source() && lhs.target() == rhs.target() {
            Ok(Self { lhs, rhs })
        } else {
            Err(FrobeniusRewriteRuleError::BoundaryMismatch)
        }
    }

    pub fn lhs(&self) -> &OpenHypergraph<VecKind, O, A> {
        &self.lhs
    }

    pub fn rhs(&self) -> &OpenHypergraph<VecKind, O, A> {
        &self.rhs
    }
}

impl<O, A> Clone for FrobeniusRewriteRule<O, A>
where
    O: Clone,
    A: Clone,
{
    fn clone(&self) -> Self {
        Self {
            lhs: self.lhs.clone(),
            rhs: self.rhs.clone(),
        }
    }
}

impl<O: core::fmt::Debug, A: core::fmt::Debug> core::fmt::Debug for FrobeniusRewriteRule<O, A> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("FrobeniusRewriteRule")
            .field("lhs", &self.lhs)
            .field("rhs", &self.rhs)
            .finish()
    }
}

/// A validated rewrite match witness for Frobenius rewriting.
pub struct FrobeniusRewriteMatch<'a, O, A> {
    rule: &'a FrobeniusRewriteRule<O, A>,
    host: &'a OpenHypergraph<VecKind, O, A>,
    w: FiniteFunction<VecKind>,
    x: FiniteFunction<VecKind>,
}

impl<'a, O, A> FrobeniusRewriteMatch<'a, O, A>
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    pub fn new(
        rule: &'a FrobeniusRewriteRule<O, A>,
        host: &'a OpenHypergraph<VecKind, O, A>,
        w: FiniteFunction<VecKind>,
        x: FiniteFunction<VecKind>,
    ) -> Option<Self> {
        Self::try_new(rule, host, w, x).ok()
    }

    pub fn try_new(
        rule: &'a FrobeniusRewriteRule<O, A>,
        host: &'a OpenHypergraph<VecKind, O, A>,
        w: FiniteFunction<VecKind>,
        x: FiniteFunction<VecKind>,
    ) -> Result<Self, FrobeniusRewriteMatchError> {
        validate_hypergraph_morphism(&rule.lhs.h, &host.h, &w, &x)
            .map_err(|_| FrobeniusRewriteMatchError::InvalidHypergraphMorphism)?;
        Ok(Self { rule, host, w, x })
    }

    pub fn rule(&self) -> &FrobeniusRewriteRule<O, A> {
        self.rule
    }

    pub fn host(&self) -> &OpenHypergraph<VecKind, O, A> {
        self.host
    }

    pub fn w(&self) -> &FiniteFunction<VecKind> {
        &self.w
    }

    pub fn x(&self) -> &FiniteFunction<VecKind> {
        &self.x
    }
}

impl<'a, O, A> Clone for FrobeniusRewriteMatch<'a, O, A> {
    fn clone(&self) -> Self {
        Self {
            rule: self.rule,
            host: self.host,
            w: self.w.clone(),
            x: self.x.clone(),
        }
    }
}

impl<'a, O: core::fmt::Debug, A: core::fmt::Debug> core::fmt::Debug
    for FrobeniusRewriteMatch<'a, O, A>
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("FrobeniusRewriteMatch")
            .field("w", &self.w)
            .field("x", &self.x)
            .finish()
    }
}

struct FrobeniusExplodedContext<O, A> {
    remainder_plus_interface: Hypergraph<VecKind, O, A>,
    to_host: FiniteFunction<VecKind>,
    to_remainder_plus_redex: FiniteFunction<VecKind>,
    interface_in_exploded: FiniteFunction<VecKind>,
    host_inputs: FiniteFunction<VecKind>,
    host_outputs: FiniteFunction<VecKind>,
}

pub fn apply_frobenius_rewrite<'a, O, A>(
    m: &FrobeniusRewriteMatch<'a, O, A>,
) -> Vec<OpenHypergraph<VecKind, O, A>>
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    try_apply_frobenius_rewrite(m).unwrap_or_default()
}

pub fn try_apply_frobenius_rewrite<'a, O, A>(
    m: &FrobeniusRewriteMatch<'a, O, A>,
) -> Result<Vec<OpenHypergraph<VecKind, O, A>>, FrobeniusRewriteApplyError>
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    let rule = m.rule();
    let host = m.host();
    let lhs = rule.lhs();

    if !identification_condition(lhs, m.w()) {
        return Err(FrobeniusRewriteApplyError::IdentificationConditionFailed);
    }
    if !dangling_condition(lhs, host, m.w(), m.x()) {
        return Err(FrobeniusRewriteApplyError::DanglingConditionFailed);
    }

    // As in the hypergraph DPO construction, first build the exploded context,
    // then enumerate the admissible quotients fiberwise.
    let exploded = exploded_context(host, lhs, m.w(), m.x());
    let fiber_inputs = fiber_partition_inputs(&exploded);
    let partitions_per_fiber: Vec<Vec<Partition<usize>>> =
        fiber_inputs.iter().map(enumerate_partitions).collect();
    if partitions_per_fiber.is_empty() {
        let results = pushout_result(rule, &exploded, &partitions_per_fiber, &[])
            .into_iter()
            .collect::<Vec<_>>();
        return if results.is_empty() {
            Err(FrobeniusRewriteApplyError::NoValidPushoutComplement)
        } else {
            Ok(results)
        };
    }

    let mut selection = Vec::with_capacity(partitions_per_fiber.len());
    let mut results = Vec::new();
    walk_partitions(
        0,
        rule,
        &exploded,
        &partitions_per_fiber,
        &mut selection,
        &mut results,
    );
    if results.is_empty() {
        Err(FrobeniusRewriteApplyError::NoValidPushoutComplement)
    } else {
        Ok(results)
    }
}

fn walk_partitions<O, A>(
    idx: usize,
    rule: &FrobeniusRewriteRule<O, A>,
    exploded: &FrobeniusExplodedContext<O, A>,
    partitions_per_fiber: &[Vec<Partition<usize>>],
    selection: &mut Vec<usize>,
    results: &mut Vec<OpenHypergraph<VecKind, O, A>>,
) where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    if idx == partitions_per_fiber.len() {
        if let Some(out) = pushout_result(rule, exploded, partitions_per_fiber, selection) {
            results.push(out);
        }
        return;
    }

    for partition_idx in 0..partitions_per_fiber[idx].len() {
        selection.push(partition_idx);
        walk_partitions(
            idx + 1,
            rule,
            exploded,
            partitions_per_fiber,
            selection,
            results,
        );
        selection.pop();
    }
}

fn pushout_result<O, A>(
    rule: &FrobeniusRewriteRule<O, A>,
    exploded: &FrobeniusExplodedContext<O, A>,
    partitions_per_fiber: &[Vec<Partition<usize>>],
    selection: &[usize],
) -> Option<OpenHypergraph<VecKind, O, A>>
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    let mut quotient_left = Vec::new();
    let mut quotient_right = Vec::new();

    for (fiber_idx, &partition_idx) in selection.iter().enumerate() {
        let partition = &partitions_per_fiber[fiber_idx][partition_idx];
        for block in &partition.blocks {
            let Some((first, rest)) = block.elements.split_first() else {
                continue;
            };
            for node in rest {
                quotient_left.push(*first);
                quotient_right.push(*node);
            }
        }
    }

    let exploded_wire_count = exploded.remainder_plus_interface.w.len();
    // Quotient the exploded context according to the chosen partition blocks.
    let q = FiniteFunction::new(VecArray(quotient_left), exploded_wire_count).and_then(|left| {
        FiniteFunction::new(VecArray(quotient_right), exploded_wire_count)
            .and_then(|right| left.coequalizer(&right))
    })?;
    let complement = exploded.remainder_plus_interface.coequalize_vertices(&q)?;
    let interface_to_complement = exploded.interface_in_exploded.compose(&q)?;
    let complement_to_host = q.coequalizer_universal(&exploded.to_host)?;

    // Unlike the SMC case, the map from complement back to the host need not be
    // injective, so we pick any compatible preimage for each boundary leg.
    let host_inputs = choose_preimage(&exploded.host_inputs, &complement_to_host)?;
    let host_outputs = choose_preimage(&exploded.host_outputs, &complement_to_host)?;

    let rhs_boundary = boundary_map(rule.rhs());
    let (h, left_arrow, _right_arrow) = Hypergraph::pushout_along_span(
        &complement,
        &rule.rhs().h,
        &interface_to_complement,
        &rhs_boundary,
    )?;
    let s = host_inputs.compose(&left_arrow.w)?;
    let t = host_outputs.compose(&left_arrow.w)?;
    OpenHypergraph::new(s, t, h).ok()
}

fn exploded_context<O, A>(
    host: &OpenHypergraph<VecKind, O, A>,
    lhs: &OpenHypergraph<VecKind, O, A>,
    matching_w: &FiniteFunction<VecKind>,
    matching_x: &FiniteFunction<VecKind>,
) -> FrobeniusExplodedContext<O, A>
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    let boundary = boundary_object(lhs);
    let host_boundary = host
        .s
        .coproduct(&host.t)
        .expect("host boundary maps share a codomain");
    // The remainder keeps unmatched edges and host boundary wires, but splits
    // matched internal wires when they still occur in the surrounding context.
    let (remainder, remainder_in_host) =
        remainder_with_boundary(host, matching_w, matching_x, &host_boundary);

    let q_interface = boundary_map(lhs)
        .compose(matching_w)
        .expect("boundary image in host");
    let to_host = remainder_in_host
        .coproduct(&q_interface)
        .expect("exploded context maps into host");

    let remainder_in_remainder_plus_redex =
        FiniteFunction::identity(remainder.w.len()).inject0(lhs.h.w.len());
    let redex_in_remainder_plus_redex =
        FiniteFunction::identity(lhs.h.w.len()).inject1(remainder.w.len());
    // This records the refinement relation f' <= q from the exploded context
    // construction: nodes only get identified within a host fiber.
    let to_remainder_plus_redex = remainder_in_remainder_plus_redex
        .coproduct(
            &boundary_map(lhs)
                .compose(&redex_in_remainder_plus_redex)
                .expect("boundary image in exploded redex"),
        )
        .expect("exploded context refines remainder-plus-redex");

    let remainder_plus_interface = remainder.coproduct(&boundary);
    let interface_in_exploded =
        FiniteFunction::identity(boundary.w.len()).inject1(remainder.w.len());

    FrobeniusExplodedContext {
        remainder_plus_interface,
        to_host,
        to_remainder_plus_redex,
        interface_in_exploded,
        host_inputs: host.s.clone(),
        host_outputs: host.t.clone(),
    }
}

fn boundary_object<O, A>(graph: &OpenHypergraph<VecKind, O, A>) -> Hypergraph<VecKind, O, A>
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    Hypergraph::discrete(graph.source().coproduct(&graph.target()))
}

fn boundary_map<O, A>(graph: &OpenHypergraph<VecKind, O, A>) -> FiniteFunction<VecKind>
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    graph
        .s
        .coproduct(&graph.t)
        .expect("boundary maps share the wire codomain")
}

fn remainder_with_boundary<O, A>(
    host: &OpenHypergraph<VecKind, O, A>,
    excluded_w: &FiniteFunction<VecKind>,
    excluded_x: &FiniteFunction<VecKind>,
    preserved_w: &FiniteFunction<VecKind>,
) -> (Hypergraph<VecKind, O, A>, FiniteFunction<VecKind>)
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    let mut excluded_wire = vec![false; host.h.w.len()];
    for &ix in excluded_w.table.iter() {
        excluded_wire[ix] = true;
    }

    let mut preserved_wire = vec![false; host.h.w.len()];
    for &ix in preserved_w.table.iter() {
        preserved_wire[ix] = true;
    }

    let mut excluded_edge = vec![false; host.h.x.len()];
    for &ix in excluded_x.table.iter() {
        excluded_edge[ix] = true;
    }

    let mut wire_map = vec![None; host.h.w.len()];
    let mut remainder_to_host_wires = Vec::new();
    let add_wire = |wire: usize,
                    wire_map: &mut Vec<Option<usize>>,
                    remainder_to_host_wires: &mut Vec<usize>| {
        if let Some(existing) = wire_map[wire] {
            existing
        } else {
            let new_ix = remainder_to_host_wires.len();
            wire_map[wire] = Some(new_ix);
            remainder_to_host_wires.push(wire);
            new_ix
        }
    };

    for (wire, &keep) in preserved_wire.iter().enumerate() {
        if keep || !excluded_wire[wire] {
            add_wire(wire, &mut wire_map, &mut remainder_to_host_wires);
        }
    }

    let mut kept_edge_sources = Vec::new();
    let mut kept_edge_targets = Vec::new();
    let mut kept_edge_labels = Vec::new();
    let mut remainder_to_host_edges = Vec::new();

    for edge in 0..host.h.x.len() {
        if excluded_edge[edge] {
            continue;
        }

        let s_seg = segment(&host.h.s, edge);
        let t_seg = segment(&host.h.t, edge);
        let mut sources = Vec::with_capacity(s_seg.table.len());
        let mut targets = Vec::with_capacity(t_seg.table.len());

        for &wire in s_seg.table.iter() {
            if excluded_wire[wire] && !preserved_wire[wire] {
                // Each surviving incidence on a deleted matched wire gets its own
                // fresh copy in the exploded context.
                let new_ix = remainder_to_host_wires.len();
                remainder_to_host_wires.push(wire);
                sources.push(new_ix);
            } else {
                sources.push(add_wire(wire, &mut wire_map, &mut remainder_to_host_wires));
            }
        }
        for &wire in t_seg.table.iter() {
            if excluded_wire[wire] && !preserved_wire[wire] {
                let new_ix = remainder_to_host_wires.len();
                remainder_to_host_wires.push(wire);
                targets.push(new_ix);
            } else {
                targets.push(add_wire(wire, &mut wire_map, &mut remainder_to_host_wires));
            }
        }

        kept_edge_sources.push(sources);
        kept_edge_targets.push(targets);
        kept_edge_labels.push(host.h.x.0[edge].clone());
        remainder_to_host_edges.push(edge);
    }

    let wire_labels = remainder_to_host_wires
        .iter()
        .map(|&wire| host.h.w.0[wire].clone())
        .collect::<Vec<_>>();

    let s = indexed_coproduct_from_segments(&kept_edge_sources, remainder_to_host_wires.len());
    let t = indexed_coproduct_from_segments(&kept_edge_targets, remainder_to_host_wires.len());
    let remainder = Hypergraph::new(
        s,
        t,
        crate::semifinite::SemifiniteFunction::new(VecArray(wire_labels)),
        crate::semifinite::SemifiniteFunction::new(VecArray(kept_edge_labels)),
    )
    .expect("remainder hypergraph must be valid");

    let remainder_in_host = FiniteFunction::new(VecArray(remainder_to_host_wires), host.h.w.len())
        .expect("remainder wires inject into host");
    let remainder_edges_in_host: FiniteFunction<VecKind> =
        FiniteFunction::new(VecArray(remainder_to_host_edges), host.h.x.len())
            .expect("remainder edges inject into host");
    debug_assert!(remainder_edges_in_host.is_injective());

    (remainder, remainder_in_host)
}

fn indexed_coproduct_from_segments(
    segments: &[Vec<usize>],
    target: usize,
) -> crate::indexed_coproduct::IndexedCoproduct<VecKind, FiniteFunction<VecKind>> {
    let lengths = segments.iter().map(Vec::len).collect::<Vec<_>>();
    let values = segments
        .iter()
        .flat_map(|segment| segment.iter().copied())
        .collect::<Vec<_>>();
    crate::indexed_coproduct::IndexedCoproduct::from_semifinite(
        crate::semifinite::SemifiniteFunction::new(VecArray(lengths)),
        FiniteFunction::new(VecArray(values), target).expect("indexed coproduct values in range"),
    )
    .expect("indexed coproduct must be valid")
}

fn choose_preimage(
    f: &FiniteFunction<VecKind>,
    codomain_map: &FiniteFunction<VecKind>,
) -> Option<FiniteFunction<VecKind>> {
    if f.target() != codomain_map.target() {
        return None;
    }

    // `codomain_map` may identify several complement wires with the same host
    // wire; any representative is enough to rebuild the open boundary.
    let mut preimage = vec![None; codomain_map.target()];
    for (src, &tgt) in codomain_map.table.iter().enumerate() {
        preimage[tgt].get_or_insert(src);
    }

    let mut table = Vec::with_capacity(f.source());
    for &tgt in f.table.iter() {
        table.push(preimage[tgt]?);
    }

    FiniteFunction::new(VecArray(table), codomain_map.source())
}

fn segment(
    coproduct: &crate::indexed_coproduct::IndexedCoproduct<VecKind, FiniteFunction<VecKind>>,
    ix: usize,
) -> FiniteFunction<VecKind> {
    let pointers = coproduct.sources.table.cumulative_sum();
    let start = pointers[ix];
    let end = pointers[ix + 1];
    FiniteFunction::new(
        VecArray(coproduct.values.table[start..end].to_vec()),
        coproduct.values.target(),
    )
    .expect("segment values stay within the common target")
}

fn identification_condition<O, A>(
    lhs: &OpenHypergraph<VecKind, O, A>,
    matching_w: &FiniteFunction<VecKind>,
) -> bool
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    let mut in_boundary = vec![false; lhs.h.w.len()];
    for &ix in lhs.s.table.iter().chain(lhs.t.table.iter()) {
        in_boundary[ix] = true;
    }

    let mut seen = vec![None; matching_w.target()];
    for wire in 0..lhs.h.w.len() {
        if in_boundary[wire] {
            continue;
        }
        let img = matching_w.table[wire];
        if let Some(existing) = seen[img] {
            if existing != wire {
                return false;
            }
        } else {
            seen[img] = Some(wire);
        }
    }

    true
}

fn dangling_condition<O, A>(
    lhs: &OpenHypergraph<VecKind, O, A>,
    host: &OpenHypergraph<VecKind, O, A>,
    matching_w: &FiniteFunction<VecKind>,
    matching_x: &FiniteFunction<VecKind>,
) -> bool
where
    O: Clone + PartialEq,
    A: Clone + PartialEq,
{
    let mut in_boundary = vec![false; lhs.h.w.len()];
    for &ix in lhs.s.table.iter().chain(lhs.t.table.iter()) {
        in_boundary[ix] = true;
    }

    let mut forbidden_wires = vec![false; host.h.w.len()];
    for wire in 0..lhs.h.w.len() {
        if in_boundary[wire] {
            continue;
        }
        forbidden_wires[matching_w.table[wire]] = true;
    }

    for &wire in host.s.table.iter().chain(host.t.table.iter()) {
        if forbidden_wires[wire] {
            return false;
        }
    }

    let mut edge_in_image = vec![false; host.h.x.len()];
    for &edge in matching_x.table.iter() {
        edge_in_image[edge] = true;
    }

    for edge in 0..host.h.x.len() {
        if edge_in_image[edge] {
            continue;
        }
        let s_seg = segment(&host.h.s, edge);
        let t_seg = segment(&host.h.t, edge);
        if s_seg
            .table
            .iter()
            .chain(t_seg.table.iter())
            .any(|wire| forbidden_wires[*wire])
        {
            return false;
        }
    }

    true
}

fn fiber_partition_inputs<O, A>(
    exploded: &FrobeniusExplodedContext<O, A>,
) -> Vec<PartitionInput<usize>> {
    let mut fibers = vec![Vec::new(); exploded.to_host.target()];
    for (src, &tgt) in exploded.to_host.table.iter().enumerate() {
        fibers[tgt].push(src);
    }

    fibers
        .into_iter()
        .filter(|nodes| !nodes.is_empty())
        .map(|nodes| {
            let mut class_index = vec![None; exploded.to_remainder_plus_redex.target()];
            let mut class_ids = Vec::with_capacity(nodes.len());
            let mut next_class = 0;
            for node in &nodes {
                let f_image = exploded.to_remainder_plus_redex.table[*node];
                let id = match class_index[f_image] {
                    Some(existing) => existing,
                    None => {
                        let id = next_class;
                        next_class += 1;
                        class_index[f_image] = Some(id);
                        id
                    }
                };
                class_ids.push(id);
            }

            PartitionInput {
                elements: nodes,
                class_ids,
                class_count: next_class,
            }
        })
        .collect()
}
