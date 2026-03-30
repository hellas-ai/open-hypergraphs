use crate::array::*;
use crate::category::Arrow;
use crate::finite_function::FiniteFunction;
use crate::strict::hypergraph::arrow::is_convex_subgraph_morphism;
use crate::strict::hypergraph::Hypergraph;
use crate::strict::open_hypergraph::OpenHypergraph;
use num_traits::Zero;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SmcRewriteRuleError {
    BoundaryMismatch,
    BoundaryLegsNotInjective,
    LhsBoundaryLegsNotDisjoint,
    LhsNotMonogamousAcyclic,
    RhsNotMonogamousAcyclic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SmcRewriteMatchError {
    HostNotMonogamous,
    HostNotAcyclic,
    NotConvexSubgraphMorphism,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SmcRewriteApplyError {
    ContextNotMonogamous,
    PushoutFailed,
}

/// A rewrite rule for strict open hypergraphs under rewriting with
/// symmetric monoidal structure (SMC).
///
pub struct SmcRewriteRule<K: ArrayKind, O, A> {
    lhs: OpenHypergraph<K, O, A>,
    rhs: OpenHypergraph<K, O, A>,
}

impl<K: ArrayKind, O, A> SmcRewriteRule<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<bool>: Array<K, bool>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A>,
{
    /// Create a new rewrite rule if the boundaries of `lhs` and `rhs` match.
    pub fn new(lhs: OpenHypergraph<K, O, A>, rhs: OpenHypergraph<K, O, A>) -> Option<Self> {
        Self::try_new(lhs, rhs).ok()
    }

    pub fn try_new(
        lhs: OpenHypergraph<K, O, A>,
        rhs: OpenHypergraph<K, O, A>,
    ) -> Result<Self, SmcRewriteRuleError> {
        Self::validate(&lhs, &rhs)?;
        Ok(Self { lhs, rhs })
    }

    pub fn validate(
        lhs: &OpenHypergraph<K, O, A>,
        rhs: &OpenHypergraph<K, O, A>,
    ) -> Result<(), SmcRewriteRuleError> {
        if !Self::boundaries_match(lhs, rhs) {
            return Err(SmcRewriteRuleError::BoundaryMismatch);
        }
        if !Self::boundary_legs_injective(lhs, rhs) {
            return Err(SmcRewriteRuleError::BoundaryLegsNotInjective);
        }
        if !Self::lhs_boundary_legs_disjoint(lhs) {
            return Err(SmcRewriteRuleError::LhsBoundaryLegsNotDisjoint);
        }
        if !(lhs.is_monogamous() && lhs.is_acyclic()) {
            return Err(SmcRewriteRuleError::LhsNotMonogamousAcyclic);
        }
        if !(rhs.is_monogamous() && rhs.is_acyclic()) {
            return Err(SmcRewriteRuleError::RhsNotMonogamousAcyclic);
        }
        Ok(())
    }

    pub fn boundaries_match(lhs: &OpenHypergraph<K, O, A>, rhs: &OpenHypergraph<K, O, A>) -> bool {
        lhs.source() == rhs.source() && lhs.target() == rhs.target()
    }

    pub fn sides_monogamous_acyclic(
        lhs: &OpenHypergraph<K, O, A>,
        rhs: &OpenHypergraph<K, O, A>,
    ) -> bool {
        lhs.is_monogamous() && lhs.is_acyclic() && rhs.is_monogamous() && rhs.is_acyclic()
    }

    fn boundary_legs_injective(
        lhs: &OpenHypergraph<K, O, A>,
        rhs: &OpenHypergraph<K, O, A>,
    ) -> bool {
        lhs.s.is_injective() && lhs.t.is_injective() && rhs.s.is_injective() && rhs.t.is_injective()
    }

    fn lhs_boundary_legs_disjoint(lhs: &OpenHypergraph<K, O, A>) -> bool {
        lhs.s.has_disjoint_image(&lhs.t)
    }
}

impl<K: ArrayKind, O, A> Clone for SmcRewriteRule<K, O, A>
where
    K::Type<O>: Clone,
    K::Type<A>: Clone,
    K::Type<K::I>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            lhs: self.lhs.clone(),
            rhs: self.rhs.clone(),
        }
    }
}

impl<K: ArrayKind, O: core::fmt::Debug, A: core::fmt::Debug> core::fmt::Debug
    for SmcRewriteRule<K, O, A>
where
    K::Index: core::fmt::Debug,
    K::Type<K::I>: core::fmt::Debug,
    K::Type<O>: core::fmt::Debug,
    K::Type<A>: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("SmcRewriteRule")
            .field("lhs", &self.lhs)
            .field("rhs", &self.rhs)
            .finish()
    }
}

/// A validated rewrite match witness for SMC rewriting, represented by maps on
/// wires and operations.
pub struct SmcRewriteMatch<'a, K: ArrayKind, O, A> {
    rule: &'a SmcRewriteRule<K, O, A>,
    host: &'a OpenHypergraph<K, O, A>,
    w: FiniteFunction<K>,
    x: FiniteFunction<K>,
}

impl<'a, K: ArrayKind, O, A> SmcRewriteMatch<'a, K, O, A> {
    pub fn new(
        rule: &'a SmcRewriteRule<K, O, A>,
        host: &'a OpenHypergraph<K, O, A>,
        w: FiniteFunction<K>,
        x: FiniteFunction<K>,
    ) -> Option<Self>
    where
        K::Type<K::I>: NaturalArray<K>,
        K::Type<bool>: Array<K, bool>,
        K::Type<O>: Array<K, O> + PartialEq,
        K::Type<A>: Array<K, A> + PartialEq,
    {
        Self::try_new(rule, host, w, x).ok()
    }

    pub fn try_new(
        rule: &'a SmcRewriteRule<K, O, A>,
        host: &'a OpenHypergraph<K, O, A>,
        w: FiniteFunction<K>,
        x: FiniteFunction<K>,
    ) -> Result<Self, SmcRewriteMatchError>
    where
        K::Type<K::I>: NaturalArray<K>,
        K::Type<bool>: Array<K, bool>,
        K::Type<O>: Array<K, O> + PartialEq,
        K::Type<A>: Array<K, A> + PartialEq,
    {
        if !host.is_monogamous() {
            return Err(SmcRewriteMatchError::HostNotMonogamous);
        }
        if !host.is_acyclic() {
            return Err(SmcRewriteMatchError::HostNotAcyclic);
        }
        if !is_convex_subgraph_morphism(&rule.lhs.h, &host.h, &w, &x) {
            return Err(SmcRewriteMatchError::NotConvexSubgraphMorphism);
        }
        Ok(Self { rule, host, w, x })
    }

    pub fn w(&self) -> &FiniteFunction<K> {
        &self.w
    }

    pub fn x(&self) -> &FiniteFunction<K> {
        &self.x
    }

    pub fn rule(&self) -> &SmcRewriteRule<K, O, A> {
        self.rule
    }

    pub fn host(&self) -> &OpenHypergraph<K, O, A> {
        self.host
    }
}

impl<'a, K: ArrayKind, O, A> Clone for SmcRewriteMatch<'a, K, O, A>
where
    K::Type<K::I>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            rule: self.rule,
            host: self.host,
            w: self.w.clone(),
            x: self.x.clone(),
        }
    }
}

impl<'a, K: ArrayKind, O: core::fmt::Debug, A: core::fmt::Debug> core::fmt::Debug
    for SmcRewriteMatch<'a, K, O, A>
where
    K::Index: core::fmt::Debug,
    K::Type<K::I>: core::fmt::Debug,
    K::Type<O>: core::fmt::Debug,
    K::Type<A>: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("SmcRewriteMatch")
            .field("w", &self.w)
            .field("x", &self.x)
            .finish()
    }
}

/// Apply a rewrite rule to `host` using a validated SMC match `m : L -> host`
/// where `L` is the left-hand side of the rule.
///
/// Returned errors correspond to genuine failures to apply the rewrite after
/// the local match has been validated. Failures of the intermediate
/// finite-function constructions are treated as invariant violations and panic.
pub fn apply_smc_rewrite<'a, K: ArrayKind, O, A>(
    m: &SmcRewriteMatch<'a, K, O, A>,
) -> Result<OpenHypergraph<K, O, A>, SmcRewriteApplyError>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<bool>: Array<K, bool>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
    O: PartialEq,
    for<'b> K::Slice<'b, K::I>: From<&'b [K::I]>,
{
    let rule = m.rule();
    let host = m.host();
    let lhs = rule.lhs();
    let rhs = rule.rhs();

    // Compute where the LHS boundary lands in the host.
    let lhs_inputs_in_host =
        (&lhs.s >> m.w()).expect("validated SMC match preserves lhs input boundary");
    let lhs_outputs_in_host =
        (&lhs.t >> m.w()).expect("validated SMC match preserves lhs output boundary");

    let kept_x_inj = m
        .x()
        .image_complement_injection()
        .expect("finite-function image complements exist");
    let s_kept = host
        .h
        .s
        .map_indexes(&kept_x_inj)
        .expect("kept edge injection reindexes source incidence");
    let t_kept = host
        .h
        .t
        .map_indexes(&kept_x_inj)
        .expect("kept edge injection reindexes target incidence");

    // Build the kept-wire injection in two steps:
    // 1) Start from the complement of matched wires in the host.
    // 2) Add wires that must remain in the context:
    //    - host boundary wires,
    //    - images of LHS boundary wires under the match,
    //    - all endpoints of kept edges (incidence closure).
    // Finally, canonicalize to an injection image -> host.w.
    let kept_w_inj = m
        .w()
        .image_complement_injection()
        .expect("finite-function image complements exist")
        .coproduct_many(&[
            &host.s,
            &host.t,
            &lhs_inputs_in_host,
            &lhs_outputs_in_host,
            &s_kept.values,
            &t_kept.values,
        ])
        .expect("all kept-wire maps share the same host codomain")
        .canonical_image_injection()
        .expect("canonical image injection exists");

    // Total inverse with explicit fill outside image(kept_w_inj).
    // The fill value is never observed here because filtered incidence values
    // lie in image(kept_w_inj) by construction.
    let kept_w_inv = kept_w_inj
        .inverse_with_fill(K::I::zero())
        .expect("canonical image injections are injective");

    // Rebuild incidence by reindexing directly along the kept-edge injection,
    // then remap values through inverse-on-image of kept_w_inj.
    let new_s = host
        .h
        .s
        .map_indexes(&kept_x_inj)
        .and_then(|s| s.map_values(&kept_w_inv))
        .expect("kept sources lie in the kept-wire image");
    let new_t = host
        .h
        .t
        .map_indexes(&kept_x_inj)
        .and_then(|t| t.map_values(&kept_w_inv))
        .expect("kept targets lie in the kept-wire image");

    let new_w = (&kept_w_inj >> &host.h.w).expect("kept wire injection composes with host labels");
    let new_x = (&kept_x_inj >> &host.h.x).expect("kept edge injection composes with host labels");

    let remainder = Hypergraph {
        s: new_s,
        t: new_t,
        w: new_w,
        x: new_x,
    };

    // Factor boundary maps through the remainder injection to build the context.
    // Mathematically: with host.s : I_H -> W_H and kept_w_inj : W_K -> W_H injective,
    // host_inputs : I_H -> W_K is the unique map such that
    // host.s = host_inputs ; kept_w_inj.
    //
    // similar for others
    //
    // note: factor_through_injective panic if factorization is not possible
    //       but it cannot happen in this function by construction
    let host_inputs = host.s.factor_through_injective(&kept_w_inj);
    let host_outputs = host.t.factor_through_injective(&kept_w_inj);
    let lhs_outputs = lhs_outputs_in_host.factor_through_injective(&kept_w_inj);
    let lhs_inputs = lhs_inputs_in_host.factor_through_injective(&kept_w_inj);

    // Intuitively, the context is host with a hole where we can plug in lhs
    // Hence, inputs are host inputs and lhs outputs, similarly for outputs
    let ctx_inputs =
        (&host_inputs + &lhs_outputs).expect("context input coproduct shares codomain");
    let ctx_outputs =
        (&host_outputs + &lhs_inputs).expect("context output coproduct shares codomain");
    let context = OpenHypergraph::new(ctx_inputs, ctx_outputs, remainder).unwrap_or_else(|_| {
        panic!("SMC context reconstructed from a valid match should be a valid open hypergraph")
    });
    if !context.is_monogamous() {
        return Err(SmcRewriteApplyError::ContextNotMonogamous);
    }

    pushout_rewrite(
        &context,
        rhs,
        &host_inputs,
        &host_outputs,
        &lhs_inputs,
        &lhs_outputs,
    )
    .ok_or(SmcRewriteApplyError::PushoutFailed)
}

impl<K: ArrayKind, O, A> SmcRewriteRule<K, O, A> {
    pub fn lhs(&self) -> &OpenHypergraph<K, O, A> {
        &self.lhs
    }

    pub fn rhs(&self) -> &OpenHypergraph<K, O, A> {
        &self.rhs
    }
}

// Build the pushout of the context and RHS along the shared boundary, then
// reconstruct the outer interface from the context's host boundary segment.
fn pushout_rewrite<K: ArrayKind, O, A>(
    context: &OpenHypergraph<K, O, A>,
    rhs: &OpenHypergraph<K, O, A>,
    host_inputs: &FiniteFunction<K>,
    host_outputs: &FiniteFunction<K>,
    lhs_inputs: &FiniteFunction<K>,
    lhs_outputs: &FiniteFunction<K>,
) -> Option<OpenHypergraph<K, O, A>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
{
    // Build the span that glues RHS into the context hole along the boundary.

    // Left leg into context wires: LHS(I) + LHS(O) -> Context(W).
    let f = (lhs_inputs + lhs_outputs)?;
    // Right leg into RHS wires: LHS(I) + LHS(O) -> RHS(W).
    let g = (&rhs.s + &rhs.t)?;

    // Pushout the span to glue RHS into the hole.
    let (h, left_arrow, _right_arrow) = Hypergraph::pushout_along_span(&context.h, &rhs.h, &f, &g)?;

    // Reindex the host-side boundary fragments through the left arrow into the pushout.
    let s = host_inputs.compose(&left_arrow.w)?;
    let t = host_outputs.compose(&left_arrow.w)?;

    OpenHypergraph::new(s, t, h).ok()
}
