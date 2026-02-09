use crate::array::*;
use crate::category::Arrow;
use crate::finite_function::FiniteFunction;
use crate::strict::hypergraph::subobject::SubgraphMorphism;
use crate::strict::hypergraph::Hypergraph;
use crate::strict::open_hypergraph::OpenHypergraph;
use num_traits::{One, Zero};

/// A rewrite rule for strict open hypergraphs.
///
/// Both sides must have equal boundaries (same source/target objects).
pub struct RewriteRule<K: ArrayKind, O, A> {
    lhs: OpenHypergraph<K, O, A>,
    rhs: OpenHypergraph<K, O, A>,
}

impl<K: ArrayKind, O, A> RewriteRule<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<bool>: Array<K, bool>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A>,
{
    /// Create a new rewrite rule if the boundaries of `lhs` and `rhs` match.
    pub fn new(lhs: OpenHypergraph<K, O, A>, rhs: OpenHypergraph<K, O, A>) -> Option<Self> {
        if Self::validate(&lhs, &rhs) {
            Some(Self { lhs, rhs })
        } else {
            None
        }
    }

    pub fn validate(lhs: &OpenHypergraph<K, O, A>, rhs: &OpenHypergraph<K, O, A>) -> bool {
        Self::boundaries_match(lhs, rhs)
            && Self::boundary_legs_injective(lhs, rhs)
            && Self::sides_monogamous_acyclic(lhs, rhs)
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
}

impl<K: ArrayKind, O, A> Clone for RewriteRule<K, O, A>
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
    for RewriteRule<K, O, A>
where
    K::Index: core::fmt::Debug,
    K::Type<K::I>: core::fmt::Debug,
    K::Type<O>: core::fmt::Debug,
    K::Type<A>: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("RewriteRule")
            .field("lhs", &self.lhs)
            .field("rhs", &self.rhs)
            .finish()
    }
}

/// A convex match witness for a morphism `L -> host`, represented by maps on wires and operations.
pub struct ConvexMatchWitness<K: ArrayKind> {
    w: FiniteFunction<K>,
    x: FiniteFunction<K>,
}

impl<K: ArrayKind> ConvexMatchWitness<K> {
    pub fn new<O, A>(
        lhs: &OpenHypergraph<K, O, A>,
        host: &MonogamousAcyclicHost<'_, K, O, A>,
        w: FiniteFunction<K>,
        x: FiniteFunction<K>,
    ) -> Option<Self>
    where
        K::Type<K::I>: NaturalArray<K>,
        K::Type<bool>: Array<K, bool>,
        K::Type<O>: Array<K, O>,
        K::Type<A>: Array<K, A>,
    {
        // Matches are required to be injective and convex.
        if w.is_injective() && x.is_injective() && is_convex_subgraph(lhs, host.inner, &w, &x) {
            Some(Self { w, x })
        } else {
            None
        }
    }

    pub fn w(&self) -> &FiniteFunction<K> {
        &self.w
    }

    pub fn x(&self) -> &FiniteFunction<K> {
        &self.x
    }
}

impl<K: ArrayKind> Clone for ConvexMatchWitness<K>
where
    K::Type<K::I>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            w: self.w.clone(),
            x: self.x.clone(),
        }
    }
}

impl<K: ArrayKind> core::fmt::Debug for ConvexMatchWitness<K>
where
    K::Index: core::fmt::Debug,
    K::Type<K::I>: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("ConvexMatchWitness")
            .field("w", &self.w)
            .field("x", &self.x)
            .finish()
    }
}

/// A witness that a host open hypergraph is monogamous and acyclic.
pub struct MonogamousAcyclicHost<'a, K: ArrayKind, O, A> {
    inner: &'a OpenHypergraph<K, O, A>,
}

impl<'a, K: ArrayKind, O, A> MonogamousAcyclicHost<'a, K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<bool>: Array<K, bool>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    pub fn new(host: &'a OpenHypergraph<K, O, A>) -> Option<Self> {
        if host.is_monogamous() && host.is_acyclic() {
            Some(Self { inner: host })
        } else {
            None
        }
    }

    pub fn host(&self) -> &OpenHypergraph<K, O, A> {
        self.inner
    }
}

fn is_convex_subgraph<K: ArrayKind, O, A>(
    _lhs: &OpenHypergraph<K, O, A>,
    _host: &OpenHypergraph<K, O, A>,
    _w: &FiniteFunction<K>,
    _x: &FiniteFunction<K>,
) -> bool
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    // TODO: implement convexity check
    true
}

fn assert_match_compatible<K: ArrayKind, O, A>(
    rule: &RewriteRule<K, O, A>,
    host: &OpenHypergraph<K, O, A>,
    m: &ConvexMatchWitness<K>,
) where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
{
    let lhs = &rule.lhs;

    assert!(
        m.w().table.len() == lhs.h.w.len() && m.w().target == host.h.w.len(),
        "match witness on wires has incompatible source/target"
    );
    assert!(
        m.x().table.len() == lhs.h.x.len() && m.x().target == host.h.x.len(),
        "match witness on operations has incompatible source/target"
    );
    assert!(
        lhs.h.w == (m.w() >> &host.h.w).expect("wire composition must be defined"),
        "match witness is not natural on wires"
    );
    assert!(
        lhs.h.x == (m.x() >> &host.h.x).expect("operation composition must be defined"),
        "match witness is not natural on operations"
    );
}

/// Apply a rewrite rule to `host` using a match `m : L -> host` where `L` is the apex of `lhs`.
///
/// Returns `None` if the rewrite is invalid.
pub fn apply_rewrite<K: ArrayKind, O, A>(
    rule: &RewriteRule<K, O, A>,
    host: &MonogamousAcyclicHost<'_, K, O, A>,
    m: &ConvexMatchWitness<K>,
) -> Option<OpenHypergraph<K, O, A>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<bool>: Array<K, bool>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
    O: PartialEq,
    for<'a> K::Slice<'a, K::I>: From<&'a [K::I]>,
{
    assert_match_compatible(rule, host.host(), m);

    let host = host.host();
    let lhs = &rule.lhs;
    let rhs = &rule.rhs;

    // Compute where the LHS boundary lands in the host.
    let lhs_inputs_in_host = (&lhs.s >> m.w())?;
    let lhs_outputs_in_host = (&lhs.t >> m.w())?;

    let mut in_match_mask = K::Type::<bool>::fill(false, host.h.w.len());
    if m.w().table.len() != K::I::zero() {
        in_match_mask.scatter_assign_constant(&m.w().table, true);
    }

    let mut lhs_input_mask = K::Type::<bool>::fill(false, host.h.w.len());
    if lhs_inputs_in_host.table.len() != K::I::zero() {
        lhs_input_mask.scatter_assign_constant(&lhs_inputs_in_host.table, true);
    }

    let mut lhs_output_mask = K::Type::<bool>::fill(false, host.h.w.len());
    if lhs_outputs_in_host.table.len() != K::I::zero() {
        lhs_output_mask.scatter_assign_constant(&lhs_outputs_in_host.table, true);
    }

    // Boundary embedding must be injective: no overlap and no duplicate images.
    if mask_overlap::<K>(&lhs_input_mask, &lhs_output_mask)
        || !lhs_inputs_in_host.is_injective()
        || !lhs_outputs_in_host.is_injective()
    {
        return None;
    }

    // Build the remainder by removing the matched interior, but keep outer interface
    // and images of the LHS boundary ports.
    let mut remove_node_mask = in_match_mask.clone();
    clear_mask_at(&mut remove_node_mask, &host.s);
    clear_mask_at(&mut remove_node_mask, &host.t);
    clear_mask_at(&mut remove_node_mask, &lhs_inputs_in_host);
    clear_mask_at(&mut remove_node_mask, &lhs_outputs_in_host);

    let mut remove_edge_mask = K::Type::<bool>::fill(false, host.h.x.len());
    if m.x().table.len() != K::I::zero() {
        remove_edge_mask.scatter_assign_constant(&m.x().table, true);
    }

    let remainder = SubgraphMorphism::from_masks(&host.h, remove_node_mask, remove_edge_mask);
    let (remainder, kept_w_inj, _kept_x_inj) = remainder.as_hypergraph_with_injections()?;

    // Factor boundary maps through the remainder injection to build the context L⊥.
    let host_inputs = factor_through_injection(&host.s, &kept_w_inj)?;
    let host_outputs = factor_through_injection(&host.t, &kept_w_inj)?;
    let lhs_outputs = factor_through_injection(&lhs_outputs_in_host, &kept_w_inj)?;
    let lhs_inputs = factor_through_injection(&lhs_inputs_in_host, &kept_w_inj)?;

    let s_ctx = (&host_inputs + &lhs_outputs)?;
    let t_ctx = (&host_outputs + &lhs_inputs)?;
    let context = OpenHypergraph::new(s_ctx, t_ctx, remainder).ok()?;
    if !context.is_monogamous() {
        return None;
    }

    let h_in = host.s.source();
    let h_out = host.t.source();
    let l_in = lhs.s.source();
    let l_out = lhs.t.source();

    pushout_rewrite(&context, rhs, h_in, h_out, l_in, l_out)
}

impl<K: ArrayKind, O, A> RewriteRule<K, O, A> {
    pub fn lhs(&self) -> &OpenHypergraph<K, O, A> {
        &self.lhs
    }

    pub fn rhs(&self) -> &OpenHypergraph<K, O, A> {
        &self.rhs
    }
}

// Factor a boundary map through an injection `inj : W_R -> W_H`.
// Returns `None` if any boundary element lands outside the image of `inj`.
fn factor_through_injection<K: ArrayKind>(
    f: &FiniteFunction<K>,
    inj: &FiniteFunction<K>,
) -> Option<FiniteFunction<K>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<bool>: Array<K, bool>,
{
    if f.target() != inj.target() {
        return None;
    }

    // Build mask for the image of inj.
    let mut in_image = K::Type::<bool>::fill(false, inj.target());
    if inj.table.len() != K::I::zero() {
        in_image.scatter_assign_constant(&inj.table, true);
    }

    // Ensure every boundary element lies in the image of inj.
    let mut i = K::I::zero();
    while i < f.table.len() {
        let w = f.table.get(i.clone());
        if !in_image.get(w) {
            return None;
        }
        i = i + K::I::one();
    }

    // Build a left-inverse on the image and reindex boundary elements.
    let mut inverse = K::Type::<K::I>::fill(K::I::zero(), inj.target());
    let values: K::Type<K::I> = K::Index::arange(&K::I::zero(), &inj.source()).into();
    inverse.scatter_assign(&inj.table, values);

    let table: K::Index = inverse.gather(f.table.get_range(..)).into();
    Some(FiniteFunction {
        table,
        target: inj.source(),
    })
}

// Check if a finite function is injective by ensuring no repeated targets.
// Check whether two masks share any true entries.
fn mask_overlap<K: ArrayKind>(a: &K::Type<bool>, b: &K::Type<bool>) -> bool
where
    K::Type<bool>: Array<K, bool>,
{
    let mut i = K::I::zero();
    while i < a.len() {
        if a.get(i.clone()) && b.get(i.clone()) {
            return true;
        }
        i = i + K::I::one();
    }
    false
}

// Clear mask entries at the image of a finite function.
fn clear_mask_at<K: ArrayKind>(mask: &mut K::Type<bool>, f: &FiniteFunction<K>)
where
    K::Type<bool>: Array<K, bool>,
{
    if f.table.len() != K::I::zero() {
        mask.scatter_assign_constant(&f.table, false);
    }
}

// Slice a finite function along a contiguous range of its source.
fn slice_map<K: ArrayKind>(
    f: &FiniteFunction<K>,
    start: K::I,
    len: K::I,
) -> Option<FiniteFunction<K>>
where
    K::Type<K::I>: NaturalArray<K>,
{
    let end = start.clone() + len.clone();
    if end > f.source() {
        return None;
    }
    let table = K::Index::from_slice(f.table.get_range(start..end));
    FiniteFunction::new(table, f.target())
}

// Build the pushout of the context and RHS along the shared boundary, then
// reconstruct the outer interface from the context's host boundary segment.
fn pushout_rewrite<K: ArrayKind, O, A>(
    context: &OpenHypergraph<K, O, A>,
    rhs: &OpenHypergraph<K, O, A>,
    host_inputs: K::I,
    host_outputs: K::I,
    lhs_inputs: K::I,
    lhs_outputs: K::I,
) -> Option<OpenHypergraph<K, O, A>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
{
    // Build the span from boundary ports into the coproduct of wires.
    let c_s_out = slice_map(&context.s, host_inputs.clone(), lhs_outputs.clone())?;
    let c_t_in = slice_map(&context.t, host_outputs.clone(), lhs_inputs.clone())?;

    let f_in = c_t_in.inject0(rhs.h.w.len());
    let f_out = c_s_out.inject0(rhs.h.w.len());
    let f = (&f_in + &f_out)?;

    let g_in = rhs.s.inject1(context.h.w.len());
    let g_out = rhs.t.inject1(context.h.w.len());
    let g = (&g_in + &g_out)?;

    // Compute the pushout hypergraph and the two induced arrows.
    let (h, left_arrow, _right_arrow) = Hypergraph::pushout_along_span(&context.h, &rhs.h, &f, &g)?;

    // Rebuild the host boundary maps (prefix of context boundaries).
    let s_host = slice_map(&context.s, K::I::zero(), host_inputs)?;
    let t_host = slice_map(&context.t, K::I::zero(), host_outputs)?;
    let s = s_host.compose(&left_arrow.w)?;
    let t = t_host.compose(&left_arrow.w)?;

    OpenHypergraph::new(s, t, h).ok()
}
