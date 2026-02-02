//! Strict symmetric monoidal hypergraph functors on lax open hypergraphs.
use crate::array::vec::VecArray;
use crate::lax::*;
use crate::strict::vec::{FiniteFunction, IndexedCoproduct};

// Problems:
//  1. No ownership of OpenHypergraph ⇒ have to copy in to_strict!
//  2. Sized constraint coming from to_dyn_functor
//  3. Can't give nice default impl - constraints from `define_map_arrow` / `to_dyn_functor`

/// An easier-to-implement `Functor` trait for lax `OpenHypergraph`
pub trait Functor<O1, A1, O2, A2> {
    /// Map a generating object of the theory
    fn map_object(&self, o: &O1) -> impl ExactSizeIterator<Item = O2>;

    /// Map a single operation of the theory with specified source and target type.
    /// This must be consistent with `map_object`, i.e. we must have:
    ///     - `F.map_operation(x, s, t).sources == F.map_object(s)`
    ///     - `F.map_operation(x, s, t).targets == F.map_object(t)`
    /// This condition is *not* checked, but may panic if not satisfied.
    fn map_operation(&self, a: &A1, source: &[O1], target: &[O1]) -> OpenHypergraph<O2, A2>;

    /// Apply this functor to an [`OpenHypergraph`].
    /// Once `map_operation` is defined, this can be defined using
    /// [`super::dyn_functor::define_map_arrow`]
    /// as `define_map_arrow(self, f)`
    fn map_arrow(&self, f: &OpenHypergraph<O1, A1>) -> OpenHypergraph<O2, A2>;
}

pub fn define_lax_map_arrow<O1: Clone, A1, O2: Clone, A2: Clone>(
    functor: &impl Functor<O1, A1, O2, A2>,
    f: &OpenHypergraph<O1, A1>,
) -> Option<OpenHypergraph<O2, A2>> {
    // The input must be strict (no pending identifications in the quotient map).
    if !f.hypergraph.is_strict() {
        return None;
    }

    // Compute tensored operations and mapped objects
    let fx = map_operations(functor, f);
    let fw = map_objects(functor, f);

    // Below here is the same as 'spider_map_arrow' in strict functor impl
    // Steps 1: build spider maps with lax composition
    spider_map_arrow(&f, &fw, fx)
}

/// Like `map_arrow`, but also returns a relation mapping
/// nodes of the input term to nodes of the output term.
///
/// The [`IndexedCoproduct`] models this relation as a mapping from an input node to zero or more
/// output nodes (i.e., a relation)
pub fn map_arrow_witness<O1: Clone, A1, O2: Clone, A2: Clone>(
    functor: &impl Functor<O1, A1, O2, A2>,
    f: &OpenHypergraph<O1, A1>,
) -> Option<(OpenHypergraph<O2, A2>, IndexedCoproduct<FiniteFunction>)> {
    // The input must be strict (no pending identifications in the quotient map).
    if !f.hypergraph.is_strict() {
        return None;
    }

    // Compute tensored operations and mapped objects
    let fx = map_operations(functor, f);
    let fw = map_objects(functor, f);

    // Below here is the same as 'spider_map_arrow' in strict functor impl
    // Steps 1: build spider maps with lax composition
    let result = spider_map_arrow(&f, &fw, fx)?;

    // Step 2: identify the nodes of `i` within the composite.
    // Since:
    //  - we composed `(sx ; (i x fx) ; yt)`
    //  - lax follows convention of *concatenating* (so sx nodes are first, then i, then fx, then yt)
    //  - the number of nodes in sx is n = fw_flat.len()
    //  - The number of nodes in i is fw_flat.len() as well
    // The nodes of i are at the slice
    // [sx.nodes.len()..sx.nodes.len()+i.nodes.len()].
    // Which is
    // [n..n+n]
    // Then it remains to identify the segment boundaries: these are exactly the *lengths* of the
    // nested vec fw.
    // So our "identification" is the IndexedCoproduct with values the injection defined by the
    // slice above, and segment sizes the sizes of fw.
    let n: usize = fw.iter().map(|v| v.len()).sum();
    let total_result_nodes = result.hypergraph.nodes.len();

    let witness_values = FiniteFunction::new(VecArray((n..2 * n).collect()), total_result_nodes)?;
    let fw_sizes = FiniteFunction::new(VecArray(fw.iter().map(|v| v.len()).collect()), n + 1)?;
    let witness = IndexedCoproduct::new(fw_sizes, witness_values)?;

    Some((result, witness))
}

/// Lax analogue of [`crate::strict::functor::spider_map_arrow`].
///
/// Given an arrow `f`, its mapped objects `fw`, and its mapped operations `fx`,
/// build the spider decomposition `sx ; (i ⊗ fx) ; yt` and compose laxly.
fn spider_map_arrow<O1, A1, O2: Clone, A2: Clone>(
    f: &OpenHypergraph<O1, A1>,
    fw: &[Vec<O2>],
    fx: OpenHypergraph<O2, A2>,
) -> Option<OpenHypergraph<O2, A2>> {
    let fw_flat: Vec<O2> = fw.iter().flat_map(|v| v.iter().cloned()).collect();
    let fw_total = fw_flat.len();

    // Compute injections through fw segments (= map_half_spider in strict functor)
    let fs = map_half_spider(fw, &f.sources)?;
    let ft = map_half_spider(fw, &f.targets)?;

    let all_edge_sources: Vec<NodeId> = f
        .hypergraph
        .adjacency
        .iter()
        .flat_map(|adj| adj.sources.iter().copied())
        .collect();
    let all_edge_targets: Vec<NodeId> = f
        .hypergraph
        .adjacency
        .iter()
        .flat_map(|adj| adj.targets.iter().copied())
        .collect();
    let e_s = map_half_spider(fw, &all_edge_sources)?;
    let e_t = map_half_spider(fw, &all_edge_targets)?;

    // Build i, sx, yt
    let id_fn = FiniteFunction::identity(fw_total);

    let i = OpenHypergraph::<O2, A2>::identity(fw_flat.clone());
    let sx = OpenHypergraph::<O2, A2>::spider(fs, (&id_fn + &e_s)?, fw_flat.clone())?;
    let yt = OpenHypergraph::<O2, A2>::spider((&id_fn + &e_t)?, ft, fw_flat)?;

    // Compose laxly: sx ; (i ⊗ fx) ; yt
    Some(sx.lax_compose(&i.tensor(&fx))?.lax_compose(&yt)?)
}

/// Lax analogue of [`crate::strict::functor::map_half_spider`].
///
/// Given mapped objects `fw` (as nested lists) and a list of node references,
/// compute the injection through the segment structure of `fw`.
/// Each `NodeId(j)` expands to the indices of `F(w_j)` in the flattened `fw`.
fn map_half_spider<O>(fw: &[Vec<O>], node_ids: &[NodeId]) -> Option<FiniteFunction> {
    let fw_total: usize = fw.iter().map(|v| v.len()).sum();

    // Encode segment sizes as a FiniteFunction
    // (same role as fw.sources in the strict functor's IndexedCoproduct)
    let fw_sizes =
        FiniteFunction::new(VecArray(fw.iter().map(|v| v.len()).collect()), fw_total + 1)?;

    // Convert NodeIds to a FiniteFunction targeting the node set
    let node_count = fw.len();
    let f = FiniteFunction::new(VecArray(node_ids.iter().map(|n| n.0).collect()), node_count)?;

    fw_sizes.injections(&f)
}

/// Fold all the operations `x₀, x₁, ...` of an OpenHypergraph together to get their tensoring
/// `F(x₀) ● F(x₁) ● ...`
fn map_operations<O1: Clone, A1, O2: Clone, A2: Clone>(
    functor: &impl Functor<O1, A1, O2, A2>,
    f: &OpenHypergraph<O1, A1>,
) -> OpenHypergraph<O2, A2> {
    let mut result = OpenHypergraph::empty();
    for (i, a) in f.hypergraph.edges.iter().enumerate() {
        let source: Vec<O1> = f.hypergraph.adjacency[i]
            .sources
            .iter()
            .map(|nid| f.hypergraph.nodes[nid.0].clone())
            .collect();
        let target: Vec<O1> = f.hypergraph.adjacency[i]
            .targets
            .iter()
            .map(|nid| f.hypergraph.nodes[nid.0].clone())
            .collect();
        result.tensor_assign(functor.map_operation(a, &source, &target));
    }
    result
}

/// Map objects `A₀ ● A₁ ● ...` to `F(A₀) ● F(A₁) ● ...`
/// returned as a list of lists, where list i is the list of generating objects `F(A_i)`.
fn map_objects<O1, A1, O2, A2>(
    functor: &impl Functor<O1, A1, O2, A2>,
    f: &OpenHypergraph<O1, A1>,
) -> Vec<Vec<O2>> {
    f.hypergraph
        .nodes
        .iter()
        .map(|o| functor.map_object(o).collect())
        .collect()
}
