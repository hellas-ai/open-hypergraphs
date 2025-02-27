//! A [Coffman-Graham](https://en.wikipedia.org/wiki/Coffman%E2%80%93Graham_algorithm)-inspired
//! layering algorithm.
use crate::array::*;
use crate::category::*;
use crate::finite_function::*;
use crate::indexed_coproduct::*;
use crate::open_hypergraph::*;

use num_traits::{One, Zero};

use std::fmt::Debug;

/// Compute a *layering* of an [`OpenHypergraph`]: a mapping `layer : X → K` from operations to
/// integers compatible with the partial ordering on `X` induced by hypergraph structure.
///
/// See also: the [Coffman-Graham Algorithm](https://en.wikipedia.org/wiki/Coffman%E2%80%93Graham_algorithm)
pub fn layer<K: ArrayKind, O, A>(f: &OpenHypergraph<K, O, A>) -> (FiniteFunction<K>, K::Type<K::I>)
where
    K::Type<A>: Array<K, A>,
    K::Type<K::I>: NaturalArray<K> + Debug,
    K::Index: Debug,
    K::Type<O>: Debug,
    K::Type<A>: Debug,
{
    let a = operation_adjacency(f);
    let (ordering, completed) = kahn(&a);
    (
        FiniteFunction::new(ordering, f.h.x.0.len()).unwrap(),
        completed,
    )
}

/// A kahn-ish algorithm for topological sorting of an adjacency relation, encoded as an
/// [`IndexedCoproduct`] (see [`converse`] for details)
fn kahn<K: ArrayKind>(
    adjacency: &IndexedCoproduct<K, FiniteFunction<K>>,
) -> (K::Index, K::Type<K::I>)
where
    K::Type<K::I>: NaturalArray<K> + Debug,
    K::Index: Debug,
{
    // The layering assignment to each node.
    // A mutable array of length n with values in {0..n}
    let mut order: K::Type<K::I> = K::Type::<K::I>::fill(K::I::zero(), adjacency.len());

    // Predicate determining if a node has been visited.
    // 1 = unvisited
    // 0 = visited
    // NOTE: we store this as "NOT visited" so we can efficiently filter using "repeat".
    let mut unvisited: K::Type<K::I> = K::Type::<K::I>::fill(K::I::one(), adjacency.len());

    // Indegree of each node.
    let mut indegree = indegree(adjacency);

    // the set of nodes on the frontier, initialized to those with zero indegree.
    let mut frontier: K::Index = zero(&indegree);

    // Loop until frontier is empty, or at max possible layering depth.
    let mut depth = K::I::zero();

    // Implementation outline:
    // 1. Compute *sparse* relative indegree, which is:
    //      - idxs of reachable nodes
    //      - counts of reachability from a given set
    // 2. Subtract from global indegree array using scatter_sub_assign
    //      - scatter_sub_assign::<K>(&mut indegree.table, &reachable_ix, &reachable_count.table);
    // 3. Compute new frontier:
    //      - Numpy-esque: `reachable_ix[indegree[reachable_ix] == 0 && unvisited[reachable_ix]]`
    while !frontier.is_empty() && depth <= adjacency.len() {
        // Mark nodes in the current frontier as visited
        // unvisited[frontier] = 0;
        unvisited.scatter_assign_constant(&frontier, K::I::zero());

        // Set the order of nodes in the frontier to the current depth.
        // order[frontier] = depth;
        order.scatter_assign_constant(&frontier, depth.clone());

        // For each node, compute the number of incoming edges from nodes in the frontier,
        // and count paths to each.
        let (reachable_ix, reachable_count) = sparse_relative_indegree(
            adjacency,
            &FiniteFunction::new(frontier, adjacency.len()).unwrap(),
        );

        // indegree = indegree - dense_relative_indegree(a, f)
        indegree
            .table
            .as_mut()
            .scatter_sub_assign(&reachable_ix.table, &reachable_count.table);

        // Reachable nodes with zero indegree...
        // frontier = reachable_ix[indegree[reachable_ix] == 0]
        frontier = {
            // *indices* i of reachable_ix such that indegree[reachable_ix[i]] == 0
            let reachable_ix_indegree_zero_ix = indegree
                .table
                .gather(reachable_ix.table.get_range(..))
                .zero();

            // only nodes in reachable_ix with indegree 0
            reachable_ix
                .table
                .gather(reachable_ix_indegree_zero_ix.get_range(..))
        };

        // .. and filter out those which have been visited.
        // frontier = frontier[unvisited[frontier]]
        frontier = filter::<K>(
            &frontier,
            &unvisited.as_ref().gather(frontier.get_range(..)),
        );

        // Increment depth
        depth = depth + K::I::one();
    }

    (order.into(), unvisited)
}

/// Given:
///
/// - `values : K → N`
/// - `predicate : K → 2`
///
/// Return the subset of `values` for which `predicate(i) = 1`
fn filter<K: ArrayKind>(values: &K::Index, predicate: &K::Index) -> K::Index {
    predicate.repeat(values.get_range(..))
}

/// Given an array of indices `values` in `{0..N}` and a predicate `N → 2`, select select values `i` for
/// which `predicate(i) = 1`.
#[allow(dead_code)]
fn filter_by_dense<K: ArrayKind>(values: &K::Index, predicate: &K::Index) -> K::Index
where
    K::Type<K::I>: NaturalArray<K>,
{
    predicate
        .gather(values.get_range(..))
        .repeat(values.get_range(..))
}

////////////////////////////////////////////////////////////////////////////////
// Graph methods

/// Using the adjacency information in `adjacency`, compute the indegree of all nodes reachable from `f`.
///
/// More formally, let:
///
/// - `a : Σ_{n ∈ A} s(n) → N` denote the adjacency information of each
/// - `f : K → N` be a subset of `K` nodes
///
/// Then `sparse_relative_indegree(a, f)` computes:
///
/// - `g : R → N`, the subset of (R)eachable nodes reachable from `f`
/// - `i : R → E+1`, the *indegree* of nodes in `R`.
///
fn sparse_relative_indegree<K: ArrayKind>(
    a: &IndexedCoproduct<K, FiniteFunction<K>>,
    f: &FiniteFunction<K>,
) -> (FiniteFunction<K>, FiniteFunction<K>)
where
    K::Type<K::I>: NaturalArray<K> + Debug,
    K::Index: Debug,
{
    // Must have that the number of nodes `adjacency.len()`
    assert_eq!(a.len(), f.target());

    // Indices of operations reachable from those in the set f.
    // Indices may appear more than once.
    let g = a.indexed_values(f).unwrap();
    let (i, c) = g.table.sparse_bincount();
    let target = a.len() + K::I::one();

    (
        FiniteFunction::new(i, a.len()).unwrap(),
        FiniteFunction::new(c, target).unwrap(),
    )
}

/// Using the adjacency information in `adjacency`, compute the indegree of all nodes reachable from `f`.
///
/// More formally, define:
///
/// ```text
/// a : Σ_{n ∈ A} s(n) → N  // the adjacency information of each
/// f : K → N               // a subset of `K` nodes
/// ```
///
/// Then `dense_relative_indegree(a, f)` computes the indegree from `f` of all `N` nodes.
///
/// # Returns
///
/// A finite function `N → E+1` denoting indegree of each node in `N` relative to `f`.
fn dense_relative_indegree<K: ArrayKind>(
    adjacency: &IndexedCoproduct<K, FiniteFunction<K>>,
    f: &FiniteFunction<K>,
) -> FiniteFunction<K>
where
    K::Type<K::I>: NaturalArray<K> + Debug,
    K::Index: Debug,
{
    // Must have that the number of nodes `adjacency.len()`
    assert_eq!(adjacency.len(), f.target());

    // Operations reachable from those in the set f.
    let reached = adjacency.indexed_values(f).unwrap();

    // target is +1 because all edges could point to the same operation, so its indegree will be
    // adjacency.len().
    let target = adjacency.len() + K::I::one();
    let table = (reached.table.as_ref() as &K::Type<K::I>).bincount(adjacency.len());
    FiniteFunction::new(table, target).unwrap()
}

/// Compute indegree of all nodes in a multigraph.
pub fn indegree<K: ArrayKind>(
    adjacency: &IndexedCoproduct<K, FiniteFunction<K>>,
) -> FiniteFunction<K>
where
    K::Type<K::I>: NaturalArray<K> + Debug,
    K::Index: Debug,
{
    // Indegree is *relative* indegree with respect to all nodes.
    // PERFORMANCE: can compute this more efficiently by just bincounting adjacency directly.
    dense_relative_indegree(adjacency, &FiniteFunction::<K>::identity(adjacency.len()))
}

/// Return the adjacency map for an [`OpenHypergraph`] `f`.
///
/// If `X` is the finite set of operations in `f`, then `operation_adjacency(f)` computes the
/// indexed coproduct `adjacency : X → X*`, where the list `adjacency(x)` is all operations reachable in
/// a single step from operation `x`.
pub fn operation_adjacency<K: ArrayKind, O, A>(
    f: &OpenHypergraph<K, O, A>,
) -> IndexedCoproduct<K, FiniteFunction<K>>
where
    K::Type<K::I>: NaturalArray<K> + Debug,
    K::Index: Debug,
    K::Type<O>: Debug,
    K::Type<A>: Debug,
{
    f.h.t.flatmap(&converse(&f.h.s))
}

/// Compute the *converse* of an [`IndexedCoproduct`] thought of as a "multirelation".
///
/// An [`IndexedCoproduct`] `c : Σ_{x ∈ X} s(x) → Q` can equivalently be thought of as `c : X →
/// Q*`, i.e. a mapping from X to finite lists of elements in Q.
///
/// Such a list defines a (multi-)relation as the multiset of pairs
///
/// `R = { ( x, f(x)_i ) | x ∈ X, i ∈ len(f(x)) }`
///
/// This function computes the *converse* of that relation as an indexed coproduct
/// `converse(c) : Q → X*`, or more precisely
/// `converse(c) : Σ_{q ∈ Q} s(q) → X`.
///
/// NOTE: An indexed coproduct does not uniquely represent a 'multirelation', since *order* of the
/// elements matters.
/// The result of this function is only unique up to permutation of the sublists.
pub fn converse<K: ArrayKind>(
    r: &IndexedCoproduct<K, FiniteFunction<K>>,
) -> IndexedCoproduct<K, FiniteFunction<K>>
where
    K::Type<K::I>: NaturalArray<K> + Debug,
    K::Index: Debug,
{
    // Create the 'values' array of the resulting [`IndexedCoproduct`]
    // Sort segmented_arange(r.sources.table) by the *values* of r.
    let values_table = {
        let arange = K::Index::arange(&K::I::zero(), &r.sources.len());
        let unsorted_values = r.sources.table.repeat(arange.get_range(..));
        unsorted_values.sort_by(&r.values.table)
    };

    // Create the "sources" array of the result
    let sources_table =
        (r.values.table.as_ref() as &K::Type<K::I>).bincount(r.values.target.clone());

    let sources = FiniteFunction::new(sources_table, r.values.table.len() + K::I::one()).unwrap();
    let values = FiniteFunction::new(values_table, r.len()).unwrap();

    IndexedCoproduct::new(sources, values).unwrap()
}

////////////////////////////////////////////////////////////////////////////////
// Array trait helpers

// FiniteFunction helpers
fn zero<K: ArrayKind>(f: &FiniteFunction<K>) -> K::Index
where
    K::Type<K::I>: NaturalArray<K>,
{
    (f.table.as_ref() as &K::Type<K::I>).zero()
}
