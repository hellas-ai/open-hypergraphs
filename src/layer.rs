use crate::array::*;
use crate::category::*;
use crate::finite_function::*;
use crate::indexed_coproduct::*;
use crate::open_hypergraph::*;

use num_traits::{One, Zero};

/// Compute a *layering* of an [`OpenHypergraph`]: a mapping `layer : X → K` from operations to
/// integers compatible with the partial ordering on `X` induced by hypergraph structure.
///
/// See also: the [Coffman-Graham Algorithm](https://en.wikipedia.org/wiki/Coffman%E2%80%93Graham_algorithm)
pub fn layer<K: ArrayKind, O, A>(f: &OpenHypergraph<K, O, A>) -> (FiniteFunction<K>, K::Type<bool>)
where
    K::Type<bool>: Array<K, bool>,
    K::Type<K::I>: NaturalArray<K>,
    K::Type<A>: Array<K, A>,
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
) -> (K::Index, K::Type<bool>)
where
    K::Type<bool>: Array<K, bool>,
    K::Type<K::I>: NaturalArray<K>,
{
    // The layering assignment to each node.
    // A mutable array of length n with values in {0..n}
    let mut order: K::Index = K::Index::fill(K::I::zero(), adjacency.len());

    // Predicate determining if a node has been visited
    let mut visited: K::Type<bool> = K::Type::<bool>::fill(false, adjacency.len());

    // Indegree of each node.
    let mut indegree = indegree(adjacency);

    // the set of nodes on the frontier, initialized to those with zero indegree.
    let mut frontier: K::Index = zero(&indegree);

    let mut depth = K::I::zero();
    while !frontier.is_empty() {
        // Mark nodes in the current frontier as visited
        // TODO: these are not correct! We need 'scatter_assign'?
        // visited[frontier] = true;
        scatter_constant_assign::<K, bool>(&mut visited, &frontier, true);

        // Set the order of nodes in the frontier to the current depth.
        // order[frontier] = depth;
        let mut tmp = order.into();
        scatter_constant_assign::<K, K::I>(&mut tmp, &frontier, depth.clone());
        order = tmp.into();

        // relative_indegree : N → E
        // For each node, compute the number of incoming edges from nodes in the frontier.
        let relative_indegree = relative_indegree(
            adjacency,
            &FiniteFunction::new(frontier, adjacency.len()).unwrap(),
        );

        // Decrement indegree of each adjacent node by the number of nodes in the frontier that can
        // reach it.
        indegree =
            FiniteFunction::new(indegree.table - relative_indegree.table, indegree.target).unwrap();

        // The new frontier consists of nodes with zero indegree.
        frontier = zero(&indegree);

        depth = depth + K::I::one();

        // PERFORMANCE: An alternative, more efficient implementation:
        // 1. Compute *sparse* relative indegree:
        //      - idxs of reachable nodes
        //      - counts of reachability
        // 2. Subtract using scatter_sub_assign
        //      - scatter_sub_assign::<K>(&mut indegree.table, &reachable_ix, &reachable_count.table);
        // 3. Compute frontier:
        //      - In python: `reachable_ix[indegree[reachable_ix] == 0]`
        //      - In Rust: `reachable_ix.gather(zero(indegree.gather(reachable_ix)))`
    }

    (order, visited)
}

////////////////////////////////////////////////////////////////////////////////
// Graph methods

/// Using the adjacency information in `adjacency`, compute the indegree of nodes reachable from `f`.
///
/// More formally, let `f : K → N` be a set of `K` nodes, and let `g : K' → N` be the nodes
/// reachable from `f`.
/// Then `relative_indegree(a, f)` computes the indegree of `g` in the subgraph of `a` containing
/// only edges between `f` and `g`.
///
/// # Returns
///
/// A finite function `f : N → E` denoting
///
/// # TODO
///
/// Return a *sparse* subgraph.
fn relative_indegree<K: ArrayKind>(
    adjacency: &IndexedCoproduct<K, FiniteFunction<K>>,
    f: &FiniteFunction<K>,
) -> FiniteFunction<K>
where
    K::Type<K::I>: NaturalArray<K>,
{
    // Array of
    let reached = adjacency.indexed_values(f).unwrap();

    let table = bincount::<K>(&reached.table, adjacency.len());
    FiniteFunction::new(table, adjacency.values.len()).unwrap()
}

/// Compute indegree of nodes
fn indegree<K: ArrayKind>(adjacency: &IndexedCoproduct<K, FiniteFunction<K>>) -> FiniteFunction<K>
where
    K::Type<K::I>: NaturalArray<K>,
{
    // Indegree is *relative* indegree with respect to all nodes.
    // PERFORMANCE: can compute this more efficiently by just bincounting adjacency directly.
    relative_indegree(adjacency, &FiniteFunction::<K>::identity(adjacency.len()))
}

/// Return the adjacency map for an [`OpenHypergraph`] `f`.
///
/// If `X` is the finite set of operations in `f`, then `operation_adjacency(f)` computes the
/// indexed coproduct `adjacency : X → X*`, where the list `adjacency(x)` is all operations reachable in
/// a single step from operation `x`.
fn operation_adjacency<K: ArrayKind, O, A>(
    f: &OpenHypergraph<K, O, A>,
) -> IndexedCoproduct<K, FiniteFunction<K>>
where
    K::Type<K::I>: NaturalArray<K>,
{
    f.h.s.flatmap(&converse(&f.h.t))
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
    K::Type<K::I>: NaturalArray<K>,
{
    let sources_table = bincount::<K>(&r.values.table, r.values.target.clone());

    let key = r.sources.table.segmented_arange();
    let values_table = sort_by::<K>(&r.values.table, &key);

    let sources = FiniteFunction::new(sources_table, values_table.len() + K::I::one()).unwrap();
    let values = FiniteFunction::new(values_table, r.len()).unwrap();

    IndexedCoproduct::new(sources, values).unwrap()
}

////////////////////////////////////////////////////////////////////////////////
// New array trait methods

fn sort_by<K: ArrayKind>(_k: &K::Index, _v: &K::Index) -> K::Index {
    todo!()
}

fn bincount<K: ArrayKind>(_x: &K::Index, _size: K::I) -> K::Index {
    todo!()
}

/*
// Compute `dst[ixs] -= rhs`
fn scatter_sub_assign<K: ArrayKind>(dst: &mut K::Index, ixs: &K::Index, rhs: &K::Index)
where
    K::Type<K::I>: NaturalArray<K>,
{
    todo!()
}
*/

// Numpy `dst[ixs] = arg`
fn scatter_constant_assign<K: ArrayKind, T>(_dst: &mut K::Type<T>, _ixs: &K::Index, _arg: T)
where
    K::Type<T>: Array<K, T>,
{
    todo!()
}

// Return indices of `f` which are zero.
fn zero<K: ArrayKind>(_f: &FiniteFunction<K>) -> K::Index {
    todo!()
}
