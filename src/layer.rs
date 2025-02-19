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
pub fn layer<K: ArrayKind, O, A>(f: &OpenHypergraph<K, O, A>) -> (FiniteFunction<K>, K::Type<bool>)
where
    K::Type<bool>: MutArray<K, bool>,
    K::Type<A>: Array<K, A>,
    K::Type<K::I>: NaturalArray<K> + MutArray<K, K::I> + Sparse<K> + Debug,
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
) -> (K::Index, K::Type<bool>)
where
    K::Type<bool>: MutArray<K, bool>,
    K::Type<K::I>: NaturalArray<K> + MutArray<K, K::I> + Sparse<K>,
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
        visited.scatter_assign_constant(&frontier, true);

        // Set the order of nodes in the frontier to the current depth.
        // order[frontier] = depth;
        let mut tmp = order.into();
        tmp.scatter_assign_constant(&frontier, depth.clone());
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
    K::Type<K::I>: NaturalArray<K> + Sparse<K>,
{
    // Array of
    let reached = adjacency.indexed_values(f).unwrap();

    let table = (reached.table.as_ref() as &K::Type<K::I>).bincount(adjacency.len());
    FiniteFunction::new(table, adjacency.values.len()).unwrap()
}

/// Compute indegree of nodes
fn indegree<K: ArrayKind>(adjacency: &IndexedCoproduct<K, FiniteFunction<K>>) -> FiniteFunction<K>
where
    K::Type<K::I>: NaturalArray<K> + Sparse<K>,
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
pub fn operation_adjacency<K: ArrayKind, O, A>(
    f: &OpenHypergraph<K, O, A>,
) -> IndexedCoproduct<K, FiniteFunction<K>>
where
    K::Type<K::I>: NaturalArray<K> + Sparse<K> + Debug,
    K::Index: Debug,
    K::Type<O>: Debug,
    K::Type<A>: Debug,
{
    let c = converse(&f.h.s);
    c.map_indexes(&f.h.t.values).unwrap()
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
    K::Type<K::I>: Sparse<K> + Debug,
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
// New array trait methods

// FiniteFunction helpers
fn zero<K: ArrayKind>(f: &FiniteFunction<K>) -> K::Index
where
    K::Type<K::I>: Sparse<K>,
{
    (f.table.as_ref() as &K::Type<K::I>).zero()
}

pub trait Sparse<K: ArrayKind>: NaturalArray<K> {
    fn bincount(&self, _size: K::I) -> K::Index;

    // Return indices of `f` which are zero.
    fn zero(&self) -> K::Index;
}

pub trait MutArray<K: ArrayKind, T>: Array<K, T> {
    // Numpy `self[ixs] = arg`
    fn scatter_assign_constant(&mut self, _ixs: &K::Index, _arg: T);

    // Compute `self[ixs] -= rhs`
    //fn scatter_sub_assign<K: ArrayKind>(&self, ixs: &K::Index, rhs: &K::Index);
}

////////////////////////////////////////////////////////////////////////////////
// VecArray impl

use crate::array::vec::*;

impl Sparse<VecKind> for VecArray<usize> {
    fn bincount(&self, size: usize) -> VecArray<usize> {
        let mut counts = vec![0; size];
        for &idx in self.iter() {
            counts[idx] += 1;
        }
        VecArray(counts)
    }

    fn zero(&self) -> VecArray<usize> {
        let mut zero_indices = Vec::with_capacity(self.len());
        for (i, &val) in self.iter().enumerate() {
            if val == 0 {
                zero_indices.push(i);
            }
        }
        VecArray(zero_indices)
    }
}

impl<T: Clone + PartialEq> MutArray<VecKind, T> for VecArray<T> {
    fn scatter_assign_constant(&mut self, ixs: &VecArray<usize>, arg: T) {
        for &idx in ixs.iter() {
            self[idx] = arg.clone();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::array::vec::*;
    use crate::finite_function::*;
    use crate::indexed_coproduct::*;
    use crate::layer::{converse, layer, operation_adjacency, MutArray, Sparse};
    use crate::open_hypergraph::*;
    use crate::semifinite::*;

    #[derive(Clone, PartialEq, Debug)]
    pub enum Arr {
        F,
        G,
    }

    #[derive(Clone, PartialEq, Debug)]
    pub enum Obj {
        A,
        B,
    }

    ////////////////////////////////////////
    // Main methods

    #[test]
    fn test_converse() {
        let sources = SemifiniteFunction::<VecKind, usize>(VecArray(vec![2, 0, 2]));
        let values = FiniteFunction::new(VecArray(vec![4, 4, 0, 1]), 5).unwrap();
        let c = IndexedCoproduct::from_semifinite(sources, values).unwrap();

        let actual = converse(&c);

        let sources = SemifiniteFunction::<VecKind, usize>(VecArray(vec![1, 1, 0, 0, 2]));
        let values = FiniteFunction::new(VecArray(vec![2, 2, 0, 0]), 3).unwrap();
        let expected = IndexedCoproduct::from_semifinite(sources, values).unwrap();

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_operation_adjacency() {
        use Arr::*;
        use Obj::*;

        let x = SemifiniteFunction(VecArray(vec![A, A]));
        let y = SemifiniteFunction(VecArray(vec![A]));

        // There are no operations adjacent to f
        //      ┌───┐
        // ●────│   │
        //      │ f │────●
        // ●────│   │
        //      └───┘
        let f = OpenHypergraph::<VecKind, _, _>::singleton(F, x.clone(), y.clone());
        let result = operation_adjacency::<VecKind, Obj, Arr>(&f);
        assert_eq!(result.sources.table, VecArray(vec![0]));
        assert_eq!(result.values.table, VecArray(vec![]));

        //      ┌───┐
        // ●────│ g │────┐    ┌───┐
        //      └───┘    ●────│   │
        //                    │ f │────●
        //      ┌───┐    ●────│   │
        // ●────│ g │────┘    └───┘
        //      └───┘
        let g = OpenHypergraph::singleton(G, y.clone(), y.clone());
        let h = (&(&g | &g) >> &f).unwrap();
        // f_op (id 1) is adjacent to f, but not vice-versa.
        let result = operation_adjacency::<VecKind, Obj, Arr>(&h);

        assert_eq!(result.sources.table, VecArray(vec![1, 1, 0]));
        assert_eq!(result.values.table, VecArray(vec![2, 2]));

        // the composition
        //
        //      ┌───┐     ┌───┐
        // ●────│   │     │   │────●
        //      │ f │──●──│ f │
        // ●────│   │     │   │────●
        //      └───┘     └───┘
        //
        // TODO: FIX BUG! Probably because converse of *sources* has 3 elements and we're not
        // computing sources of result properly.
        let f_op = OpenHypergraph::singleton(F, y.clone(), x.clone());
        let h = (&f >> &f_op).unwrap();
        let result = operation_adjacency(&h);
        dbg!(&h.h.x);
        assert_eq!(result.sources.table, VecArray(vec![1, 0]));
        assert_eq!(result.values.table, VecArray(vec![1]));
    }

    #[test]
    fn test_layer_simple() {
        use Arr::*;
        use Obj::*;

        let x = SemifiniteFunction(VecArray(vec![A, A]));
        let y = SemifiniteFunction(VecArray(vec![A]));
        let f = OpenHypergraph::<VecKind, _, _>::singleton(F, x, y);

        layer::<VecKind, Obj, Arr>(&f);
    }

    ////////////////////////////////////////
    // Array method tests

    #[test]
    fn test_scatter_assign_constant() {
        let mut actual = VecArray(vec![0, 1, 2, 3, 4, 5]);
        let i = VecArray(vec![0, 2, 4]);
        actual.scatter_assign_constant(&i, 10);
        let expected = VecArray(vec![10, 1, 10, 3, 10, 5]);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_zero() {
        let x = VecArray(vec![0, 1, 0, 2, 0, 3]);
        let actual = x.zero();
        let expected = VecArray(vec![0, 2, 4]);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_bincount() {
        let input = VecArray(vec![0, 3, 1, 3, 0, 3, 3]);
        let actual = input.bincount(4);
        let expected = VecArray(vec![2, 1, 0, 4]);
        assert_eq!(actual, expected);

        // Test with empty array
        let empty = VecArray(vec![]);
        let actual_empty = empty.bincount(3);
        let expected_empty = VecArray(vec![0, 0, 0]);
        assert_eq!(actual_empty, expected_empty);

        // Test with single element
        let single = VecArray(vec![1]);
        let actual_single = single.bincount(2);
        let expected_single = VecArray(vec![0, 1]);
        assert_eq!(actual_single, expected_single);
    }
}
