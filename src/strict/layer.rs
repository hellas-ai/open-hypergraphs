//! A [Coffman-Graham](https://en.wikipedia.org/wiki/Coffman%E2%80%93Graham_algorithm)-inspired
//! layering algorithm.
use crate::array::*;
use crate::finite_function::*;

use crate::strict::graph;
use crate::strict::graph::converse_iter;
use crate::strict::open_hypergraph::*;

/// Compute a *layering* of an [`OpenHypergraph`]: a mapping `layer : X → K` from operations to
/// integers compatible with the partial ordering on `X` induced by hypergraph structure.
///
/// See also: the [Coffman-Graham Algorithm](https://en.wikipedia.org/wiki/Coffman%E2%80%93Graham_algorithm)
///
/// # Returns
///
/// - A finite function `layer : X → L` assigning a *layer* to each operation in the set `X`
/// - An array of *flags* for each operation determining if it was visited in the layering. If any
///   operation was unvisited, the hypergraph was not monogamous acyclic.
pub fn layer<K: ArrayKind, O, A>(f: &OpenHypergraph<K, O, A>) -> (FiniteFunction<K>, K::Type<K::I>)
where
    K::Type<A>: Array<K, A>,
    K::Type<K::I>: NaturalArray<K>,
{
    let a = graph::operation_adjacency(f);
    let (ordering, completed) = graph::kahn(&a);
    (
        FiniteFunction::new(ordering, f.h.x.0.len()).unwrap(),
        completed,
    )
}

/// Given an [`OpenHypergraph`], compute a layering of its operations as a finite function `X → L`,
/// then return this as an array-of-arrays `r`.
///
/// # Returns
///
/// - A `Vec` of arrays `r`, where `r[i]` is the array of operations in layer `i`.
/// - An array of unvisited nodes, as in [`layer`]
pub fn layered_operations<K: ArrayKind, O, A>(
    f: &OpenHypergraph<K, O, A>,
) -> (Vec<K::Index>, K::Index)
where
    K::Type<A>: Array<K, A>,
    K::Type<K::I>: NaturalArray<K>,
    K::I: Into<usize>,
{
    let (order, unvisited) = layer(f);
    (converse_iter(order).collect(), unvisited.into())
}
