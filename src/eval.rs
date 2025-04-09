//! An array-backend-agnostic evaluator
//!
use crate::array::*;
use crate::finite_function::*;
use crate::indexed_coproduct::*;
use crate::layer::{converse, layer};
use crate::open_hypergraph::*;
use crate::semifinite::*;

use num_traits::Zero;
use std::default::Default;

// Given a "layering function" `f : N → K` which maps each operation `n ∈ N` into some layer `k ∈
// K`,
// return the converse relation `r : K → N*` giving the list of operations in each layer as a list
// of `FiniteFunction`.
fn layer_function_to_layers<K: ArrayKind>(f: FiniteFunction<K>) -> Vec<FiniteFunction<K>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::I: Into<usize> + From<usize>,
{
    let c = converse(&IndexedCoproduct::elements(f));
    c.into_iter().collect()
}

/// Evaluate an acyclic open hypergraph `f` thought of as a function using some specified input
/// values `s`, and a function `apply` which maps a list of operations and their inputs to their
/// outputs.
pub fn eval<K: ArrayKind, O, A, T: Default>(
    f: &OpenHypergraph<K, O, A>,
    s: K::Type<T>,
    apply: impl Fn(
        SemifiniteFunction<K, A>,
        IndexedCoproduct<K, SemifiniteFunction<K, T>>,
    ) -> IndexedCoproduct<K, SemifiniteFunction<K, T>>,
) -> Option<K::Type<T>>
where
    K::I: Into<usize> + From<usize>,
    K::Type<K::I>: NaturalArray<K>,
    K::Type<T>: Array<K, T>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    let (order, unvisited) = layer(f);
    let layering = layer_function_to_layers(order);

    // Check that max of 'unvisited' is 0: i.e., no unvisited nodes.
    // TODO: this has to evaluate the whole array, when it could just use 'any'.
    if unvisited.max().unwrap_or(K::I::zero()) == K::I::zero() {
        let (_, outputs) = eval_order(f, s, layering, apply);
        Some(outputs)
    } else {
        None
    }
}

// Evaluate an acyclic open hypergraph using a specified order of operations.
fn eval_order<K: ArrayKind, O, A, T: Default>(
    // The term to evaluate
    f: &OpenHypergraph<K, O, A>,
    // Source wire inputs
    s: K::Type<T>,
    // A chosen order of operations
    // TODO: this should be an *iterator* over arrays?
    //order: &IndexedCoproduct<K, FiniteFunction<K>>,
    order: Vec<FiniteFunction<K>>,
    apply: impl Fn(
        SemifiniteFunction<K, A>,
        IndexedCoproduct<K, SemifiniteFunction<K, T>>,
    ) -> IndexedCoproduct<K, SemifiniteFunction<K, T>>,
) -> (K::Type<T>, K::Type<T>)
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<T>: Array<K, T>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    // Create memory prefilled with default data
    let mut mem: SemifiniteFunction<K, T> =
        SemifiniteFunction::new(K::Type::<T>::fill(T::default(), f.h.w.len()));

    // Overwrite input locations with values in s.
    mem.0.scatter_assign(&f.s.table, s);

    for op_ix in order {
        // Compute *labels* of operations to pass to `apply`.
        let op_labels = (&op_ix >> &f.h.x).unwrap();

        // Get the wire indices and values which are inputs to the operations in op_ix.
        let input_indexes = f.h.s.map_indexes(&op_ix).unwrap();
        let input_values = input_indexes.map_semifinite(&mem).unwrap();

        // Compute an IndexedCoproduct of output values.
        let outputs = apply(op_labels, input_values);

        let output_indexes = f.h.t.map_indexes(&op_ix).unwrap();

        // write outputs to memory
        mem.0
            .scatter_assign(&output_indexes.values.table, outputs.values.0);

        // TODO: evaluate all 'ops' in parallel using a user-supplied function
    }
    let outputs = mem.0.gather(f.t.table.get_range(..));
    (mem.0, outputs)
}
