use open_hypergraphs::{
    array::{vec::*, *},
    category::*,
    finite_function::*,
    indexed_coproduct::*,
    layer::*,
};

use core::fmt::Debug;

use crate::theory::polycirc::*;

////////////////////////////////////////////////////////////////////////////////
// Custom eval code - not using library eval.
// Makes sure we test *layering* code.

// TODO: is there a more data-parallel-friendly interface?
fn to_slices(c: &IndexedCoproduct<VecKind, FiniteFunction<VecKind>>) -> Vec<Vec<usize>> {
    let ptr = c.sources.table.cumulative_sum();
    let mut result = Vec::with_capacity(c.len());
    for i in 0..c.len() {
        result.push(c.values.table[ptr[i]..ptr[i + 1]].to_vec())
    }
    result
}

fn layer_function_to_layers(f: FiniteFunction<VecKind>) -> Vec<Vec<usize>> {
    let c = converse(&IndexedCoproduct::elements(f));
    to_slices(&c)
}

fn apply<T: Semiring + Copy>(op: &Arr, args: &Vec<T>) -> Vec<T> {
    use Arr::*;
    match op {
        Add => vec![args.iter().copied().sum()],
        Zero => vec![T::zero()],
        Mul => vec![args.iter().copied().product()],
        One => vec![T::one()],
        Copy => vec![args[0], args[0]],
        Discard => vec![],
    }
}

fn eval<T: Semiring + PartialEq + Clone + Default + Debug>(
    f: &Term,
    inputs: Vec<T>,
) -> Option<Vec<T>> {
    // Get layering
    let (order, unvisited) = layer(f);
    let layering = &layer_function_to_layers(order);

    if unvisited.0.contains(&1) {
        None
    } else {
        let (_, outputs) = eval_layers(f, inputs, layering);
        Some(outputs)
    }
}

/// Given an [`OpenHypergraph`] `f` and a relation `layer : X → K` where `K <= X`,
/// evaluate the
fn eval_layers<T: Semiring + Clone + PartialEq + Default + Debug>(
    f: &Term,
    inputs: Vec<T>,
    // "level sets" of operations: evaluating f level-by-level will always result in
    layer: &[Vec<usize>],
) -> (Vec<T>, Vec<T>) {
    // Create mutable memory for all wires in diagram.
    // Write supplied inputs to named input wires.
    let mut mem = VecArray(inputs).scatter(&f.s.table, f.h.w.len());

    // Indices of sources/targets for each operation
    let sources = to_slices(&f.h.s);
    let targets = to_slices(&f.h.t);

    //
    for ops in layer.iter() {
        for i in ops {
            // look up the op at this index
            let op = &f.h.x.0[*i];

            // NOTE: we don't bother looking up source/target type, since there's only one object!
            let output_values = apply(op, &mem.gather(&sources[*i]));

            // TODO: add a scatter_assign method
            for (target_ix, value) in targets[*i].iter().zip(output_values) {
                mem[*target_ix] = value;
            }
        }
    }

    // Get values of all outputs wires, in order.
    let outputs = mem.gather(&f.t.table);

    (mem.0, outputs.to_vec())
}

////////////////////////////////////////////////////////////////////////////////
// Test programs

fn square() -> Option<Term> {
    use Arr::*;
    &arr(Copy) >> &arr(Mul)
}

////////////////////////////////////////////////////////////////////////////////
// Tests

#[test]
fn test_square() {
    let f = square().unwrap();

    assert_eq!(f.source(), mktype(1));
    assert_eq!(f.target(), mktype(1));

    //let (_, result) = eval_layers::<usize>(&f, vec![3], &[vec![0], vec![1]]);
    let result = eval::<usize>(&f, vec![3]).expect("eval failed");

    // 3**2
    assert_eq!(result, vec![9]);
}

#[test]
fn test_parallel_squares() {
    // ----[square]-----
    //
    // ----[square]-----
    let f = square().unwrap();
    let g = &f | &f;

    assert_eq!(f.source(), mktype(1));
    assert_eq!(f.target(), mktype(1));

    //let (_, result) = eval_layers::<usize>(&g, vec![3, 4], &[vec![0, 2], vec![1, 3]]);
    let result = eval::<usize>(&g, vec![3, 4]).expect("eval failed");

    // 3**2, 4**2
    assert_eq!(result, vec![9, 16]);
}
