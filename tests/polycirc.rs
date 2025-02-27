use open_hypergraphs::array::{vec::*, *};
use open_hypergraphs::category::*;
use open_hypergraphs::open_hypergraph::*;
use open_hypergraphs::semifinite::*;

////////////////////////////////////////////////////////////////////////////////
// Define the theory of polynomial circuits

// There is only one generating object
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Obj;

// Generating arrows are basic arithmetic operations with copying and discarding
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Arr {
    Add,
    Zero,
    Mul,
    One,
    Copy,
    Discard,
}

////////////////////////////////////////////////////////////////////////////////
// Helper types and functions

type Term = OpenHypergraph<VecKind, Obj, Arr>;

// Get the type (arity and coarity) of a generating operation
fn arr_type(a: &Arr) -> (usize, usize) {
    use Arr::*;
    match a {
        Add => (2, 1),
        Zero => (0, 1),
        Mul => (2, 1),
        One => (0, 1),
        Copy => (1, 2),
        Discard => (1, 0),
    }
}

fn mktype(n: usize) -> SemifiniteFunction<VecKind, Obj> {
    SemifiniteFunction(VecArray(vec![Obj; n]))
}

// Turn an operation into an OpenHypergraph using `singleton`
fn arr(op: Arr) -> Term {
    let (a, b) = arr_type(&op);
    OpenHypergraph::singleton(op, mktype(a), mktype(b))
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

////////////////////////////////////////////////////////////////////////////////
// Evaluation

use core::fmt::Debug;
use core::ops::{Add, Mul};
use num_traits::{One, Zero};
use open_hypergraphs::finite_function::*;
use open_hypergraphs::indexed_coproduct::*;
// TODO
//use open_hypergraphs::layer::{converse, layer};
use std::iter::{Product, Sum};

trait Semiring: Sized + Add + Zero + Sum + Mul + One + Product + Copy {}

impl Semiring for usize {}

// TODO: is there a more data-parallel-friendly interface?
fn to_slices(c: &IndexedCoproduct<VecKind, FiniteFunction<VecKind>>) -> Vec<&[usize]> {
    let ptr = c.sources.table.cumulative_sum();
    let mut result = Vec::with_capacity(c.len());
    for i in 0..c.len() {
        result.push(&c.values.table[ptr[i]..ptr[i + 1]])
    }
    result
}

/*
fn eval<T: Semiring + Clone + Default>(f: &Term) -> Option<Vec<T>> {
    // Get layering
    let (order, unvisited) = layer(&f);
    if unvisited.any() {
        return None;
    } else {
    }
}
*/

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
    let sources: Vec<&[usize]> = to_slices(&f.h.s);
    let targets: Vec<&[usize]> = to_slices(&f.h.t);

    //
    for ops in layer.iter() {
        for i in ops {
            // look up the op at this index
            let op = &f.h.x.0[*i];

            // NOTE: we don't bother looking up source/target type, since there's only one object!
            let output_values = apply(op, &mem.gather(sources[*i]));

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

    // TODO: compute layers manually!
    let (_, result) = eval_layers::<usize>(&f, vec![3], &[vec![0], vec![1]]);

    // 3**2
    assert_eq!(result.len(), 1);
    assert_eq!(result[0], 9);
}
