use open_hypergraphs::array::vec::*;
use open_hypergraphs::category::*;
use open_hypergraphs::indexed_coproduct::*;
use open_hypergraphs::semifinite::*;
use open_hypergraphs::strict::eval::*;

use crate::theory::polycirc::*;

/// Apply a single operation to its arguments.
pub fn apply_op<T: Semiring + Copy>(op: &Arr, args: &[T]) -> Vec<T> {
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

// Apply a list of operations to some arguments.
pub fn apply<T: Clone + PartialEq + Semiring + Copy>(
    ops: SemifiniteFunction<VecKind, Arr>,
    args: IndexedCoproduct<VecKind, SemifiniteFunction<VecKind, T>>,
) -> IndexedCoproduct<VecKind, SemifiniteFunction<VecKind, T>> {
    let args: Vec<SemifiniteFunction<VecKind, T>> = args.into_iter().collect();
    let mut coargs = Vec::with_capacity(args.len());

    for (op, x) in ops.0.iter().zip(args.iter()) {
        coargs.push(apply_op(op, &x.0));
    }

    // First collect the lengths for the sources array
    let sizes: Vec<usize> = coargs.iter().map(|v| v.len()).collect();

    // Then flatten all the values for the values array
    let flat_values: Vec<T> = coargs.into_iter().flatten().collect();

    IndexedCoproduct::from_semifinite(
        SemifiniteFunction(VecArray(sizes)),
        SemifiniteFunction(VecArray(flat_values)),
    )
    .expect("Invalid IndexedCoproduct construction")
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

    let inputs = VecArray(vec![3]);
    let result = eval::<VecKind, Obj, Arr, usize>(&f, inputs, apply).expect("eval failed");

    // 3**2
    assert_eq!(result, VecArray(vec![9]));
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
    let result =
        eval::<VecKind, Obj, Arr, usize>(&g, VecArray(vec![3, 4]), apply).expect("eval failed");

    // 3**2, 4**2
    assert_eq!(result, VecArray(vec![9, 16]));
}
