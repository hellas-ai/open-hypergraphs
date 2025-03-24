use open_hypergraphs::eval::eval;
use open_hypergraphs::prelude::*;

use core::ops::{Add, Mul};
use num_traits::{One, Zero};
use std::iter::{Product, Sum};

////////////////////////////////////////////////////////////////////////////////
// Define the theory of polynomial circuits

/// There is a single generating object in the category; thought of as a primitive type (like "Int"
/// or "Real".
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Obj;

/// Generating arrows are basic arithmetic operations with copying and discarding.
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

/// Type alias for a polynomial circuit term
pub type Term = OpenHypergraph<Obj, Arr>;

/// Trait for semiring values that can be used in polynomial circuit evaluation
pub trait Semiring: Sized + Add + Zero + Sum + Mul + One + Product + Copy {}
impl Semiring for usize {}

/// Get the type (arity and coarity) of a generating operation
pub fn arr_type(a: &Arr) -> (usize, usize) {
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

/// Turn an operations arity/coarity `n` into a *type*: the n-fold repetition of [`Obj`].
pub fn mktype(n: usize) -> SemifiniteFunction<Obj> {
    SemifiniteFunction::new(VecArray(vec![Obj; n]))
}

/// Turn an operation into an [`OpenHypergraph`] using [`OpenHypergraph::singleton`]
pub fn arr(op: Arr) -> Term {
    let (a, b) = arr_type(&op);
    OpenHypergraph::singleton(op, mktype(a), mktype(b))
}

/// Apply a single operation to its arguments.
/// For example: `apply_op(Copy, vec![1]) == vec![1, 1]`.
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

/// Apply a *list* of operations to some arguments.
/// This function can be passed directly to [`eval`] to evaluate an [`OpenHypergraph`]
pub fn apply<T: Clone + PartialEq + Semiring + Copy>(
    ops: SemifiniteFunction<Arr>,
    args: IndexedCoproduct<SemifiniteFunction<T>>,
) -> IndexedCoproduct<SemifiniteFunction<T>> {
    let args: Vec<SemifiniteFunction<T>> = args.into_iter().collect();
    let mut coargs = Vec::with_capacity(args.len());

    for (op, x) in ops.0.iter().zip(args.iter()) {
        coargs.push(apply_op(op, &x.0));
    }

    // First collect the lengths for the sources array
    let sizes: Vec<usize> = coargs.iter().map(|v| v.len()).collect();

    // Then flatten all the values for the values array
    let flat_values: Vec<T> = coargs.into_iter().flatten().collect();

    IndexedCoproduct::from_semifinite(
        SemifiniteFunction::new(VecArray(sizes)),
        SemifiniteFunction::new(VecArray(flat_values)),
    )
    .expect("Invalid IndexedCoproduct construction")
}

////////////////////////////////////////////////////////////////////////////////
// Test programs (algebraic interface)

/// A test program, which copies its input and multiplies it by itself.
/// i.e.. the function `x ⟼ x²`:
///
/// ```
/// fn square<T: Semiring>(x: T) {
///     x * x
/// }
/// ```
fn square() -> Option<Term> {
    use Arr::*;
    &arr(Copy) >> &arr(Mul)
}

////////////////////////////////////////////////////////////////////////////////
// Test programs (imperative interface)

mod imperative {
    use super::{Arr, Obj};
    use open_hypergraphs::relaxed::*;
    type Term = open_hypergraphs::relaxed::OpenHypergraph<Obj, Arr>;

    /// Creates a non-typed-annotated binary operation, and unpacks its variables.
    fn binop(state: &mut Term, x: Arr) -> ((NodeId, NodeId), NodeId) {
        let (_, (x, y)) = state.new_operation(x, vec![Obj, Obj], vec![Obj]);
        ((x[0], x[1]), y[0])
    }

    /// Creates a multiply operation, and unpacks ...
    pub fn mul(state: &mut Term) -> ((NodeId, NodeId), NodeId) {
        binop(state, Arr::Mul)
    }

    pub fn copy(state: &mut Term) -> (NodeId, (NodeId, NodeId)) {
        let (_, (x, y)) = state.new_operation(Arr::Copy, vec![Obj], vec![Obj, Obj]);
        (x[0], (y[0], y[1]))
    }

    pub fn square() -> open_hypergraphs::prelude::OpenHypergraph<Obj, Arr> {
        let mut state = OpenHypergraph::<Obj, Arr>::empty();

        let (a, (x0, x1)) = copy(&mut state);
        let ((y0, y1), b) = mul(&mut state);
        state.unify(x0, y0);
        state.unify(x1, y1);

        // set interfaces
        state.sources = vec![a];
        state.targets = vec![b];

        // build the (strict) open hypergraph
        state.quotient();
        state.to_open_hypergraph()
    }
}

fn main() {
    let f = square().unwrap();

    assert_eq!(f.source(), mktype(1));
    assert_eq!(f.target(), mktype(1));

    let inputs = VecArray(vec![3]);
    let result = eval::<VecKind, Obj, Arr, usize>(&f, inputs, apply).expect("eval failed");

    println!("3^2 = {:?}", result[0]);

    let f = imperative::square();

    assert_eq!(f.source(), mktype(1));
    assert_eq!(f.target(), mktype(1));

    let inputs = VecArray(vec![3]);
    let result = eval::<VecKind, Obj, Arr, usize>(&f, inputs, apply).expect("eval failed");

    println!("3^2 = {:?}", result[0]);
}
