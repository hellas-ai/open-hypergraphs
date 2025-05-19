use open_hypergraphs::array::vec::*;
use open_hypergraphs::semifinite::*;
use open_hypergraphs::strict::open_hypergraph::*;

use core::ops::{Add, Mul};
use num_traits::{One, Zero};
use std::iter::{Product, Sum};

pub trait Semiring: Sized + Add + Zero + Sum + Mul + One + Product + Copy {}
impl Semiring for usize {}

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

pub type Term = OpenHypergraph<VecKind, Obj, Arr>;

// Get the type (arity and coarity) of a generating operation
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

pub fn mktype(n: usize) -> SemifiniteFunction<VecKind, Obj> {
    SemifiniteFunction(VecArray(vec![Obj; n]))
}

// Turn an operation into an OpenHypergraph using `singleton`
pub fn arr(op: Arr) -> Term {
    let (a, b) = arr_type(&op);
    OpenHypergraph::singleton(op, mktype(a), mktype(b))
}
