use open_hypergraphs::array::vec::*;
use open_hypergraphs::category::*;
use open_hypergraphs::eval::*;
use open_hypergraphs::lax;

use crate::theory::polycirc::*;

use crate::eval::eval::apply;

pub type LaxTerm = lax::OpenHypergraph<Obj, Arr>;

fn lax_arr(op: Arr) -> LaxTerm {
    use open_hypergraphs::prelude::OpenHypergraph;
    let (a, b) = arr_type(&op);
    let f = OpenHypergraph::singleton(op, mktype(a), mktype(b));
    lax::OpenHypergraph::from_strict(f)
}

////////////////////////////////////////////////////////////////////////////////
// Test programs

// NOTE: this is really testing *composition* of lax open hypergraphs: note how lax_arr creates a
// LaxTerm, not a Term.
fn square() -> Option<Term> {
    use Arr::*;
    let lhs = lax_arr(Copy);
    let rhs = lax_arr(Mul);
    let term = (&lhs >> &rhs)?;
    Some(term.to_open_hypergraph())
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
