use open_hypergraphs::array::vec::*;
use open_hypergraphs::lax;
use open_hypergraphs::strict::eval::*;

use crate::theory::polycirc::*;

use crate::eval::eval::apply;

pub type LaxTerm = lax::OpenHypergraph<Obj, Arr>;

fn lax_arr(op: Arr) -> LaxTerm {
    use open_hypergraphs::strict::OpenHypergraph;
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
    Some(term.to_strict())
}

fn one_plus_one() -> Option<Term> {
    use Arr::*;
    let lhs = &lax_arr(One) | &lax_arr(One);
    let rhs = lax_arr(Add);
    let term = (&lhs >> &rhs)?;
    Some(term.to_strict())
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
fn test_one_plus_one() {
    let f = one_plus_one().unwrap();

    assert_eq!(f.source(), mktype(0));
    assert_eq!(f.target(), mktype(1));

    let inputs = VecArray(vec![]);
    let result = eval::<VecKind, Obj, Arr, usize>(&f, inputs, apply).expect("eval failed");

    // 3**2
    assert_eq!(result, VecArray(vec![2]));
}

#[test]
fn test_one_plus_one_algebraic() {
    use Arr::*;
    let one = LaxTerm::singleton(One, vec![], vec![Obj]);
    let lhs = &one | &one;
    let rhs = LaxTerm::singleton(Add, vec![Obj, Obj], vec![Obj]);

    let f = (&lhs >> &rhs).unwrap().to_strict();

    assert_eq!(f.source(), mktype(0));
    assert_eq!(f.target(), mktype(1));

    let inputs = VecArray(vec![]);
    let result = eval::<VecKind, Obj, Arr, usize>(&f, inputs, apply).expect("eval failed");

    // 3**2
    assert_eq!(result, VecArray(vec![2]));
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
