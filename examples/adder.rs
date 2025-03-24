//! An example of building circuits using the "var" interface for lax open hypergraphs.
//!
//! This file contains the following:
//!
//! - A theory of boolean circuits where wires carry bits, and operations are logic gates
//! - An implementation of HasBitOr, HasBitAnd, etc. for logic gates, so we can write `x & y` to
//!   logically-and values in the hypergraph.
//! - An example of an n-bit ripple carry adder
//!
use open_hypergraphs::lax::var;
use open_hypergraphs::lax::*;

// There is a single generating object in the category: the bit.
#[derive(PartialEq, Clone, Debug)]
pub struct Bit;

// The generating operations are logic gates
#[derive(PartialEq, Clone, Debug)]
pub enum Gate {
    Xor,
    Zero, // 0 → 1
    Or,
    And,
    One,
    Copy, // explicit copying of values
}

impl var::HasVar for Gate {
    fn var() -> Gate {
        Gate::Copy
    }
}

impl var::HasBitXor<Bit, Gate> for Gate {
    fn bitxor(_: Bit, _: Bit) -> (Bit, Gate) {
        (Bit, Gate::Xor)
    }
}

impl var::HasBitAnd<Bit, Gate> for Gate {
    fn bitand(_: Bit, _: Bit) -> (Bit, Gate) {
        (Bit, Gate::And)
    }
}

impl var::HasBitOr<Bit, Gate> for Gate {
    fn bitor(_: Bit, _: Bit) -> (Bit, Gate) {
        (Bit, Gate::Or)
    }
}

use std::cell::RefCell;
use std::rc::Rc;

type Term = OpenHypergraph<Bit, Gate>;
type Builder = Rc<RefCell<Term>>;
type Var = var::Var<Bit, Gate>;

// TODO: helper for 0 → 1 operations.
// This is hard to get right with borrow_mut!
fn zero(state: Builder) -> Var {
    // zero operation edge_id
    let z = {
        let mut term = state.borrow_mut();
        term.new_operation(Gate::Zero, vec![], vec![Bit]).1 .1[0]
    };

    // the var itself
    let v = Var::new(state.clone(), Bit);
    let v_source = v.new_source();

    state.borrow_mut().unify(z, v_source);
    v
}

fn full_adder(a: Var, b: Var, cin: Var) -> (Var, Var) {
    // we reuse this computation twice, so bind it here.
    // This implicitly creats a Copy edge
    let a_xor_b = a.clone() ^ b.clone();

    let sum = a_xor_b.clone() ^ cin.clone();
    let cout = (a & b) | (cin & a_xor_b.clone());

    (sum, cout)
}

fn ripple_carry_adder(state: Builder, a: &[Var], b: &[Var]) -> (Vec<Var>, Var) {
    let n = a.len();
    assert_eq!(n, b.len(), "Input bit arrays must have the same length");

    let mut sum = Vec::with_capacity(n);

    // Start with carry_in = 0
    let mut carry = zero(state);

    // Process each bit position
    for i in 0..n {
        let (s, c) = full_adder(a[i].clone(), b[i].clone(), carry);
        sum.push(s);
        carry = c;
    }

    // Return the sum bits and the final carry (overflow bit)
    (sum, carry)
}

// build a ripple_carry_adder and set its inputs/outputs
fn n_bit_adder() -> Term {
    let state = Rc::new(RefCell::new(Term::empty()));

    {
        let n = 10;

        // inputs
        let xs = vec![Var::new(state.clone(), Bit); 2 * n];
        let (zs, _cout) = ripple_carry_adder(state.clone(), &xs[..n], &xs[n..]);

        let sources: Vec<NodeId> = xs.into_iter().map(|x| x.new_source()).collect();
        let targets: Vec<NodeId> = zs.into_iter().map(|x| x.new_target()).collect();

        let mut term = state.borrow_mut();
        term.sources = sources;
        term.targets = targets;
    }

    println!("reference count: {:?}", Rc::strong_count(&state));
    Rc::try_unwrap(state).unwrap().into_inner()
}

fn xor() -> Term {
    let state = Rc::new(RefCell::new(Term::empty()));

    // Block contents make sure we don't
    {
        let xs = vec![Var::new(state.clone(), Bit); 2];
        let y = xs[0].clone() ^ xs[1].clone();

        state.borrow_mut().sources = vec![xs[0].new_source(), xs[1].new_source()];
        state.borrow_mut().targets = vec![y.new_target()];
    }

    Rc::try_unwrap(state).unwrap().into_inner()
}

fn main() {
    let _xor_term = xor();
    let adder_term = n_bit_adder();
    println!("{:?}", adder_term);
}
