use super::var::*;
use std::ops::{Add, BitXor};

// helper function to create binary ops
fn binop<O: Clone, A: HasVar>(lhs: Var<O, A>, rhs: Var<O, A>, result_label: O, op: A) -> Var<O, A> {
    let lhs_node = lhs.new_target();
    let rhs_node = rhs.new_target();

    let source = vec![lhs.label, rhs.label];
    let target = vec![result_label.clone()];

    let mut state = lhs.state.borrow_mut();

    // Create a new Add operation
    let (_, (s, t)) = state.new_operation(op, source, target);

    // op is a binary operation by construction
    assert_eq!(s.len(), 2);
    assert_eq!(t.len(), 1);

    // Connect the input vars to the new operation
    state.unify(s[0], lhs_node);
    state.unify(s[1], rhs_node);

    // Return a var
    let v = Var::new(rhs.state, result_label);
    state.unify(t[0], v.new_source());
    v
}

/// Vars can be added when the underlying signature has an operation for 'addition'.
pub trait HasAdd<O, A> {
    fn add(lhs_type: O, rhs_type: O) -> (O, A);
}

impl<O: Clone, A: HasVar + HasAdd<O, A>> Add for Var<O, A> {
    type Output = Var<O, A>;

    fn add(self, rhs: Self) -> Self::Output {
        // only difference between impls
        let (result_label, op) = A::add(self.label.clone(), rhs.label.clone());
        binop(self, rhs, result_label, op)
    }
}

/// Vars can be added when the underlying signature has an operation for 'addition'.
pub trait HasBitXor<O, A> {
    fn bitxor(lhs_type: O, rhs_type: O) -> (O, A);
}

impl<O: Clone, A: HasVar + HasBitXor<O, A>> BitXor for Var<O, A> {
    type Output = Var<O, A>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        // only difference between impls
        let (result_label, op) = A::bitxor(self.label.clone(), rhs.label.clone());
        binop(self, rhs, result_label, op)
    }
}
