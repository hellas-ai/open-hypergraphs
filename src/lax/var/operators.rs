use super::var::*;
use std::ops::{Add, BitAnd, BitOr, BitXor};

// helper function to create binary ops
fn binop<O: Clone, A: HasVar>(lhs: Var<O, A>, rhs: Var<O, A>, result_label: O, op: A) -> Var<O, A> {
    // Get nodes for lhs and rhs vars
    let lhs_node = lhs.new_target();
    let rhs_node = rhs.new_target();

    // Get source/target types
    let source_type = vec![lhs.label.clone(), rhs.label.clone()];
    let target_type = vec![result_label.clone()];

    let target = {
        let mut state = lhs.state.borrow_mut();

        // Create a new Add operation
        let (_, (s, t)) = state.new_operation(op, source_type, target_type);

        // op is a binary operation by construction
        assert_eq!(s.len(), 2);
        assert_eq!(t.len(), 1);

        // Connect the input vars to the new operation
        state.unify(s[0], lhs_node);
        state.unify(s[1], rhs_node);

        t[0]
    };

    // Create a Var for the result, unify op target (t[0]) with its input, and return the Var.
    // TODO: helper method to create a var from a node?
    let v = Var::new(rhs.state.clone(), result_label);
    let v_source = v.new_source(); // new_source calls borrow_mut, so must be done first.
    v.state.borrow_mut().unify(target, v_source); // unify target from above with source of v.
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

// Macro to reduce boilerplate for binary operators
macro_rules! define_binary_op {
    ($trait_name:ident, $fn_name:ident, $has_trait_name:ident) => {
        #[doc = r" Vars support this operator when the underlying signature has the appropriate operation."]
        pub trait $has_trait_name<O, A> {
            fn $fn_name(lhs_type: O, rhs_type: O) -> (O, A);
        }

        impl<O: Clone, A: HasVar + $has_trait_name<O, A>> $trait_name for Var<O, A> {
            type Output = Var<O, A>;

            fn $fn_name(self, rhs: Self) -> Self::Output {
                let (result_label, op) = A::$fn_name(self.label.clone(), rhs.label.clone());
                binop(self, rhs, result_label, op)
            }
        }
    };
}

define_binary_op!(BitAnd, bitand, HasBitAnd);
define_binary_op!(BitOr, bitor, HasBitOr);
