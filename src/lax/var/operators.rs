use crate::lax::hypergraph::Hyperedge;
use crate::lax::open_hypergraph::OpenHypergraph;

use super::var::*;

use std::ops::*;
use std::sync::{Arc, Mutex};

/// A general helper for constructing `m → n` maps
pub fn operation<O: Clone, A: HasVar>(
    // TODO: generalise to something which borrows a mutable OpenHypergraph?
    builder: &Arc<Mutex<OpenHypergraph<O, A>>>,
    vars: &[Var<O, A>],
    result_types: Vec<O>, // types of output vars
    op: A,
) -> Vec<Var<O, A>> {
    let mut nodes = Vec::with_capacity(vars.len());
    for v in vars {
        nodes.push(v.new_target());
    }

    let result_vars: Vec<Var<O, A>> = result_types
        .into_iter()
        .map(|t| Var::new(builder.clone(), t))
        .collect();
    let result_nodes = result_vars.iter().map(|v| v.new_source()).collect();

    let mut term = builder.lock().unwrap();
    let _ = term.new_edge(
        op,
        Hyperedge {
            sources: nodes,
            targets: result_nodes,
        },
    );

    result_vars
}

/// An `n → 1` operation, returning its sole target `Var`.
pub fn fn_operation<O: Clone, A: HasVar>(
    // TODO: generalise to something which borrows a mutable OpenHypergraph?
    builder: &Arc<Mutex<OpenHypergraph<O, A>>>,
    vars: &[Var<O, A>],
    result_type: O,
    op: A,
) -> Var<O, A> {
    let vs = operation(builder, vars, vec![result_type], op);
    assert_eq!(vs.len(), 1);
    vs.into_iter().next().unwrap()
}

/// Vars can be XORed when the underlying signature has an operation for 'xor'.
pub trait HasBitXor<O, A> {
    fn bitxor(lhs_type: O, rhs_type: O) -> (O, A);
}

impl<O: Clone, A: HasVar + HasBitXor<O, A>> BitXor for Var<O, A> {
    type Output = Var<O, A>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        // only difference between impls
        let (result_label, op) = A::bitxor(self.label.clone(), rhs.label.clone());
        fn_operation(&self.state.clone(), &[self, rhs], result_label, op)
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
                //binop(self, rhs, result_label, op)
                fn_operation(&self.state.clone(), &[self, rhs], result_label, op)
            }
        }
    };
}

// Macro to reduce boilerplate for unary operators
macro_rules! define_unary_op {
    ($trait_name:ident, $fn_name:ident, $has_trait_name:ident) => {
        #[doc = r" Vars support this unary operator when the underlying signature has the appropriate operation."]
        pub trait $has_trait_name<O, A> {
            fn $fn_name(operand_type: O) -> (O, A);
        }

        impl<O: Clone, A: HasVar + $has_trait_name<O, A>> $trait_name for Var<O, A> {
            type Output = Var<O, A>;

            fn $fn_name(self) -> Self::Output {
                let (result_label, op) = A::$fn_name(self.label.clone());
                fn_operation(&self.state.clone(), &[self], result_label, op)
            }
        }
    };
}

//define_binary_op!(BitXor, bitand, HasBitXor); // hand-written
define_binary_op!(BitAnd, bitand, HasBitAnd);
define_binary_op!(BitOr, bitor, HasBitOr);
define_binary_op!(Shl, shl, HasShl);
define_binary_op!(Shr, shr, HasShr);
define_unary_op!(Not, not, HasNot);

define_binary_op!(Add, add, HasAdd);
define_binary_op!(Mul, mul, HasMul);
define_binary_op!(Sub, sub, HasSub);
define_binary_op!(Div, div, HasDiv);
define_unary_op!(Neg, neg, HasNeg);
