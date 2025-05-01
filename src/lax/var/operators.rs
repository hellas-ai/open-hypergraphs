use {
    super::var::*,
    crate::lax::{hypergraph::Hyperedge, open_hypergraph::OpenHypergraph},
    std::ops::{Add, BitAnd, BitOr, BitXor},
};


use std::{cell::RefCell, ops::*, rc::Rc};

/// A general helper for constructing `n → 1` maps
pub fn operation<O: Clone, A: HasVar>(
    // TODO: generalise to something which borrows a mutable OpenHypergraph?
    builder: &Rc<RefCell<OpenHypergraph<O, A>>>,
    vars: &[Var<O, A>],
    result_label: O,
    op: A,
) -> Var<O, A> {
    let mut nodes = Vec::with_capacity(vars.len());
    for v in vars {
        nodes.push(v.new_target());
    }

    let v = Var::new(builder.clone(), result_label);
    let v_source = v.new_source();

    let mut term = builder.borrow_mut();
    let _ = term.new_edge(
        op,
        Hyperedge {
            sources: nodes,
            targets: vec![v_source],
        },
    );

    v
}

/// Vars can be added when the underlying signature has an operation for 'addition'.
pub trait HasAdd<O, A> {
    fn add(lhs_type: O, rhs_type: O) -> (O, A);
}

impl<O: Clone, A: HasVar + HasAdd<O, A>> Add for Var<O, A> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        // only difference between impls
        let (result_label, op) = A::add(self.label.clone(), rhs.label.clone());
        operation(&self.state.clone(), &[self, rhs], result_label, op)
    }
}

/// Vars can be added when the underlying signature has an operation for 'addition'.
pub trait HasBitXor<O, A> {
    fn bitxor(lhs_type: O, rhs_type: O) -> (O, A);
}

impl<O: Clone, A: HasVar + HasBitXor<O, A>> BitXor for Var<O, A> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        // only difference between impls
        let (result_label, op) = A::bitxor(self.label.clone(), rhs.label.clone());
        operation(&self.state.clone(), &[self, rhs], result_label, op)
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
            type Output = Self;

            fn $fn_name(self, rhs: Self) -> Self {
                let (result_label, op) = A::$fn_name(self.label.clone(), rhs.label.clone());
                //binop(self, rhs, result_label, op)
                operation(&self.state.clone(), &[self, rhs], result_label, op)
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
                operation(&self.state.clone(), &[self], result_label, op)
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

//define_binary_op!(Add, add, HasAdd); // hand-written
define_binary_op!(Mul, mul, HasMul);
define_binary_op!(Sub, sub, HasSub);
define_binary_op!(Div, div, HasDiv);
define_unary_op!(Neg, neg, HasNeg);
