//! # Serializing and deserializing lax open hypergraphs with serde
//!
//! In this example we have:
//!
//! - Node labels which can be arbitrarily serialized as JSON (NumberType)
//! - Edge labels which can be arbitrarily serialized as JSON (ArithOp)
use open_hypergraphs::lax::var::{
    self, fn_operation, forget::forget_monogamous, HasAdd, HasNeg, HasVar,
};
use open_hypergraphs::lax::OpenHypergraph;
use serde_json;

/// A node label type containing structured data, must be serde-serializable
#[derive(PartialEq, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum NumberType {
    /// Integers
    Int,

    /// numbers x such that `lower ≤ x ≤ upper`
    Interval { lower: usize, upper: usize },
}

impl NumberType {
    fn interval(lower: usize, upper: usize) -> NumberType {
        Self::Interval { lower, upper }
    }
}

/// An edge label type (can also contain structured data), must also be serde-serializable
#[derive(PartialEq, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ArithOp {
    Add,
    Neg,
    Cast,
    Copy,
}

impl HasVar for ArithOp {
    fn var() -> ArithOp {
        ArithOp::Copy
    }
}

impl HasAdd<NumberType, ArithOp> for ArithOp {
    fn add(lhs: NumberType, rhs: NumberType) -> (NumberType, ArithOp) {
        assert_eq!(lhs, rhs);
        (lhs, ArithOp::Add)
    }
}

impl HasNeg<NumberType, ArithOp> for ArithOp {
    fn neg(ty: NumberType) -> (NumberType, ArithOp) {
        (ty, ArithOp::Neg)
    }
}

type Term = OpenHypergraph<NumberType, ArithOp>;
type Var = var::Var<NumberType, ArithOp>;

fn build_subtraction_term() -> Term {
    var::build(|state| {
        let x = Var::new(state.clone(), NumberType::Int);
        let y = Var::new(state.clone(), NumberType::interval(0, 1));

        // Cast interval y to nat
        let y_int = fn_operation(state, &[y.clone()], NumberType::Int, ArithOp::Cast);
        let result = x.clone() + (-y_int.clone());

        (vec![x, y], vec![result])
    })
    .unwrap()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Build term and remove 1 → 1 copy operations
    let term = forget_monogamous(&build_subtraction_term());

    // Serialize and print
    let json = serde_json::to_string(&term)?;
    println!("{}", json);

    // Deserialize and check equality
    let term_deserialized = serde_json::from_str(&json).unwrap();
    assert_eq!(term, term_deserialized);
    Ok(())
}
