//! # Serializing and deserializing lax open hypergraphs with serde
//!
//! In this example we have:
//!
//! - Node labels which can be arbitrarily serialized as JSON (NumberType)
//! - Edge labels which can be arbitrarily serialized as JSON (ArithOp)
use serde_json;
use std::fmt::{Debug, Display};

use open_hypergraphs::lax::OpenHypergraph;
use open_hypergraphs::lax::var::{self, HasAdd, HasNeg, HasVar, fn_operation, forget};

/// A node label type containing structured data, must be serde-serializable
#[derive(PartialEq, Clone, Debug, serde::Serialize, serde::Deserialize)]
pub enum NumberType {
    /// Integers
    Int,

    /// numbers x such that `lower ≤ x ≤ upper`
    Interval { lower: usize, upper: usize },
}

/// An edge label type (can also contain structured data), must also be serde-serializable
#[derive(PartialEq, Clone, Debug, serde::Serialize, serde::Deserialize)]
pub enum ArithOp {
    Add,
    Neg,
    Cast,
    Copy,
}

type Term = OpenHypergraph<NumberType, ArithOp>;
type Var = var::Var<NumberType, ArithOp>;

// An example term: subtraction : Int × [0, 1] → Int
fn subtract() -> Term {
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

// Serialize the example term, generate an SVG, and deserialize it again.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Build term and remove 1 → 1 copy operations
    let term = forget::forget_monogamous(&subtract());

    // Generate SVG
    let svg_bytes = to_svg(&term)?;
    std::fs::write("subtract.svg", svg_bytes)?;
    println!("Generated subtract.svg");

    // Serialize and print
    let json = serde_json::to_string(&term)?;
    println!("{}", json);

    // Deserialize and check equality
    let term_deserialized = serde_json::from_str(&json).unwrap();
    assert_eq!(term, term_deserialized);
    Ok(())
}

////////////////////////////////////////////////////////////////////////////////
// Boilerplate and instances

impl NumberType {
    fn interval(lower: usize, upper: usize) -> NumberType {
        Self::Interval { lower, upper }
    }
}

impl Display for NumberType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberType::Int => write!(f, "ℤ"),
            NumberType::Interval { lower, upper } => write!(f, "[{}, {}]", lower, upper),
        }
    }
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

pub fn to_svg<O: PartialEq + Clone + Debug + Display, A: PartialEq + Clone + Debug>(
    term: &OpenHypergraph<O, A>,
) -> Result<Vec<u8>, std::io::Error> {
    use graphviz_rust::{
        cmd::{CommandArg, Format},
        exec,
        printer::PrinterContext,
    };
    use open_hypergraphs_dot::{Orientation, generate_dot_with};

    let opts = open_hypergraphs_dot::Options {
        node_label: Box::new(|n| format!("{}", n)),
        edge_label: Box::new(|e| format!("{:?}", e)),
        orientation: Orientation::LR,
        ..Default::default()
    };

    let dot_graph = generate_dot_with(term, &opts);

    exec(
        dot_graph,
        &mut PrinterContext::default(),
        vec![CommandArg::Format(Format::Svg)],
    )
}
