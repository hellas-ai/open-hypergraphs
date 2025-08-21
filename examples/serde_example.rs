use open_hypergraphs::lax::var::{self, fn_operation, HasAdd, HasNeg, HasVar, forget::Forget};
use open_hypergraphs::lax::OpenHypergraph;
use open_hypergraphs::lax::functor::Functor;
use serde_json;

#[derive(PartialEq, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum NumberType {
    Natural,
    Real,
}

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
    fn add(_: NumberType, _: NumberType) -> (NumberType, ArithOp) {
        (NumberType::Real, ArithOp::Add)
    }
}

impl HasNeg<NumberType, ArithOp> for ArithOp {
    fn neg(_: NumberType) -> (NumberType, ArithOp) {
        (NumberType::Real, ArithOp::Neg)
    }
}

// Custom cast operation
trait HasCast<O, A> {
    fn cast(input: O) -> (O, A);
}

impl HasCast<NumberType, ArithOp> for ArithOp {
    fn cast(_: NumberType) -> (NumberType, ArithOp) {
        (NumberType::Real, ArithOp::Cast)
    }
}

type Term = OpenHypergraph<NumberType, ArithOp>;
type Var = var::Var<NumberType, ArithOp>;

fn build_subtraction_term() -> Term {
    var::build(|state| {
        let x = Var::new(state.clone(), NumberType::Natural);
        let y = Var::new(state.clone(), NumberType::Real);

        // Cast natural x to real
        let x_real = fn_operation(state, &[x.clone()], NumberType::Real, ArithOp::Cast);
        let neg_y = -y.clone();
        let result = x_real + neg_y;

        (vec![x, y], vec![result])
    })
    .unwrap()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let term = build_subtraction_term();
    
    // Apply forget functor to remove copy operations
    let forget_functor = Forget;
    let term_without_copies = forget_functor.map_arrow(&term);
    
    let term_json = serde_json::to_string_pretty(&term_without_copies)?;
    println!("Arrow: {}", term_json);

    Ok(())
}
