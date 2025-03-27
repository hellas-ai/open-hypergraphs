//! Indexed coproducts over finite and semifinite functions
mod arrow;
mod iterator;
pub mod semifinite_iterator;

pub use {arrow::*, iterator::*, semifinite_iterator::*};
