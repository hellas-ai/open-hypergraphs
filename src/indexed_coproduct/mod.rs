//! Indexed coproducts over finite and semifinite functions
mod arrow;
mod iterator;
pub mod semifinite_iterator;

pub use arrow::*;
pub use iterator::*;
pub use semifinite_iterator::*;
