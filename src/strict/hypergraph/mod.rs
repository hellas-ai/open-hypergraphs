//! The category of hypergraphs has objects represented by [`Hypergraph`]
//! and arrows by [`arrow::HypergraphArrow`].
mod acyclic;
pub mod arrow;
pub mod matching;
mod object;

pub use object::*;
