//! The category of hypergraphs has objects represented by [`Hypergraph`]
//! and arrows by [`arrow::HypergraphArrow`].
pub mod arrow;
mod acyclic;
mod object;

pub use object::*;
