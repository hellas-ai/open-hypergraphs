//! The category of hypergraphs has objects represented by [`Hypergraph`]
//! and arrows by [`arrow::HypergraphArrow`].
mod acyclic;
pub mod arrow;
mod object;

pub use object::*;
