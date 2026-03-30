//! The category of hypergraphs has objects represented by [`Hypergraph`]
//! and arrows by [`arrow::HypergraphArrow`].
mod acyclic;
pub mod arrow;
#[cfg(any(feature = "experimental", test))]
pub mod matching;
mod object;

pub use object::*;
