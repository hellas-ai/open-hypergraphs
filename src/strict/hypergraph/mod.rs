//! The category of hypergraphs has objects represented by [`Hypergraph`]
//! and arrows by [`arrow::HypergraphArrow`].
mod acyclic;
pub mod arrow;
mod object;
pub(crate) mod subobject;
#[cfg(test)]
mod subobject_tests;

pub use object::*;
