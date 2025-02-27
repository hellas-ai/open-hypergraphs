//! # Open Hypergraphs
//!
//! A library for representing the two-dimensional syntax of *string diagrams*.
//! Use it if you think of your data in terms of "boxes and wires" like this:
//!
//! ```text
//!           ┌───┐
//!       ────│ f │────┐    ┌───┐
//!           └───┘    └────│   │
//!                         │ h │────
//!           ┌───┐    ┌────│   │
//!       ────│ g │────┘    └───┘
//!           └───┘
//! ```
//!
//! A concrete example: neural networks, where wires carry tensors (e.g. a 3×4 matrix) and boxes
//! are tensor operations (e.g. matmul)

pub mod array;
pub mod category;
pub mod finite_function;
pub mod indexed_coproduct;
pub mod operations;
pub mod semifinite;

pub mod hypergraph;
pub mod open_hypergraph;

pub mod functor;
pub mod layer;
