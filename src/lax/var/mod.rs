//! A higher-level interface for building lax Open Hypergraphs, including helpers to use *rust
//! operators* like `+`, `&`, etc. to build terms.
//! Here's an example of using this interface to build a "full adder" circuit.
//!
//! ```ignore
//! fn full_adder(a: Var, b: Var, cin: Var) -> (Var, Var) {
//!    let a_xor_b = a.clone() ^ b.clone();
//!    let a_and_b = a & b;
//!
//!    (a_xor_b, a_and_b)
//! }
//! ```
//!
//! This constructs a [`crate::lax::OpenHypergraph`] like the one below, where dashed lines
//! indicate nodes connected by the quotient map.
//!
//! ```text
//!     ┌──────┐             ┌──────┐
//! a   │      ├───●╌╌╌╌╌●───┤      │    a ^ b
//! ●───┤ Var  │             │ XOR  ├───●
//!     │      ├───●     ●───┤      │
//!     └──────┘    \   /    └──────┘
//!                  \ /
//!                   X
//!                  / \
//!     ┌──────┐    /   \    ┌──────┐
//! b   │      ├───●     ●───┤      │    a & b
//! ●───┤ Var  │             │ AND  ├───●
//!     │      ├───●╌╌╌╌╌●───┤      │
//!     └──────┘   x1    y1  └──────┘
//! ```
//!
//! The purpose of a [`Var`] is to explicitly track variable usage:
//! each time the var is used in an expression, a new node is created in the underlying
//! OpenHypergraph, representing a *copy*.
//! Note however that a [`Var`] correponds to a *hyperedge* rather than a node.
//!
//! See `examples/adder.rs` for a comprehensive example, including a definition of a simple
//! language of simple digital circuits.

mod operators;
mod var;

pub use operators::*;
pub use var::*;
