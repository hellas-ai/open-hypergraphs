//! An imperative/sequential interface for constructing "Lax" Open Hypergraphs.
//!
//! A lax open hypergraph is similar to an open hypergraph, but where the connecting of nodes
//! is deferred until the [`OpenHypergraph::quotient`] operation is called.
//!
//! For example, a "strict" [`OpenHypergraph`] representing the composite of "copy" and "multiply"
//! operations can be depicted as follows:
//!
//! ```text
//!     ┌──────┐       ┌──────┐
//!     │      ├───●───┤      │
//! ●───┤ Copy │       │ Mul  ├───●
//!     │      ├───●───┤      │
//!     └──────┘       └──────┘
//! ```
//!
//! A *lax* OpenHypergraph supports having distinct nodes for each operation, with a "quotient
//! map" (depicted as a dotted line) recording which nodes are to be identified.
//!
//! ```text
//!     ┌──────┐   x0    y0  ┌──────┐
//! a   │      ├───●╌╌╌╌╌●───┤      │    b
//! ●───┤ Copy │             │ Mul  ├───●
//!     │      ├───●╌╌╌╌╌●───┤      │
//!     └──────┘   x1    y1  └──────┘
//! ```
//!
//! Calling [`OpenHypergraph::quotient`] on this datastructure recovers the strict
//! [`crate::strict::OpenHypergraph`] by identifying `x0` with `y0` and `x1` with `y1`.
//!
//! The lax [`OpenHypergraph`] can be constructed with the following Rust code.
//!
//! ```rust
//! use open_hypergraphs::lax::OpenHypergraph;
//!
//! #[derive(PartialEq, Clone)]
//! pub enum Obj { A }
//!
//! #[derive(PartialEq, Clone)]
//! pub enum Op { Copy, Mul }
//!
//! fn copy_mul() -> OpenHypergraph<Obj, Op> {
//!     use Obj::A;
//!
//!     // we use the stateful builder interface to add nodes and edges to the hypergraph
//!     let mut state = OpenHypergraph::empty();
//!
//!     // create a `Copy : 1 → 2` operation using the `new_operation` method.
//!     // This creates nodes for each type in the input and output, and returns their `NodeId`s.
//!     let (_, (_, x)) = state.new_operation(Op::Copy, vec![A], vec![A, A]) else {
//!         // OpenHypergraph::new_operation will always turn as many sources/targets as appear in
//!         // the provided source/target types, respectively.
//!         panic!("impossible!");
//!     };
//!
//!     // create a `Mul : 2 → 1` operation
//!     let (_, (y, _)) = state.new_operation(Op::Mul, vec![A, A], vec![A]) else {
//!         panic!("impossible!");
//!     };
//!
//!     // Connect the copy operation to the multiply operation
//!     state.unify(x[0], y[0]);
//!     state.unify(x[1], y[1]);
//!
//!     // return the OpenHypergraph that we built
//!     state
//! }
//! ```
//!
//! The "lax" representation also allows for type checking and inference:
//! when composing two operations as in the former diagram, we can have distinct types for
//! connected nodes, e.g., x0 and y0. this allows both *checking* (of e.g. equality) and
//! *inference*: inequal types might be *unified* into a single type.
pub mod category;
pub mod csp;
pub mod functor;
pub mod hypergraph;
pub mod subgraph;
pub mod mut_category;
pub mod open_hypergraph;

pub use crate::category::*;
pub use hypergraph::*;
pub use open_hypergraph::*;

pub mod optic;
pub mod var;
