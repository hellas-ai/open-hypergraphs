//! # Open Hypergraphs
//!
//! An
//! [OpenHypergraph](crate::open_hypergraph::OpenHypergraph)
//! is a [GPU-accelerated](#gpu-acceleration) datastructure for representing
//! large networks (circuits) of operations having multiple inputs and outputs.
//! For example:
//!
//! ```text
//!       ─────────────┐    ┌───┐    ┌───┐
//!                    └────│   │    │   │────
//!                         │ + │────│ Δ │
//!           ┌───┐    ┌────│   │    │   │────
//!       ────│ - │────┘    └───┘    └───┘
//!           └───┘
//! ```
//!
//! The above term represents the expression `(x + (-y), x + (-y))`, where `Δ : 1 → 2` is an
//! explicit copying operation.
//! Note that multiple outputs are "native" to the representation: in the textual description, the
//! arithmetic expression is repeated twice, while copying is explicit in the open hypergraph representation.
//!
//! # Theory & Example
//!
//! Open Hypergraphs are a representation of *cospans of hypergraphs* which correspond directly to
//! morphisms of a symmetric monoidal category presented by generators and equations.
//!
//! Pragmatically, this means one can define a set of objects (e.g. types) and operations (e.g. functions),
//! and represent terms over those generators.
//!
//! For example, we can define a simple expression language of arithmetic over integers and
//! constrct the example term above as follows:
//!
//! ```rust
//! use open_hypergraphs::prelude::*;
//!
//! #[derive(PartialEq, Eq, Debug, Clone, Copy)]
//! enum Operation {
//!     Negate,
//!     Add,
//!     Copy,
//! }
//!
//! #[derive(PartialEq, Eq, Debug, Clone)]
//! enum Object {
//!     Int,
//! }
//!
//! // Return the type (inputs and outputs) of the specified op
//! fn op_type(op: Operation) -> (SemifiniteFunction<Object>, SemifiniteFunction<Object>) {
//!     use Operation::*;
//!     use Object::*;
//!
//!     // Objects (or "types") in an open hypergraph are *lists* of generating types.
//!     // 'int' is the singleton list Int
//!     let int = SemifiniteFunction::new(VecArray(vec![Int]));
//!     // ... 'int2' is the list `Int ● Int` - the `●` denotes *tensor product* in the category;
//!     // you can think of `X₁ ● X₂ ● ... ● Xn` as a list of generating objects `[X₁, X₂, ..., Xn]`.
//!     let int2 = SemifiniteFunction::new(VecArray(vec![Int, Int]));
//!
//!     match op {
//!         // `Negate : Int → Int` has 1 integer input and 1 integer output
//!         Negate => (int.clone(), int),
//!         // `Add : Int ● Int → Int` a binary operation
//!         Add => (int2, int),
//!         // `Copy : Int → Int ● Int` has a single integer input, but *two* outputs.
//!         Copy => (int, int2),
//!     }
//! }
//!
//! // Create the OpenHypergraph corresponding to a given op
//! fn op(op: Operation) -> OpenHypergraph<Object, Operation> {
//!     let (s, t) = op_type(op);
//!     OpenHypergraph::singleton(op, s, t)
//! }
//!
//! // Construct the example term ((x + (-y), x + (-y))
//! fn example_term() -> OpenHypergraph<Object, Operation> {
//!     use Operation::*;
//!     use Object::*;
//!     let int = SemifiniteFunction::new(VecArray(vec![Int]));
//!     let id = OpenHypergraph::identity(int);
//!     id.tensor(&op(Negate)).compose(&op(Add)).unwrap().compose(&op(Copy)).unwrap()
//! }
//! ```
//!
//! # GPU Acceleration
//!
//! The
//! [OpenHypergraph](crate::open_hypergraph::OpenHypergraph) datastructure is a flat, array-based
//! representation which works on both CPU and GPU.
//!
//! Algorithms and datastructures are all parametrised by a type `K` implementing
//! [ArrayKind](crate::array::ArrayKind).
//! This type is used to represent a kind `K : * → *` rather than a concrete type.
//! For example, the kind of [`Vec`]-backed arrays is given by
//! [VecKind](crate::array::vec::VecKind),
//! and type aliases for this backend are exported in
//! [the prelude](crate::prelude).
//!
//! To add a new array backend, create a new type implementing
//! [ArrayKind](crate::array::ArrayKind).

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

pub mod prelude {
    //! Type alises for Open Hypergraphs using the [`VecKind`] array backend.
    pub use crate::array::vec::*;
    pub use crate::category::*;

    pub type OpenHypergraph<Obj, Arr> = crate::open_hypergraph::OpenHypergraph<VecKind, Obj, Arr>;
    pub type Hypergraph<Obj, Arr> = crate::hypergraph::Hypergraph<VecKind, Obj, Arr>;
    pub type FiniteFunction = crate::finite_function::FiniteFunction<VecKind>;
    pub type SemifiniteFunction<T> = crate::semifinite::SemifiniteFunction<VecKind, T>;
    pub type IndexedCoproduct<F> = crate::indexed_coproduct::IndexedCoproduct<VecKind, F>;
}
