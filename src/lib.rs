//! # Prelude
//!
//! `open-hypergraphs` is a [GPU-accelerated](#data-parallelism) implementation of the
//! [OpenHypergraph](crate::open_hypergraph::OpenHypergraph)
//! datastructure from the paper
//! ["Data-Parallel Algorithms for String Diagrams"](https://arxiv.org/pdf/2305.01041).
//! Open hypergraphs are used for representing, evaluating, and differentiating large networks of operations with multiple
//! inputs and outputs.
//!
//! Here's a drawing of an open hypergraph with labeled nodes `●` and hyperedges `□`.
//!
//! ```text
//!                  /───────────────────────────────────
//!                 ╱
//!     ───────────●
//!               i8\      ┌─────┐
//!                  \─────┤     │        ┌─────┐
//!          2             │ Sub ├───●────┤ Neg ├───●───
//!     ─────●─────────────┤     │  i8    └─────┘  i8
//!         i8             └─────┘
//! ```
//!
//! This open hypergraph represents a circuit with two inputs.
//! Call the two inputs `x` and `y`, then
//! this circuit computes `x` on its first output and
//! `- (x - y)` on its second.
//!
//! See the [datastructure](#datastructure) section for a formal definition.
//!
//! # What are Open Hypergraphs For?
//!
//! Open Hypergraphs are a general, differentiable and data-parallel datastructure for *syntax*.
//! Here's a few examples of suitable uses:
//!
//! - Differentiable array programs for deep learning in [catgrad](https://catgrad.com)
//! - Terms in [first order logic](https://arxiv.org/pdf/2401.07055)
//! - Programs in the [λ-calculus](https://en.wikipedia.org/wiki/Cartesian_closed_category)
//! - [Circuits with feedback](https://arxiv.org/pdf/2201.10456)
//!
//! Differentiability of open hypergraphs (as used in [catgrad](https://catgrad.com))
//! comes from the [data-parallel algorithm](crate::functor::optic::Optic) for generalised
//! ahead-of-time automatic differentiation by optic composition.
//! You can read more about this in the papers
//! ["Categorical Foundations of Gradient-Based Learning"](https://arxiv.org/abs/2103.01931)
//! and ["Data-Parallel Algorithms for String Diagrams](https://arxiv.org/pdf/2305.01041).
//! See the [Theory](#theory) section for more detail.
//!
//! # Usage
//!
//! If you're new to the library, you should start with the [`crate::lax`] module.
//! This provides a mutable, imperative, single-threaded interface to building open hypergraphs
//! which should be familiar if you've used a graph library before.
//!
//! We can build the example open hypergraph above as follows:
//!
//! ```rust
//! use open_hypergraphs::lax::*;
//!
//! pub enum NodeLabel { I8 };
//! pub enum EdgeLabel { Sub, Neg };
//!
//! #[test]
//! fn build() -> OpenHypergraph<NodeLabel, EdgeLabel> {
//!     use NodeLabel::*;
//!     use EdgeLabel::*;
//!
//!     // Create an empty OpenHypergraph.
//!     let mut example = OpenHypergraph::<NodeLabel, EdgeLabel>::empty();
//!
//!     // Create all 4 nodes
//!     let x = example.new_node(I8);
//!     let a = example.new_node(I8);
//!     let y = example.new_node(I8);
//!     let z = example.new_node(I8);
//!
//!     // Add the "Sub" hyperedge with source nodes `[x, y]` and targets `[a]`
//!     let sub = example.new_edge(Sub, Hyperedge { sources: vec![x, y], targets: vec![a] });
//!
//!     // Add the 'Neg' hyperedge with sources `[a]` and targets `[z]`
//!     let sub = example.new_edge(Neg, Hyperedge { sources: vec![a], targets: vec![z] });
//!
//!     // set the sources and targets of the example
//!     example.sources = vec![x, y];
//!     example.targets = vec![z];
//!
//!     // return the example
//!     example
//! }
//! ```
//!
//! The [`crate::lax::var::Var`] struct is a helper on top of the imperative interface which
//! reduces some boilerplate, especially when operators are involved.
//! We can rewrite the above example as follows:
//!
//! ```ignore
//! pub fn example() {
//!     let state = OpenHypergraph::empty();
//!     let x = Var::new(state, I8);
//!     let y = Var::new(state, I8);
//!     let (z0, z1) = (x.clone(), -(x - y));
//! }
//! ```
//!
//! See `examples/adder.rs` for a more complete example using this interface to build an n-bit full
//! adder from half-adder circuits.
//!
//! # Datastructure
//!
//! Before giving the formal definition, let's revisit the example above.
//!
//! ```text
//!                  /───────────────────────────────────
//!                0╱
//!     ───────────●
//!               i8\      ┌─────┐
//!                  \─────┤     │   1    ┌─────┐   3
//!          2             │ Sub ├───●────┤ Neg ├───●───
//!     ─────●─────────────┤     │  i8    └─────┘  i8
//!         i8             └─────┘
//! ```
//!
//! There are 4 nodes in this open hypergraph, depicted as `●` with a label `i8` and a
//! node ID in the set `{0..3}`.
//! There are two hyperedges depicted as a boxes labeled `Sub` and `Neg`.
//!
//! Each hyperedge has an *ordered list* of sources and targets.
//! For example, the `Sub` edge has sources `[0, 2]` and targets `[1]`,
//! while `Neg` has sources `[1]` and targets `[3]`.
//! Note: the order is important!
//! Without it, we couldn't represent non-commutative operations like `Sub`.
//!
//! As well as the sources and targets for each *hyperedge*, the whole "open hypergraph" also has
//! sources and targets.
//! These are drawn as dangling wires on the left and right.
//! In this example, the sources are `[0, 2]`, and the targets are `[0, 3]`.
//!
//! Observe that nodes can appear as *both* a source and a target (see e.g., node `0`), and
//! that not every node needs to be either (see e.g., node `1`).
//!
//! # Formal Definition
//!
//! Formally, an open hypergraph is a triple of:
//!
//! 1. A [`crate::hypergraph::Hypergraph`] `h` with `N ∈ ℕ` nodes
//! 2. An array `s` of length `A ∈ ℕ` whose elements `s_i ∈ {0..N-1}` are nodes
//! 3. An array `t` of length `B ∈ ℕ` whose elements `t_i ∈ {0..N-1}` are nodes
//!
//! Concretely, the particular kind of hypergraph in an *open* hypergraph has:
//!
//! - A finite set of `N` nodes, labeled with an element from a set `Σ₀`
//! - A finite set of `E` *hyperedges*, labeled from the set `Σ₁`
//! - For each hyperedge `e ∈ E`,
//!   - An ordered array of *source nodes*
//!   - An ordered array of *target nodes*
//!
//! # Compared to Trees
//!
//! Open Hypergraphs have a unique advantage compared to tree-based syntax representations:
//! hyperedges can represent operations with *multiple outputs*.
//! This means that syntaxes involving e.g., variable names can be naturally captured: a variable becomes a *node* in the hypergraph.
//!
//! # Theory
//!
//! Describe relationship to category theory
//!
//! ## String Diagrams
//!
//! # Data-Parallelism
//!
//! Refer to paper
//!
//! # Differentiability
//!
//! TODO

pub mod array;
pub mod category;
pub mod finite_function;
pub mod indexed_coproduct;
pub mod operations;
pub mod semifinite;

pub mod hypergraph;
pub mod open_hypergraph;

pub mod eval;
pub mod functor;
pub mod layer;

// imperative interface to building open hypergraphs
pub mod lax;

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
