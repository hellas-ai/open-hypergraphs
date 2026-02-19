//! # Open Hypergraphs
//!
//! `open-hypergraphs` is a [GPU-accelerated](#data-parallelism) implementation of the
//! [OpenHypergraph](crate::strict::OpenHypergraph)
//! datastructure from the paper
//! ["Data-Parallel Algorithms for String Diagrams"](https://arxiv.org/pdf/2305.01041).
//! Open hypergraphs are used for representing, evaluating, and differentiating large networks of operations with multiple
//! inputs and outputs.
//!
//! Here's a drawing of an open hypergraph with labeled nodes `●` and hyperedges `□`.
//!
//! ```text
//!                    /───────────────────────────────────   x
//!                   ╱
//!   x   ───────────●
//!                 i8\      ┌─────┐
//!                    \─────┤     │        ┌─────┐
//!            2             │ Sub ├───●────┤ Neg ├───●───    -(x - y)
//!   y   ─────●─────────────┤     │  i8    └─────┘  i8
//!           i8             └─────┘
//! ```
//!
//! This open hypergraph represents a circuit with two inputs, `x` and `y`.
//! this circuit computes `x` on its first output and `- (x - y)` on its second.
//! (The input/output labels `x`, `y`, and `-(x - y)` are only illustrative, and not part of the
//! datastructure.)
//!
//! <div class="warning">
//! Note carefully: in contrast to typical graph-based syntax representations,
//! operations correspond to hyperedges,
//! and values correspond to nodes!
//! This is why nodes are labeled with types like i8 and hyperedges with operations like
//! Sub.
//! </div>
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
//! - [Interaction nets](https://dl.acm.org/doi/10.1006/inco.1997.2643)
//!
//! Open Hypergraphs have some unique advantages compared to tree-based representations of syntax.
//! For example, they can represent operations with *multiple outputs*, and structures with
//! *feedback*.
//! See the [comparison to trees and graphs](#comparison-to-trees-and-graphs) for more detail.
//!
//! Differentiability of open hypergraphs (as used in [catgrad](https://catgrad.com))
//! comes from the [data-parallel algorithm](crate::strict::functor::optic::Optic) for generalised
//! ahead-of-time automatic differentiation by optic composition.
//! This algorithm is actually more general than just differentiability: read more in the papers
//! ["Categorical Foundations of Gradient-Based Learning"](https://arxiv.org/abs/2103.01931)
//! and ["Data-Parallel Algorithms for String Diagrams"](https://arxiv.org/pdf/2305.01041).
//! See the [Theory](#theory) section for more pointers.
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
//!     example.new_edge(Sub, Hyperedge { sources: vec![x, y], targets: vec![a] });
//!
//!     // Add the 'Neg' hyperedge with sources `[a]` and targets `[z]`
//!     example.new_edge(Neg, Hyperedge { sources: vec![a], targets: vec![z] });
//!
//!     // set the sources and targets of the example
//!     example.sources = vec![x, y];
//!     example.targets = vec![x, z];
//!
//!     // return the example
//!     example
//! }
//!
//! let f = build();
//! assert_eq!(f.sources.len(), 2);
//! assert_eq!(f.targets.len(), 2);
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
//! By contrast, the [`crate::strict`] module in principle supports GPU acceleration, but has a
//! much more complicated interface.
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
//! <div class="warning">
//! There are no restrictions on how many times a node can appear as a source or target of both
//! hyperedges and the open hypergraph as a whole.
//! </div>
//!
//! For example, node `0` is a source and target of the open hypergraph, *and* a source of the
//! `Sub` edge.
//! Another example: node `1` is not a source or target of the open hypergraph, although it *is* a
//! target of the `Sub` hyperedge and a source of the `Neg` hyperedge.
//!
//! It's also possible to have nodes which are neither sources nor targets of the open hypergraph
//! *or* any hyperedge, but that isn't pictured here. See the [theory](#theory) section for more
//! detail.
//!
//! # Formal Definition
//!
//! Formally, an open hypergraph is a triple of:
//!
//! 1. A Hypergraph `h` with `N ∈ ℕ` nodes
//! 2. An array `s` of length `A ∈ ℕ` whose elements `s_i ∈ {0..N-1}` are nodes
//! 3. An array `t` of length `B ∈ ℕ` whose elements `t_i ∈ {0..N-1}` are nodes
//!
//! Many different kinds of [Hypergraph](https://en.wikipedia.org/wiki/Hypergraph) exist,
//! but an *open* hypergraph uses a specific kind of directed hypergraph, which has:
//!
//! - A finite set of `N` nodes, labeled with an element from a set `Σ₀`
//! - A finite set of `E` *hyperedges*, labeled from the set `Σ₁`
//! - For each hyperedge `e ∈ E`,
//!   - An ordered array of *source nodes*
//!   - An ordered array of *target nodes*
//!
//! # Comparison to Trees and Graphs
//!
//! Let's compare the open hypergraph representation of the example term above against *tree* and
//! *graph* representations.
//!
//! When considered as a tree, the term `(x, - (x - y))` can be drawn as follows:
//!
//! ```text
//!         Pair
//!        /    \
//!       /      Neg
//!      x        |
//!              Sub
//!             /   \
//!            x     y
//! ```
//!
//! There are two problems here:
//!
//! 1. To handle multiple outputs, we had to include a tuple constructor "Pair" in our language.
//!    This means we'd also need to add other functions to deal with pairs, "polluting" the base
//!    language.
//! 2. The "sharing" of variables is not evident from the tree structure: x is used twice, but we
//!    have to compare strings to "discover" that fact.
//!
//! In contrast, the open hypergraph:
//!
//! 1. Allows for terms with **multiple outputs**, without having to introduce a tuple type to the
//!    language.
//! 2. Encodes the **sharing** of variables naturally by allowing nodes to appear in multiple
//!    sources and targets.
//!
//! Another common approach is to use a *graph* for syntax where nodes are operations, and an edge
//! between two nodes indicates the *output* of the source node is the *input* of the target.
//! Problems:
//!
//! 1. Nodes don't distinguish the order of edges, so argument order has to be tracked separately
//! 2. There is no notion of input or output to the whole system.
//!
//! In contrast, the open hypergraph:
//!
//! 1. Naturally handles operations with multiple ordered inputs and outputs (as *hyperedges*)
//! 2. Comes equipped with global source and target nodes
//!
//! Open Hypergraphs have general utility because they model any system which can be described in terms of symmetric monoidal
//! categories.
//! Some examples are listed [above](#what-are-open-hypergraphs-for);
//! see the [Theory](#theory) section for more pointers to detail on the mathematical
//! underpinnings.
//!
//! # Theory
//!
//! Formally, an `OpenHypergraph<Σ₀, Σ₁>` is an arrow of
//! the free [symmetric monoidal category](https://en.wikipedia.org/wiki/Symmetric_monoidal_category)
//! presented by the signature `(Σ₀, Σ₁)` plus "Special Frobenius" structure.
//!
//! This extra structure is sometimes useful (e.g. in autodiff), but can be removed by restricting
//! the open hypergraph such that nodes always appear in exactly one source and target.
//! This condition is called "monogamous acyclicity".
//!
//! A complete mathematical explanation can be found in the papers
//! [String Diagram Rewrite Theory I](https://arxiv.org/abs/2012.01847),
//! [II](https://arxiv.org/abs/2104.14686),
//! and
//! [III](https://arxiv.org/abs/2109.06049),
//! which also includes details on how to *rewrite* open hypergraphs.
//!
//! The implementation in *this* library is based on the data-parallel algorithms described in the
//! paper [Data Parallel Algorithms for String Diagrams](https://arxiv.org/pdf/2305.01041).
//! In particular, the "generalised autodiff" algorithm can be found in Section 10 ("Optic
//! Composition using Frobenius Structure") of that paper.

pub mod array;
pub mod category;
pub mod finite_function;
pub mod indexed_coproduct;
pub mod operations;
pub mod semifinite;

// Strict open hypergraphs
pub mod strict;

// imperative interface to building open hypergraphs
pub mod lax;
