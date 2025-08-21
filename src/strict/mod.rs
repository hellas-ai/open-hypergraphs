//! An implementation of Open Hypergraphs directly following the paper
//! ["Data-Parallel Algorithms for String Diagrams"](https://arxiv.org/pdf/2305.01041).
//!
//! To use `Vec`-backed open hypergraphs, use the [`crate::strict::vec`] module, which provides some
//! type-aliases specialised to the `Vec` backend.
pub mod hypergraph;
pub mod open_hypergraph;

pub mod eval;
pub mod functor;
pub mod layer;

pub mod isomorphism;

pub use crate::array::*;
pub use crate::category::*;
pub use crate::finite_function::FiniteFunction;
pub use crate::indexed_coproduct::IndexedCoproduct;
pub use crate::semifinite::SemifiniteFunction;
pub use hypergraph::Hypergraph;
pub use open_hypergraph::*;

pub mod vec {
    //! Type alises for strict Open Hypergraphs using the [`VecKind`] array backend.
    pub use crate::array::vec::*;
    pub use crate::category::*;

    pub type OpenHypergraph<Obj, Arr> =
        crate::strict::open_hypergraph::OpenHypergraph<VecKind, Obj, Arr>;
    pub type Hypergraph<Obj, Arr> = crate::strict::hypergraph::Hypergraph<VecKind, Obj, Arr>;
    pub type FiniteFunction = crate::finite_function::FiniteFunction<VecKind>;
    pub type SemifiniteFunction<T> = crate::semifinite::SemifiniteFunction<VecKind, T>;
    pub type IndexedCoproduct<F> = crate::indexed_coproduct::IndexedCoproduct<VecKind, F>;
}
