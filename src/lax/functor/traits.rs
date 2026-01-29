//! Strict symmetric monoidal hypergraph functors on lax open hypergraphs.
use crate::lax::open_hypergraph::*;

// Problems:
//  1. No ownership of OpenHypergraph ⇒ have to copy in to_strict!
//  2. Sized constraint coming from to_dyn_functor
//  3. Can't give nice default impl - constraints from `define_map_arrow` / `to_dyn_functor`

/// An easier-to-implement `Functor` trait for lax `OpenHypergraph`
pub trait Functor<O1, A1, O2, A2> {
    /// Map a generating object of the theory
    fn map_object(&self, o: &O1) -> impl ExactSizeIterator<Item = O2>;

    /// Map a single operation of the theory with specified source and target type.
    /// This must be consistent with `map_object`, i.e. we must have:
    ///     - `F.map_operation(x, s, t).sources == F.map_object(s)`
    ///     - `F.map_operation(x, s, t).targets == F.map_object(t)`
    /// This condition is *not* checked, but may panic if not satisfied.
    fn map_operation(&self, a: &A1, source: &[O1], target: &[O1]) -> OpenHypergraph<O2, A2>;

    /// Apply this functor to an [`OpenHypergraph`].
    /// Once `map_operation` is defined, this can be defined using
    /// [`super::dyn_functor::define_map_arrow`]
    /// as `define_map_arrow(self, f)`
    fn map_arrow(&self, f: &OpenHypergraph<O1, A1>) -> OpenHypergraph<O2, A2>;
}
