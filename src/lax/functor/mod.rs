mod traits;
pub use traits::*;

pub mod dyn_functor;

// shim around the dyn_functor re-export
#[deprecated(
    since = "0.2.10",
    note = "Moved: use `open_hypergraphs::lax::functor::dyn_functor::define_map_arrow`"
)]
pub fn define_map_arrow<
    F: Functor<O1, A1, O2, A2> + Clone,
    O1: Clone + PartialEq,
    A1: Clone,
    O2: Clone + PartialEq,
    A2: Clone,
>(
    functor: &F,
    f: &super::OpenHypergraph<O1, A1>,
) -> super::OpenHypergraph<O2, A2> {
    dyn_functor::define_map_arrow(functor, f)
}
