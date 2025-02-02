//! Symmetric Monoidal Hypergraph Functors on Open Hypergraphs
use crate::array::*;
use crate::open_hypergraph::*;
use crate::semifinite::*;

/// Strict symmetric monoidal hypergraph functors
pub trait Functor<K: ArrayKind, O1, A1, O2, A2> {
    /// Action on objects
    fn map_object(&self, a: SemifiniteFunction<K, O1>) -> SemifiniteFunction<K, O2>;

    /// Action on arrows
    fn map_arrow(&self, a: &OpenHypergraph<K, O1, A1>) -> OpenHypergraph<K, O2, A2>;
}

/// The identity functor, which implements [`Functor`] for any signature.
pub struct Identity;

impl<K: ArrayKind, O, A> Functor<K, O, A, O, A> for Identity
where
    K::Type<K::I>: Clone,
    K::Type<O>: Clone,
    K::Type<A>: Clone,
{
    fn map_object(&self, a: SemifiniteFunction<K, O>) -> SemifiniteFunction<K, O> {
        a
    }

    fn map_arrow(&self, f: &OpenHypergraph<K, O, A>) -> OpenHypergraph<K, O, A> {
        f.clone()
    }
}
