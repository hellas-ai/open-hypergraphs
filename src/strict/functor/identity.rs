use super::spider::*;
use crate::strict::*;

/// The identity functor, which implements [`Functor`] for any signature.
pub struct Identity;

impl<K: ArrayKind, O, A> Functor<K, O, A, O, A> for Identity
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    fn map_object(
        &self,
        a: &SemifiniteFunction<K, O>,
    ) -> IndexedCoproduct<K, SemifiniteFunction<K, O>> {
        IndexedCoproduct::elements(a.clone())
    }

    fn map_arrow(&self, f: &OpenHypergraph<K, O, A>) -> OpenHypergraph<K, O, A> {
        define_map_arrow(self, f, |ops| OpenHypergraph::tensor_operations(ops))
    }
}
