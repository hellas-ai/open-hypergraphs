use crate::array::vec::VecKind;
use crate::lax::open_hypergraph::*;
use crate::operations::Operations;
use crate::strict;

use super::traits::Functor;

// TODO: write a to_strict_functor returning impl strict::functor::Functor
pub fn define_map_arrow<
    F: Functor<O1, A1, O2, A2> + Clone,
    O1: Clone + PartialEq,
    A1: Clone,
    O2: Clone + PartialEq,
    A2: Clone,
>(
    functor: &F,
    f: &OpenHypergraph<O1, A1>,
) -> OpenHypergraph<O2, A2> {
    // Default implementation using strict::functor::Functor
    let strict_functor: DynFunctor<F, O1, A1, O2, A2> = to_dyn_functor(functor.clone());
    let strict_f = to_strict(f.clone());
    let strict_g = <_ as strict::functor::Functor<VecKind, O1, A1, O2, A2>>::map_arrow(
        &strict_functor,
        &strict_f,
    );
    OpenHypergraph::from_strict(strict_g)
}

pub fn to_dyn_functor<F: Functor<O1, A1, O2, A2>, O1, A1, O2, A2>(
    functor: F,
) -> DynFunctor<F, O1, A1, O2, A2> {
    DynFunctor {
        inner: functor,
        _phantom: std::marker::PhantomData,
    }
}

fn to_strict<O: Clone + PartialEq, A: Clone>(
    f: OpenHypergraph<O, A>,
) -> strict::OpenHypergraph<VecKind, O, A> {
    f.to_strict()
}

// A dynamic functor using closures for map_object and map_operation
pub struct DynFunctor<F: Functor<O1, A1, O2, A2>, O1, A1, O2, A2> {
    inner: F,
    _phantom: std::marker::PhantomData<(O1, A1, O2, A2)>,
}

impl<
        F: Functor<O1, A1, O2, A2>,
        O1: Clone + PartialEq,
        A1: Clone,
        O2: Clone + PartialEq,
        A2: Clone,
    > strict::functor::Functor<VecKind, O1, A1, O2, A2> for DynFunctor<F, O1, A1, O2, A2>
{
    fn map_object(
        &self,
        a: &strict::SemifiniteFunction<VecKind, O1>,
    ) -> strict::IndexedCoproduct<VecKind, strict::SemifiniteFunction<VecKind, O2>> {
        let mut sizes = Vec::new();
        let mut values = Vec::new();

        for obj in a.0 .0.iter() {
            let iter = self.inner.map_object(obj);
            sizes.push(iter.len());
            values.extend(iter);
        }

        use crate::array::vec::VecArray;
        use crate::semifinite::SemifiniteFunction;
        let sizes = SemifiniteFunction::<VecKind, usize>(VecArray(sizes));
        let values = SemifiniteFunction(VecArray(values));
        strict::IndexedCoproduct::from_semifinite(sizes, values).unwrap()
    }

    fn map_operations(
        &self,
        ops: Operations<VecKind, O1, A1>,
    ) -> strict::OpenHypergraph<VecKind, O2, A2> {
        // TODO: add a *slice* iterator for IndexedCoproduct<VecKind, _>
        let op_iter = ops.x.0 .0.iter();
        let source_iter = ops.a.into_iter();
        let target_iter = ops.b.into_iter();

        let mut acc: OpenHypergraph<O2, A2> = OpenHypergraph::empty();
        for (op, (source, target)) in op_iter.zip(source_iter.zip(target_iter)) {
            acc.tensor_assign(self.inner.map_operation(op, &source.0, &target.0))
        }

        acc.to_strict()
    }

    fn map_arrow(
        &self,
        f: &strict::OpenHypergraph<VecKind, O1, A1>,
    ) -> strict::OpenHypergraph<VecKind, O2, A2> {
        strict::functor::define_map_arrow(self, f)
    }
}

/// The identity functor for lax open hypergraphs.
#[derive(Clone)]
pub struct Identity;

impl<O: PartialEq + Clone, A: Clone + PartialEq> Functor<O, A, O, A> for Identity {
    fn map_object(&self, o: &O) -> impl ExactSizeIterator<Item = O> {
        std::iter::once(o.clone())
    }

    fn map_operation(&self, a: &A, source: &[O], target: &[O]) -> crate::lax::OpenHypergraph<O, A> {
        OpenHypergraph::singleton(a.clone(), source.to_vec(), target.to_vec())
    }

    fn map_arrow(&self, f: &crate::lax::OpenHypergraph<O, A>) -> crate::lax::OpenHypergraph<O, A> {
        define_map_arrow(self, f)
    }
}
