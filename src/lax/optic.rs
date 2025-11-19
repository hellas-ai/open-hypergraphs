//! Optics for lax open hypergraphs
use std::fmt::Debug;

use crate::lax::functor::{to_dyn_functor, Functor};
use crate::operations::Operations;
use crate::strict::vec::VecArray;
use crate::strict::vec::VecKind;
use crate::strict::IndexedCoproduct;
use crate::strict::SemifiniteFunction;
use crate::{lax, lax::OpenHypergraph, strict::functor::optic::Optic as StrictOptic};

// TODO: can 'static be removed here?
pub trait Optic<
    O1: Clone + PartialEq,
    A1: Clone,
    O2: Clone + PartialEq + std::fmt::Debug,
    A2: Clone,
>: Clone + 'static
{
    fn fwd_object(&self, o: &O1) -> Vec<O2>;
    fn fwd_operation(&self, a: &A1, source: &[O1], target: &[O1]) -> OpenHypergraph<O2, A2>;
    fn rev_object(&self, o: &O1) -> Vec<O2>;
    fn rev_operation(&self, a: &A1, source: &[O1], target: &[O1]) -> OpenHypergraph<O2, A2>;
    fn residual(&self, a: &A1) -> Vec<O2>;

    fn map_arrow(&self, term: OpenHypergraph<O1, A1>) -> OpenHypergraph<O2, A2> {
        let fwd = to_dyn_functor(Fwd::new(self.clone()));
        let rev = to_dyn_functor(Rev::new(self.clone()));

        // Clone self to avoid lifetime issues in the closure
        let self_clone = self.clone();

        //let optic = StrictOptic::<Fwd<Self, _, _, _, _>, Rev<Self, _, _, _, _>, VecKind, O1, A1, O2, A2>::new(fwd, rev, Box::new(|ops: &Operations<VecKind, Obj, Arr| {
        let optic = StrictOptic::new(
            fwd,
            rev,
            Box::new(move |ops: &Operations<VecKind, O1, A1>| {
                let mut sources_vec = Vec::new();
                let mut residuals = Vec::new();

                for i in 0..ops.len() {
                    let obj = &ops.x.0[i];
                    let m = self_clone.residual(&obj);
                    sources_vec.push(m.len());
                    residuals.extend(m);
                }

                let sources = SemifiniteFunction::<VecKind, usize>(VecArray(sources_vec));
                let values = SemifiniteFunction(VecArray(residuals));
                IndexedCoproduct::from_semifinite(sources, values).unwrap()
            }),
        );
        let strict = term.to_strict();
        let optic_term = {
            use crate::strict::functor::Functor;
            optic.map_arrow(&strict)
        };
        lax::OpenHypergraph::from_strict(optic_term)
    }
}

////////////////////////////////////////////////////////////////////////////////
// Fwd and Rev lax functor helpers, needed for Optic

#[derive(Clone, PartialEq)]
struct Fwd<T, O1, A1, O2, A2> {
    _phantom: std::marker::PhantomData<(O1, A1, O2, A2)>,
    optic: Box<T>,
}

impl<
        T: Optic<O1, A1, O2, A2>,
        O1: Clone + PartialEq,
        A1: Clone,
        O2: Clone + PartialEq + Debug,
        A2: Clone,
    > Functor<O1, A1, O2, A2> for Fwd<T, O1, A1, O2, A2>
{
    fn map_object(&self, o: &O1) -> impl ExactSizeIterator<Item = O2> {
        self.optic.fwd_object(o).into_iter()
    }

    fn map_operation(&self, a: &A1, source: &[O1], target: &[O1]) -> OpenHypergraph<O2, A2> {
        self.optic.fwd_operation(a, source, target)
    }

    fn map_arrow(&self, _f: &OpenHypergraph<O1, A1>) -> OpenHypergraph<O2, A2> {
        panic!("Fwd is not a functor!");
    }
}

impl<
        T: Optic<O1, A1, O2, A2>,
        O1: Clone + PartialEq,
        A1: Clone,
        O2: Clone + PartialEq + Debug,
        A2: Clone,
    > Fwd<T, O1, A1, O2, A2>
{
    fn new(t: T) -> Self {
        Self {
            _phantom: std::marker::PhantomData,
            optic: Box::new(t),
        }
    }
}

#[derive(Clone, PartialEq)]
struct Rev<T, O1, A1, O2, A2> {
    _phantom: std::marker::PhantomData<(O1, A1, O2, A2)>,
    optic: Box<T>,
}

impl<
        T: Optic<O1, A1, O2, A2>,
        O1: Clone + PartialEq,
        A1: Clone,
        O2: Clone + PartialEq + Debug,
        A2: Clone,
    > Functor<O1, A1, O2, A2> for Rev<T, O1, A1, O2, A2>
{
    fn map_object(&self, o: &O1) -> impl ExactSizeIterator<Item = O2> {
        self.optic.rev_object(o).into_iter()
    }

    fn map_operation(&self, a: &A1, source: &[O1], target: &[O1]) -> OpenHypergraph<O2, A2> {
        self.optic.rev_operation(a, source, target)
    }

    fn map_arrow(&self, _f: &OpenHypergraph<O1, A1>) -> OpenHypergraph<O2, A2> {
        panic!("Rev is not a functor!");
    }
}

impl<
        T: Optic<O1, A1, O2, A2>,
        O1: Clone + PartialEq,
        A1: Clone,
        O2: Clone + PartialEq + Debug,
        A2: Clone,
    > Rev<T, O1, A1, O2, A2>
{
    fn new(t: T) -> Self {
        Self {
            _phantom: std::marker::PhantomData,
            optic: Box::new(t),
        }
    }
}
