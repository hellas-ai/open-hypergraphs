//! # Optics for lax open hypergraphs
//!
//! This module provides an interface for defining optics on [`crate::lax::OpenHypergraph`]
//! via the [`Optic`] trait.
//!
//! By defining the fwd and reverse mappings on objects and operations, the [`Optic`] trait will
//! give you `map_arrow` and `map_adapted` methods for free.
use std::fmt::Debug;

use crate::lax::functor::dyn_functor::{to_dyn_functor, DynFunctor};
use crate::lax::functor::Functor;

use crate::operations::Operations;
use crate::strict::vec::VecArray;
use crate::strict::vec::VecKind;
use crate::strict::IndexedCoproduct;
use crate::strict::SemifiniteFunction;
use crate::{lax, lax::OpenHypergraph, strict::functor::optic::Optic as StrictOptic};

/// #
///
/// foo
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
        let optic = to_strict_optic(self);
        let strict = term.to_strict();
        lax::OpenHypergraph::from_strict({
            // Get the right trait in scope.
            use crate::strict::functor::Functor;
            optic.map_arrow(&strict)
        })
    }

    fn map_adapted(&self, term: OpenHypergraph<O1, A1>) -> OpenHypergraph<O2, A2> {
        let optic = to_strict_optic(self);
        let strict = term.to_strict();
        lax::OpenHypergraph::from_strict({
            use crate::strict::functor::Functor;
            let optic_term = optic.map_arrow(&strict);
            // Adapt the produced term so it's monogamous again (as long as the input was).
            optic.adapt(&optic_term, &strict.source(), &strict.target())
        })
    }
}

fn to_strict_optic<
    T: Optic<O1, A1, O2, A2> + 'static,
    O1: Clone + PartialEq,
    A1: Clone,
    O2: Clone + PartialEq + Debug,
    A2: Clone,
>(
    this: &T,
) -> StrictOptic<
    DynFunctor<Fwd<T, O1, A1, O2, A2>, O1, A1, O2, A2>,
    DynFunctor<Rev<T, O1, A1, O2, A2>, O1, A1, O2, A2>,
    VecKind,
    O1,
    A1,
    O2,
    A2,
> {
    let fwd = to_dyn_functor(Fwd::new(this.clone()));
    let rev = to_dyn_functor(Rev::new(this.clone()));

    // Clone self to avoid lifetime issues in the closure
    let self_clone = this.clone();

    StrictOptic::new(
        fwd,
        rev,
        Box::new(move |ops: &Operations<VecKind, O1, A1>| {
            let mut sources_vec = Vec::new();
            let mut residuals = Vec::new();

            for (op, _, _) in ops.iter() {
                let m = self_clone.residual(op);
                sources_vec.push(m.len());
                residuals.extend(m);
            }

            let sources = SemifiniteFunction::<VecKind, usize>(VecArray(sources_vec));
            let values = SemifiniteFunction(VecArray(residuals));
            IndexedCoproduct::from_semifinite(sources, values).unwrap()
        }),
    )
}

////////////////////////////////////////////////////////////////////////////////
// Fwd and Rev lax functor helpers, needed for Optic
// **IMPORTANT NOTE**: never expose these in the public API.
// They rely on never having their `map_arrow` methods called, and panic! in that case.

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

    // NOTE: this method is never called; and the struct *must* remain private.
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

    // NOTE: this method is never called; and the struct *must* remain private.
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
