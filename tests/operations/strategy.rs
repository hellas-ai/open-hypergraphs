use core::fmt::Debug;

use open_hypergraphs::array::vec::*;
use open_hypergraphs::indexed_coproduct::*;
use open_hypergraphs::operations::*;
use open_hypergraphs::semifinite::*;

use crate::hypergraph::strategy::{arb_indexed_coproduct_finite, arb_semifinite};

use proptest::strategy::{BoxedStrategy, Strategy};

type SingletonOperationArguments<O, A> = (
    A,
    SemifiniteFunction<VecKind, O>,
    SemifiniteFunction<VecKind, O>,
);

/// Generate a single operation
pub fn arb_singleton_operation<
    O: PartialEq + Clone + Debug + 'static,
    A: PartialEq + Clone + Debug + 'static,
>(
    arb_object: BoxedStrategy<O>,
    arb_arrow: BoxedStrategy<A>,
) -> BoxedStrategy<SingletonOperationArguments<O, A>> {
    let sources = arb_semifinite::<O>(arb_object.clone(), None);
    let targets = arb_semifinite::<O>(arb_object, None);

    (arb_arrow, sources, targets).boxed()
}

fn arb_indexed_coproduct_semifinite<T: PartialEq + Clone + Debug + 'static>(
    arb_element: BoxedStrategy<T>,
    len: usize,
) -> BoxedStrategy<IndexedCoproduct<VecKind, SemifiniteFunction<VecKind, T>>> {
    // Generate a semifinite function of type n → T
    arb_semifinite::<T>(arb_element, None)
        .prop_flat_map(move |xs| {
            // Then generate an indexed coproduct with target n...
            arb_indexed_coproduct_finite(len, xs.len()).prop_map(
                move |IndexedCoproduct {
                          sources, values, ..
                      }| {
                    // ... and precompose its values with the semifinite function x
                    // to get an indexed coproduct with target T.
                    IndexedCoproduct::new(sources, (&values >> &xs).unwrap()).unwrap()
                },
            )
        })
        .boxed()
}

pub fn arb_operations<
    O: PartialEq + Clone + Debug + 'static,
    A: PartialEq + Clone + Debug + 'static,
>(
    arb_object: BoxedStrategy<O>,
    arb_arrow: BoxedStrategy<A>,
) -> BoxedStrategy<Operations<VecKind, O, A>> {
    let op_labels = arb_semifinite::<A>(arb_arrow.clone(), None);

    op_labels
        .prop_flat_map(move |ops| {
            let sources = arb_indexed_coproduct_semifinite(arb_object.clone(), ops.len());
            let targets = arb_indexed_coproduct_semifinite(arb_object.clone(), ops.len());

            (sources, targets).prop_map(move |(sources, targets)| {
                Operations::new(ops.clone(), sources, targets).unwrap()
            })
        })
        .boxed()
}
