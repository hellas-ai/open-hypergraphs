use open_hypergraphs::{array::vec::*, finite_function::*, indexed_coproduct::*};

use {
    crate::hypergraph::strategy::*,
    proptest::prelude::{BoxedStrategy, Strategy},
};

const MAX_SIZE: usize = 10;

pub(crate) fn arb_map_with_indexed_coproducts() -> BoxedStrategy<(
    IndexedCoproduct<VecKind, FiniteFunction<VecKind>>,
    FiniteFunction<VecKind>,
)> {
    (0..MAX_SIZE, 0..MAX_SIZE)
        .prop_flat_map(|(len, target)| {
            arb_indexed_coproduct_finite(len, target).prop_flat_map(move |c| {
                arb_finite_function_type(MAX_SIZE, None, Some(c.len()))
                    .prop_flat_map(arb_finite_function)
                    .prop_map(move |f| (c.clone(), f))
            })
        })
        .boxed()
}
