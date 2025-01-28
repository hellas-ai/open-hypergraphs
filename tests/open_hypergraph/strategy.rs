use open_hypergraphs::array::vec::*;
use open_hypergraphs::category::*;
use open_hypergraphs::open_hypergraph::OpenHypergraph;

use crate::hypergraph::strategy::{
    arb_finite_function, arb_finite_function_type, arb_hypergraph, arb_semifinite, Labels,
};
use proptest::strategy::{BoxedStrategy, Strategy};

use core::fmt::Debug;

/// Generate random identity open hypergraphs.
pub fn identities<
    O: Debug + Clone + PartialEq + 'static,
    A: Debug + Clone + PartialEq + 'static,
>(
    arb_object: BoxedStrategy<O>,
) -> BoxedStrategy<OpenHypergraph<VecKind, O, A>> {
    arb_semifinite(arb_object, None)
        .prop_map(OpenHypergraph::identity)
        .boxed()
}

pub fn arb_open_hypergraph<
    O: Debug + Clone + PartialEq + 'static,
    A: Debug + Clone + PartialEq + 'static,
>(
    labels: Labels<O, A>,
) -> BoxedStrategy<OpenHypergraph<VecKind, O, A>> {
    arb_hypergraph(labels)
        .prop_flat_map(move |h| {
            let source = arb_finite_function_type(10, None, Some(h.w.len()))
                .prop_flat_map(arb_finite_function)
                .boxed();
            let target = arb_finite_function_type(10, None, Some(h.w.len()))
                .prop_flat_map(arb_finite_function)
                .boxed();

            (source, target).prop_map(move |(s, t)| OpenHypergraph::new(s, t, h.clone()).unwrap())
        })
        .boxed()
}
