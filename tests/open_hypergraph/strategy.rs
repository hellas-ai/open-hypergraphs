use open_hypergraphs::array::vec::*;
use open_hypergraphs::category::*;
use open_hypergraphs::open_hypergraph::OpenHypergraph;

use crate::hypergraph::strategy::*;
use core::fmt::Debug;
use proptest::strategy::{BoxedStrategy, Strategy};

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
