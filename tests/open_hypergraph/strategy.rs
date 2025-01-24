use open_hypergraphs::array::{vec::*, *};
use open_hypergraphs::category::*;
use open_hypergraphs::open_hypergraph::OpenHypergraph;

use crate::hypergraph::strategy::{arb_hypergraph, arb_semifinite, Labels};
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

#[allow(unreachable_code)]
pub fn arb_open_hypergraph<
    K: ArrayKind,
    O: Debug + Clone + PartialEq + 'static,
    A: Debug + Clone + PartialEq + 'static,
>(
    labels: Labels<O, A>,
) -> BoxedStrategy<OpenHypergraph<VecKind, O, A>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + Debug,
    K::Type<A>: Array<K, A> + Debug,
{
    arb_hypergraph(labels)
        .prop_map(move |_h| OpenHypergraph::new(todo!(), todo!(), _h).unwrap())
        .boxed()
}
