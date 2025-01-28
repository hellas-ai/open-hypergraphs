// A meaningless theory with no given interpretation.
// Generating objects are 8-bit integers.
// Generating operations are unsigned 8-bit integers of arbitrary type.

use open_hypergraphs::array::vec::*;
use open_hypergraphs::hypergraph::{arrow::*, *};
use open_hypergraphs::open_hypergraph::*;

use crate::hypergraph::strategy::{DiscreteSpan, Labels};
use proptest::strategy::{BoxedStrategy, Strategy};

// Objects are integers
pub type Obj = i8;

// Arrows are unsigned
pub type Arr = u8;

// Generate objects (vertices) as usize values in range 0..10
pub fn arb_object() -> BoxedStrategy<Obj> {
    (0..10i8).boxed()
}

// Generate arrows (edges) as usize values in range 0..5
pub fn arb_arrow() -> BoxedStrategy<Arr> {
    (0..5u8).boxed()
}

pub fn arb_labels() -> BoxedStrategy<Labels<Obj, Arr>> {
    crate::hypergraph::strategy::arb_labels(arb_object(), arb_arrow())
}

pub fn arb_hypergraph() -> BoxedStrategy<Hypergraph<VecKind, Obj, Arr>> {
    arb_labels()
        .prop_flat_map(crate::hypergraph::strategy::arb_hypergraph)
        .boxed()
}

pub fn arb_inclusion() -> BoxedStrategy<HypergraphArrow<VecKind, Obj, Arr>> {
    // NOTE: this is just `liftM2 arb_inclusion arb_labels arb_hypergraph`
    arb_hypergraph()
        .prop_flat_map(move |g| {
            arb_labels().prop_flat_map(move |labels| {
                crate::hypergraph::strategy::arb_inclusion(labels, g.clone())
            })
        })
        .boxed()
}

pub fn arb_discrete_span() -> BoxedStrategy<DiscreteSpan<Obj, Arr>> {
    arb_labels()
        .prop_flat_map(crate::hypergraph::strategy::arb_discrete_span)
        .boxed()
}

pub fn arb_open_hypergraph() -> BoxedStrategy<OpenHypergraph<VecKind, Obj, Arr>> {
    arb_labels()
        .prop_flat_map(crate::open_hypergraph::strategy::arb_open_hypergraph::<Obj, Arr>)
        .boxed()
}

pub fn arb_composite_open_hypergraph(
    n: usize,
) -> BoxedStrategy<Vec<OpenHypergraph<VecKind, Obj, Arr>>> {
    crate::open_hypergraph::strategy::arb_composite_open_hypergraph(n, arb_labels())
}
