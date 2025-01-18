use open_hypergraphs::hypergraph::*;

use proptest::proptest;

// TODO: move arb_object, arb_arrow to their own file.
use proptest::strategy::{BoxedStrategy, Strategy};

// Generate objects (vertices) as usize values in range 0..10
fn arb_object() -> BoxedStrategy<usize> {
    (0..10usize).boxed()
}

// Generate arrows (edges) as usize values in range 0..5
fn arb_arrow() -> BoxedStrategy<usize> {
    (0..5usize).boxed()
}

proptest! {
    #[test]
    fn new(h in super::strategy::arb_hypergraph(arb_object(), arb_arrow())) {
        // Check the hypergraph validates when created with new
        let _ = Hypergraph::new(h.s, h.t, h.w, h.x);
    }
}
