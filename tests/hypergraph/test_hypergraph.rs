use open_hypergraphs::array::vec::*;
use open_hypergraphs::hypergraph::*;
use open_hypergraphs::semifinite::SemifiniteFunction;

use proptest::proptest;

// TODO: move arb_object, arb_arrow to their own file.
use proptest::strategy::{BoxedStrategy, Strategy};

// Define a signature within this file for `i8` objects and `u8` arrows.
// We then wrap super::strategy methods to use this signature for convenience.

type Obj = i8;
type Arr = u8;

// Generate objects (vertices) as usize values in range 0..10
fn arb_object() -> BoxedStrategy<Obj> {
    (0..10i8).boxed()
}

// Generate arrows (edges) as usize values in range 0..5
fn arb_arrow() -> BoxedStrategy<Arr> {
    (0..5u8).boxed()
}

fn arb_labels() -> BoxedStrategy<(
    SemifiniteFunction<VecKind, Obj>,
    SemifiniteFunction<VecKind, Arr>,
)> {
    super::strategy::arb_labels(arb_object(), arb_arrow())
}

fn arb_hypergraph() -> BoxedStrategy<Hypergraph<VecKind, Obj, Arr>> {
    super::strategy::arb_hypergraph(arb_object(), arb_arrow())
}

proptest! {
    #[test]
    fn test_new(h in arb_hypergraph()) {
        // Check the hypergraph validates when created with new
        let _ = Hypergraph::new(h.s, h.t, h.w, h.x).unwrap();
    }

    #[test]
    fn test_discrete((obj, _arr) in arb_labels()) {
        let num_obj = obj.len();
        let h: Hypergraph<VecKind, Obj, Arr> = Hypergraph::discrete(obj);

        assert_eq!(h.x.len(), 0);
        assert_eq!(h.w.len(), num_obj);
        assert!(h.is_discrete());
    }

    #[test]
    fn test_coproduct(h0 in arb_hypergraph(), h1 in arb_hypergraph()) {
        // Take coproduct of two arbitrary hypergraphs
        let h = h0.coproduct(&h1);

        // Check that sources and targets segments are a concatenation of inputs
        assert_eq!(h.s.len(), h0.s.len() + h1.s.len());
        assert_eq!(h.t.len(), h0.t.len() + h1.t.len());

        // Check that sources/targets *values* are a *tensor* of inputs
        assert_eq!(h.s.values, &h0.s.values | &h1.s.values);
        assert_eq!(h.t.values, &h0.t.values | &h1.t.values);

        // Check that wire labels and operation labels are concatenations
        assert_eq!(h.w, h0.w + h1.w);
        assert_eq!(h.x, h0.x + h1.x);
    }
}

// No proptest data required
#[test]
fn test_empty() {
    let e: Hypergraph<VecKind, usize, usize> = Hypergraph::empty();
    assert_eq!(e.w.len(), 0);
    assert_eq!(e.x.len(), 0);
}
