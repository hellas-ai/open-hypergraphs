use open_hypergraphs::array::vec::*;
use open_hypergraphs::finite_function::FiniteFunction;
use open_hypergraphs::indexed_coproduct::IndexedCoproduct;
use open_hypergraphs::lax::{Hyperedge, Hypergraph as LaxHypergraph, NodeId};
use open_hypergraphs::semifinite::SemifiniteFunction;
use open_hypergraphs::strict::hypergraph::*;

use super::strategy::{DiscreteSpan, Labels};

use proptest::proptest;

// Actual test generators using a specific (but meaningless) ground theory.
use crate::theory::meaningless::*;

proptest! {
    #[test]
    fn test_new(h in arb_hypergraph()) {
        // Check the hypergraph validates when created with new
        let _ = Hypergraph::new(h.s, h.t, h.w, h.x).unwrap();
    }

    #[test]
    fn test_discrete(Labels { w, .. } in arb_labels()) {
        let num_obj = w.len();
        let h: Hypergraph<VecKind, Obj, Arr> = Hypergraph::discrete(w);

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

    // Ensure arb_inclusion runs without error.
    #[test]
    fn test_inclusion(_ in arb_inclusion()) {}

    #[test]
    fn test_discrete_span(_ in arb_discrete_span()) {}

    #[test]
    fn test_coequalize_vertices(DiscreteSpan{l, h, r} in arb_discrete_span()) {
        // We have a discrete span
        //
        //      l   r
        //   gl ← h → gr
        //
        // This represents two distinct hypergraphs (gl and gr) with some shared nodes (h)

        // first compute the coproduct of gl and gr:
        let gl = l.target;
        let gr = r.target;

        // get the coproduct `gl + gr`.
        let g = gl.coproduct(&gr);

        // Identify the nodes of gl + gr which come from h:
        let q = l.w.inject0(gr.w.len()).coequalizer(&r.w.inject1(gl.w.len()))
            .expect("construct coequalizer");
        let k = g.coequalize_vertices(&q).expect("coequalize vertices");

        // The number of wires in the 'quotiented' hypergraph k should have the number in the
        // coproduct less the number of 'shared' wires.
        assert_eq!(k.w.len(), g.w.len() - h.w.len());

        // ... but the number of operations should be unchanged!
        assert_eq!(k.x.len(), g.x.len());
    }
}

// No proptest data required
#[test]
fn test_empty() {
    let e: Hypergraph<VecKind, usize, usize> = Hypergraph::empty();
    assert_eq!(e.w.len(), 0);
    assert_eq!(e.x.len(), 0);
}

fn build_hypergraph(
    node_count: usize,
    edges: &[(Vec<usize>, Vec<usize>)],
) -> Hypergraph<VecKind, Obj, Arr> {
    let mut h = LaxHypergraph::empty();
    h.nodes = vec![0; node_count];
    for (sources, targets) in edges {
        let edge = Hyperedge {
            sources: sources.iter().map(|&i| NodeId(i)).collect(),
            targets: targets.iter().map(|&i| NodeId(i)).collect(),
        };
        h.new_edge(0, edge);
    }
    h.to_hypergraph()
}

#[test]
fn test_is_acyclic_true() {
    let h = build_hypergraph(3, &[(vec![0], vec![1]), (vec![1], vec![2])]);
    assert!(h.is_acyclic());
}

#[test]
fn test_is_acyclic_false_cycle() {
    let h = build_hypergraph(2, &[(vec![0], vec![1]), (vec![1], vec![0])]);
    assert!(!h.is_acyclic());
}

#[test]
fn test_is_acyclic_false_self_loop() {
    let h = build_hypergraph(1, &[(vec![0], vec![0])]);
    assert!(!h.is_acyclic());
}

#[test]
fn test_in_out_degree_counts_multiplicity() {
    let sources = IndexedCoproduct::from_semifinite(
        SemifiniteFunction(VecArray(vec![3, 1])),
        FiniteFunction::new(VecArray(vec![0, 1, 1, 2]), 3).unwrap(),
    )
    .unwrap();
    let targets = IndexedCoproduct::from_semifinite(
        SemifiniteFunction(VecArray(vec![2, 2])),
        FiniteFunction::new(VecArray(vec![2, 0, 1, 1]), 3).unwrap(),
    )
    .unwrap();
    let w = SemifiniteFunction(VecArray(vec![1i8, 2i8, 3i8]));
    let x = SemifiniteFunction(VecArray(vec![10u8, 20u8]));

    let h: Hypergraph<VecKind, Obj, Arr> = Hypergraph::new(sources, targets, w, x).unwrap();

    assert_eq!(h.in_degree(0), 1);
    assert_eq!(h.out_degree(0), 1);
    assert_eq!(h.in_degree(1), 2);
    assert_eq!(h.out_degree(1), 2);
    assert_eq!(h.in_degree(2), 1);
    assert_eq!(h.out_degree(2), 1);
}
