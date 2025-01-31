use open_hypergraphs::category::*;
use open_hypergraphs::open_hypergraph::*;

use crate::hypergraph::equality::assert_hypergraph_equality_invariants;
use crate::theory::meaningless::*;

use proptest::proptest;

////////////////////////////////////////////////////////////////////////////////
// Symmetric monoidal structure
proptest! {
    #[test]
    fn test_dagger(f in arb_open_hypergraph()) {
        let g = f.dagger();

        // dagger swaps source/target
        assert_eq!(f.source(), g.target());
        assert_eq!(f.target(), g.source());
        assert_eq!(f.s, g.t);
        assert_eq!(f.t, g.s);

        // apex is unchanged.
        assert_hypergraph_equality_invariants(&f.h, &g.h);
    }

    #[test]
    fn test_spider_discrete((s, t, w) in arb_labeled_cospan()) {
        let f = OpenHypergraph::<_, _, Arr>::spider(s, t, w).unwrap();
        assert!(f.h.is_discrete());
    }

    #[test]
    fn test_spider_composed_dagger_node_count((s, t, w) in arb_labeled_cospan()) {
        let s = OpenHypergraph::<_, _, Arr>::spider(s, t, w).unwrap();
        let f = s.compose(&s.dagger()).unwrap();

        assert_eq!(f.source(), s.source());
        assert_eq!(f.target(), s.source());

        // We can have at most 2x the wires of the spider s (in the fully disconnected case)
        assert!(f.h.w.len() <= s.h.w.len() * 2);
    }
}
