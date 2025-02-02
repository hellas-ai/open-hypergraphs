use open_hypergraphs::category::*;
use open_hypergraphs::functor::*;

use crate::open_hypergraph::equality::*;
use crate::theory::meaningless::*;

use proptest::proptest;

proptest! {
    #[test]
    fn test_identity_functor_reflexive(f in arb_open_hypergraph()) {
        let g = Identity.map_arrow(&f);
        assert_open_hypergraph_equality_invariants(&f, &g);
    }

    #[test]
    fn test_identity_functor_preserves_composition(v in arb_composite_open_hypergraph(2)) {
        let [f, g] = v.as_slice() else { panic!("arb_composite_open_hypergraph returned unexpected size result") };

        let identity_composed = Identity.map_arrow(&f.compose(g).unwrap());
        let composed_identity = Identity.map_arrow(f).compose(&Identity.map_arrow(g)).unwrap();

        assert_open_hypergraph_equality_invariants(&identity_composed, &composed_identity);
    }
}
