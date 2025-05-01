use open_hypergraphs::{category::*, functor::*};

use crate::{open_hypergraph::equality::*, theory::meaningless::*};

use proptest::proptest;

proptest! {
    #[test]
    fn test_identity_functor_reflexive(f in arb_open_hypergraph()) {
        let g = <Identity as Functor<_, _, _, _, _>>::map_arrow(&f);
        assert_open_hypergraph_equality_invariants(&f, &g);
    }

    #[test]
    fn test_identity_spider_functor_reflexive(f in arb_open_hypergraph()) {
        let g = <Identity as SpiderFunctor<_, _, _, _, _>>::map_arrow(&f);
        assert_open_hypergraph_equality_invariants(&f, &g);
    }

    #[test]
    fn test_identity_functor_preserves_composition(v in arb_composite_open_hypergraph(2)) {
        let [f, g] = v.as_slice() else { panic!("arb_composite_open_hypergraph returned unexpected size result") };

        let identity_composed = <Identity as Functor<_, _, _, _, _>>::map_arrow(&f.compose(g).unwrap());

        let mapped_f = <Identity as Functor<_, _, _, _, _>>::map_arrow(f);
        let mapped_g = <Identity as Functor<_, _, _, _, _>>::map_arrow(g);
        let composed_identity = mapped_f.compose(&mapped_g).unwrap();

        assert_open_hypergraph_equality_invariants(&identity_composed, &composed_identity);
    }

    #[test]
    fn test_identity_spider_functor_preserves_composition(v in arb_composite_open_hypergraph(2)) {
        let [f, g] = v.as_slice() else { panic!("arb_composite_open_hypergraph returned unexpected size result") };

        let identity_composed = <Identity as SpiderFunctor<_, _, _, _, _>>::map_arrow(&f.compose(g).unwrap());

        let mapped_f = <Identity as SpiderFunctor<_, _, _, _, _>>::map_arrow(f);
        let mapped_g = <Identity as SpiderFunctor<_, _, _, _, _>>::map_arrow(g);
        let composed_identity = mapped_f.compose(&mapped_g).unwrap();

        assert_open_hypergraph_equality_invariants(&identity_composed, &composed_identity);
    }
}
