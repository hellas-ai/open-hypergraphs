use open_hypergraphs::{category::*, open_hypergraph::*};

use {
    super::strategy,
    crate::{
        open_hypergraph::equality::assert_open_hypergraph_equality_invariants,
        theory::meaningless::*,
    },
};

use proptest::proptest;

proptest! {
    #[test]
    fn test_identity_type(f in strategy::identities::<Obj, Arr>(arb_object()))
    {
        // Check discreteness
        assert!(f.h.is_discrete());

        // Check identity map has type `id : A → A`
        assert_eq!(f.source(), f.target());

        // Check hypergraph labels are equal to source/target
        assert_eq!(f.source(), f.h.w);
    }

    #[test]
    fn test_identity_law(f in arb_open_hypergraph()) {
        let id_a = OpenHypergraph::identity(f.source());
        let id_b = OpenHypergraph::identity(f.target());

        let pre_compose = (&id_a >> &f).expect("precompose ok by construction");
        let post_compose = (&f >> &id_b).expect("postcompose ok by construction");
        assert_open_hypergraph_equality_invariants(&f, &pre_compose);
        assert_open_hypergraph_equality_invariants(&f, &post_compose);
    }

    #[test]
    fn test_composition_wire_count(v in arb_composite_open_hypergraph(2)) {
        let [f, g] = v.as_slice() else { panic!("arb_composite_open_hypergraph returned unexpected size result") };
        let h = (f.compose(g)).unwrap();

        assert_eq!(h.source(), f.source());
        assert_eq!(h.target(), g.target());

        // The composition h = f ; g should have less than or equal to the number of wires than f, g.
        assert!(h.h.w.len() <= f.h.w.len() + g.h.w.len());

        // ... and *more* than or equal to the number of wires in f, g less those on the boundary.
        assert!(h.h.w.len() >= f.h.w.len() + g.h.w.len() - f.t.source());
    }

    #[test]
    fn test_composition_associative(fgh in arb_composite_open_hypergraph(3)) {
        let [f, g, h] = fgh.as_slice() else { panic!("arb_composite_open_hypergraph returned unexpected size result") };
        let x = (f.compose(g)).unwrap().compose(h).unwrap();
        let y = f.compose(&g.compose(h).unwrap()).unwrap();

        assert_open_hypergraph_equality_invariants(&x, &y);
    }
}
