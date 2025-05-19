use open_hypergraphs::category::*;
use open_hypergraphs::strict::open_hypergraph::*;

use crate::open_hypergraph::equality::assert_open_hypergraph_equality_invariants;
use crate::theory::meaningless::*;

use proptest::proptest;

////////////////////////////////////////////////////////////////////////////////
// Symmetric monoidal structure
proptest! {
    #[test]
    fn test_monoidal_interchange(f in arb_composite_open_hypergraph(2), g in arb_composite_open_hypergraph(2)) {
        let a = &(&f[0] >> &f[1]).unwrap() | &(&g[0] >> &g[1]).unwrap();
        let b = (&(&f[0] | &g[0]) >> &(&f[1] | &g[1])).unwrap();

        assert_open_hypergraph_equality_invariants(&a, &b);
    }

    #[test]
    fn test_tensor_unit(f in arb_open_hypergraph()) {
        let unit_object = OpenHypergraph::<_, Obj, Arr>::unit();
        let u = OpenHypergraph::identity(unit_object);
        assert_open_hypergraph_equality_invariants(&f, &f.tensor(&u));
        assert_open_hypergraph_equality_invariants(&u.tensor(&f), &f);
    }

    #[test]
    fn test_tensor_type(f in arb_open_hypergraph(), g in arb_open_hypergraph()) {
        assert_eq!(&f.tensor(&g).source(), &f.source().coproduct(&g.source()));
    }

    #[test]
    fn test_twist_naturality(f in arb_open_hypergraph(), g in arb_open_hypergraph()) {
        // Check naturality of twist:
        //
        //       s_Y         s_X
        //
        //  --f--\ /--     --\ /--g--
        //        x     =     x
        //  --g--/ \--     --/ \--f--
        let s_y = OpenHypergraph::twist(f.target(), g.target());
        let a = f.tensor(&g).compose(&s_y).unwrap();

        let s_x = OpenHypergraph::twist(f.source(), g.source());
        let b = s_x.compose(&g.tensor(&f)).unwrap();
        assert_open_hypergraph_equality_invariants(&a, &b);
    }
}
