use open_hypergraphs::category::*;
use open_hypergraphs::open_hypergraph::*;

use super::strategy;
use crate::open_hypergraph::equality::assert_open_hypergraph_equality_invariants;
use crate::theory::meaningless::*;

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
}
