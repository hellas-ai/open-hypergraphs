use open_hypergraphs::category::*;

use super::strategy;
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
}
