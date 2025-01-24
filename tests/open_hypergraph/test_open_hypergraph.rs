use open_hypergraphs::array::*;
use open_hypergraphs::category::*;
use open_hypergraphs::open_hypergraph::*;

use super::strategy;
use crate::hypergraph::equality::assert_hypergraph_equality_invariants;
use crate::theory::meaningless::*;

use core::fmt::Debug;
use proptest::proptest;

#[allow(dead_code)]
fn assert_open_hypergraph_equality_invariants<K: ArrayKind, O, A>(
    f: &OpenHypergraph<K, O, A>,
    g: &OpenHypergraph<K, O, A>,
) where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + Debug,
    K::Type<A>: Array<K, A> + Debug,
{
    assert_eq!(f.source(), g.source());
    assert_eq!(f.target(), g.target());

    assert_hypergraph_equality_invariants(&f.h, &g.h);
}

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

    /*
    #[test]
    fn test_identity_law(f in arb_open_hypergraph()) {
        let id_a = OpenHypergraph::identity(f.source());
        let id_b = OpenHypergraph::identity(f.target());

        let pre_compose = (&id_a >> &f).expect("precompose ok by construction");
        let post_compose = (&f >> &id_b).expect("postcompose ok by construction");
        assert_open_hypergraph_equality_invariants(&f, &pre_compose);
        assert_open_hypergraph_equality_invariants(&f, &post_compose);
    }
    */
}
