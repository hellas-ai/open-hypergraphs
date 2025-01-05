use open_hypergraphs::category::*;
use open_hypergraphs::{array::vec::*, array::*, finite_function::*};

use super::strategy::{
    arrow_strategy, composible_arrows_strategy, three_composible_arrows_strategy,
};

use proptest::{prop_assert, prop_assert_eq, proptest};

proptest! {
    #[test]
    fn category_identity_left(f in arrow_strategy(None,None,true)) {
        let id_source = FiniteFunction::<VecKind>::identity(f.source());
        let res = id_source.compose(&f);
        prop_assert!(res.is_some_and(|composed| composed==f));
    }

    #[test]
    fn category_identity_right(f in arrow_strategy(None,None,true)) {
        let id_target = FiniteFunction::<VecKind>::identity(f.target);
        let res = f.compose(&id_target);
        prop_assert!(res.is_some_and(|composed| composed==f));
    }

    #[test]
    fn composible_composes([f,g] in composible_arrows_strategy(None,None,true)) {
        let _fg = (f.compose(&g))
            .expect("By construction composible");
    }

    #[test]
    fn composition_associative([f,g,h] in three_composible_arrows_strategy(None,None,true)) {
        let fg_h = (f.compose(&g))
            .expect("By construction composible")
            .compose(&h)
            .expect("By construction composible");
        let f_gh = f.compose(
            &g.compose(&h)
                .expect("By construction composible"))
            .expect("By construction composible");
        prop_assert_eq!(fg_h,f_gh);
    }

    #[test]
    fn initial_unique(f in arrow_strategy(Some(0),None,true)) {
        let codomain = f.target;
        prop_assert_eq!(f, FiniteFunction::<VecKind>::initial(codomain));
    }

    #[test]
    fn to_initial(f in arrow_strategy(None,None,true)) {
        let codomain = f.target;
        prop_assert_eq!(f.to_initial(), FiniteFunction::<VecKind>::initial(codomain));
    }

}
