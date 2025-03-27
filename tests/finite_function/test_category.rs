use open_hypergraphs::{array::vec::*, category::*, finite_function::*};

use super::strategy::{
    arrow_strategy, composible_arrows_strategy, parallel_arrows_strategy,
    three_composible_arrows_strategy,
};

use proptest::{prop_assert, prop_assert_eq, prop_assert_ne, proptest};

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

proptest! {
    #[test]
    fn reflexivity(f in arrow_strategy(None,None,true)) {
        /*
        equality is derived from equality of table and target
        rather than writing a fresh __eq__ method
        so not the same impact
        */
        prop_assert_eq!(f.clone(),f);
    }

    #[test]
    fn inequal_parallel([f,g] in parallel_arrows_strategy(None,None,true)) {
        /*
        equality is derived from equality of table and target
        rather than writing a fresh __eq__ method
        so not the same impact
        */
        if f.table.iter().zip(g.table.iter()).any(|(x,y)| x!=y) {
            prop_assert_ne!(f,g);
        } else {
            prop_assert_eq!(f,g);
        }
    }

    #[test]
    fn inequal_different_type(f in arrow_strategy(None,None,true),g in arrow_strategy(None, None, true)) {
        /*
        equality is derived from equality of table and target
        rather than writing a fresh __eq__ method
        so not the same impact
        */
        if f.source() != g.source() || f.target != g.target {
            prop_assert_ne!(f,g);
        }
    }
}
