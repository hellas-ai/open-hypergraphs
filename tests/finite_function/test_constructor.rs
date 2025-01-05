use open_hypergraphs::category::*;
use open_hypergraphs::{array::vec::*, array::*, finite_function::*};

use super::strategy::*;

use proptest::{prop_assert_eq, prop_assert_ne, proptest};

proptest! {
    #[test]
    fn test_identity(domain in 0..10) {
        #[allow(clippy::cast_sign_loss)]
        let domain_usize = domain as usize;
        let f = FiniteFunction::<VecKind>::identity(domain_usize);
        assert_eq!(f.table, VecArray((0..domain_usize).collect::<Vec<_>>()));
        assert_eq!(f.target, domain_usize);
    }
}

proptest! {
    #[test]
    fn test_initial(target in 0..) {
        #[allow(clippy::cast_sign_loss)]
        let target_usize = target as usize;
        let f = FiniteFunction::<VecKind>::initial(target_usize);
        assert_eq!(f.table, VecArray(vec![]));
        assert_eq!(f.target, target_usize);
    }
}

proptest! {
    #[test]
    fn new_construction((f_table, target) in finite_function_strategy(None,1..10)) {
        let f_array = f_table;
        let f = FiniteFunction::<VecKind>::new(f_array.clone(), target).expect("By construction");
        prop_assert_eq!(f.table, f_array);
        prop_assert_eq!(f.target, target);
    }
}

proptest! {
    #[test]
    fn new_construction_v2((f_table,target) in arrow_strategy_helper(None,None,true)) {
        let f = FiniteFunction::<VecKind>::new(f_table.clone(), target).expect("By construction");
        if target==0 {
            prop_assert_eq!(f.source(),0);
        }
        prop_assert_eq!(f.table, f_table);
        prop_assert_eq!(f.target, target);
    }

    #[test]
    fn new_construction_v3((f_table,target) in arrow_strategy_helper(None,None,false)) {
        let f = FiniteFunction::<VecKind>::new(f_table.clone(), target).expect("By construction");
        prop_assert_eq!(f.table, f_table);
        prop_assert_eq!(f.target, target);
        prop_assert_ne!(target, 0);
    }

    #[test]
    fn round_trip(f in arrow_strategy(None,None,true)) {
        let array = f.table.clone();
        let g = FiniteFunction::new(array, f.target).expect("By construction");
        prop_assert_eq!(f,g);
    }

}
