use {
    super::strategy::{arrow_strategy, objects_strategy},
    open_hypergraphs::{array::vec::*, category::*, finite_function::*},
    proptest::{prop_assert_eq, proptest},
};

proptest! {
    #[test]
    fn test_f_cp_inj0_equals_inject0(f in arrow_strategy(None,None,true), b in objects_strategy(true,None)) {
        let lhs = f.compose(&FiniteFunction::inj0(f.target(), b)).expect("By construction composible");
        let rhs = f.inject0(b);
        prop_assert_eq!(lhs, rhs);
    }

    #[test]
    fn test_f_cp_inj1_equals_inject1(f in arrow_strategy(None,None,true), a in objects_strategy(true,None)) {
        let lhs = f.compose(&FiniteFunction::inj1(a, f.target())).expect("By construction composible");
        let rhs = f.inject1(a);
        prop_assert_eq!(lhs, rhs);
    }

    /// Test that the (finite) coproduct of injections is the identity
    /// ι_0 + ι_1 + ... + ι_N = identity(sum_{i ∈ N} s(i))
    #[test]
    fn test_injection_coproduct_identity(s in arrow_strategy(None,None,true)) {
        //let s_semi : SemifiniteFunction<usize,VecKind> = s.into();
        let i : FiniteFunction<VecKind> = FiniteFunction::identity(s.source());
        let n : usize = s.table.iter().sum();
        let lhs = s.injections(&i).expect("By construction");
        let rhs = FiniteFunction::identity(n);
        prop_assert_eq!(lhs, rhs);
    }
}
