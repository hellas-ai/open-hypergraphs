use open_hypergraphs::{array::vec::*, category::*, finite_function::*};

use {
    super::strategy::{arrow_strategy, objects_strategy},
    proptest::{prelude::Strategy, prop_assert_eq, proptest},
};

proptest! {

    /// Verify that the tensor product of arrows corresponds to its
    /// definition in terms of coproducts and injections
    #[test]
    fn test_tensor_vs_injections(
        f in arrow_strategy(None,None,true),
        g in arrow_strategy(None,None,true)) {
        let i0 = FiniteFunction::inj0(f.target,g.target);
        let i1 = FiniteFunction::inj1(f.target,g.target);
        let lhs = f.tensor(&g);
        let rhs0 = f.compose(&i0).expect("By construction composible");
        let rhs1 = g.compose(&i1).expect("By construction composible");
        let rhs = rhs0.coproduct(&rhs1).expect("By construction coproductible");
        prop_assert_eq!(lhs,rhs);
    }
}

proptest! {
    /// Verify that σ ; σ = id
    #[test]
    fn test_twist_inverse(
        a in objects_strategy(true,None),
        b in objects_strategy(true,None)) {
        let f : FiniteFunction<VecKind> = FiniteFunction::twist(a,b);
        let g : FiniteFunction<VecKind> = FiniteFunction::twist(b,a);
        let id = FiniteFunction::<VecKind>::identity(a+b);
        prop_assert_eq!(f.compose(&g).expect("By construction composible"),id);
        let id = FiniteFunction::<VecKind>::identity(a+b);
        prop_assert_eq!(g.compose(&f).expect("By construction composible"),id);
    }

    /// Check naturality of σ, so that (f @ g) ; σ = σ ; (g @ f)
    #[test]
    fn test_twist_naturality(
        f in arrow_strategy(None,None,true),
        g in arrow_strategy(None,None,true)) {
        let post_twist : FiniteFunction<VecKind> = FiniteFunction::twist(f.target,g.target);
        let pre_twist : FiniteFunction<VecKind> = FiniteFunction::twist(f.source(),g.source());
        let lhs = f.tensor(&g).compose(&post_twist).expect("By construction composible");
        let rhs = pre_twist.compose(&g.tensor(&f)).expect("By construction composible");
        prop_assert_eq!(lhs,rhs);
    }
}

#[test]
fn test_transpose_inverse_hardcode() {
    let f: FiniteFunction<VecKind> = FiniteFunction::transpose(2, 3);
    assert_eq!(f.source(), 6);
    assert_eq!(f.target(), 6);
    let g: FiniteFunction<VecKind> = FiniteFunction::transpose(3, 2);
    assert_eq!(g.source(), 6);
    assert_eq!(g.target(), 6);
    let id = FiniteFunction::identity(6);
    let lhs = f.compose(&g).expect("By construction composible");
    assert_eq!(lhs, id);
}

proptest! {

    #[test]
    fn test_transpose_inverse(
        a in proptest::prelude::any::<u8>()
            .prop_filter_map("want below 64",
                |x| if x<=64 {Some(x as usize)} else {None}),
        b in proptest::prelude::any::<u8>()
            .prop_filter_map("want below 64",
                |x| if x<=64 {Some(x as usize)} else {None})
    ) {
        let a_times_b = a*b;
        let f : FiniteFunction<VecKind> = FiniteFunction::transpose(a,b);
        prop_assert_eq!(f.source(), a_times_b);
        prop_assert_eq!(f.target(), a_times_b);
        let g : FiniteFunction<VecKind> = FiniteFunction::transpose(b,a);
        prop_assert_eq!(g.source(), a_times_b);
        prop_assert_eq!(g.target(), a_times_b);
        let id = FiniteFunction::identity(b*a);
        let lhs = f.compose(&g).expect("By construction composible");
        prop_assert_eq!(lhs,id);

    }
}
