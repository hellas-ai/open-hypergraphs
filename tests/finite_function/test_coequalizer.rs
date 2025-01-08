use open_hypergraphs::category::*;
use open_hypergraphs::{array::vec::*, finite_function::*};

use super::strategy::{arrow_strategy, parallel_arrows_strategy};

use proptest::{prelude::Strategy, prop_assert_eq, proptest};

proptest! {
    #[test]
    fn test_coequalizer_commutes([f,g] in parallel_arrows_strategy(None,None,true)) {
        let c = f.coequalizer(&g).expect("By construction same domain");
        let lhs = f.compose(&c).expect("By construction composible");
        let rhs = g.compose(&c).expect("By construction composible");

        // Check that c really coequalizes f and g, i.e., `f >> c == g >> c`
        prop_assert_eq!(lhs, rhs);
    }

    // Check that coequalizing with the zero map gives a map where all elements in the input map
    // to a single component, and all other indices map to discrete components.
    #[test]
    fn test_coequalize_zero_map(
        f in arrow_strategy(None, None, false)
            .prop_filter("0 domain", |f| f.source()>0)
    ) {
        // compute the image of f
        use std::collections::HashSet;
        let im_f: HashSet<usize> = f.table.iter().copied().collect();

        // Construct the zero map parallel to f, and coequalizer(f, zero)
        let zero_map : FiniteFunction<VecKind> = FiniteFunction::constant(f.source(),0,f.target()-1);
        let coeq = f.coequalizer(&zero_map).expect("correct arguments");
        println!("zero: {:?}", zero_map);
        println!("f: {:?}", f);
        println!("coeq: {:?}", coeq);

        // Let Q be the finite set of connected components, so that `coeq : B → Q`
        // Check that coeq(0) = 0, and for all elements `q ∈ Q`, we have
        // if q ∈ im(f), then coeq(q) = 0
        // else coeq(q) > 0 and there is no q' != q with coeq(q') = coeq(q).

        // Collect which elements map to each component
        let mut components_seen = HashSet::new();

        for (q, &component) in coeq.table.iter().enumerate() {
            if im_f.contains(&q) || q == 0 {
                // Elements in f's image should map to component 0
                assert_eq!(component, 0);
            } else {
                // Elements not in f's image should map to unique non-zero components
                assert!(component > 0);
                assert!(components_seen.insert(component),
                    "Component {} was used multiple times", component);
            }
        }
    }

}
