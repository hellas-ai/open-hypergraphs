use open_hypergraphs::array::vec::*;
use open_hypergraphs::finite_function::*;
use open_hypergraphs::indexed_coproduct::*;

use crate::hypergraph::strategy::*;
use proptest::{prelude::*, proptest};

static MAX_SIZE: usize = 10;

proptest! {
    // Check we can construct a singleton indexed coproduct from a finite function
    #[test]
    fn test_singleton_finite(f in arb_finite_function_type(MAX_SIZE, None, None).prop_flat_map(arb_finite_function)) {
        let c = IndexedCoproduct::<VecKind, FiniteFunction<VecKind>>::singleton(f.clone());
        assert_eq!(c.len(), 1);
        assert_eq!(c.sources.target, c.sources.table.0.into_iter().sum::<usize>() + 1);
        assert_eq!(c.values, f);
    }

    #[test]
    fn test_elements_finite(f in arb_finite_function_type(MAX_SIZE, None, None).prop_flat_map(arb_finite_function)) {
        let c = IndexedCoproduct::<VecKind, FiniteFunction<VecKind>>::elements(f.clone());
        assert_eq!(c.len(), f.len()); // one 'element' of the sum for each array entry
        assert_eq!(c.values, f); // values are equal to the finite function
        assert_eq!(c.sources.target, 2); // always 2: biggest entry is 1.
    }

    /*

    #[test]
    fn test_indexed_coproduct_map(c in map_with_indexed_coproducts()) {
        todo!()
    }
    */
}
