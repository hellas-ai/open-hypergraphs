use open_hypergraphs::array::vec::*;
use open_hypergraphs::category::*;
use open_hypergraphs::finite_function::*;
use open_hypergraphs::indexed_coproduct::*;

use super::strategy::*;
use crate::hypergraph::strategy::*;
use proptest::{prelude::*, proptest};

static MAX_SIZE: usize = 10;

proptest! {
    #[test]
    /// Check basic invariants hold for the `IndexedCoproduct::singleton` constructor
    fn test_singleton_finite(f in arb_finite_function_type(MAX_SIZE, None, None).prop_flat_map(arb_finite_function)) {
        let c = IndexedCoproduct::<VecKind, FiniteFunction<VecKind>>::singleton(f.clone());
        assert_eq!(c.len(), 1);
        assert_eq!(c.sources.target, c.sources.table.0.into_iter().sum::<usize>() + 1);
        assert_eq!(c.values, f);
    }

    #[test]
    /// Check basic invariants hold for the `IndexedCoproduct::elements` constructor
    fn test_elements_finite(f in arb_finite_function_type(MAX_SIZE, None, None).prop_flat_map(arb_finite_function)) {
        let c = IndexedCoproduct::<VecKind, FiniteFunction<VecKind>>::elements(f.clone());
        assert_eq!(c.len(), f.len()); // one 'element' of the sum for each array entry
        assert_eq!(c.values, f); // values are equal to the finite function
        assert_eq!(c.sources.target, 2); // always 2: biggest entry is 1.
    }

    #[test]
    /// Verify that `IndexedCoproduct::map_indexes` works as expected
    fn test_indexed_coproduct_map((c, x) in arb_map_with_indexed_coproducts()) {
        let d = c.map_indexes(&x).unwrap();

        // TODO: also check that sources and values arrays are as expected
        assert_eq!(d.len(), x.source());
    }
}
