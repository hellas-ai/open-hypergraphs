use open_hypergraphs::category::*;
use open_hypergraphs::strict::open_hypergraph::*;

//use super::strategy;
//use crate::open_hypergraph::equality::assert_open_hypergraph_equality_invariants;
use crate::operations::strategy::*;
use crate::theory::meaningless::*;

use proptest::proptest;

proptest! {
    #[test]
    fn test_singleton((x, a, b) in arb_singleton_operation(arb_object(), arb_arrow())) {
        let f = OpenHypergraph::singleton(x, a, b);

        assert_eq!(f.h.x.len(), 1);

        // this invariant is only true for our particular implementation of 'singleton'.
        // The general condition is that there must only exist *permutations* between:
        //      - f.h.w
        //      - f.h.s.values + f.h.t.values
        //      - f.s + f.t
        assert_eq!(f.h.s.values, f.s);
        assert_eq!(f.h.t.values, f.t);

        // Number of internal wires must equal size of boundaries.
        assert_eq!(f.h.s.values.source(), f.source().len());
        assert_eq!(f.h.t.values.source(), f.target().len());
        assert_eq!(f.h.w.len(), f.source().len() + f.target().len());
    }

    #[test]
    fn test_tensor_operations(ops in arb_operations(arb_object(), arb_arrow())) {
        let f = OpenHypergraph::tensor_operations(ops.clone());

        // Number of operations in result should be the same as in the Operations struct
        assert_eq!(f.h.x.len(), ops.len());

        // Same condition for sources
        assert_eq!(f.source(), ops.a.values);
        assert_eq!(f.target(), ops.b.values);

        // Check number of wires equal to the size of source/target types in Operations struct.
        assert_eq!(f.h.w.len(), ops.a.values.len() + ops.b.values.len());
    }
}
