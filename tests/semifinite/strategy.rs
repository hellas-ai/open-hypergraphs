use crate::finite_function::strategy::arrow_type_strategy;
use open_hypergraphs::{array::vec::*, semifinite::*};
use proptest::prelude::{Arbitrary, Strategy};

pub(crate) fn semifinite_strategy<T: Arbitrary>(
    source: Option<usize>,
    length_must_be_nonzero: bool,
) -> impl Strategy<Value = SemifiniteFunction<VecKind, T>> {
    if length_must_be_nonzero {
        assert!(source.is_none() || source.is_some_and(|z| z > 0));
    }
    let signature = arrow_type_strategy(source, None, !length_must_be_nonzero);
    let signature = signature.prop_filter("nonzero semifinite function domain", move |(a, _)| {
        !length_must_be_nonzero || *a > 0
    });
    signature.prop_ind_flat_map(|(a, _)| {
        let v = proptest::collection::vec(proptest::arbitrary::any::<T>(), a);
        v.prop_map(|v| SemifiniteFunction(VecArray(v)))
    })
}
