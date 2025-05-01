use {
    open_hypergraphs::{array::vec::*, finite_function::*},
    proptest::prelude::{Just, Strategy},
};

const MAX_OBJECT: usize = 8;

pub(crate) fn objects_strategy(
    allow_initial: bool,
    max_value: Option<usize>,
) -> impl Strategy<Value = usize> {
    #[allow(clippy::bool_to_int_with_if)]
    let min_value = if allow_initial { 0 } else { 1 };
    let max_value = max_value.unwrap_or(MAX_OBJECT);
    min_value..max_value
}

pub(crate) fn arrow_type_strategy(
    source: Option<usize>,
    target: Option<usize>,
    zero_to_zero_allowed: bool,
) -> impl Strategy<Value = (usize, usize)> {
    // unlike the python finite_target is fixed to true here
    let source_random = objects_strategy(true, None);
    let target_random = objects_strategy(source == Some(0), None);
    (source_random, target_random).prop_filter_map(
        if zero_to_zero_allowed {
            "No arrows exist of type n → 0 for n != 0."
        } else {
            "No arrows exist of type n → 0 for n != 0 and 0 → 0 has been excluded"
        },
        move |(s, t)| match (source, target) {
            (None, None) => {
                if t == 0 {
                    if zero_to_zero_allowed {
                        Some((0, 0))
                    } else {
                        None
                    }
                } else {
                    Some((s, t))
                }
            }
            (None, Some(pin_target)) => {
                if pin_target == 0 {
                    if zero_to_zero_allowed {
                        Some((0, 0))
                    } else {
                        None
                    }
                } else {
                    Some((s, pin_target))
                }
            }
            (Some(pin_source), None) => {
                if zero_to_zero_allowed {
                    Some((pin_source, t))
                } else if pin_source == 0 && t == 0 {
                    None
                } else {
                    Some((pin_source, t))
                }
            }
            (Some(pin_source), Some(pin_target)) => {
                if pin_target == 0 && pin_source != 0 {
                    None
                } else if zero_to_zero_allowed {
                    Some((pin_source, pin_target))
                } else if pin_source == 0 && pin_target == 0 {
                    None
                } else {
                    Some((pin_source, pin_target))
                }
            }
        },
    )
}

pub(crate) fn arrow_strategy_helper(
    source: Option<usize>,
    target: Option<usize>,
    zero_to_zero_allowed: bool,
) -> impl Strategy<Value = (VecArray<usize>, usize)> {
    // unlike the python finite_target is fixed to true here
    let signature = arrow_type_strategy(source, target, zero_to_zero_allowed);
    signature.prop_ind_flat_map(|(a, b)| {
        if b == 0 {
            let z = proptest::collection::vec(0usize..1, 0);
            (z.prop_map(VecArray), Just(0))
        } else {
            let z = proptest::collection::vec(0usize..b, a);
            (z.prop_map(VecArray), Just(b))
        }
    })
}

pub(crate) fn arrow_strategy(
    source: Option<usize>,
    target: Option<usize>,
    zero_to_zero_allowed: bool,
) -> impl Strategy<Value = FiniteFunction<VecKind>> {
    arrow_strategy_helper(source, target, zero_to_zero_allowed).prop_map(|(f_table, codomain)| {
        FiniteFunction::<VecKind>::new(f_table, codomain).expect("By construction")
    })
}

pub(crate) fn parallel_arrows_strategy<const N: usize>(
    source: Option<usize>,
    target: Option<usize>,
    zero_to_zero_allowed: bool,
) -> impl Strategy<Value = [FiniteFunction<VecKind>; N]> {
    let signature = arrow_type_strategy(source, target, zero_to_zero_allowed);
    signature.prop_ind_flat_map(move |(a, b)| {
        proptest::array::uniform(arrow_strategy(Some(a), Some(b), zero_to_zero_allowed))
    })
}

pub(crate) fn composible_arrows_strategy(
    source: Option<usize>,
    target: Option<usize>,
    zero_to_zero_allowed: bool,
) -> impl Strategy<Value = [FiniteFunction<VecKind>; 2]> {
    let signature = arrow_type_strategy(source, target, zero_to_zero_allowed);
    signature.prop_flat_map(move |(s, t)| {
        let intermediate = if s == 0 && t == 0 {
            objects_strategy(true, None)
        } else {
            objects_strategy(false, None)
        };
        intermediate.prop_flat_map(move |i| {
            let arrow_1 = arrow_strategy(Some(s), Some(i), zero_to_zero_allowed);
            let arrow_2 = arrow_strategy(Some(i), Some(t), zero_to_zero_allowed);
            [arrow_1, arrow_2]
        })
    })
}

pub(crate) fn three_composible_arrows_strategy(
    source: Option<usize>,
    target: Option<usize>,
    zero_to_zero_allowed: bool,
) -> impl Strategy<Value = [FiniteFunction<VecKind>; 3]> {
    let signature = arrow_type_strategy(source, target, zero_to_zero_allowed);
    signature.prop_flat_map(move |(s, t)| {
        let (intermediate_1, intermediate_2) = if s == 0 && t == 0 {
            (objects_strategy(true, None), objects_strategy(true, None))
        } else {
            (objects_strategy(false, None), objects_strategy(false, None))
        };
        (intermediate_1, intermediate_2).prop_flat_map(move |(i1, i2)| {
            let arrow_1 = arrow_strategy(Some(s), Some(i1), zero_to_zero_allowed);
            let arrow_2 = arrow_strategy(Some(i1), Some(i2), zero_to_zero_allowed);
            let arrow_3 = arrow_strategy(Some(i2), Some(t), zero_to_zero_allowed);
            [arrow_1, arrow_2, arrow_3]
        })
    })
}

pub(crate) fn finite_function_strategy(
    max_value: Option<usize>,
    size: impl Into<proptest::sample::SizeRange>,
) -> impl Strategy<Value = (VecArray<usize>, usize)> {
    let max_value = max_value.unwrap_or(MAX_OBJECT);
    let z = proptest::collection::vec(0usize..max_value, size);
    (z, proptest::prelude::any::<usize>()).prop_map(|(x, y)| {
        let max_seen = *x.iter().max().expect("Has a maximum");
        (VecArray(x), max_seen + y)
    })
}

// TODO: Permutation module?
pub(crate) fn permutation_strategy(n: usize) -> impl Strategy<Value = Vec<usize>> {
    let v: Vec<usize> = (0..n).collect();
    Just(v).prop_shuffle()
}

/// A custom strategy to generate a coequalizer
///   q : A → Q
/// and a compatible permutation
///   p : Q → Q
pub(crate) fn coequalizer_and_permutation_strategy(
    zero_to_zero_allowed: bool,
) -> impl Strategy<Value = [FiniteFunction<VecKind>; 4]> {
    let fg = parallel_arrows_strategy(None, None, zero_to_zero_allowed);
    fg.prop_flat_map(move |[f, g]| {
        let q = f.coequalizer(&g).expect("By construction same domain");
        let p = permutation_strategy(q.target);
        p.prop_map(move |p_ff| {
            [
                f.clone(),
                g.clone(),
                q.clone(),
                FiniteFunction::new(VecArray(p_ff), q.target).unwrap(),
            ]
        })
    })
}
