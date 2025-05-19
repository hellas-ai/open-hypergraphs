use open_hypergraphs::array::vec::*;
use open_hypergraphs::category::*;
use open_hypergraphs::semifinite::*;
use open_hypergraphs::strict::hypergraph::Hypergraph;
use open_hypergraphs::strict::open_hypergraph::OpenHypergraph;

use crate::hypergraph::strategy::{
    arb_finite_function, arb_finite_function_type, arb_hypergraph, arb_inclusion, arb_semifinite,
    Labels,
};
use proptest::strategy::{BoxedStrategy, Just, Strategy};

use core::fmt::Debug;

/// Generate random identity open hypergraphs.
pub fn identities<
    O: Debug + Clone + PartialEq + 'static,
    A: Debug + Clone + PartialEq + 'static,
>(
    arb_object: BoxedStrategy<O>,
) -> BoxedStrategy<OpenHypergraph<VecKind, O, A>> {
    arb_semifinite(arb_object, None)
        .prop_map(OpenHypergraph::identity)
        .boxed()
}

pub fn arb_open_hypergraph<
    O: Debug + Clone + PartialEq + 'static,
    A: Debug + Clone + PartialEq + 'static,
>(
    labels: Labels<O, A>,
) -> BoxedStrategy<OpenHypergraph<VecKind, O, A>> {
    arb_hypergraph(labels)
        .prop_flat_map(move |h| {
            let source = arb_finite_function_type(10, None, Some(h.w.len()))
                .prop_flat_map(arb_finite_function)
                .boxed();
            let target = arb_finite_function_type(10, None, Some(h.w.len()))
                .prop_flat_map(arb_finite_function)
                .boxed();

            (source, target).prop_map(move |(s, t)| OpenHypergraph::new(s, t, h.clone()).unwrap())
        })
        .boxed()
}

/// Generate an OpenHypergraph with a specified source (domain).
pub fn arb_open_hypergraph_with_source_type<
    O: Debug + Clone + PartialEq + 'static,
    A: Debug + Clone + PartialEq + 'static,
>(
    source: SemifiniteFunction<VecKind, O>,
    labels: Labels<O, A>,
) -> BoxedStrategy<OpenHypergraph<VecKind, O, A>> {
    arb_inclusion(labels, Hypergraph::discrete(source))
        .prop_flat_map(move |a| {
            let s = a.w;
            let h = a.target;

            arb_finite_function_type(0, None, Some(h.w.len()))
                .prop_flat_map(arb_finite_function)
                .prop_map(move |t| OpenHypergraph::new(s.clone(), t, h.clone()).unwrap())
        })
        .boxed()
}

/// Construct a sequence of composable open hypergraphs
///
/// ```text
/// f₁ ; f₂ ; ... ; fn
/// ```
///
/// so that `f_i : A_i → A_{i+1}`
///
/// NOTE: this uses a lot of `clone()`, so it's probably very inefficient!
pub fn arb_composite_open_hypergraph<
    O: Debug + Clone + PartialEq + 'static,
    A: Debug + Clone + PartialEq + 'static,
>(
    n: usize,
    arb_labels: BoxedStrategy<Labels<O, A>>,
) -> BoxedStrategy<Vec<OpenHypergraph<VecKind, O, A>>> {
    if n == 0 {
        return Just(vec![]).boxed();
    }

    // Create loop state: last morphism in the chain, plus a vec of composable morphisms.
    let mut result = arb_labels
        .clone()
        .prop_flat_map(arb_open_hypergraph)
        .prop_map(move |oh| vec![oh])
        .boxed();

    for _ in 0..(n - 1) {
        let arb_labels = arb_labels.clone();
        result = result
            .prop_flat_map(move |xs| {
                arb_labels.clone().prop_flat_map(move |labels| {
                    let xs = xs.clone();
                    let source = xs[xs.len() - 1].target();
                    arb_open_hypergraph_with_source_type(source, labels).prop_map(move |f| {
                        let mut xs = xs.clone();
                        xs.push(f);
                        xs
                    })
                })
            })
            .boxed();
        println!("hi")
    }

    result.boxed()
}
