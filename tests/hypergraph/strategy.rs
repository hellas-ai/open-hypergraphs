use core::fmt::Debug;

use open_hypergraphs::array::vec::*;
use open_hypergraphs::category::*;
use open_hypergraphs::finite_function::*;
use open_hypergraphs::hypergraph::{arrow::*, *};
use open_hypergraphs::indexed_coproduct::*;
use open_hypergraphs::semifinite::*;

use proptest::collection::vec;
use proptest::prelude::*;
use proptest::strategy::{BoxedStrategy, Strategy};

const MAX_HYPERNODES: usize = 32;

// NOTE: this definition is not safe, because we could generate a finite function of type
// `n → 1` for `n > 0`. However, it's only ever called safely: see arb_num_hypernodes
pub fn arb_finite_function(
    source: BoxedStrategy<usize>,
    target: BoxedStrategy<usize>,
) -> BoxedStrategy<FiniteFunction<VecKind>> {
    (source, target)
        .prop_flat_map(|(source_size, target_size)| {
            // Generate a vector of values in range 0..target_size with length source_size
            vec((0..target_size).boxed(), source_size..=source_size)
                .prop_map(move |values| FiniteFunction::new(VecArray(values), target_size).unwrap())
        })
        .boxed()
}

pub fn arb_semifinite<T: Debug + 'static>(
    arb_element: BoxedStrategy<T>,
    num_elements: Option<BoxedStrategy<usize>>,
) -> BoxedStrategy<SemifiniteFunction<VecKind, T>> {
    // Use provided size strategy or default to 0..=10
    let size_strategy = num_elements.unwrap_or_else(|| (0usize..=10).boxed());

    size_strategy
        .prop_flat_map(move |size| vec(arb_element.clone(), size..=size))
        .prop_map(|v| SemifiniteFunction(VecArray(v)))
        .boxed()
}

pub fn arb_indexed_coproduct_finite(
    len: BoxedStrategy<usize>,
    target: BoxedStrategy<usize>,
) -> BoxedStrategy<IndexedCoproduct<VecKind, FiniteFunction<VecKind>>> {
    // Generate sources - a SemifiniteFunction<VecKind, usize>
    // We need each element to be small enough that their sum won't exceed the target
    let small_nums = (0..=5usize).boxed(); // Small numbers to keep sums manageable
    let sources = arb_semifinite(small_nums, Some(len));

    // Create strategy for the values FiniteFunction
    sources
        .prop_flat_map(move |sources| {
            let sources = sources.clone();
            // Calculate total size needed for values - sum of sources
            let total_size = sources.0.as_ref().iter().sum();

            // Generate a FiniteFunction of appropriate size
            // TODO: likely to crash when target is 0.
            let ff = arb_finite_function(Just(total_size).boxed(), target.clone());

            // Combine into IndexedCoproduct
            ff.prop_map(move |values| {
                let sources = sources.clone();
                IndexedCoproduct::new(sources, values).unwrap()
            })
        })
        .boxed()
}

/// Generate a number of hypernodes given the number of hyperedges
pub fn arb_num_hypernodes(num_hyperedges: usize) -> BoxedStrategy<usize> {
    let min_value = if num_hyperedges > 0 { 1 } else { 0 };
    (min_value..=MAX_HYPERNODES).boxed()
}

/// The *label arrays* for a hypergraph.
/// Arbitrary arrays of elements from the sets O and A respectively.
#[derive(Clone, Debug)]
pub struct Labels<O, A> {
    pub w: SemifiniteFunction<VecKind, O>,
    pub x: SemifiniteFunction<VecKind, A>,
}

/// Generate random label functions (w, x) for a hypergraph
pub fn arb_labels<
    O: PartialEq + Clone + Debug + 'static,
    A: PartialEq + Clone + Debug + 'static,
>(
    arb_object: BoxedStrategy<O>,
    arb_arrow: BoxedStrategy<A>,
) -> BoxedStrategy<Labels<O, A>> {
    let operations = arb_semifinite::<A>(arb_arrow, None);
    operations
        .prop_flat_map(move |x| {
            let num_hyperedges = x.len();
            let num_wires = arb_num_hypernodes(num_hyperedges);
            let wires = arb_semifinite::<O>(arb_object.clone(), Some(num_wires));
            wires.prop_flat_map(move |w| Just(Labels { w, x: x.clone() }))
        })
        .boxed()
}

/// Generate an arbitrary hypergraph from the two arrays of labels w and x.
/// Note that this hypergraph need not be monogamous acyclic.
pub fn arb_hypergraph<
    O: PartialEq + Clone + Debug + 'static,
    A: PartialEq + Clone + Debug + 'static,
>(
    Labels { w, x }: Labels<O, A>,
) -> BoxedStrategy<Hypergraph<VecKind, O, A>> {
    let num_arr = Just(x.len()).boxed();
    let num_obj = Just(w.len()).boxed();
    let s = arb_indexed_coproduct_finite(num_arr.clone(), num_obj.clone());
    let t = arb_indexed_coproduct_finite(num_arr, num_obj);
    (s, t)
        .prop_flat_map(move |(s, t)| {
            Just(Hypergraph {
                s,
                t,
                w: w.clone(),
                x: x.clone(),
            })
        })
        .boxed()
}

////////////////////////////////////////////////////////////////////////////////
// Discrete spans

/// Given a hypergraph `G`, generate an inclusion `i : G → G + H` by generating a random `H`.
pub fn arb_inclusion<
    O: PartialEq + Clone + Debug + 'static,
    A: PartialEq + Clone + Debug + 'static,
>(
    labels: Labels<O, A>,
    g: Hypergraph<VecKind, O, A>,
) -> BoxedStrategy<HypergraphArrow<VecKind, O, A>> {
    // We have an arbitrary hypergraph g, and some arbitrary label arrays.
    let h_labels = Labels {
        w: g.w.clone() + labels.w,
        x: g.x.clone() + labels.x,
    };
    arb_hypergraph(h_labels)
        .prop_flat_map(move |h| {
            let w = FiniteFunction::inj0(g.w.len(), h.w.len() - g.w.len());
            let x = FiniteFunction::inj0(g.x.len(), h.x.len() - g.x.len());

            Just(HypergraphArrow::new(g.clone(), h, w, x).expect("valid HypergraphArrow"))
        })
        .boxed()
}
