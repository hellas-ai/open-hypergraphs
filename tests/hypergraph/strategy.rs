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

#[non_exhaustive]
#[derive(Debug, PartialEq, Clone)]
pub struct FiniteFunctionType {
    pub source: usize,
    pub target: usize,
}

impl FiniteFunctionType {
    pub fn new(source: usize, target: usize) -> Option<Self> {
        if source > 0 && target == 0 {
            None
        } else {
            Some(Self { source, target })
        }
    }
}

pub fn arb_finite_function_type(
    max_size: usize,
    fixed_a: Option<usize>,
    fixed_b: Option<usize>,
) -> BoxedStrategy<FiniteFunctionType> {
    // If the user specified an impossible type (e.g., 1 → 0), crash immediately.
    if let (Some(a), Some(b)) = (fixed_a, fixed_b) {
        assert!(
            b > 0 || a == 0,
            "Impossible FiniteFunction type specified: {:?} → {:?}",
            a,
            b
        );
    }

    let a_strategy = match fixed_a {
        Some(a) => Just(a).boxed(),
        None => (0..=max_size).boxed(),
    };

    let b_strategy = match fixed_b {
        Some(b) => Just(b).boxed(),
        None => (1..=max_size).boxed(),
    };

    // Filter out impossible types.
    (a_strategy, b_strategy)
        .prop_filter("Domain size must be >= codomain size for n>0", |(a, b)| {
            *b > 0 || *a == 0
        })
        .prop_map(|(a, b)| FiniteFunctionType::new(a, b).unwrap())
        .boxed()
}

pub fn arb_finite_function(
    FiniteFunctionType { source, target }: FiniteFunctionType,
) -> BoxedStrategy<FiniteFunction<VecKind>> {
    assert!(target > 0 || source == 0, "what"); // check this is a valid finite function type

    // Generate a vector of values in range 0..target with length source
    vec((0..target).boxed(), source..=source)
        .prop_map(move |values| FiniteFunction::new(VecArray(values), target).unwrap())
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
    len: usize,
    target: usize,
) -> BoxedStrategy<IndexedCoproduct<VecKind, FiniteFunction<VecKind>>> {
    // Max source is arbitrarily chosen as 10, unless target is 0, in which case it must also be 0.
    let max_source: usize = if target == 0 { 0 } else { 10 };
    let sources = arb_semifinite((0..=max_source).boxed(), Some(Just(len).boxed()));

    // Create strategy for the values FiniteFunction
    sources
        .prop_flat_map(move |sources| {
            let sources = sources.clone();
            // Calculate total size needed for values - sum of sources
            let total_size = sources.0.as_ref().iter().sum();

            let ff = arb_finite_function_type(1, Some(total_size), Some(target))
                .prop_flat_map(arb_finite_function)
                .boxed();

            // Combine into IndexedCoproduct
            ff.prop_map(move |values| {
                let sources = sources.clone();
                IndexedCoproduct::new(sources, values).unwrap()
            })
        })
        .boxed()
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
    let objects = arb_semifinite::<O>(arb_object, None);
    (objects, operations)
        .prop_map(|(w, x)| Labels { w, x })
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
    let num_arr = x.len();
    let num_obj = w.len();

    let s = arb_indexed_coproduct_finite(num_arr, num_obj);
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

#[derive(Clone, Debug)]
pub struct DiscreteSpan<O: Debug, A: Debug> {
    pub l: HypergraphArrow<VecKind, O, A>,
    #[allow(dead_code)]
    pub h: Hypergraph<VecKind, O, A>,
    pub r: HypergraphArrow<VecKind, O, A>,
}

impl<O: PartialEq + Clone + Debug, A: PartialEq + Clone + Debug> DiscreteSpan<O, A> {
    pub fn validate(self) -> Self {
        let DiscreteSpan { ref l, ref r, .. } = self;

        // 0: Check that targets of l and r have the correct number of wires, operations
        assert_eq!(l.w.target(), l.target.w.len());
        assert_eq!(r.w.target(), r.target.w.len());
        assert_eq!(l.x.target(), l.target.x.len());
        assert_eq!(r.x.target(), r.target.x.len());

        self
    }
}

pub fn arb_discrete_span<
    O: PartialEq + Clone + Debug + 'static,
    A: PartialEq + Clone + Debug + 'static,
>(
    labels: Labels<O, A>,
) -> BoxedStrategy<DiscreteSpan<O, A>> {
    // discrete hypergraph k
    let h = Hypergraph::<VecKind, O, A>::discrete(labels.w.clone());

    // Create two inclusions of h into arbitrary destinations.
    arb_inclusion(labels.clone(), h.clone())
        .prop_flat_map(move |l| {
            let l = l.clone();
            let h = h.clone();
            arb_inclusion(labels.clone(), h.clone()).prop_flat_map(move |r| {
                let l = l.clone();
                let h = h.clone();
                Just(DiscreteSpan { l, h, r }.validate())
            })
        })
        .boxed()
}

pub type LabeledCospan<T> = (
    FiniteFunction<VecKind>,
    FiniteFunction<VecKind>,
    SemifiniteFunction<VecKind, T>,
);

pub fn arb_cospan_type() -> BoxedStrategy<(FiniteFunctionType, FiniteFunctionType)> {
    let max_size = 10;
    let s = arb_finite_function_type(max_size.clone(), None, None);
    s.prop_flat_map(move |s| {
        let target = s.target;
        (
            Just(s),
            arb_finite_function_type(max_size, None, Some(target)),
        )
    })
    .boxed()
}

pub fn arb_labeled_cospan<O: Debug + Clone + PartialEq + 'static>(
    arb_object: BoxedStrategy<O>,
) -> BoxedStrategy<LabeledCospan<O>> {
    arb_cospan_type()
        .prop_flat_map(move |(ts, tt)| {
            (
                arb_finite_function(ts),
                arb_finite_function(tt.clone()),
                arb_semifinite::<O>(arb_object.clone(), Some(Just(tt.target).boxed())),
            )
        })
        .boxed()
}
