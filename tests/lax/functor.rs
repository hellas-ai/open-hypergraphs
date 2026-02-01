use open_hypergraphs::array::vec::VecArray;
use open_hypergraphs::category::*;
use open_hypergraphs::lax::functor::{map_arrow_witness, Functor};
use open_hypergraphs::lax::{NodeId, OpenHypergraph};
use open_hypergraphs::strict::vec::FiniteFunction;

use crate::open_hypergraph::equality::*;
use crate::theory::meaningless::*;

use proptest::proptest;

/// Identity functor implemented via the lax `map_arrow_witness` code path
/// (as opposed to `dyn_functor::Identity` which round-trips through strict).
#[derive(Clone)]
struct LaxIdentity;

impl<O: PartialEq + Clone, A: Clone + PartialEq> Functor<O, A, O, A> for LaxIdentity {
    fn map_object(&self, o: &O) -> impl ExactSizeIterator<Item = O> {
        std::iter::once(o.clone())
    }

    fn map_operation(&self, a: &A, source: &[O], target: &[O]) -> OpenHypergraph<O, A> {
        OpenHypergraph::singleton(a.clone(), source.to_vec(), target.to_vec())
    }

    fn map_arrow(&self, f: &OpenHypergraph<O, A>) -> OpenHypergraph<O, A> {
        let f_strict;
        let f = if f.hypergraph.is_strict() {
            f
        } else {
            f_strict = {
                let mut c = f.clone();
                c.quotient();
                c
            };
            &f_strict
        };
        let (mut result, _witness) = map_arrow_witness(self, f).unwrap();
        result.quotient();
        result
    }
}

proptest! {
    // Id(f) == f
    #[test]
    fn test_lax_identity_functor_reflexive(f in arb_open_hypergraph()) {
        let lax_f = OpenHypergraph::from_strict(f.clone());
        let g = LaxIdentity.map_arrow(&lax_f);
        assert_open_hypergraph_equality_invariants(&f, &g.to_strict());
    }

    /// Id(f ; g) == f ; g
    #[test]
    fn test_lax_identity_functor_preserves_composition(v in arb_composite_open_hypergraph(2)) {
        let [f, g] = v.as_slice() else { panic!("arb_composite_open_hypergraph returned unexpected size result") };

        let lax_f = OpenHypergraph::from_strict(f.clone());
        let lax_g = OpenHypergraph::from_strict(g.clone());

        let identity_composed = LaxIdentity.map_arrow(&lax_f.compose(&lax_g).unwrap());

        let mapped_f = LaxIdentity.map_arrow(&lax_f);
        let mapped_g = LaxIdentity.map_arrow(&lax_g);
        let composed_identity = mapped_f.compose(&mapped_g).unwrap();

        assert_open_hypergraph_equality_invariants(
            &identity_composed.to_strict(),
            &composed_identity.to_strict(),
        );
    }
}

////////////////////////////////////////////////////////////////////////////////
// Simple language tests

#[derive(Clone, PartialEq, Debug)]
pub enum Arr {
    Add, // 2 -> 1
    Neg, // 1 -> 1
    Sub, // 2 -> 1, expands to `(id x neg) ; add`
}

// A functor that expands the "Sub" definition in terms of add and neg.
#[derive(Clone)]
struct ExpandDefinitions;

impl Functor<(), Arr, (), Arr> for ExpandDefinitions {
    fn map_object(&self, _o: &()) -> impl ExactSizeIterator<Item = ()> {
        std::iter::once(())
    }

    fn map_operation(&self, a: &Arr, source: &[()], target: &[()]) -> OpenHypergraph<(), Arr> {
        match a {
            Arr::Add | Arr::Neg => {
                OpenHypergraph::singleton(a.clone(), source.to_vec(), target.to_vec())
            }
            Arr::Sub => {
                // (id ⊗ neg) ; add
                let id = OpenHypergraph::<(), Arr>::identity(vec![()]);
                let neg = OpenHypergraph::singleton(Arr::Neg, vec![()], vec![()]);
                let add = OpenHypergraph::singleton(Arr::Add, vec![(), ()], vec![()]);
                id.tensor(&neg).compose(&add).unwrap()
            }
        }
    }

    fn map_arrow(&self, f: &OpenHypergraph<(), Arr>) -> OpenHypergraph<(), Arr> {
        let f_strict;
        let f = if f.hypergraph.is_strict() {
            f
        } else {
            f_strict = {
                let mut c = f.clone();
                c.quotient();
                c
            };
            &f_strict
        };
        let (mut result, _witness) = map_arrow_witness(self, f).unwrap();
        result.quotient();
        result
    }
}

/// Helper: convert a `&[NodeId]` to a `FiniteFunction` targeting `n`.
fn node_ids_to_ff(ids: &[NodeId], n: usize) -> FiniteFunction {
    FiniteFunction::new(VecArray(ids.iter().map(|x| x.0).collect()), n).unwrap()
}

#[test]
fn test_sub_expand() {
    let sub = OpenHypergraph::singleton(Arr::Sub, vec![(), ()], vec![()]);
    let (mut result, witness) = map_arrow_witness(&ExpandDefinitions, &sub).unwrap();

    // The input `sub` has 3 nodes, map_object maps () → [()],
    // so the witness should have 3 segments each of size 1.
    assert_eq!(witness.sources.table.0, vec![1, 1, 1]);

    // Quotient the result, mapping the witness through the coequalizer.
    let q = result.quotient_witness();
    let witness = witness.map_values(&q).unwrap();

    // Source/target types are preserved
    assert_eq!(result.source(), vec![(), ()]);
    assert_eq!(result.target(), vec![()]);

    // Sub expands to (id ⊗ neg) ; add, which has 2 edges: Neg and Add
    assert_eq!(result.hypergraph.edges.len(), 2);
    assert!(result.hypergraph.edges.contains(&Arr::Neg));
    assert!(result.hypergraph.edges.contains(&Arr::Add));

    // The input source/target maps composed with the witness should equal
    // the output source/target maps.
    let n_in = sub.hypergraph.nodes.len();
    let n_out = result.hypergraph.nodes.len();
    let sub_s = node_ids_to_ff(&sub.sources, n_in);
    let sub_t = node_ids_to_ff(&sub.targets, n_in);
    assert_eq!(
        (&sub_s >> &witness.values).unwrap(),
        node_ids_to_ff(&result.sources, n_out)
    );
    assert_eq!(
        (&sub_t >> &witness.values).unwrap(),
        node_ids_to_ff(&result.targets, n_out)
    );
}
