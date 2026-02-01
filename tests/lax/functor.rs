use open_hypergraphs::category::*;
use open_hypergraphs::lax::functor::{map_arrow_witness, Functor};
use open_hypergraphs::lax::OpenHypergraph;

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
