use core::fmt::Debug;
use open_hypergraphs::array::*;
use open_hypergraphs::finite_function::*;
use open_hypergraphs::strict::hypergraph::*;

pub fn assert_hypergraph_equality_invariants<K: ArrayKind, O, A>(
    h: &Hypergraph<K, O, A>,
    g: &Hypergraph<K, O, A>,
) where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: OrdArray<K, O> + PartialEq + Debug,
    K::Type<A>: OrdArray<K, A> + PartialEq + Debug,
{
    assert_eq!(h.w.len(), g.w.len());

    // Verify that wire labels are the same up to permutation.
    let i = FiniteFunction::<K>::new(h.w.0.argsort(), h.w.0.len()).unwrap();
    let j = FiniteFunction::<K>::new(g.w.0.argsort(), g.w.0.len()).unwrap();

    let h_w_sorted = (&i >> &h.w).unwrap();
    let g_w_sorted = (&j >> &g.w).unwrap();
    assert_eq!(h_w_sorted, g_w_sorted);

    // Verify that operation labels are the same up to permutation.
    let i = FiniteFunction::<K>::new(h.x.0.argsort(), h.x.0.len()).unwrap();
    let j = FiniteFunction::<K>::new(g.x.0.argsort(), g.x.0.len()).unwrap();

    let h_x_sorted = (&i >> &h.x).unwrap();
    let g_x_sorted = (&j >> &g.x).unwrap();
    assert_eq!(h_x_sorted, g_x_sorted);
}
