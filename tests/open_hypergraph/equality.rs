use open_hypergraphs::{array::*, category::*, open_hypergraph::*};

use {crate::hypergraph::equality::assert_hypergraph_equality_invariants, core::fmt::Debug};

pub fn assert_open_hypergraph_equality_invariants<K: ArrayKind, O, A>(
    f: &OpenHypergraph<K, O, A>,
    g: &OpenHypergraph<K, O, A>,
) where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: OrdArray<K, O> + Debug,
    K::Type<A>: OrdArray<K, A> + Debug,
{
    assert_eq!(f.source(), g.source());
    assert_eq!(f.target(), g.target());

    assert_hypergraph_equality_invariants(&f.h, &g.h);
}
