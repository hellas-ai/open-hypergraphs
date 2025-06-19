use open_hypergraphs::array::*;
use open_hypergraphs::strict::open_hypergraph::*;

use crate::hypergraph::equality::assert_hypergraph_equality_invariants;
use core::fmt::Debug;

pub fn assert_open_hypergraph_equality_invariants<K: ArrayKind, O, A>(
    f: &OpenHypergraph<K, O, A>,
    g: &OpenHypergraph<K, O, A>,
) where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: OrdArray<K, O> + PartialEq + Debug,
    K::Type<A>: OrdArray<K, A> + PartialEq + Debug,
{
    assert_eq!(f.source(), g.source());
    assert_eq!(f.target(), g.target());

    assert_hypergraph_equality_invariants(&f.h, &g.h);
}
