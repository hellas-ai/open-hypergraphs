use core::fmt::Debug;
use open_hypergraphs::array::*;
use open_hypergraphs::hypergraph::*;

pub fn assert_hypergraph_equality_invariants<K: ArrayKind, O, A>(
    _h0: &Hypergraph<K, O, A>,
    _h1: &Hypergraph<K, O, A>,
) where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + Debug,
    K::Type<A>: Array<K, A> + Debug,
{
    todo!();
}
