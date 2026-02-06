use crate::array::{Array, ArrayKind, NaturalArray};
use crate::strict::graph;
use crate::strict::hypergraph::Hypergraph;
use num_traits::Zero;

impl<K: ArrayKind, O, A> Hypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
{
    /// Returns true if there is no directed path from any node to itself.
    ///
    pub fn is_acyclic(&self) -> bool {
        if self.w.len() == K::I::zero() {
            return true;
        }

        let adjacency = graph::node_adjacency(self);
        let (_order, unvisited) = graph::kahn(&adjacency);
        unvisited.sum() == K::I::zero()
    }
}
