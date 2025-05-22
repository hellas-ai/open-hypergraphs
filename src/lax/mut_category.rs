//! Mutable versions of categorical operations.
use super::hypergraph::*;
use super::open_hypergraph::*;

fn add_offset<'a, I>(n: usize, xs: I) -> impl Iterator<Item = NodeId> + 'a
where
    I: Iterator<Item = &'a NodeId> + 'a,
{
    xs.map(move |i| NodeId(i.0 + n))
}

impl<O, A> Hypergraph<O, A> {
    /// Compute the coproduct `H₀ + H₁` by mutating the data of `H₁`
    pub fn coproduct_assign(&mut self, rhs: Hypergraph<O, A>) {
        let n = self.nodes.len();

        self.adjacency
            .extend(rhs.adjacency.into_iter().map(|edge| Hyperedge {
                sources: add_offset(n, edge.sources.iter()).collect(),
                targets: add_offset(n, edge.targets.iter()).collect(),
            }));

        self.quotient.0.extend(add_offset(n, rhs.quotient.0.iter()));
        self.quotient.1.extend(add_offset(n, rhs.quotient.1.iter()));

        // no offset; these are coproducts.
        self.nodes.extend(rhs.nodes);
        self.edges.extend(rhs.edges);
    }
}

impl<O, A> OpenHypergraph<O, A> {
    /// Compute the tensor product `f.tensor(g)` by mutating the data of `f`
    pub fn tensor_assign(&mut self, rhs: OpenHypergraph<O, A>) {
        let n = self.hypergraph.nodes.len();
        self.hypergraph.coproduct_assign(rhs.hypergraph);
        self.sources
            .extend(rhs.sources.iter().map(|&i| NodeId(i.0 + n)));
        self.targets
            .extend(rhs.targets.iter().map(|&i| NodeId(i.0 + n)));
    }
}
