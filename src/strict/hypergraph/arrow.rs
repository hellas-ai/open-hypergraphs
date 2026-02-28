use super::object::Hypergraph;
use crate::array::{Array, ArrayKind, NaturalArray};
use crate::finite_function::FiniteFunction;
use crate::indexed_coproduct::IndexedCoproduct;
use crate::strict::graph::{
    node_adjacency, node_adjacency_from_incidence, sparse_relative_indegree,
};

use core::fmt::Debug;
use num_traits::{One, Zero};

fn successors<K: ArrayKind>(
    adjacency: &IndexedCoproduct<K, FiniteFunction<K>>,
    frontier: &K::Index,
) -> K::Index
where
    K::Type<K::I>: NaturalArray<K>,
{
    if frontier.is_empty() {
        return K::Index::empty();
    }

    let f = FiniteFunction::new(frontier.clone(), adjacency.len()).unwrap();
    let (g, _) = sparse_relative_indegree(adjacency, &f);
    g.table
}

fn filter_unvisited<K: ArrayKind>(visited: &K::Index, candidates: &K::Index) -> K::Index
where
    K::Type<K::I>: NaturalArray<K>,
{
    if candidates.is_empty() {
        return K::Index::empty();
    }

    let visited_on_candidates = visited.gather(candidates.get_range(..));
    let unvisited_ix = visited_on_candidates.zero();
    candidates.gather(unvisited_ix.get_range(..))
}

#[derive(Debug)]
pub enum InvalidHypergraphArrow {
    TypeMismatchW,
    TypeMismatchX,
    NotNaturalW,
    NotNaturalX,
    NotNaturalS,
    NotNaturalT,
}

pub struct HypergraphArrow<K: ArrayKind, O, A> {
    /// Source hypergraph
    pub source: Hypergraph<K, O, A>,

    /// target hypergraph
    pub target: Hypergraph<K, O, A>,

    /// Natural transformation on wires
    pub w: FiniteFunction<K>,

    /// Natural transformation on operations
    pub x: FiniteFunction<K>,
}

impl<K: ArrayKind, O, A> HypergraphArrow<K, O, A>
where
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
{
    /// Safely create a new HypergraphArrow by checking naturality of `w` and `x`.
    pub fn new(
        source: Hypergraph<K, O, A>,
        target: Hypergraph<K, O, A>,
        w: FiniteFunction<K>,
        x: FiniteFunction<K>,
    ) -> Result<Self, InvalidHypergraphArrow>
    where
        K::Type<K::I>: NaturalArray<K>,
    {
        HypergraphArrow {
            source,
            target,
            w,
            x,
        }
        .validate()
    }

    /// Check validity of a HypergraphArrow.
    pub fn validate(self) -> Result<Self, InvalidHypergraphArrow>
    where
        K::Type<K::I>: NaturalArray<K>,
    {
        // for self : g → h
        let g = &self.source;
        let h = &self.target;

        // Naturality checks:
        // wire labels, operation labels, and operation types should be preserved under the natural
        // transformations w and x.

        // Check naturality on node labels:
        // g.w = w ; h.w
        let composed_w = (&self.w >> &h.w).ok_or(InvalidHypergraphArrow::TypeMismatchW)?;
        if g.w != composed_w {
            return Err(InvalidHypergraphArrow::NotNaturalW);
        }

        // Check naturality on operation labels:
        // g.x = x ; h.x
        let composed_x = (&self.x >> &h.x).ok_or(InvalidHypergraphArrow::TypeMismatchX)?;
        if g.x != composed_x {
            return Err(InvalidHypergraphArrow::NotNaturalX);
        }

        // Check naturality of incidence (sources):
        // g.s; w = h.s reindexed along x.
        let s_lhs =
            g.s.map_values(&self.w)
                .ok_or(InvalidHypergraphArrow::NotNaturalS)?;
        let s_rhs =
            h.s.map_indexes(&self.x)
                .ok_or(InvalidHypergraphArrow::NotNaturalS)?;
        if s_lhs != s_rhs {
            return Err(InvalidHypergraphArrow::NotNaturalS);
        }

        // Check naturality of incidence (targets).
        // g.t; w = h.t reindexed along x.
        let t_lhs =
            g.t.map_values(&self.w)
                .ok_or(InvalidHypergraphArrow::NotNaturalT)?;
        let t_rhs =
            h.t.map_indexes(&self.x)
                .ok_or(InvalidHypergraphArrow::NotNaturalT)?;
        if t_lhs != t_rhs {
            return Err(InvalidHypergraphArrow::NotNaturalT);
        }

        Ok(self)
    }

    /// True when this arrow is injective on both nodes and edges.
    pub fn is_monomorphism(&self) -> bool
    where
        K::Type<K::I>: NaturalArray<K>,
    {
        self.w.is_injective() && self.x.is_injective()
    }

    /// Check convexity of a subgraph `H → G`.
    ///
    pub fn is_convex_subgraph(&self) -> bool
    where
        K::Type<K::I>: NaturalArray<K>,
    {
        if !self.is_monomorphism() {
            return false;
        }

        let g = &self.target;
        let n_nodes = g.w.len();
        let n_edges = g.x.len();

        // Build the complement set of edges (those not in the image of x).
        let mut edge_mask = K::Index::fill(K::I::zero(), n_edges.clone());
        edge_mask.scatter_assign_constant(&self.x.table, K::I::one());
        let outside_edge_ix = edge_mask.zero();
        let outside_edges = FiniteFunction::new(outside_edge_ix, n_edges).unwrap();

        // Adjacency restricted to edges in the subobject (inside edges).
        let s_in = g.s.map_indexes(&self.x).unwrap();
        let t_in = g.t.map_indexes(&self.x).unwrap();
        let adj_in = node_adjacency_from_incidence(&s_in, &t_in);

        // Adjacency restricted to edges outside the subobject.
        let s_out = g.s.map_indexes(&outside_edges).unwrap();
        let t_out = g.t.map_indexes(&outside_edges).unwrap();
        let adj_out = node_adjacency_from_incidence(&s_out, &t_out);

        // Full adjacency (used after we've already left the subobject).
        let adj_all = node_adjacency(g);

        // Two-layer reachability:
        // - layer 0: paths that have used only inside edges
        // - layer 1: paths that have used at least one outside edge
        //
        // Convexity fails iff some selected node is reachable in layer 1.
        let mut visited0 = K::Index::fill(K::I::zero(), n_nodes.clone());
        let mut visited1 = K::Index::fill(K::I::zero(), n_nodes.clone());
        let mut frontier0 = self.w.table.clone();
        let mut frontier1 = K::Index::empty();

        // Seed search from the selected nodes.
        visited0.scatter_assign_constant(&frontier0, K::I::one());

        while !frontier0.is_empty() || !frontier1.is_empty() {
            // From layer 0, inside edges stay in layer 0.
            let next0: K::Index = successors::<K>(&adj_in, &frontier0);
            // From layer 0, outside edges move to layer 1.
            let next1_from0 = successors::<K>(&adj_out, &frontier0);
            // From layer 1, any edge keeps you in layer 1.
            let next1_from1 = successors::<K>(&adj_all, &frontier1);

            // Avoid revisiting nodes we've already seen in the same layer.
            let next0: K::Index = filter_unvisited::<K>(&visited0, &next0);

            let next1: K::Index = {
                let merged = next1_from0.concatenate(&next1_from1);
                if merged.is_empty() {
                    K::Index::empty()
                } else {
                    let (unique, _) = merged.sparse_bincount();
                    filter_unvisited::<K>(&visited1, &unique)
                }
            };

            if next0.is_empty() && next1.is_empty() {
                break;
            }

            // Mark and advance frontiers.
            visited0.scatter_assign_constant(&next0, K::I::one());
            visited1.scatter_assign_constant(&next1, K::I::one());
            frontier0 = next0;
            frontier1 = next1;
        }

        // If any selected node is reachable in layer 1, it's not convex.
        let reached_selected = visited1.gather(self.w.table.get_range(..));
        !reached_selected.max().map_or(false, |m| m >= K::I::one())
    }
}

impl<K: ArrayKind, O: Debug, A: Debug> Debug for HypergraphArrow<K, O, A>
where
    K::Index: Debug,
    K::Type<K::I>: Debug,
    K::Type<A>: Debug,
    K::Type<O>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HypergraphArrow")
            .field("source", &self.source)
            .field("target", &self.target)
            .field("w", &self.w)
            .field("x", &self.x)
            .finish()
    }
}

impl<K: ArrayKind, O: Debug, A: Debug> Clone for HypergraphArrow<K, O, A>
where
    K::Type<K::I>: Clone,
    K::Type<A>: Clone,
    K::Type<O>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            source: self.source.clone(),
            target: self.target.clone(),
            w: self.w.clone(),
            x: self.x.clone(),
        }
    }
}
