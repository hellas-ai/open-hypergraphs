use crate::array::{Array, ArrayKind, NaturalArray};
use crate::finite_function::FiniteFunction;

use super::Hypergraph;
use num_traits::{One, Zero};

// A monic subobject of a host hypergraph, represented by masks on wires and edges.
// i.e. morphism: H -> host where H is the graph obtained by G removing nodes/edges in the mask.
//
// This avoids materializing the subgraph morphism data (new node/edge arrays + maps) up front:
// we keep just the host reference and two boolean masks and compute images/injections on demand.
// Performance notes:
// - masks are compact and cheap to copy compared to rebuilding incidence arrays
// - dangling checks and complements can be done by scanning incidence once
// - the concrete subgraph hypergraph H is only built when explicitly requested
pub(crate) struct SubgraphMorphism<'a, K: ArrayKind, O, A> {
    host: &'a Hypergraph<K, O, A>,
    remove_node_mask: K::Type<bool>,
    remove_edge_mask: K::Type<bool>,
}

impl<'a, K: ArrayKind, O, A> SubgraphMorphism<'a, K, O, A>
where
    K::Type<bool>: Array<K, bool>,
{
    pub(crate) fn from_masks(
        host: &'a Hypergraph<K, O, A>,
        remove_node_mask: K::Type<bool>,
        remove_edge_mask: K::Type<bool>,
    ) -> Option<Self>
    where
        K::Type<K::I>: NaturalArray<K>,
        K::Type<O>: Array<K, O>,
        K::Type<A>: Array<K, A>,
    {
        let subgraph = Self {
            host,
            remove_node_mask,
            remove_edge_mask,
        };

        // Complexity: O(|E| + |incidence|) to scan sources/targets once.
        if subgraph.is_dangling() {
            None
        } else {
            Some(subgraph)
        }
    }

    // The maps need not be monic; we only use their images to build a monic subobject.
    // Any non-injectivity in w_map/x_map is collapsed by taking the image masks.
    pub(crate) fn from_maps(
        host: &'a Hypergraph<K, O, A>,
        w_map: &FiniteFunction<K>,
        x_map: &FiniteFunction<K>,
    ) -> Option<Self>
    where
        K::Type<K::I>: NaturalArray<K>,
        K::Type<O>: Array<K, O>,
        K::Type<A>: Array<K, A>,
    {
        // Complexity: O(|W| + |X|) to build masks from the images of w_map/x_map.
        let mut remove_node_mask = K::Type::<bool>::fill(false, host.w.len());
        let mut remove_edge_mask = K::Type::<bool>::fill(false, host.x.len());
        if w_map.table.len() != K::I::zero() {
            remove_node_mask.scatter_assign_constant(&w_map.table, true);
        }
        if x_map.table.len() != K::I::zero() {
            remove_edge_mask.scatter_assign_constant(&x_map.table, true);
        }

        Self::from_masks(host, remove_node_mask, remove_edge_mask)
    }

    // Complexity: O(|E| + |incidence|) where incidence is total number of source/target endpoints.
    fn is_dangling(&self) -> bool
    where
        K::Type<K::I>: NaturalArray<K>,
        K::Type<O>: Array<K, O>,
        K::Type<A>: Array<K, A>,
    {
        let host = self.host;
        let s_ptr = host.s.sources.table.cumulative_sum();
        let t_ptr = host.t.sources.table.cumulative_sum();
        let mut edge = K::I::zero();
        while edge < host.x.len() {
            if !self.remove_edge_mask.get(edge.clone()) {
                let s_start = s_ptr.get(edge.clone());
                let s_end = s_ptr.get(edge.clone() + K::I::one());
                let mut k = s_start.clone();
                while k < s_end {
                    let v = host.s.values.table.get(k.clone());
                    if self.remove_node_mask.get(v) {
                        return true;
                    }
                    k = k + K::I::one();
                }

                let t_start = t_ptr.get(edge.clone());
                let t_end = t_ptr.get(edge.clone() + K::I::one());
                let mut k = t_start.clone();
                while k < t_end {
                    let v = host.t.values.table.get(k.clone());
                    if self.remove_node_mask.get(v) {
                        return true;
                    }
                    k = k + K::I::one();
                }
            }
            edge = edge + K::I::one();
        }
        false
    }

    pub(crate) fn as_hypergraph_with_injections(
        &self,
    ) -> Option<(Hypergraph<K, O, A>, FiniteFunction<K>, FiniteFunction<K>)>
    where
        K::Type<K::I>: NaturalArray<K>,
        K::Type<O>: Array<K, O>,
        K::Type<A>: Array<K, A>,
        K::Type<bool>: Array<K, bool>,
        for<'b> K::Slice<'b, K::I>: From<&'b [K::I]>,
    {
        // Since this subgraph is monic and non-dangling, its image is a valid hypergraph.
        // Complexity: O(|W| + |X| + |incidence|) to build injections and remap incidence.
        let host = self.host;

        // Compute kept node/edge indices and remap table.
        let mut kept_nodes = Vec::<K::I>::new();
        let mut remap_vec = Vec::<K::I>::new();
        let mut next = K::I::zero();
        let mut i = K::I::zero();
        while i < host.w.len() {
            if !self.remove_node_mask.get(i.clone()) {
                kept_nodes.push(i.clone());
                remap_vec.push(next.clone());
                next = next + K::I::one();
            } else {
                // Placeholder index for removed nodes. Kept segments never reference these,
                // because by construction the subgraph is non-dangling
                // so the value is irrelevant as long as the remap target is nonempty.
                remap_vec.push(K::I::zero());
            }
            i = i + K::I::one();
        }
        let remap_table = index_from_vec::<K>(&remap_vec);
        let new_w_len = next.clone();

        let mut kept_edges = Vec::<K::I>::new();
        let mut e = K::I::zero();
        while e < host.x.len() {
            if !self.remove_edge_mask.get(e.clone()) {
                kept_edges.push(e.clone());
            }
            e = e + K::I::one();
        }

        // Injections from subgraph into original.
        let kept_w_inj = FiniteFunction::new(index_from_vec::<K>(&kept_nodes), host.w.len())?;
        let kept_x_inj = FiniteFunction::new(index_from_vec::<K>(&kept_edges), host.x.len())?;

        // Build labels for the subgraph image.
        let new_w = (&kept_w_inj >> &host.w)?;
        let new_x = (&kept_x_inj >> &host.x)?;

        if new_w_len == K::I::zero() {
            // No nodes remain. For a non-dangling subgraph this can only yield an empty hypergraph.
            if !kept_edges.is_empty() {
                return None;
            }
            return Some((Hypergraph::empty(), kept_w_inj, kept_x_inj));
        }

        // Rebuild incidence by filtering kept edges and remapping node indices.
        let remap = FiniteFunction::new(remap_table, new_w_len)?;
        let new_s = host
            .s
            .remove_segments(&self.remove_edge_mask)?
            .map_values(&remap)?;
        let new_t = host
            .t
            .remove_segments(&self.remove_edge_mask)?
            .map_values(&remap)?;

        let remainder = Hypergraph {
            s: new_s,
            t: new_t,
            w: new_w,
            x: new_x,
        };

        Some((remainder, kept_w_inj, kept_x_inj))
    }
}

fn index_from_vec<K: ArrayKind>(v: &Vec<K::I>) -> K::Index
where
    K::Index: Array<K, K::I>,
    for<'a> K::Slice<'a, K::I>: From<&'a [K::I]>,
{
    let slice: K::Slice<'_, K::I> = v.as_slice().into();
    K::Index::from_slice(slice)
}
