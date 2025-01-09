use std::collections::HashMap;

struct UnionFind {
    /// arbitrarily chosen ancestor of each node
    parent: Vec<usize>,
    /// used to order subtrees by depth
    rank: Vec<usize>,
}

impl UnionFind {
    /// Create a new `UnionFind` with n elements, each in their own set.
    fn new(n: usize) -> Self {
        Self {
            parent: (0..n).collect(),
            rank: vec![0; n],
        }
    }

    /// Find the representative (root) of the set containing x.
    /// Uses path compression to flatten the structure for efficiency.
    fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            self.parent[x] = self.find(self.parent[x]);
        }
        self.parent[x]
    }

    /// Union the sets containing x and y.
    /// Uses union by rank to keep the structure shallow.
    fn union(&mut self, x: usize, y: usize) {
        let root_x = self.find(x);
        let root_y = self.find(y);
        if root_x != root_y {
            #[allow(clippy::comparison_chain)]
            if self.rank[root_x] > self.rank[root_y] {
                self.parent[root_y] = root_x;
            } else if self.rank[root_x] < self.rank[root_y] {
                self.parent[root_x] = root_y;
            } else {
                self.parent[root_y] = root_x;
                self.rank[root_x] += 1;
            }
        }
    }
}

/// Find connected components of a graph stored as a pair of arrays of nodes,
/// interpreted as a list of edges `sources[i] → targets[i]`
///
/// # Panics
///
/// * `sources.len() != targets.len()`
/// * When any `sources[i] >= n` or `targets[i] >= n`
pub fn connected_components(sources: &[usize], targets: &[usize], n: usize) -> (Vec<usize>, usize) {
    // Must have equal sized arrays
    assert_eq!(sources.len(), targets.len());
    // Arrays must be empty if graph has no nodes.
    assert!(n > 0 || sources.is_empty());
    let mut uf = UnionFind::new(n);

    // Union each pair of nodes connected by an edge.
    for (u, v) in sources.iter().zip(targets) {
        uf.union(*u, *v);
    }

    // Find the connected component representative for each node.
    let node_to_other_node = (0..n).map(|i| uf.find(i)).collect::<Vec<_>>();
    let (node_to_component_number, num_components) = to_dense(&node_to_other_node);
    (node_to_component_number, num_components)
}

#[must_use]
/// compress a sparse mapping into a set `[0..A)` into a surjective
/// mapping into a set `[0..num_components)`
pub fn to_dense(sparse: &[usize]) -> (Vec<usize>, usize) {
    let mut representative_nodes: HashMap<usize, usize> = HashMap::new();
    let mut num_components = 0;
    let mut dense: Vec<usize> = Vec::with_capacity(sparse.len());
    for a in sparse {
        if let Some(found_component_number) = representative_nodes.get(a) {
            dense.push(*found_component_number);
        } else {
            representative_nodes.insert(*a, num_components);
            dense.push(num_components);
            num_components += 1;
        }
    }
    (dense, num_components)
}

#[cfg(test)]
mod test {
    use proptest::prelude::{Just, Strategy};
    use proptest::{prop_assert_eq, proptest};

    use super::{connected_components, to_dense};

    #[test]
    fn to_dense_example() {
        assert_eq!(to_dense(&[0, 2, 5, 5, 7]), (vec![0, 1, 2, 2, 3], 4));
    }

    #[test]
    fn small_graph_components() {
        let components = connected_components(&[0, 1, 3], &[1, 2, 4], 5);
        let expected = (vec![0, 0, 0, 1, 1], 2);

        assert_eq!(components, expected);
    }

    fn edges_strategy() -> impl Strategy<Value = (Vec<usize>, Vec<usize>, usize)> {
        (1_usize..100).prop_flat_map(|n| {
            (Just(n), (0..(n + 2) / 2)).prop_flat_map(|(n, e)| {
                (
                    proptest::collection::vec(0..n, e),
                    proptest::collection::vec(0..n, e),
                    Just(n),
                )
            })
        })
    }

    proptest! {
        #[test]
        fn general_components((sources,targets,num_nodes) in edges_strategy()) {
            let (mut z, component_count) = connected_components(&sources, &targets, num_nodes);
            prop_assert_eq!(z.len(),num_nodes);
            // if there is an edge between two nodes they must be in the same component
            for (a,b) in sources.into_iter().zip(targets) {
                prop_assert_eq!(z[a],z[b]);
            }
            // there should be the expected number of components
            let num_components = z.iter().max().copied().expect("At least 1 node") + 1;
            prop_assert_eq!(num_components,component_count);
            // the component labels seen should be surjective onto `[0, num_components)`
            z.sort_unstable();
            z.dedup();
            prop_assert_eq!(z, (0..num_components).collect::<Vec<_>>());
        }
    }
}
