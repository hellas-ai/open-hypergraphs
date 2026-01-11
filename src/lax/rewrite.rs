use crate::array::vec::{VecArray, VecKind};
use crate::finite_function::FiniteFunction;
use crate::lax::{Arrow, Coproduct, Hyperedge, Hypergraph, LaxSpan, NodeEdgeMap, NodeId};

struct ExplodedContext<O, A> {
    graph: Hypergraph<O, A>,
    to_g: NodeEdgeMap,
    to_copied_plus_left: NodeEdgeMap,
}

/// Rewrite a lax hypergraph using a rule span and candidate map.
pub fn rewrite<O: Clone + PartialEq, A: Clone + PartialEq>(
    g: &Hypergraph<O, A>,
    rule: &LaxSpan<O, A>,
    candidate: &NodeEdgeMap,
) -> Option<Vec<Hypergraph<O, A>>> {
    validate_candidate_map(rule, g, candidate);
    if !identification_condition(rule, candidate) || !dangling_condition(rule, candidate, g) {
        return None;
    }
    let exploded = exploded_context(g, rule, candidate);
    let fiber_inputs = fiber_partition_inputs(&exploded);
    let partitions_per_fiber: Vec<Vec<FiberPartition>> = fiber_inputs
        .iter()
        .map(enumerate_fiber_partitions)
        .collect();

    if partitions_per_fiber.is_empty() {
        return Some(vec![exploded.graph]);
    }

    let mut complements = Vec::new();
    let mut selection: Vec<usize> = Vec::with_capacity(partitions_per_fiber.len());

    fn pushout_complement<O: Clone + PartialEq, A: Clone>(
        exploded: &ExplodedContext<O, A>,
        partitions_per_fiber: &[Vec<FiberPartition>],
        selection: &[usize],
    ) -> Hypergraph<O, A> {
        let mut quotient_left = Vec::new();
        let mut quotient_right = Vec::new();

        for (fiber_idx, &partition_idx) in selection.iter().enumerate() {
            let partition = &partitions_per_fiber[fiber_idx][partition_idx];
            for block in &partition.blocks {
                let Some((first, rest)) = block.nodes.split_first() else {
                    continue;
                };
                for node in rest {
                    quotient_left.push(*first);
                    quotient_right.push(*node);
                }
            }
        }

        let mut complement = exploded.graph.clone();
        complement.quotient = (quotient_left, quotient_right);
        complement.quotient();
        complement
    }

    fn walk<O: Clone + PartialEq, A: Clone>(
        idx: usize,
        exploded: &ExplodedContext<O, A>,
        partitions_per_fiber: &[Vec<FiberPartition>],
        selection: &mut Vec<usize>,
        complements: &mut Vec<Hypergraph<O, A>>,
    ) {
        if idx == partitions_per_fiber.len() {
            complements.push(pushout_complement(
                exploded,
                partitions_per_fiber,
                selection,
            ));
            return;
        }

        for partition_idx in 0..partitions_per_fiber[idx].len() {
            selection.push(partition_idx);
            walk(
                idx + 1,
                exploded,
                partitions_per_fiber,
                selection,
                complements,
            );
            selection.pop();
        }
    }

    walk(
        0,
        &exploded,
        &partitions_per_fiber,
        &mut selection,
        &mut complements,
    );

    Some(complements)
}

fn exploded_context<O: Clone, A: Clone>(
    g: &Hypergraph<O, A>,
    rule: &LaxSpan<O, A>,
    candidate: &NodeEdgeMap,
) -> ExplodedContext<O, A> {
    let mut in_image_nodes = vec![false; g.nodes.len()];
    for i in 0..candidate.nodes.source() {
        let idx = candidate.nodes.table[i];
        in_image_nodes[idx] = true;
    }

    let mut in_image_edges = vec![false; g.edges.len()];
    for i in 0..candidate.edges.source() {
        let idx = candidate.edges.table[i];
        in_image_edges[idx] = true;
    }

    let mut h = Hypergraph::empty();
    let mut h_node_to_g = Vec::new();
    let mut node_map: Vec<Option<usize>> = vec![None; g.nodes.len()];
    for (idx, label) in g.nodes.iter().enumerate() {
        if in_image_nodes[idx] {
            continue;
        }
        let new_id = h.new_node(label.clone());
        h_node_to_g.push(idx);
        node_map[idx] = Some(new_id.0);
    }

    let mut h_edge_to_g = Vec::new();
    for (edge_id, edge) in g.adjacency.iter().enumerate() {
        if in_image_edges[edge_id] {
            continue;
        }

        let mut sources = Vec::with_capacity(edge.sources.len());
        for node in &edge.sources {
            let new_id = match node_map[node.0] {
                Some(existing) => NodeId(existing),
                None => {
                    let new_id = h.new_node(g.nodes[node.0].clone());
                    h_node_to_g.push(node.0);
                    new_id
                }
            };
            sources.push(new_id);
        }

        let mut targets = Vec::with_capacity(edge.targets.len());
        for node in &edge.targets {
            let new_id = match node_map[node.0] {
                Some(existing) => NodeId(existing),
                None => {
                    let new_id = h.new_node(g.nodes[node.0].clone());
                    h_node_to_g.push(node.0);
                    new_id
                }
            };
            targets.push(new_id);
        }

        h.new_edge(g.edges[edge_id].clone(), Hyperedge { sources, targets });
        h_edge_to_g.push(edge_id);
    }

    let q_h_nodes = FiniteFunction::<VecKind>::new(VecArray(h_node_to_g), g.nodes.len()).unwrap();
    let q_h_edges = FiniteFunction::<VecKind>::new(VecArray(h_edge_to_g), g.edges.len()).unwrap();
    let q_k_nodes = rule
        .left_map
        .nodes
        .compose(&candidate.nodes)
        .expect("candidate map left nodes compose");
    let q_k_edges = rule
        .left_map
        .edges
        .compose(&candidate.edges)
        .expect("candidate map left edges compose");

    let to_g = NodeEdgeMap {
        nodes: q_h_nodes.coproduct(&q_k_nodes).expect("node coproduct"),
        edges: q_h_edges.coproduct(&q_k_edges).expect("edge coproduct"),
    };

    let copied_nodes = h.nodes.len();
    let copied_edges = h.edges.len();
    let left_nodes = rule.left.nodes.len();
    let left_edges = rule.left.edges.len();
    let to_copied_plus_left = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::identity(copied_nodes)
            .inject0(left_nodes)
            .coproduct(&rule.left_map.nodes.inject1(copied_nodes))
            .expect("coproduct id + left nodes"),
        edges: FiniteFunction::<VecKind>::identity(copied_edges)
            .inject0(left_edges)
            .coproduct(&rule.left_map.edges.inject1(copied_edges))
            .expect("coproduct id + left edges"),
    };

    ExplodedContext {
        graph: h.coproduct(&rule.apex),
        to_g,
        to_copied_plus_left,
    }
}

fn fiber_partition_inputs<O, A>(exploded: &ExplodedContext<O, A>) -> Vec<FiberPartitionInput> {
    let mut fibers = vec![Vec::new(); exploded.to_g.nodes.target()];
    for (src, &tgt) in exploded.to_g.nodes.table.iter().enumerate() {
        fibers[tgt].push(NodeId(src));
    }

    fibers
        .into_iter()
        .filter(|nodes| !nodes.is_empty())
        .map(|nodes| {
            // f' refines q, so f'-classes are contained within each q-fiber.
            let mut class_index = vec![None; exploded.to_copied_plus_left.nodes.target()];
            let mut class_ids = Vec::with_capacity(nodes.len());
            let mut next_class = 0;
            for node in &nodes {
                let f_image = exploded.to_copied_plus_left.nodes.table[node.0];
                let id = match class_index[f_image] {
                    Some(existing) => existing,
                    None => {
                        let id = next_class;
                        next_class += 1;
                        class_index[f_image] = Some(id);
                        id
                    }
                };
                class_ids.push(id);
            }

            FiberPartitionInput {
                nodes,
                class_ids,
                class_count: next_class,
            }
        })
        .collect()
}

fn enumerate_fiber_partitions(fiber: &FiberPartitionInput) -> Vec<FiberPartition> {
    let mut results = Vec::new();
    let mut blocks: Vec<BlockState> = Vec::new();
    let mut uf = UnionFind::new(fiber.class_count);

    fn all_connected(uf: &UnionFind) -> bool {
        uf.components == 1
    }

    fn walk(
        idx: usize,
        fiber: &FiberPartitionInput,
        blocks: &mut Vec<BlockState>,
        uf: &mut UnionFind,
        results: &mut Vec<FiberPartition>,
    ) {
        if idx == fiber.nodes.len() {
            if all_connected(uf) {
                let blocks = blocks
                    .iter()
                    .map(|b| FiberBlock {
                        nodes: b.nodes.clone(),
                    })
                    .collect();
                results.push(FiberPartition { blocks });
            }
            return;
        }

        let node = fiber.nodes[idx];
        let class_id = fiber.class_ids[idx];

        for i in 0..blocks.len() {
            let snap = uf.snapshot();
            let (nodes_len, classes_len) = {
                let block = &mut blocks[i];
                let nodes_len = block.nodes.len();
                let classes_len = block.classes.len();

                block.nodes.push(node);
                if !block.classes.contains(&class_id) {
                    if let Some(&rep) = block.classes.first() {
                        uf.union(rep, class_id);
                    }
                    block.classes.push(class_id);
                }

                (nodes_len, classes_len)
            };

            walk(idx + 1, fiber, blocks, uf, results);

            uf.rollback(snap);
            let block = &mut blocks[i];
            block.nodes.truncate(nodes_len);
            block.classes.truncate(classes_len);
        }

        blocks.push(BlockState {
            nodes: vec![node],
            classes: vec![class_id],
        });
        walk(idx + 1, fiber, blocks, uf, results);
        blocks.pop();
    }

    walk(0, fiber, &mut blocks, &mut uf, &mut results);
    results
}

struct FiberPartitionInput {
    nodes: Vec<NodeId>,
    class_ids: Vec<usize>,
    class_count: usize,
}

struct FiberPartition {
    blocks: Vec<FiberBlock>,
}

struct FiberBlock {
    nodes: Vec<NodeId>,
}

struct BlockState {
    nodes: Vec<NodeId>,
    classes: Vec<usize>,
}

struct UnionFind {
    parent: Vec<usize>,
    size: Vec<usize>,
    history: Vec<HistoryEntry>,
    components: usize,
}

enum HistoryEntry {
    Noop,
    Merge {
        root: usize,
        parent: usize,
        size_parent: usize,
    },
}

impl UnionFind {
    fn new(n: usize) -> Self {
        Self {
            parent: (0..n).collect(),
            size: vec![1; n],
            history: Vec::new(),
            components: n,
        }
    }

    fn len(&self) -> usize {
        self.parent.len()
    }

    fn find(&mut self, x: usize) -> usize {
        let mut node = x;
        while self.parent[node] != node {
            node = self.parent[node];
        }
        node
    }

    fn union(&mut self, x: usize, y: usize) {
        let root_x = self.find(x);
        let root_y = self.find(y);
        if root_x == root_y {
            self.history.push(HistoryEntry::Noop);
            return;
        }

        let (root, parent) = if self.size[root_x] >= self.size[root_y] {
            (root_y, root_x)
        } else {
            (root_x, root_y)
        };

        self.history.push(HistoryEntry::Merge {
            root,
            parent,
            size_parent: self.size[parent],
        });

        self.parent[root] = parent;
        self.size[parent] += self.size[root];
        self.components -= 1;
    }

    fn snapshot(&self) -> usize {
        self.history.len()
    }

    fn rollback(&mut self, snapshot: usize) {
        while self.history.len() > snapshot {
            match self.history.pop().expect("rollback history") {
                HistoryEntry::Noop => {}
                HistoryEntry::Merge {
                    root,
                    parent,
                    size_parent,
                } => {
                    self.parent[root] = root;
                    self.size[parent] = size_parent;
                    self.components += 1;
                }
            }
        }
    }
}

fn validate_candidate_map<O, A>(
    rule: &LaxSpan<O, A>,
    g: &Hypergraph<O, A>,
    candidate: &NodeEdgeMap,
) {
    if candidate.nodes.source() != rule.left.nodes.len() {
        panic!(
            "candidate map node source size mismatch: got {}, expected {}",
            candidate.nodes.source(),
            rule.left.nodes.len()
        );
    }
    if candidate.nodes.target() != g.nodes.len() {
        panic!(
            "candidate map node target size mismatch: got {}, expected {}",
            candidate.nodes.target(),
            g.nodes.len()
        );
    }
    if candidate.edges.source() != rule.left.edges.len() {
        panic!(
            "candidate map edge source size mismatch: got {}, expected {}",
            candidate.edges.source(),
            rule.left.edges.len()
        );
    }
    if candidate.edges.target() != g.edges.len() {
        panic!(
            "candidate map edge target size mismatch: got {}, expected {}",
            candidate.edges.target(),
            g.edges.len()
        );
    }
}

fn identification_condition<O, A>(rule: &LaxSpan<O, A>, candidate: &NodeEdgeMap) -> bool {
    let mut in_image = vec![false; rule.left.nodes.len()];
    for i in 0..rule.left_map.nodes.source() {
        let idx = rule.left_map.nodes.table[i];
        in_image[idx] = true;
    }

    let mut seen = vec![None; candidate.nodes.target()];
    for i in 0..rule.left.nodes.len() {
        if in_image[i] {
            continue;
        }
        let img = candidate.nodes.table[i];
        if let Some(existing) = seen[img] {
            if existing != i {
                return false;
            }
        } else {
            seen[img] = Some(i);
        }
    }

    true
}

fn dangling_condition<O, A>(
    rule: &LaxSpan<O, A>,
    candidate: &NodeEdgeMap,
    g: &Hypergraph<O, A>,
) -> bool {
    let mut in_l_image = vec![false; rule.left.nodes.len()];
    for i in 0..rule.left_map.nodes.source() {
        let idx = rule.left_map.nodes.table[i];
        in_l_image[idx] = true;
    }

    let mut forbidden_nodes = vec![false; g.nodes.len()];
    for i in 0..rule.left.nodes.len() {
        if in_l_image[i] {
            continue;
        }
        let img = candidate.nodes.table[i];
        forbidden_nodes[img] = true;
    }

    let mut edge_in_image = vec![false; g.edges.len()];
    for i in 0..candidate.edges.source() {
        let idx = candidate.edges.table[i];
        edge_in_image[idx] = true;
    }

    for (edge_id, edge) in g.adjacency.iter().enumerate() {
        if edge_in_image[edge_id] {
            continue;
        }
        let touches_forbidden = edge
            .sources
            .iter()
            .chain(edge.targets.iter())
            .any(|n| forbidden_nodes[n.0]);
        if touches_forbidden {
            return false;
        }
    }

    true
}

#[cfg(test)]
mod tests {
    // Tests from working examples in
    // Bonchi, Filippo, et al.
    // "String diagram rewrite theory I: Rewriting with Frobenius structure."
    // Journal of the ACM (JACM) 69.2 (2022): 1-58.
    use super::{
        enumerate_fiber_partitions, exploded_context, fiber_partition_inputs, rewrite,
        FiberPartition,
    };
    use crate::array::vec::{VecArray, VecKind};
    use crate::finite_function::FiniteFunction;
    use crate::lax::{Arrow, Hyperedge, Hypergraph, LaxSpan, NodeEdgeMap, NodeId};
    use std::collections::HashMap;

    fn empty_map(target: usize) -> FiniteFunction<VecKind> {
        FiniteFunction::<VecKind>::new(VecArray(vec![]), target).unwrap()
    }

    #[test]
    fn test_exploded_context_construction() {
        let (f_label, g_label, g, rule, candidate) = example_rewrite_input();
        let exploded = exploded_context(&g, &rule, &candidate);

        let mut expected: Hypergraph<String, String> = Hypergraph::empty();
        let e_w1 = expected.new_node("w".to_string());
        let e_w2 = expected.new_node("w".to_string());
        let e_w3 = expected.new_node("w".to_string());
        let e_w5 = expected.new_node("w".to_string());
        let e_w4a = expected.new_node("w".to_string());
        let e_w4b = expected.new_node("w".to_string());

        expected.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![e_w1],
                targets: vec![e_w2],
            },
        );
        expected.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![e_w2],
                targets: vec![e_w3],
            },
        );
        expected.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![e_w1],
                targets: vec![e_w4a],
            },
        );
        expected.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![e_w4b],
                targets: vec![e_w5],
            },
        );

        expected.new_node("w".to_string());
        expected.new_node("w".to_string());

        assert_eq!(exploded.graph, expected);
    }

    #[test]
    fn test_f_prime_refines_q() {
        let (_f_label, _g_label, g, rule, candidate) = example_rewrite_input();
        let exploded = exploded_context(&g, &rule, &candidate);

        let mut f_prime_to_q = vec![None; exploded.to_copied_plus_left.nodes.target()];
        for (src, &f_prime_image) in exploded.to_copied_plus_left.nodes.table.iter().enumerate() {
            let q_image = exploded.to_g.nodes.table[src];
            match f_prime_to_q[f_prime_image] {
                Some(existing) => assert_eq!(existing, q_image),
                None => f_prime_to_q[f_prime_image] = Some(q_image),
            }
        }
    }

    #[test]
    fn test_fiber_partitions_expected_blocks() {
        let (_f_label, _g_label, g, rule, candidate) = example_rewrite_input();
        let exploded = exploded_context(&g, &rule, &candidate);
        let fibers = fiber_partition_inputs(&exploded);

        let copied_nodes = exploded.graph.nodes.len() - rule.apex.nodes.len();
        let target_fiber = fibers
            .iter()
            .find(|fiber| {
                let apex_count = fiber
                    .nodes
                    .iter()
                    .filter(|node| node.0 >= copied_nodes)
                    .count();
                apex_count == rule.apex.nodes.len()
            })
            .expect("expected fiber containing apex nodes");

        let mut apex_nodes = target_fiber
            .nodes
            .iter()
            .cloned()
            .filter(|node| node.0 >= copied_nodes)
            .collect::<Vec<_>>();
        apex_nodes.sort_by_key(|node| node.0);

        let mut w4_nodes = target_fiber
            .nodes
            .iter()
            .cloned()
            .filter(|node| node.0 < copied_nodes)
            .collect::<Vec<_>>();
        w4_nodes.sort_by_key(|node| node.0);

        let mut name_map: HashMap<NodeId, &'static str> = HashMap::new();
        name_map.insert(w4_nodes[0], "a0");
        name_map.insert(w4_nodes[1], "a1");
        name_map.insert(apex_nodes[0], "k0");
        name_map.insert(apex_nodes[1], "k1");

        let partitions = enumerate_fiber_partitions(target_fiber);
        let mut actual = partitions
            .iter()
            .map(|partition| normalize_partition(partition, &name_map))
            .collect::<Vec<_>>();
        actual.sort();

        let mut expected = vec![
            vec![vec!["a0", "k0"], vec!["a1", "k1"]],
            vec![vec!["a0", "k1"], vec!["a1", "k0"]],
            vec![vec!["k0"], vec!["a0", "a1", "k1"]],
            vec![vec!["k1"], vec!["a0", "a1", "k0"]],
            vec![vec!["a0", "a1", "k0", "k1"]],
        ]
        .into_iter()
        .map(|blocks| {
            let mut normalized = blocks
                .into_iter()
                .map(|mut block| {
                    block.sort();
                    block
                })
                .collect::<Vec<_>>();
            normalized.sort();
            normalized
        })
        .collect::<Vec<_>>();
        expected.sort();

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_rewrite_complements_working_example() {
        let (f_label, g_label, g, rule, candidate) = example_rewrite_input();

        let mut expected = Vec::new();

        let mut h1: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = h1.new_node("w".to_string());
        let w2 = h1.new_node("w".to_string());
        let w3 = h1.new_node("w".to_string());
        let w5 = h1.new_node("w".to_string());
        let a0 = h1.new_node("w".to_string());
        let a1 = h1.new_node("w".to_string());
        let k0 = h1.new_node("w".to_string());
        let k1 = h1.new_node("w".to_string());
        h1.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        h1.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        h1.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![a0],
            },
        );
        h1.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![a1],
                targets: vec![w5],
            },
        );
        h1.quotient = (vec![a1, a0], vec![k0, k1]);
        h1.quotient();
        expected.push(h1);

        let mut h2: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = h2.new_node("w".to_string());
        let w2 = h2.new_node("w".to_string());
        let w3 = h2.new_node("w".to_string());
        let w5 = h2.new_node("w".to_string());
        let a0 = h2.new_node("w".to_string());
        let a1 = h2.new_node("w".to_string());
        let k0 = h2.new_node("w".to_string());
        let k1 = h2.new_node("w".to_string());
        h2.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        h2.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        h2.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![a0],
            },
        );
        h2.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![a1],
                targets: vec![w5],
            },
        );
        h2.quotient = (vec![a1, a0], vec![k1, k0]);
        h2.quotient();
        expected.push(h2);

        let mut h3: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = h3.new_node("w".to_string());
        let w2 = h3.new_node("w".to_string());
        let w3 = h3.new_node("w".to_string());
        let w5 = h3.new_node("w".to_string());
        let a0 = h3.new_node("w".to_string());
        let a1 = h3.new_node("w".to_string());
        let k0 = h3.new_node("w".to_string());
        let k1 = h3.new_node("w".to_string());
        h3.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        h3.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        h3.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![a0],
            },
        );
        h3.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![a1],
                targets: vec![w5],
            },
        );
        h3.quotient = (vec![a0, a0], vec![a1, k1]);
        h3.quotient();
        expected.push(h3);

        let mut h4: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = h4.new_node("w".to_string());
        let w2 = h4.new_node("w".to_string());
        let w3 = h4.new_node("w".to_string());
        let w5 = h4.new_node("w".to_string());
        let a0 = h4.new_node("w".to_string());
        let a1 = h4.new_node("w".to_string());
        let k0 = h4.new_node("w".to_string());
        let k1 = h4.new_node("w".to_string());
        h4.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        h4.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        h4.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![a0],
            },
        );
        h4.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![a1],
                targets: vec![w5],
            },
        );
        h4.quotient = (vec![a0, a0], vec![a1, k0]);
        h4.quotient();
        expected.push(h4);

        let mut h5: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = h5.new_node("w".to_string());
        let w2 = h5.new_node("w".to_string());
        let w3 = h5.new_node("w".to_string());
        let w5 = h5.new_node("w".to_string());
        let a0 = h5.new_node("w".to_string());
        let a1 = h5.new_node("w".to_string());
        let k0 = h5.new_node("w".to_string());
        let k1 = h5.new_node("w".to_string());
        h5.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        h5.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        h5.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![a0],
            },
        );
        h5.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![a1],
                targets: vec![w5],
            },
        );
        h5.quotient = (vec![a0, a0, a0], vec![a1, k0, k1]);
        h5.quotient();
        expected.push(h5);

        let complements = rewrite(&g, &rule, &candidate).expect("expected complements");
        assert_eq!(complements.len(), 5);
        for expected_graph in expected {
            assert!(complements.iter().any(|h| h == &expected_graph));
        }
    }

    fn normalize_partition(
        partition: &FiberPartition,
        name_map: &HashMap<NodeId, &'static str>,
    ) -> Vec<Vec<&'static str>> {
        let mut blocks = partition
            .blocks
            .iter()
            .map(|block| {
                let mut names = block
                    .nodes
                    .iter()
                    .map(|node| *name_map.get(node).expect("name map"))
                    .collect::<Vec<_>>();
                names.sort();
                names
            })
            .collect::<Vec<_>>();
        blocks.sort();
        blocks
    }

    fn example_rewrite_input() -> (
        String,
        String,
        Hypergraph<String, String>,
        LaxSpan<String, String>,
        NodeEdgeMap,
    ) {
        // From Session 4.5 Pushout Complements and Rewriting Modulo Frobenius
        let f_label = "f".to_string();
        let g_label = "g".to_string();

        let mut g: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = g.new_node("w".to_string());
        let w2 = g.new_node("w".to_string());
        let w3 = g.new_node("w".to_string());
        let w4 = g.new_node("w".to_string());
        let w5 = g.new_node("w".to_string());

        g.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        g.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        g.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w4],
            },
        );
        g.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w4],
                targets: vec![w5],
            },
        );

        let mut left: Hypergraph<String, String> = Hypergraph::empty();
        left.new_node("w".to_string());

        let mut right: Hypergraph<String, String> = Hypergraph::empty();
        let b0 = right.new_node("w".to_string());
        let b2 = right.new_node("w".to_string());
        let b1 = right.new_node("w".to_string());
        right.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![b0],
                targets: vec![b2],
            },
        );
        right.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![b2],
                targets: vec![b1],
            },
        );

        let mut apex: Hypergraph<String, String> = Hypergraph::empty();
        apex.new_node("w".to_string());
        apex.new_node("w".to_string());

        let left_map = NodeEdgeMap {
            nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0, 0]), left.nodes.len()).unwrap(),
            edges: empty_map(left.edges.len()),
        };
        let right_map = NodeEdgeMap {
            nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0, 2]), right.nodes.len()).unwrap(),
            edges: empty_map(right.edges.len()),
        };
        let rule = LaxSpan::new(apex, left, right, left_map, right_map);

        let candidate = NodeEdgeMap {
            nodes: FiniteFunction::<VecKind>::new(VecArray(vec![3]), g.nodes.len()).unwrap(),
            edges: empty_map(g.edges.len()),
        };

        (f_label, g_label, g, rule, candidate)
    }
}
