use crate::array::vec::{VecArray, VecKind};
use crate::finite_function::FiniteFunction;
use crate::lax::{Arrow, Coproduct, Hyperedge, Hypergraph, NodeEdgeMap, NodeId, Span};
use crate::union_find::UnionFind;

struct ExplodedContext<O, A> {
    remainder_plus_interface: Hypergraph<O, A>,
    to_host: NodeEdgeMap,
    to_remainder_plus_redex: NodeEdgeMap,
    interface_in_exploded_nodes: FiniteFunction<VecKind>,
}

impl<O, A> ExplodedContext<O, A> {
    fn interface_in_exploded_nodes(&self) -> &FiniteFunction<VecKind> {
        &self.interface_in_exploded_nodes
    }
}

/// Rewrite a lax hypergraph using a rule span and candidate map.
pub fn rewrite<O: Clone + PartialEq, A: Clone + PartialEq>(
    g: &Hypergraph<O, A>,
    rule: &Span<'_, O, A>,
    candidate: &NodeEdgeMap,
) -> Vec<Hypergraph<O, A>> {
    validate_candidate_map(rule, g, candidate);
    if !identification_condition(rule, candidate) || !dangling_condition(rule, candidate, g) {
        return Vec::new();
    }
    let exploded = exploded_context(g, rule, candidate);
    let fiber_inputs = fiber_partition_inputs(&exploded);
    let partitions_per_fiber: Vec<Vec<FiberPartition>> = fiber_inputs
        .iter()
        .map(enumerate_fiber_partitions)
        .collect();

    if partitions_per_fiber.is_empty() {
        return vec![pushout_result(&exploded, rule, &partitions_per_fiber, &[])];
    }

    let mut complements = Vec::new();
    let mut selection: Vec<usize> = Vec::with_capacity(partitions_per_fiber.len());

    fn pushout_result<O: Clone + PartialEq, A: Clone + PartialEq>(
        exploded: &ExplodedContext<O, A>,
        rule: &Span<'_, O, A>,
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

        let (complement, q) = exploded
            .remainder_plus_interface
            .clone()
            .quotiented_with(quotient_left, quotient_right);

        let interface_to_complement = {
            let nodes = exploded
                .interface_in_exploded_nodes()
                .compose(&q)
                .expect("interface to complement map");
            NodeEdgeMap {
                nodes,
                edges: FiniteFunction::<VecKind>::initial(complement.edges.len()),
            }
        };

        let span = Span::new(
            rule.apex,
            rule.right,
            &complement,
            rule.right_map,
            &interface_to_complement,
        );
        span.pushout()
    }

    fn walk<O: Clone + PartialEq, A: Clone + PartialEq>(
        idx: usize,
        exploded: &ExplodedContext<O, A>,
        rule: &Span<'_, O, A>,
        partitions_per_fiber: &[Vec<FiberPartition>],
        selection: &mut Vec<usize>,
        results: &mut Vec<Hypergraph<O, A>>,
    ) {
        if idx == partitions_per_fiber.len() {
            results.push(pushout_result(
                exploded,
                rule,
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
                rule,
                partitions_per_fiber,
                selection,
                results,
            );
            selection.pop();
        }
    }

    walk(
        0,
        &exploded,
        rule,
        &partitions_per_fiber,
        &mut selection,
        &mut complements,
    );

    complements
}

fn exploded_context<O: Clone, A: Clone>(
    host: &Hypergraph<O, A>,
    rule: &Span<'_, O, A>,
    matching: &NodeEdgeMap,
) -> ExplodedContext<O, A> {
    let mut in_image_nodes = vec![false; host.nodes.len()];
    for i in 0..matching.nodes.source() {
        let idx = matching.nodes.table[i];
        in_image_nodes[idx] = true;
    }

    let mut in_image_edges = vec![false; host.edges.len()];
    for i in 0..matching.edges.source() {
        let idx = matching.edges.table[i];
        in_image_edges[idx] = true;
    }

    let mut remainder = Hypergraph::empty();
    let mut remainder_node_to_host = Vec::new();
    let mut node_map: Vec<Option<usize>> = vec![None; host.nodes.len()];
    for (idx, label) in host.nodes.iter().enumerate() {
        if in_image_nodes[idx] {
            continue;
        }
        let new_id = remainder.new_node(label.clone());
        remainder_node_to_host.push(idx);
        node_map[idx] = Some(new_id.0);
    }

    let mut remainder_edge_to_host = Vec::new();
    for (edge_id, edge) in host.adjacency.iter().enumerate() {
        if in_image_edges[edge_id] {
            continue;
        }

        let mut sources = Vec::with_capacity(edge.sources.len());
        for node in &edge.sources {
            let new_id = match node_map[node.0] {
                Some(existing) => NodeId(existing),
                None => {
                    let new_id = remainder.new_node(host.nodes[node.0].clone());
                    remainder_node_to_host.push(node.0);
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
                    let new_id = remainder.new_node(host.nodes[node.0].clone());
                    remainder_node_to_host.push(node.0);
                    new_id
                }
            };
            targets.push(new_id);
        }

        remainder.new_edge(host.edges[edge_id].clone(), Hyperedge { sources, targets });
        remainder_edge_to_host.push(edge_id);
    }

    let q_remainder_nodes =
        FiniteFunction::<VecKind>::new(VecArray(remainder_node_to_host), host.nodes.len()).unwrap();
    let q_remainder_edges =
        FiniteFunction::<VecKind>::new(VecArray(remainder_edge_to_host), host.edges.len()).unwrap();
    let q_interface = rule.left_map.compose(matching);
    let to_host = NodeEdgeMap {
        nodes: q_remainder_nodes,
        edges: q_remainder_edges,
    }
    .coproduct(&q_interface);

    let copied_nodes = remainder.nodes.len();
    let copied_edges = remainder.edges.len();
    let left_nodes = rule.left.nodes.len();
    let left_edges = rule.left.edges.len();
    let to_remainder_plus_redex = NodeEdgeMap {
        nodes: FiniteFunction::<VecKind>::identity(copied_nodes).inject0(left_nodes),
        edges: FiniteFunction::<VecKind>::identity(copied_edges).inject0(left_edges),
    }
    .coproduct(&NodeEdgeMap {
        nodes: rule.left_map.nodes.inject1(copied_nodes),
        edges: rule.left_map.edges.inject1(copied_edges),
    });

    let interface_in_exploded_nodes =
        FiniteFunction::<VecKind>::identity(rule.apex.nodes.len()).inject1(copied_nodes);

    ExplodedContext {
        remainder_plus_interface: remainder.coproduct(&rule.apex),
        to_host,
        to_remainder_plus_redex,
        interface_in_exploded_nodes,
    }
}

fn fiber_partition_inputs<O, A>(exploded: &ExplodedContext<O, A>) -> Vec<FiberPartitionInput> {
    let mut fibers = vec![Vec::new(); exploded.to_host.nodes.target()];
    for (src, &tgt) in exploded.to_host.nodes.table.iter().enumerate() {
        fibers[tgt].push(NodeId(src));
    }

    fibers
        .into_iter()
        .filter(|nodes| !nodes.is_empty())
        .map(|nodes| {
            // f' refines q, so f'-classes are contained within each q-fiber.
            let mut class_index = vec![None; exploded.to_remainder_plus_redex.nodes.target()];
            let mut class_ids = Vec::with_capacity(nodes.len());
            let mut next_class = 0;
            for node in &nodes {
                let f_image = exploded.to_remainder_plus_redex.nodes.table[node.0];
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
        uf.components() == 1
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

fn validate_candidate_map<O, A>(
    rule: &Span<'_, O, A>,
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

fn identification_condition<O, A>(rule: &Span<'_, O, A>, candidate: &NodeEdgeMap) -> bool {
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
    rule: &Span<'_, O, A>,
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
    use crate::lax::{Arrow, Hyperedge, Hypergraph, NodeEdgeMap, NodeId, Span};
    use std::collections::HashMap;

    fn empty_map(target: usize) -> FiniteFunction<VecKind> {
        FiniteFunction::<VecKind>::new(VecArray(vec![]), target).unwrap()
    }

    #[test]
    fn test_exploded_context_construction() {
        let (f_label, g_label, g, apex, left, right, left_map, right_map, candidate) =
            example_rewrite_input();
        let rule = Span::new(&apex, &left, &right, &left_map, &right_map);
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

        assert_eq!(exploded.remainder_plus_interface, expected);
    }

    #[test]
    fn test_f_prime_refines_q() {
        let (_f_label, _g_label, g, apex, left, right, left_map, right_map, candidate) =
            example_rewrite_input();
        let rule = Span::new(&apex, &left, &right, &left_map, &right_map);
        let exploded = exploded_context(&g, &rule, &candidate);

        let mut f_prime_to_q = vec![None; exploded.to_remainder_plus_redex.nodes.target()];
        for (src, &f_prime_image) in exploded
            .to_remainder_plus_redex
            .nodes
            .table
            .iter()
            .enumerate()
        {
            let q_image = exploded.to_host.nodes.table[src];
            match f_prime_to_q[f_prime_image] {
                Some(existing) => assert_eq!(existing, q_image),
                None => f_prime_to_q[f_prime_image] = Some(q_image),
            }
        }
    }

    #[test]
    fn test_fiber_partitions_expected_blocks() {
        let (_f_label, _g_label, g, apex, left, right, left_map, right_map, candidate) =
            example_rewrite_input();
        let rule = Span::new(&apex, &left, &right, &left_map, &right_map);
        let exploded = exploded_context(&g, &rule, &candidate);
        let fibers = fiber_partition_inputs(&exploded);

        let copied_nodes = exploded.remainder_plus_interface.nodes.len() - rule.apex.nodes.len();
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
        let (f_label, g_label, g, apex, left, right, left_map, right_map, candidate) =
            example_rewrite_input();
        let rule = Span::new(&apex, &left, &right, &left_map, &right_map);
        let mut expected = Vec::new();

        let mut c1: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = c1.new_node("w".to_string());
        let w2 = c1.new_node("w".to_string());
        let w3 = c1.new_node("w".to_string());
        let w5 = c1.new_node("w".to_string());
        let a0 = c1.new_node("w".to_string());
        let a1 = c1.new_node("w".to_string());
        let k0 = c1.new_node("w".to_string());
        let k1 = c1.new_node("w".to_string());
        c1.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        c1.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        c1.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![a0],
            },
        );
        c1.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![a1],
                targets: vec![w5],
            },
        );
        let (c1, q1) = c1.quotiented_with(vec![a1, a0], vec![k0, k1]);
        let k0_c1 = NodeId(q1.table[k0.0]);
        let k1_c1 = NodeId(q1.table[k1.0]);
        let mut p1 = rule.right.coproduct(&c1);
        let left_nodes = rule.right.nodes.len();
        p1.quotient.0.push(NodeId(rule.right_map.nodes.table[0]));
        p1.quotient.1.push(NodeId(k0_c1.0 + left_nodes));
        p1.quotient.0.push(NodeId(rule.right_map.nodes.table[1]));
        p1.quotient.1.push(NodeId(k1_c1.0 + left_nodes));
        p1.quotient();
        expected.push(p1);

        let mut c2: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = c2.new_node("w".to_string());
        let w2 = c2.new_node("w".to_string());
        let w3 = c2.new_node("w".to_string());
        let w5 = c2.new_node("w".to_string());
        let a0 = c2.new_node("w".to_string());
        let a1 = c2.new_node("w".to_string());
        let k0 = c2.new_node("w".to_string());
        let k1 = c2.new_node("w".to_string());
        c2.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        c2.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        c2.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![a0],
            },
        );
        c2.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![a1],
                targets: vec![w5],
            },
        );
        let (c2, q2) = c2.quotiented_with(vec![a1, a0], vec![k1, k0]);
        let k0_c2 = NodeId(q2.table[k0.0]);
        let k1_c2 = NodeId(q2.table[k1.0]);
        let mut p2 = rule.right.coproduct(&c2);
        let left_nodes = rule.right.nodes.len();
        p2.quotient.0.push(NodeId(rule.right_map.nodes.table[0]));
        p2.quotient.1.push(NodeId(k0_c2.0 + left_nodes));
        p2.quotient.0.push(NodeId(rule.right_map.nodes.table[1]));
        p2.quotient.1.push(NodeId(k1_c2.0 + left_nodes));
        p2.quotient();
        expected.push(p2);

        let mut c3: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = c3.new_node("w".to_string());
        let w2 = c3.new_node("w".to_string());
        let w3 = c3.new_node("w".to_string());
        let w5 = c3.new_node("w".to_string());
        let a0 = c3.new_node("w".to_string());
        let a1 = c3.new_node("w".to_string());
        let k0 = c3.new_node("w".to_string());
        let k1 = c3.new_node("w".to_string());
        c3.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        c3.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        c3.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![a0],
            },
        );
        c3.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![a1],
                targets: vec![w5],
            },
        );
        let (c3, q3) = c3.quotiented_with(vec![a0, a0], vec![a1, k1]);
        let k0_c3 = NodeId(q3.table[k0.0]);
        let k1_c3 = NodeId(q3.table[k1.0]);
        let mut p3 = rule.right.coproduct(&c3);
        let left_nodes = rule.right.nodes.len();
        p3.quotient.0.push(NodeId(rule.right_map.nodes.table[0]));
        p3.quotient.1.push(NodeId(k0_c3.0 + left_nodes));
        p3.quotient.0.push(NodeId(rule.right_map.nodes.table[1]));
        p3.quotient.1.push(NodeId(k1_c3.0 + left_nodes));
        p3.quotient();
        expected.push(p3);

        let mut c4: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = c4.new_node("w".to_string());
        let w2 = c4.new_node("w".to_string());
        let w3 = c4.new_node("w".to_string());
        let w5 = c4.new_node("w".to_string());
        let a0 = c4.new_node("w".to_string());
        let a1 = c4.new_node("w".to_string());
        let k0 = c4.new_node("w".to_string());
        let k1 = c4.new_node("w".to_string());
        c4.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        c4.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        c4.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![a0],
            },
        );
        c4.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![a1],
                targets: vec![w5],
            },
        );
        let (c4, q4) = c4.quotiented_with(vec![a0, a0], vec![a1, k0]);
        let k0_c4 = NodeId(q4.table[k0.0]);
        let k1_c4 = NodeId(q4.table[k1.0]);
        let mut p4 = rule.right.coproduct(&c4);
        let left_nodes = rule.right.nodes.len();
        p4.quotient.0.push(NodeId(rule.right_map.nodes.table[0]));
        p4.quotient.1.push(NodeId(k0_c4.0 + left_nodes));
        p4.quotient.0.push(NodeId(rule.right_map.nodes.table[1]));
        p4.quotient.1.push(NodeId(k1_c4.0 + left_nodes));
        p4.quotient();
        expected.push(p4);

        let mut c5: Hypergraph<String, String> = Hypergraph::empty();
        let w1 = c5.new_node("w".to_string());
        let w2 = c5.new_node("w".to_string());
        let w3 = c5.new_node("w".to_string());
        let w5 = c5.new_node("w".to_string());
        let a0 = c5.new_node("w".to_string());
        let a1 = c5.new_node("w".to_string());
        let k0 = c5.new_node("w".to_string());
        let k1 = c5.new_node("w".to_string());
        c5.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![w2],
            },
        );
        c5.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![w2],
                targets: vec![w3],
            },
        );
        c5.new_edge(
            f_label.clone(),
            Hyperedge {
                sources: vec![w1],
                targets: vec![a0],
            },
        );
        c5.new_edge(
            g_label.clone(),
            Hyperedge {
                sources: vec![a1],
                targets: vec![w5],
            },
        );
        let (c5, q5) = c5.quotiented_with(vec![a0, a0, a0], vec![a1, k0, k1]);
        let k0_c5 = NodeId(q5.table[k0.0]);
        let k1_c5 = NodeId(q5.table[k1.0]);
        let mut p5 = rule.right.coproduct(&c5);
        let left_nodes = rule.right.nodes.len();
        p5.quotient.0.push(NodeId(rule.right_map.nodes.table[0]));
        p5.quotient.1.push(NodeId(k0_c5.0 + left_nodes));
        p5.quotient.0.push(NodeId(rule.right_map.nodes.table[1]));
        p5.quotient.1.push(NodeId(k1_c5.0 + left_nodes));
        p5.quotient();
        expected.push(p5);

        let complements = rewrite(&g, &rule, &candidate);
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
        Hypergraph<String, String>,
        Hypergraph<String, String>,
        Hypergraph<String, String>,
        NodeEdgeMap,
        NodeEdgeMap,
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
        let candidate = NodeEdgeMap {
            nodes: FiniteFunction::<VecKind>::new(VecArray(vec![3]), g.nodes.len()).unwrap(),
            edges: empty_map(g.edges.len()),
        };

        (
            f_label, g_label, g, apex, left, right, left_map, right_map, candidate,
        )
    }
}
