use crate::array::vec::VecKind;
use crate::finite_function::FiniteFunction;
use crate::lax::{Arrow, Coproduct as _, Hypergraph, NodeEdgeMap, NodeId, Span};
use crate::partition::{enumerate_partitions, Partition, PartitionInput};

struct ExplodedContext<O, A> {
    remainder_plus_interface: Hypergraph<O, A>,
    to_host: NodeEdgeMap,
    to_remainder_plus_redex: NodeEdgeMap,
    interface_in_exploded: NodeEdgeMap,
}

impl<O, A> ExplodedContext<O, A> {
    fn interface_in_exploded(&self) -> &NodeEdgeMap {
        &self.interface_in_exploded
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
    let partitions_per_fiber: Vec<Vec<Partition<NodeId>>> =
        fiber_inputs.iter().map(enumerate_partitions).collect();

    if partitions_per_fiber.is_empty() {
        return vec![pushout_result(&exploded, rule, &partitions_per_fiber, &[])];
    }

    let mut complements = Vec::new();
    let mut selection: Vec<usize> = Vec::with_capacity(partitions_per_fiber.len());

    fn pushout_result<O: Clone + PartialEq, A: Clone + PartialEq>(
        exploded: &ExplodedContext<O, A>,
        rule: &Span<'_, O, A>,
        partitions_per_fiber: &[Vec<Partition<NodeId>>],
        selection: &[usize],
    ) -> Hypergraph<O, A> {
        let mut quotient_left = Vec::new();
        let mut quotient_right = Vec::new();

        for (fiber_idx, &partition_idx) in selection.iter().enumerate() {
            let partition = &partitions_per_fiber[fiber_idx][partition_idx];
            for block in &partition.blocks {
                let Some((first, rest)) = block.elements.split_first() else {
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
                .interface_in_exploded()
                .nodes
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
        partitions_per_fiber: &[Vec<Partition<NodeId>>],
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
    let (remainder, remainder_in_host) = host.remainder_with_injection(matching);
    let q_interface = rule.left_map.compose(matching);
    let to_host = remainder_in_host.coproduct(&q_interface);

    let (_remainder_plus_redex, remainder_in_remainder_plus_redex, redex_in_remainder_plus_redex) =
        remainder.coproduct_with_injections(rule.left);
    let to_remainder_plus_redex = remainder_in_remainder_plus_redex
        .coproduct(&rule.left_map.compose(&redex_in_remainder_plus_redex));

    let (remainder_plus_interface, _remainder_in_exploded, interface_in_exploded) =
        remainder.coproduct_with_injections(rule.apex);

    ExplodedContext {
        remainder_plus_interface,
        to_host,
        to_remainder_plus_redex,
        interface_in_exploded,
    }
}

fn fiber_partition_inputs<O, A>(exploded: &ExplodedContext<O, A>) -> Vec<PartitionInput<NodeId>> {
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

            PartitionInput {
                elements: nodes,
                class_ids,
                class_count: next_class,
            }
        })
        .collect()
}

fn validate_candidate_map<O: PartialEq, A: PartialEq>(
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
    assert_eq!(
        rule.left.edges.len(),
        rule.left.adjacency.len(),
        "malformed hypergraph: edges and adjacency lengths differ"
    );

    for (node_id, node_label) in rule.left.nodes.iter().enumerate() {
        let host_node_id = candidate.nodes.table[node_id];
        let host_label = g.nodes.get(host_node_id).unwrap_or_else(|| {
            panic!(
                "candidate node image out of bounds: got {}, max {}",
                host_node_id,
                g.nodes.len()
            )
        });
        if host_label != node_label {
            panic!("candidate node label mismatch for node {}", node_id);
        }
    }

    for (edge_id, edge) in rule.left.adjacency.iter().enumerate() {
        let host_edge_id = candidate.edges.table[edge_id];
        let host_edge = g.adjacency.get(host_edge_id).unwrap_or_else(|| {
            panic!(
                "candidate edge image out of bounds: got {}, max {}",
                host_edge_id,
                g.adjacency.len()
            )
        });
        let host_edge_label = g.edges.get(host_edge_id).unwrap_or_else(|| {
            panic!(
                "candidate edge image out of bounds: got {}, max {}",
                host_edge_id,
                g.edges.len()
            )
        });
        let edge_label = rule.left.edges.get(edge_id).unwrap_or_else(|| {
            panic!(
                "left edge index out of bounds: got {}, max {}",
                edge_id,
                rule.left.edges.len()
            )
        });
        if host_edge_label != edge_label {
            panic!("candidate edge label mismatch for edge {}", edge_id);
        }

        if edge.sources.len() != host_edge.sources.len()
            || edge.targets.len() != host_edge.targets.len()
        {
            panic!("candidate edge image arity mismatch for edge {}", edge_id);
        }

        for (idx, node) in edge.sources.iter().enumerate() {
            let image = candidate.nodes.table[node.0];
            let host_node = host_edge.sources[idx].0;
            if image != host_node {
                panic!(
                    "candidate edge image source mismatch for edge {} at position {}: got {}, expected {}",
                    edge_id,
                    idx,
                    host_node,
                    image
                );
            }
        }

        for (idx, node) in edge.targets.iter().enumerate() {
            let image = candidate.nodes.table[node.0];
            let host_node = host_edge.targets[idx].0;
            if image != host_node {
                panic!(
                    "candidate edge image target mismatch for edge {} at position {}: got {}, expected {}",
                    edge_id,
                    idx,
                    host_node,
                    image
                );
            }
        }
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
    // Examples from
    // 1. Bonchi, Filippo, et al. "String diagram rewrite theory I: Rewriting with Frobenius structure." Journal of the ACM (JACM) 69.2 (2022): 1-58.
    // 2. Heumüller, Marvin, et al. "Construction of pushout complements in the category of hypergraphs." Electronic Communications of the EASST 39 (2011).
    use super::{exploded_context, fiber_partition_inputs, rewrite};
    use crate::array::vec::{VecArray, VecKind};
    use crate::finite_function::FiniteFunction;
    use crate::lax::{Arrow as _, Hyperedge, Hypergraph, NodeEdgeMap, NodeId, Span};
    use crate::partition::{enumerate_partitions, Partition};
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
                    .elements
                    .iter()
                    .filter(|node| node.0 >= copied_nodes)
                    .count();
                apex_count == rule.apex.nodes.len()
            })
            .expect("expected fiber containing apex nodes");

        let mut apex_nodes = target_fiber
            .elements
            .iter()
            .cloned()
            .filter(|node| node.0 >= copied_nodes)
            .collect::<Vec<_>>();
        apex_nodes.sort_by_key(|node| node.0);

        let mut w4_nodes = target_fiber
            .elements
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

        let partitions = enumerate_partitions(target_fiber);
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
        let (_, _, g, apex, left, right, left_map, right_map, candidate) = example_rewrite_input();
        let rule = Span::new(&apex, &left, &right, &left_map, &right_map);
        let expected = vec![
            crate::hg! {
                nodes: {
                    a0a1k0k1: "w",
                    b2: "w",
                    w1: "w",
                    w2: "w",
                    w3: "w",
                    w5: "w",
                },
                edges: [
                    ("f", [a0a1k0k1], [b2]),
                    ("g", [b2], [a0a1k0k1]),
                    ("f", [w1], [w2]),
                    ("g", [w2], [w3]),
                    ("f", [w1], [a0a1k0k1]),
                    ("g", [a0a1k0k1], [w5]),
                ],
            },
            crate::hg! {
                nodes: {
                    a0k0: "w",
                    b2: "w",
                    a1k1: "w",
                    w1: "w",
                    w2: "w",
                    w3: "w",
                    w5: "w",
                },
                edges: [
                    ("f", [a0k0], [b2]),
                    ("g", [b2], [a1k1]),
                    ("f", [w1], [w2]),
                    ("g", [w2], [w3]),
                    ("f", [w1], [a0k0]),
                    ("g", [a1k1], [w5]),
                ],
            },
            crate::hg! {
                nodes: {
                    a1k0: "w",
                    b2: "w",
                    a0k1: "w",
                    w1: "w",
                    w2: "w",
                    w3: "w",
                    w5: "w",
                },
                edges: [
                    ("f", [a1k0], [b2]),
                    ("g", [b2], [a0k1]),
                    ("f", [w1], [w2]),
                    ("g", [w2], [w3]),
                    ("f", [w1], [a0k1]),
                    ("g", [a1k0], [w5]),
                ],
            },
            crate::hg! {
                nodes: {
                    k0: "w",
                    b2: "w",
                    a0a1k1: "w",
                    w1: "w",
                    w2: "w",
                    w3: "w",
                    w5: "w",
                },
                edges: [
                    ("f", [k0], [b2]),
                    ("g", [b2], [a0a1k1]),
                    ("f", [w1], [w2]),
                    ("g", [w2], [w3]),
                    ("f", [w1], [a0a1k1]),
                    ("g", [a0a1k1], [w5]),
                ],
            },
            crate::hg! {
                nodes: {
                    a0a1k0: "w",
                    b2: "w",
                    k1: "w",
                    w1: "w",
                    w2: "w",
                    w3: "w",
                    w5: "w",
                },
                edges: [
                    ("f", [a0a1k0], [b2]),
                    ("g", [b2], [k1]),
                    ("f", [w1], [w2]),
                    ("g", [w2], [w3]),
                    ("f", [w1], [a0a1k0]),
                    ("g", [a0a1k0], [w5]),
                ],
            },
        ];

        let complements = rewrite(&g, &rule, &candidate);
        assert_eq!(complements.len(), 5);
        let missing = expected
            .iter()
            .filter(|expected_graph| !complements.iter().any(|h| h == *expected_graph))
            .collect::<Vec<_>>();
        assert!(
            missing.is_empty(),
            "Missing {} expected complement(s).",
            missing.len()
        );
    }

    #[test]
    fn test_rewrite_pushout_complement_split_match_node() {
        // [2] Example 1
        let (host, apex, left, right, left_map, right_map, candidate) =
            example_rewrite_pushout_complement_split_match_node_input();
        let rule = Span::new(&apex, &left, &right, &left_map, &right_map);

        let complements = rewrite(&host, &rule, &candidate);
        assert!(complements.len() == 61, "Found {}", complements.len());

        let mut has_split_loop = false;
        for complement in &complements {
            assert_eq!(complement.edges.len(), 2);
            let f_idx = complement
                .edges
                .iter()
                .position(|label| label == "f")
                .expect("f edge exists");
            let f_edge = &complement.adjacency[f_idx];
            assert_eq!(f_edge.sources.len(), 1);
            assert_eq!(f_edge.targets.len(), 1);
            if f_edge.sources[0] != f_edge.targets[0] {
                has_split_loop = true;
            }
        }

        assert!(has_split_loop);
    }

    fn normalize_partition(
        partition: &Partition<NodeId>,
        name_map: &HashMap<NodeId, &'static str>,
    ) -> Vec<Vec<&'static str>> {
        let mut blocks = partition
            .blocks
            .iter()
            .map(|block| {
                let mut names = block
                    .elements
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
        // [1] Session 4.5 Pushout Complements and Rewriting Modulo Frobenius
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

    fn example_rewrite_pushout_complement_split_match_node_input() -> (
        Hypergraph<String, String>,
        Hypergraph<String, String>,
        Hypergraph<String, String>,
        Hypergraph<String, String>,
        NodeEdgeMap,
        NodeEdgeMap,
        NodeEdgeMap,
    ) {
        // [2] Example 1
        let mut host: Hypergraph<String, String> = Hypergraph::empty();
        let f_node = host.new_node("w".to_string());
        let u_node = host.new_node("w".to_string());
        host.new_edge(
            "e".to_string(),
            Hyperedge {
                sources: vec![u_node],
                targets: vec![u_node],
            },
        );
        host.new_edge(
            "f".to_string(),
            Hyperedge {
                sources: vec![f_node],
                targets: vec![f_node],
            },
        );

        let mut apex: Hypergraph<String, String> = Hypergraph::empty();
        apex.new_node("w".to_string());
        apex.new_node("w".to_string());
        apex.new_node("w".to_string());
        apex.new_node("w".to_string());

        let mut left: Hypergraph<String, String> = Hypergraph::empty();
        left.new_node("w".to_string());
        left.new_node("w".to_string());

        let right = apex.clone();

        let left_map = NodeEdgeMap {
            nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0, 0, 1, 1]), left.nodes.len())
                .unwrap(),
            edges: empty_map(left.edges.len()),
        };
        let right_map = NodeEdgeMap {
            nodes: FiniteFunction::<VecKind>::new(VecArray(vec![0, 1, 2, 3]), right.nodes.len())
                .unwrap(),
            edges: empty_map(right.edges.len()),
        };
        let candidate = NodeEdgeMap {
            nodes: FiniteFunction::<VecKind>::new(
                VecArray(vec![f_node.0, f_node.0]),
                host.nodes.len(),
            )
            .unwrap(),
            edges: empty_map(host.edges.len()),
        };

        (host, apex, left, right, left_map, right_map, candidate)
    }
}
