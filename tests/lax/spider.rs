use open_hypergraphs::lax::{Hyperedge, Hypergraph, NodeId, OpenHypergraph, WithSpider};
use proptest::proptest;

use crate::theory::meaningless::{arb_open_hypergraph, Arr, Obj};

fn forget_spiders<O: Clone + PartialEq, A: Clone>(
    f: OpenHypergraph<O, WithSpider<A>>,
) -> OpenHypergraph<O, A> {
    let OpenHypergraph {
        sources,
        targets,
        hypergraph,
    } = f;

    let mut result = OpenHypergraph {
        sources,
        targets,
        hypergraph: Hypergraph {
            nodes: hypergraph.nodes,
            edges: vec![],
            adjacency: vec![],
            quotient: hypergraph.quotient,
        },
    };

    for (operation, adjacency) in hypergraph.edges.into_iter().zip(hypergraph.adjacency) {
        match operation {
            WithSpider::Operation(operation) => {
                result.hypergraph.edges.push(operation);
                result.hypergraph.adjacency.push(adjacency);
            }
            WithSpider::Spider { sources, targets } => {
                assert_eq!(sources, adjacency.sources.len());
                assert_eq!(targets, adjacency.targets.len());

                let mut incident = adjacency.sources.into_iter().chain(adjacency.targets);
                if let Some(first) = incident.next() {
                    for node in incident {
                        result.unify(first, node);
                    }
                }
            }
        }
    }

    result.quotient().expect("spider legs have equal labels");
    result
}

#[test]
fn spiderize_makes_a_cyclic_non_monogamous_hypergraph_syntactic() {
    let mut f = OpenHypergraph::empty();
    let x = f.new_node(());
    let y = f.new_node(());

    f.new_edge("forward", ([x], [y]));
    f.new_edge("backward", ([y], [x]));
    f.sources = vec![x, x];
    f.targets = vec![y, y];

    let spiderized = f.spiderize().unwrap();
    let strict = spiderized.clone().to_strict();

    assert!(strict.is_monogamous());
    assert!(strict.is_acyclic());
    assert!(spiderized.hypergraph.is_strict());
    assert_eq!(spiderized.hypergraph.edges.len(), 2 + 2 * 2);
}

#[test]
fn spider_arities_count_all_occurrences() {
    let mut f = OpenHypergraph::empty();
    let node = f.new_node(());
    f.new_edge("op", ([node, node], [node]));
    f.sources = vec![node, node];
    f.targets = vec![node];

    let spiderized = f.spiderize().unwrap();

    assert_eq!(
        spiderized.hypergraph.edges,
        vec![
            WithSpider::Operation("op"),
            WithSpider::Spider {
                sources: 0,
                targets: 4,
            },
            WithSpider::Spider {
                sources: 4,
                targets: 0,
            },
        ]
    );

    assert_eq!(spiderized.sources.len(), 2);
    assert_ne!(spiderized.sources[0], spiderized.sources[1]);
}

#[test]
fn spiderize_applies_pending_quotients() {
    let mut f = OpenHypergraph::empty();
    let x = f.new_node(());
    let y = f.new_node(());
    f.new_edge("loop", ([x], [y]));
    f.unify(x, y);

    let spiderized = f.spiderize().unwrap();

    assert_eq!(spiderized.hypergraph.edges.len(), 1 + 2);
    assert!(spiderized.clone().to_strict().is_acyclic());
    assert_eq!(
        forget_spiders(spiderized),
        OpenHypergraph {
            sources: vec![],
            targets: vec![],
            hypergraph: Hypergraph {
                nodes: vec![()],
                edges: vec!["loop"],
                adjacency: vec![Hyperedge {
                    sources: vec![NodeId(0)],
                    targets: vec![NodeId(0)],
                }],
                quotient: (vec![], vec![]),
            },
        }
    );
}

#[test]
fn spiderize_rejects_inconsistent_quotients() {
    let mut f = OpenHypergraph::<_, ()>::identity(vec!["a", "b"]);
    f.unify(NodeId(0), NodeId(1));

    assert!(f.spiderize().is_err());
}

proptest! {
    #[test]
    fn spiderize_is_acyclic_monogamous_and_forgetful(
        strict_input in arb_open_hypergraph()
    ) {
        let input: OpenHypergraph<Obj, Arr> =
            OpenHypergraph::from_strict(strict_input);
        let input_node_count = input.hypergraph.nodes.len();
        let input_operation_count = input.hypergraph.edges.len();
        let spiderized = input.clone().spiderize().unwrap();
        let strict_spiderized = spiderized.clone().to_strict();

        assert!(strict_spiderized.is_monogamous());
        assert!(strict_spiderized.is_acyclic());
        assert_eq!(
            spiderized.hypergraph.edges.len(),
            input_operation_count + 2 * input_node_count
        );
        assert_eq!(forget_spiders(spiderized), input);
    }
}
