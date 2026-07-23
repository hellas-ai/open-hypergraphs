//! Fast cycle-breaking heuristics for lax open hypergraphs.

use super::{NodeId, OpenHypergraph};
use std::cmp::Reverse;
use std::collections::{BinaryHeap, VecDeque};

/// Choose nodes whose spiderization is guaranteed to break every directed
/// cycle.
///
/// This computes a greedy directed feedback vertex set on the bipartite
/// incidence graph
///
/// ```text
/// node -> operation -> node.
/// ```
///
/// Basic idea:
///
/// - Construct a bipartite node-operation incidence graph.
/// - Vertices with indegree zero or outdegree zero cannot participate in a cycle.
/// - Repeatedly peel those vertices until no more acyclic fringe can be removed.
/// - Select the remaining wire node with the largest `indegree * outdegree`
///   score, remove it, and repeat.
///
/// Selecting high-degree nodes in this way tends to break several cycles at once.
///
/// The result is deterministic and is guaranteed to make
/// `f.clone().spiderize_nodes(&result)` acyclic. It is a heuristic: the
/// returned set need not have minimum size.
///
/// Returns [`None`] if `f` has pending node identifications. Quotienting can
/// create cycles, so cycle breaking must operate on the semantic, quotiented
/// graph. When successful, the returned IDs refer directly to `f`.
#[must_use]
pub fn cycle_breaking_nodes<O, A>(f: &OpenHypergraph<O, A>) -> Option<Vec<NodeId>> {
    if !f.hypergraph.is_strict() {
        return None;
    }

    assert_eq!(
        f.hypergraph.edges.len(),
        f.hypergraph.adjacency.len(),
        "malformed hypergraph: edges and adjacency lengths differ"
    );

    let node_count = f.hypergraph.nodes.len();

    // Allocate a bipartite incidence graph.
    // Nodes occupy 0..node_count, followed by one vertex for each operation.
    // Keeping operation vertices avoids expanding an m -> n hyperedge into m*n arcs.
    let vertex_count = node_count + f.hypergraph.adjacency.len();
    let mut outgoing = vec![Vec::new(); vertex_count];
    let mut incoming = vec![Vec::new(); vertex_count];

    // Populate the incidence graph.
    // Preserve the hypergraph's direction: every source node points to its
    // operation, and that operation points to each target node.
    for (operation, adjacency) in f.hypergraph.adjacency.iter().enumerate() {
        let operation = node_count + operation;

        for source in &adjacency.sources {
            add_arc(source.0, operation, &mut outgoing, &mut incoming);
        }
        for target in &adjacency.targets {
            add_arc(operation, target.0, &mut outgoing, &mut incoming);
        }
    }

    // Only node vertices are selectable; operation vertices may be peeled but
    // never returned.
    let selected = greedy_feedback_nodes(node_count, &outgoing, &incoming);

    Some(selected.into_iter().map(NodeId).collect())
}

/// Add one directed arc to both forward and reverse adjacency lists.
///
/// Keeping both directions lets vertex removal update neighboring in- and
/// out-degrees without searching the whole graph.
fn add_arc(source: usize, target: usize, outgoing: &mut [Vec<usize>], incoming: &mut [Vec<usize>]) {
    outgoing[source].push(target);
    incoming[target].push(source);
}

////////////////////////////////////////////////////////////////////////////////
// Cycle breaking logic

/// Greedily select wire vertices that hit every directed cycle.
///
/// Vertices `0..selectable_count` are wire nodes and may be selected; the
/// remaining vertices are operations. Sources and sinks are peeled, then the
/// highest-scoring wire vertex is selected whenever cyclic structure remains.
///
/// Runs in roughly `O((vertices + arcs) log(selectable vertices))` time.
fn greedy_feedback_nodes(
    selectable_count: usize,
    outgoing: &[Vec<usize>],
    incoming: &[Vec<usize>],
) -> Vec<usize> {
    debug_assert_eq!(outgoing.len(), incoming.len());

    // Degrees and `active` describe the current residual graph. Every vertex
    // is removed exactly once, either freely or as a selected cycle breaker.
    let vertex_count = outgoing.len();
    let mut active = vec![true; vertex_count];
    let mut active_count = vertex_count;
    let mut indegree: Vec<usize> = incoming.iter().map(Vec::len).collect();
    let mut outdegree: Vec<usize> = outgoing.iter().map(Vec::len).collect();
    let mut peel = VecDeque::new();
    let mut candidates = BinaryHeap::new();
    let mut selected = Vec::new();

    // Seed both worklists: immediately peelable vertices go in the FIFO queue,
    // while selectable vertices with two-sided connectivity go in the heap.
    for vertex in 0..vertex_count {
        if indegree[vertex] == 0 || outdegree[vertex] == 0 {
            peel.push_back(vertex);
        }
        push_candidate(
            vertex,
            selectable_count,
            &active,
            &indegree,
            &outdegree,
            &mut candidates,
        );
    }

    while active_count > 0 {
        // Exhaust all consequences of previous removals before choosing
        // another feedback node. This prevents selecting vertices already
        // proven not to participate in the residual cycles.
        while let Some(vertex) = peel.pop_front() {
            if !active[vertex] || (indegree[vertex] > 0 && outdegree[vertex] > 0) {
                continue;
            }
            remove_vertex(
                vertex,
                selectable_count,
                outgoing,
                incoming,
                &mut active,
                &mut indegree,
                &mut outdegree,
                &mut peel,
                &mut candidates,
            );
            active_count -= 1;
        }

        if active_count == 0 {
            break;
        }

        // Peeling got stuck, so the residual graph is cyclic. Pop until the
        // degree snapshot agrees with current state, skipping lazy stale
        // entries, then choose the best-scoring node.
        let vertex = loop {
            let (_, Reverse(vertex), candidate_indegree, candidate_outdegree) = candidates
                .pop()
                .expect("cyclic residual incidence graph must contain a selectable node");
            if active[vertex]
                && indegree[vertex] == candidate_indegree
                && outdegree[vertex] == candidate_outdegree
            {
                break vertex;
            }
        };

        // This is the only non-free removal: record it for spiderization.
        // Removing it may expose a large acyclic fringe for the next peel.
        selected.push(vertex);
        remove_vertex(
            vertex,
            selectable_count,
            outgoing,
            incoming,
            &mut active,
            &mut indegree,
            &mut outdegree,
            &mut peel,
            &mut candidates,
        );
        active_count -= 1;
    }

    // Heap choice order is an implementation detail; node order is a more
    // stable and convenient public result.
    selected.sort_unstable();
    selected
}

/// Estimate how much cyclic connectivity removing a node will disrupt.
///
/// A node with many incoming and outgoing arcs joins many possible paths. The
/// saturating product avoids overflow for unusually large incidence graphs.
fn score(indegree: usize, outdegree: usize) -> usize {
    indegree.saturating_mul(outdegree)
}

/// A lazily validated heap entry.
///
/// Entries contain `(score, node, indegree, outdegree)`. [`Reverse`] makes the
/// lower node index win deterministic ties in the max-heap. The degree
/// snapshots let us recognize entries made stale by later removals.
type Candidate = (usize, Reverse<usize>, usize, usize);

/// Add a selectable vertex's current state to the candidate heap.
///
/// The heap is deliberately lazy: degree changes push new entries rather than
/// locating and updating old ones. Stale entries are discarded when popped.
fn push_candidate(
    node: usize,
    selectable_count: usize,
    active: &[bool],
    indegree: &[usize],
    outdegree: &[usize],
    candidates: &mut BinaryHeap<Candidate>,
) {
    if node < selectable_count && active[node] && indegree[node] > 0 && outdegree[node] > 0 {
        candidates.push((
            score(indegree[node], outdegree[node]),
            Reverse(node),
            indegree[node],
            outdegree[node],
        ));
    }
}

/// Remove a vertex from the active graph and update its neighbors.
///
/// Removing outgoing arcs lowers target in-degrees; removing incoming arcs
/// lowers source out-degrees. Neighbors that become sources or sinks enter the
/// free peeling queue, while still-cyclic node vertices receive refreshed heap
/// entries.
#[allow(clippy::too_many_arguments)]
fn remove_vertex(
    vertex: usize,
    selectable_count: usize,
    outgoing: &[Vec<usize>],
    incoming: &[Vec<usize>],
    active: &mut [bool],
    indegree: &mut [usize],
    outdegree: &mut [usize],
    peel: &mut VecDeque<usize>,
    candidates: &mut BinaryHeap<Candidate>,
) {
    // Mark first so a self-arc, if one is ever supplied, cannot update the
    // removed vertex's own degree.
    active[vertex] = false;

    // Delete vertex -> target arcs.
    for &target in &outgoing[vertex] {
        if active[target] {
            indegree[target] -= 1;
            if indegree[target] == 0 || outdegree[target] == 0 {
                peel.push_back(target);
            }
            push_candidate(
                target,
                selectable_count,
                active,
                indegree,
                outdegree,
                candidates,
            );
        }
    }

    // Delete source -> vertex arcs.
    for &source in &incoming[vertex] {
        if active[source] {
            outdegree[source] -= 1;
            if indegree[source] == 0 || outdegree[source] == 0 {
                peel.push_back(source);
            }
            push_candidate(
                source,
                selectable_count,
                active,
                indegree,
                outdegree,
                candidates,
            );
        }
    }
}
