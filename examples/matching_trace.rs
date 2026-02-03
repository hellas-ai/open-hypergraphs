use open_hypergraphs::lax::matching::{MatchEvent, MatchTrace};
use open_hypergraphs::lax::Hypergraph;
use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;

struct PrintTrace {
    indent: Cell<usize>,
    edge_decision: RefCell<BTreeMap<usize, usize>>,
    node_decision: RefCell<BTreeMap<usize, Vec<usize>>>,
    edge_assign: RefCell<BTreeMap<usize, usize>>,
    node_assign: RefCell<BTreeMap<usize, usize>>,
    frame_edges: RefCell<BTreeMap<usize, Vec<usize>>>,
    frame_nodes: RefCell<BTreeMap<usize, Vec<usize>>>,
    frame_stack: RefCell<Vec<usize>>,
}

impl PrintTrace {
    fn new() -> Self {
        Self {
            indent: Cell::new(0),
            edge_decision: RefCell::new(BTreeMap::new()),
            node_decision: RefCell::new(BTreeMap::new()),
            edge_assign: RefCell::new(BTreeMap::new()),
            node_assign: RefCell::new(BTreeMap::new()),
            frame_edges: RefCell::new(BTreeMap::new()),
            frame_nodes: RefCell::new(BTreeMap::new()),
            frame_stack: RefCell::new(Vec::new()),
        }
    }

    fn indent(&self) -> usize {
        self.indent.get() * 2
    }

    fn print_solution(&self) {
        let edge_assign = self.edge_assign.borrow();
        let node_assign = self.node_assign.borrow();
        let mut parts = Vec::new();
        for (p_edge, t_edge) in edge_assign.iter() {
            parts.push(format!("e{p_edge}->t{t_edge}"));
        }
        for (p_node, t_node) in node_assign.iter() {
            parts.push(format!("n{p_node}->t{t_node}"));
        }

        if parts.is_empty() {
            println!("{:indent$}solution", "", indent = self.indent());
        } else {
            println!(
                "{:indent$}solution [{}]",
                "",
                parts.join(", "),
                indent = self.indent()
            );
        }
    }
}

impl MatchTrace for PrintTrace {
    fn on_event(&self, event: MatchEvent) {
        match event {
            MatchEvent::EnterFrame { depth: _, frame_id } => {
                println!(
                    "{:indent$}enter frame #{frame_id}",
                    "",
                    indent = self.indent()
                );
                self.indent.set(self.indent.get() + 1);
                self.frame_stack.borrow_mut().push(frame_id);
            }
            MatchEvent::ExitFrame { depth, frame_id } => {
                let indent = self.indent.get().saturating_sub(1);
                self.indent.set(indent);
                println!(
                    "{:indent$}exit frame #{frame_id}",
                    "",
                    indent = self.indent()
                );
                if let Some(top) = self.frame_stack.borrow_mut().pop() {
                    self.edge_decision.borrow_mut().remove(&top);
                    self.node_decision.borrow_mut().remove(&top);
                    if let Some(edges) = self.frame_edges.borrow_mut().remove(&top) {
                        let mut edge_assign = self.edge_assign.borrow_mut();
                        for p_edge in edges {
                            edge_assign.remove(&p_edge);
                        }
                    }
                    if let Some(nodes) = self.frame_nodes.borrow_mut().remove(&top) {
                        let mut node_assign = self.node_assign.borrow_mut();
                        for p_node in nodes {
                            node_assign.remove(&p_node);
                        }
                    }
                }
            }
            MatchEvent::Decision {
                pattern_edge,
                pattern_node,
                choice_features,
                candidate_count,
                heuristic_tag,
                depth,
            } => {
                let current = self.frame_stack.borrow().last().copied();
                if let Some(frame_id) = current {
                    if let Some(edge) = pattern_edge {
                        self.edge_decision.borrow_mut().insert(frame_id, edge);
                    }
                    if let Some(node) = pattern_node {
                        self.node_decision
                            .borrow_mut()
                            .entry(frame_id)
                            .or_default()
                            .push(node);
                    }
                }
                println!(
                    "{:indent$}decision pattern_edge={pattern_edge:?} pattern_node={pattern_node:?} candidates={candidate_count} features={choice_features} heuristic={heuristic_tag}",
                    "",
                    indent = self.indent()
                );
            }
            MatchEvent::Branch {
                target_edge,
                target_node,
                depth,
            } => {
                let current = self.frame_stack.borrow().last().copied();
                if let Some(frame_id) = current {
                    if let Some(edge) = target_edge {
                        if let Some(p_edge) = self.edge_decision.borrow().get(&frame_id) {
                            self.edge_assign.borrow_mut().insert(*p_edge, edge);
                            self.frame_edges
                                .borrow_mut()
                                .entry(frame_id)
                                .or_default()
                                .push(*p_edge);
                        }
                    }
                    if let Some(node) = target_node {
                        let mut pending = self.node_decision.borrow_mut();
                        if let Some(stack) = pending.get_mut(&frame_id) {
                            if let Some(p_node) = stack.pop() {
                                self.node_assign.borrow_mut().insert(p_node, node);
                                self.frame_nodes
                                    .borrow_mut()
                                    .entry(frame_id)
                                    .or_default()
                                    .push(p_node);
                            }
                        }
                    }
                }
                let frame_id = self.frame_stack.borrow().last().copied();
                let edge_map = target_edge.and_then(|edge| {
                    let frame_id = frame_id?;
                    let edge_decision = self.edge_decision.borrow();
                    let p_edge = edge_decision.get(&frame_id)?;
                    Some(format!("map e{p_edge}->t{edge}"))
                });
                let node_map = target_node.and_then(|node| {
                    let frame_id = frame_id?;
                    let node_decision = self.node_decision.borrow();
                    let p_node = node_decision.get(&frame_id)?.last().copied()?;
                    Some(format!("map n{p_node}->t{node}"))
                });
                let map_note = match (edge_map, node_map) {
                    (Some(edge), Some(node)) => format!("{edge}, {node}"),
                    (Some(edge), None) => edge,
                    (None, Some(node)) => node,
                    (None, None) => String::new(),
                };
                println!(
                    "{:indent$}branch target_edge={target_edge:?} target_node={target_node:?} {map_note}",
                    "",
                    indent = self.indent()
                );
            }
            MatchEvent::PropagationSummary { depth: _ } => {
                println!("{:indent$}propagation done", "", indent = self.indent());
            }
            MatchEvent::Prune {
                reason,
                detail,
                depth: _,
            } => {
                println!(
                    "{:indent$}prune: {reason} ({detail})",
                    "",
                    indent = self.indent()
                );
            }
            MatchEvent::Solution => {
                self.print_solution();
            }
        }
    }
}

fn build_target() -> Hypergraph<i32, char> {
    let mut target = Hypergraph::empty();
    let n0 = target.new_node(0);
    let n1 = target.new_node(0);
    target.new_edge('f', (vec![n0], vec![n1]));
    target.new_edge('f', (vec![n1], vec![n0]));
    target.new_edge('f', (vec![n0], vec![n0]));
    target.new_edge('f', (vec![n1], vec![n1]));
    target
}

fn build_pattern() -> Hypergraph<i32, char> {
    let mut pattern = Hypergraph::empty();
    let p0 = pattern.new_node(0);
    let p1 = pattern.new_node(0);
    pattern.new_edge('f', (vec![p0], vec![p1]));
    pattern
}

fn main() {
    let target = build_target();
    let pattern = build_pattern();
    let trace = PrintTrace::new();

    println!("=== isomorphisms ===");
    let iso = target.find_subgraph_isomorphisms(&pattern, Some(&trace));
    println!("isomorphisms: {}", iso.len());

    println!();
    println!("=== homomorphisms ===");
    let homo = target.find_subgraph_homomorphisms(&pattern, Some(&trace));
    println!("homomorphisms: {}", homo.len());
}
