use crate::array::vec::{VecArray, VecKind};
use crate::category::{Arrow, Coproduct};
use crate::finite_function::FiniteFunction;
use crate::indexed_coproduct::IndexedCoproduct;
use crate::semifinite::SemifiniteFunction;
use crate::strict::hypergraph::Hypergraph;
use crate::strict::open_hypergraph::{
    apply_rewrite, ConvexMatchWitness, MonogamousAcyclicHost, OpenHypergraph, RewriteRule,
};
use std::collections::HashMap;

const OBJ: i32 = 0;
const MU: i32 = 1;
const DELTA: i32 = 2;

fn make_indexed_coproduct(
    segments: &[Vec<usize>],
    target: usize,
) -> IndexedCoproduct<VecKind, FiniteFunction<VecKind>> {
    let mut lengths = Vec::with_capacity(segments.len());
    let mut values = Vec::new();
    for seg in segments {
        lengths.push(seg.len());
        values.extend_from_slice(seg);
    }
    let sources = SemifiniteFunction::new(VecArray(lengths));
    let values = FiniteFunction::new(VecArray(values), target).unwrap();
    IndexedCoproduct::from_semifinite(sources, values).unwrap()
}

fn make_hypergraph(
    sources: &[Vec<usize>],
    targets: &[Vec<usize>],
    w_labels: Vec<i32>,
    x_labels: Vec<i32>,
) -> Hypergraph<VecKind, i32, i32> {
    let w_len = w_labels.len();
    let s = make_indexed_coproduct(sources, w_len);
    let t = make_indexed_coproduct(targets, w_len);
    let w = SemifiniteFunction::new(VecArray(w_labels));
    let x = SemifiniteFunction::new(VecArray(x_labels));
    Hypergraph::new(s, t, w, x).unwrap()
}

#[derive(Clone)]
struct NamedEdge<'a> {
    logical_name: &'a str,
    sources: Vec<&'a str>,
    targets: Vec<&'a str>,
    label: i32,
}

#[derive(Clone, Copy)]
struct NamedWire<'a> {
    logical_name: &'a str,
    label: i32,
}

#[derive(Clone, Copy)]
struct BoundaryPort<'a> {
    logical_name: &'a str,
}

fn w<'a>(logical_name: &'a str, label: i32) -> NamedWire<'a> {
    NamedWire {
        logical_name,
        label,
    }
}

fn inp<'a>(logical_name: &'a str) -> BoundaryPort<'a> {
    BoundaryPort { logical_name }
}

fn out<'a>(logical_name: &'a str) -> BoundaryPort<'a> {
    BoundaryPort { logical_name }
}

fn e<'a, const S: usize, const T: usize>(
    logical_name: &'a str,
    sources: [&'a str; S],
    targets: [&'a str; T],
    label: i32,
) -> NamedEdge<'a> {
    NamedEdge {
        logical_name,
        sources: sources.into(),
        targets: targets.into(),
        label,
    }
}

fn wire_indices(names: &[&str], name_to_index: &HashMap<&str, usize>, context: &str) -> Vec<usize> {
    names
        .iter()
        .map(|name| {
            *name_to_index
                .get(name)
                .unwrap_or_else(|| panic!("unknown wire `{name}` in {context}"))
        })
        .collect()
}

/// Declarative test helper: build a strict Hypergraph from named wires and
/// edges described by wire names.
fn make_hypergraph_named<'a, W, E>(wires: W, edges: E) -> Hypergraph<VecKind, i32, i32>
where
    W: IntoIterator<Item = NamedWire<'a>>,
    E: IntoIterator<Item = NamedEdge<'a>>,
{
    let wires: Vec<NamedWire<'a>> = wires.into_iter().collect();
    let edges: Vec<NamedEdge<'a>> = edges.into_iter().collect();

    let name_to_index: HashMap<&str, usize> = wires
        .iter()
        .enumerate()
        .map(|(ix, wire)| (wire.logical_name, ix))
        .collect();

    let sources: Vec<Vec<usize>> = edges
        .iter()
        .map(|edge| wire_indices(&edge.sources, &name_to_index, "edge sources"))
        .collect();
    let targets: Vec<Vec<usize>> = edges
        .iter()
        .map(|edge| wire_indices(&edge.targets, &name_to_index, "edge targets"))
        .collect();
    let w_labels: Vec<i32> = wires.iter().map(|wire| wire.label).collect();
    let x_labels: Vec<i32> = edges.iter().map(|edge| edge.label).collect();

    make_hypergraph(&sources, &targets, w_labels, x_labels)
}

/// Declarative test helper: build a strict OpenHypergraph from named wires,
/// named edge endpoints, and named boundary wires.
fn make_open_hypergraph_named<'a, W, E, I, O>(
    wires: W,
    edges: E,
    inputs: I,
    outputs: O,
) -> OpenHypergraph<VecKind, i32, i32>
where
    W: IntoIterator<Item = NamedWire<'a>>,
    E: IntoIterator<Item = NamedEdge<'a>>,
    I: IntoIterator<Item = BoundaryPort<'a>>,
    O: IntoIterator<Item = BoundaryPort<'a>>,
{
    let wires: Vec<NamedWire<'a>> = wires.into_iter().collect();
    let edges: Vec<NamedEdge<'a>> = edges.into_iter().collect();
    let inputs: Vec<BoundaryPort<'a>> = inputs.into_iter().collect();
    let outputs: Vec<BoundaryPort<'a>> = outputs.into_iter().collect();

    let h = make_hypergraph_named(wires.clone(), edges.clone());
    let name_to_index: HashMap<&str, usize> = wires
        .iter()
        .enumerate()
        .map(|(ix, wire)| (wire.logical_name, ix))
        .collect();
    let input_names: Vec<&str> = inputs.iter().map(|p| p.logical_name).collect();
    let output_names: Vec<&str> = outputs.iter().map(|p| p.logical_name).collect();
    let s_map = wire_indices(&input_names, &name_to_index, "open boundary inputs");
    let t_map = wire_indices(&output_names, &name_to_index, "open boundary outputs");
    let s = FiniteFunction::new(VecArray(s_map), h.w.len()).unwrap();
    let t = FiniteFunction::new(VecArray(t_map), h.w.len()).unwrap();
    OpenHypergraph::new(s, t, h).unwrap()
}

fn make_map(indices: &[usize], target: usize) -> FiniteFunction<VecKind> {
    FiniteFunction::new(VecArray(indices.to_vec()), target).unwrap()
}

struct NamedOpenGraph {
    graph: OpenHypergraph<VecKind, i32, i32>,
    wire_ix: HashMap<String, usize>,
    edge_ix: HashMap<String, usize>,
}

fn make_named_open_hypergraph<'a, W, E, I, O>(
    wires: W,
    edges: E,
    inputs: I,
    outputs: O,
) -> NamedOpenGraph
where
    W: IntoIterator<Item = NamedWire<'a>>,
    E: IntoIterator<Item = NamedEdge<'a>>,
    I: IntoIterator<Item = BoundaryPort<'a>>,
    O: IntoIterator<Item = BoundaryPort<'a>>,
{
    let wires: Vec<NamedWire<'a>> = wires.into_iter().collect();
    let edges: Vec<NamedEdge<'a>> = edges.into_iter().collect();
    let inputs: Vec<BoundaryPort<'a>> = inputs.into_iter().collect();
    let outputs: Vec<BoundaryPort<'a>> = outputs.into_iter().collect();

    let graph = make_open_hypergraph_named(
        wires.clone(),
        edges.clone(),
        inputs.clone(),
        outputs.clone(),
    );

    let wire_ix: HashMap<String, usize> = wires
        .iter()
        .enumerate()
        .map(|(ix, wire)| (wire.logical_name.to_string(), ix))
        .collect();

    let mut edge_ix: HashMap<String, usize> = HashMap::new();
    for (ix, edge) in edges.iter().enumerate() {
        let name = edge.logical_name.to_string();
        assert!(
            edge_ix.insert(name, ix).is_none(),
            "duplicate edge logical_name"
        );
    }

    NamedOpenGraph {
        graph,
        wire_ix,
        edge_ix,
    }
}

fn named_match_witness(
    lhs: &NamedOpenGraph,
    host: &NamedOpenGraph,
    wire_pairs: &[(&str, &str)],
    edge_pairs: &[(&str, &str)],
    host_ma: &MonogamousAcyclicHost<'_, VecKind, i32, i32>,
) -> ConvexMatchWitness<VecKind> {
    let mut w_table = vec![usize::MAX; lhs.graph.h.w.len()];
    for (lhs_name, host_name) in wire_pairs {
        let l = *lhs
            .wire_ix
            .get(*lhs_name)
            .unwrap_or_else(|| panic!("unknown lhs wire name `{lhs_name}`"));
        let h = *host
            .wire_ix
            .get(*host_name)
            .unwrap_or_else(|| panic!("unknown host wire name `{host_name}`"));
        w_table[l] = h;
    }
    assert!(
        w_table.iter().all(|ix| *ix != usize::MAX),
        "wire_pairs must provide a total map from lhs wires to host wires",
    );

    let mut x_table = vec![usize::MAX; lhs.graph.h.x.len()];
    for (lhs_name, host_name) in edge_pairs {
        let l = *lhs
            .edge_ix
            .get(*lhs_name)
            .unwrap_or_else(|| panic!("unknown lhs edge name `{lhs_name}`"));
        let h = *host
            .edge_ix
            .get(*host_name)
            .unwrap_or_else(|| panic!("unknown host edge name `{host_name}`"));
        x_table[l] = h;
    }
    assert!(
        x_table.iter().all(|ix| *ix != usize::MAX),
        "edge_pairs must provide a total map from lhs edges to host edges",
    );

    let w = make_map(&w_table, host.graph.h.w.len());
    let x = make_map(&x_table, host.graph.h.x.len());
    ConvexMatchWitness::new(&lhs.graph, host_ma, w, x).unwrap()
}

struct FrobeniusSemiAlgebraRules {
    fs3: RewriteRule<VecKind, i32, i32>,
    fs4: RewriteRule<VecKind, i32, i32>,
    fs3_lhs: NamedOpenGraph,
    fs4_lhs: NamedOpenGraph,
}

/// Build the two oriented interaction rules (FS3, FS4) used in the paper's
/// Frobenius semi-algebra rewriting system FS.
///
/// Reference:
/// Bonchi et al., "String Diagram Rewrite Theory II: Rewriting with Symmetric
/// Monoidal Structure", arXiv:2104.14686v2, Section 5.1 (rules FS3/FS4).
fn frobenius_semi_algebra_rules() -> FrobeniusSemiAlgebraRules {
    // Common "middle" shape: μ followed by δ (2 -> 2).
    let mu_then_delta = make_named_open_hypergraph(
        [
            w("a", OBJ),
            w("b", OBJ),
            w("m", OBJ),
            w("x", OBJ),
            w("y", OBJ),
        ],
        [
            e("mu_then_delta_mu", ["a", "b"], ["m"], MU),
            e("mu_then_delta_delta", ["m"], ["x", "y"], DELTA),
        ],
        [inp("a"), inp("b")],
        [out("x"), out("y")],
    );

    // Left interaction wing (one orientation of the Frobenius law).
    let left_wing = make_named_open_hypergraph(
        [
            w("a", OBJ),
            w("b", OBJ),
            w("m", OBJ),
            w("x", OBJ),
            w("y", OBJ),
        ],
        [
            e("delta", ["a"], ["x", "m"], DELTA),
            e("mu", ["m", "b"], ["y"], MU),
        ],
        [inp("a"), inp("b")],
        [out("x"), out("y")],
    );

    // Right interaction wing (the mirrored orientation).
    let right_wing = make_named_open_hypergraph(
        [
            w("a", OBJ),
            w("b", OBJ),
            w("m", OBJ),
            w("x", OBJ),
            w("y", OBJ),
        ],
        [
            e("delta", ["b"], ["m", "y"], DELTA),
            e("mu", ["a", "m"], ["x"], MU),
        ],
        [inp("a"), inp("b")],
        [out("x"), out("y")],
    );

    // We orient both interaction rules towards the shared "μ then δ" shape.
    let fs3 = RewriteRule::new(left_wing.graph.clone(), mu_then_delta.graph.clone()).unwrap();
    let fs4 = RewriteRule::new(right_wing.graph.clone(), mu_then_delta.graph).unwrap();
    FrobeniusSemiAlgebraRules {
        fs3,
        fs4,
        fs3_lhs: left_wing,
        fs4_lhs: right_wing,
    }
}

/// Example 45 host diagram from the paper (a 2 -> 2 open hypergraph with
/// two comultiplications feeding two multiplications).
fn frobenius_example45_host() -> OpenHypergraph<VecKind, i32, i32> {
    frobenius_example45_host_named().graph
}

fn frobenius_example45_host_named() -> NamedOpenGraph {
    make_named_open_hypergraph(
        [
            w("in_l", OBJ),
            w("in_r", OBJ),
            w("l_mid", OBJ),
            w("l_out", OBJ),
            w("r_mid", OBJ),
            w("r_out", OBJ),
            w("out_l", OBJ),
            w("out_r", OBJ),
        ],
        [
            e("delta_l", ["in_l"], ["l_out", "l_mid"], DELTA),
            e("delta_r", ["in_r"], ["r_out", "r_mid"], DELTA),
            e("mu_l", ["l_mid", "r_mid"], ["out_l"], MU),
            e("mu_r", ["l_out", "r_out"], ["out_r"], MU),
        ],
        [inp("in_l"), inp("in_r")],
        [out("out_l"), out("out_r")],
    )
}

fn expected_example45_after_fs3() -> OpenHypergraph<VecKind, i32, i32> {
    make_open_hypergraph_named(
        [
            w("in_l", OBJ),
            w("in_r", OBJ),
            w("l_mid", OBJ),
            w("l_out", OBJ),
            w("r_mid", OBJ),
            w("r_out", OBJ),
            w("out_l", OBJ),
            w("out_r", OBJ),
        ],
        [
            e("exp_h1_delta_r", ["in_r"], ["r_out", "r_mid"], DELTA),
            e("exp_h1_mu_r", ["l_out", "r_out"], ["out_r"], MU),
            e("exp_h1_mu_new", ["in_l", "r_mid"], ["l_mid"], MU),
            e("exp_h1_delta_new", ["l_mid"], ["l_out", "out_l"], DELTA),
        ],
        [inp("in_l"), inp("in_r")],
        [out("out_l"), out("out_r")],
    )
}

fn expected_example45_after_fs4() -> OpenHypergraph<VecKind, i32, i32> {
    make_open_hypergraph_named(
        [
            w("in_l", OBJ),
            w("in_r", OBJ),
            w("l_mid", OBJ),
            w("l_out", OBJ),
            w("r_mid", OBJ),
            w("r_out", OBJ),
            w("out_l", OBJ),
            w("out_r", OBJ),
        ],
        [
            e("exp_h2_delta_l", ["in_l"], ["l_out", "l_mid"], DELTA),
            e("exp_h2_mu_l", ["l_mid", "r_mid"], ["out_l"], MU),
            e("exp_h2_mu_new", ["l_out", "in_r"], ["r_out"], MU),
            e("exp_h2_delta_new", ["r_out"], ["out_r", "r_mid"], DELTA),
        ],
        [inp("in_l"), inp("in_r")],
        [out("out_l"), out("out_r")],
    )
}

// Circuit DSL (test-only):
// Keep this tiny and declarative so circuit rewrite tests stay readable.
const G_AND: i32 = 100;
const G_XOR: i32 = 101;
const G_NOT: i32 = 102;
const G_CONST0: i32 = 103;
const G_CONST1: i32 = 104;

fn cw<'a>(logical_name: &'a str) -> NamedWire<'a> {
    w(logical_name, OBJ)
}

fn g_and<'a>(logical_name: &'a str, a: &'a str, b: &'a str, out_w: &'a str) -> NamedEdge<'a> {
    e(logical_name, [a, b], [out_w], G_AND)
}

fn g_xor<'a>(logical_name: &'a str, a: &'a str, b: &'a str, out_w: &'a str) -> NamedEdge<'a> {
    e(logical_name, [a, b], [out_w], G_XOR)
}

fn g_not<'a>(logical_name: &'a str, in_w: &'a str, out_w: &'a str) -> NamedEdge<'a> {
    e(logical_name, [in_w], [out_w], G_NOT)
}

fn g_const0<'a>(logical_name: &'a str, out_w: &'a str) -> NamedEdge<'a> {
    e(logical_name, [], [out_w], G_CONST0)
}

fn g_const1<'a>(logical_name: &'a str, out_w: &'a str) -> NamedEdge<'a> {
    e(logical_name, [], [out_w], G_CONST1)
}

// Program-optimization DSL (test-only):
// We model a tiny SSA-like program graph with assignments and arithmetic.
const P_ASSIGN: i32 = 200;
const P_ADD: i32 = 201;
const P_CONST1: i32 = 202;
const P_ADD_CONST1: i32 = 203;
const P_DISCARD: i32 = 204;

fn pw<'a>(logical_name: &'a str) -> NamedWire<'a> {
    w(logical_name, OBJ)
}

fn p_assign<'a>(logical_name: &'a str, src: &'a str, dst: &'a str) -> NamedEdge<'a> {
    e(logical_name, [src], [dst], P_ASSIGN)
}

fn p_add<'a>(logical_name: &'a str, x: &'a str, y: &'a str, out_w: &'a str) -> NamedEdge<'a> {
    e(logical_name, [x, y], [out_w], P_ADD)
}

fn p_const1<'a>(logical_name: &'a str, out_w: &'a str) -> NamedEdge<'a> {
    e(logical_name, [], [out_w], P_CONST1)
}

fn p_add_const1<'a>(logical_name: &'a str, x: &'a str, out_w: &'a str) -> NamedEdge<'a> {
    e(logical_name, [x], [out_w], P_ADD_CONST1)
}

fn p_discard<'a>(logical_name: &'a str, in_w: &'a str) -> NamedEdge<'a> {
    e(logical_name, [in_w], [], P_DISCARD)
}

fn program_rule_assign_then_discard() -> (
    RewriteRule<VecKind, i32, i32>,
    NamedOpenGraph,
    NamedOpenGraph,
) {
    // Markov-style dead elimination:
    // assign(u -> v); discard(v) -> discard(u)
    let lhs = make_named_open_hypergraph(
        [pw("u"), pw("v")],
        [p_assign("assign", "u", "v"), p_discard("discard_v", "v")],
        [inp("u")],
        [],
    );
    let rhs = make_named_open_hypergraph([pw("u")], [p_discard("discard_u", "u")], [inp("u")], []);
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();
    (rule, lhs, rhs)
}

struct CircuitRule {
    rule: RewriteRule<VecKind, i32, i32>,
    lhs: NamedOpenGraph,
    rhs: NamedOpenGraph,
}

fn circuit_rule_and_one() -> CircuitRule {
    let lhs = make_named_open_hypergraph(
        [cw("x"), cw("one"), cw("y")],
        [g_const1("k1", "one"), g_and("and", "x", "one", "y")],
        [inp("x")],
        [out("y")],
    );
    let rhs = make_named_open_hypergraph([cw("x")], [], [inp("x")], [out("x")]);
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();
    CircuitRule { rule, lhs, rhs }
}

fn circuit_rule_xor_zero() -> CircuitRule {
    let lhs = make_named_open_hypergraph(
        [cw("x"), cw("zero"), cw("y")],
        [g_const0("k0", "zero"), g_xor("xor", "x", "zero", "y")],
        [inp("x")],
        [out("y")],
    );
    let rhs = make_named_open_hypergraph([cw("x")], [], [inp("x")], [out("x")]);
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();
    CircuitRule { rule, lhs, rhs }
}

fn circuit_rule_double_not() -> CircuitRule {
    let lhs = make_named_open_hypergraph(
        [cw("x"), cw("m"), cw("y")],
        [g_not("n1", "x", "m"), g_not("n2", "m", "y")],
        [inp("x")],
        [out("y")],
    );
    let rhs = make_named_open_hypergraph([cw("x")], [], [inp("x")], [out("x")]);
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();
    CircuitRule { rule, lhs, rhs }
}

fn fs1_associativity_rule_named() -> (
    RewriteRule<VecKind, i32, i32>,
    NamedOpenGraph,
    NamedOpenGraph,
) {
    // FS1-style associativity orientation for μ:
    // μ(μ(a,b), c) -> μ(a, μ(b,c))
    let lhs = make_named_open_hypergraph(
        [
            w("a", OBJ),
            w("b", OBJ),
            w("c", OBJ),
            w("m", OBJ),
            w("out", OBJ),
        ],
        [
            e("mu_left", ["a", "b"], ["m"], MU),
            e("mu_top", ["m", "c"], ["out"], MU),
        ],
        [inp("a"), inp("b"), inp("c")],
        [out("out")],
    );
    let rhs = make_named_open_hypergraph(
        [
            w("a", OBJ),
            w("b", OBJ),
            w("c", OBJ),
            w("m", OBJ),
            w("out", OBJ),
        ],
        [
            e("mu_right", ["b", "c"], ["m"], MU),
            e("mu_top", ["a", "m"], ["out"], MU),
        ],
        [inp("a"), inp("b"), inp("c")],
        [out("out")],
    );
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();
    (rule, lhs, rhs)
}

fn fs2_coassociativity_rule_named() -> (
    RewriteRule<VecKind, i32, i32>,
    NamedOpenGraph,
    NamedOpenGraph,
) {
    // FS2-style coassociativity orientation for δ:
    // (δ ⊗ id); δ -> (id ⊗ δ); δ  (dually, 1 -> 3 reassociation)
    let lhs = make_named_open_hypergraph(
        [
            w("in", OBJ),
            w("m", OBJ),
            w("a", OBJ),
            w("b", OBJ),
            w("c", OBJ),
        ],
        [
            e("delta_top", ["in"], ["m", "c"], DELTA),
            e("delta_left", ["m"], ["a", "b"], DELTA),
        ],
        [inp("in")],
        [out("a"), out("b"), out("c")],
    );
    let rhs = make_named_open_hypergraph(
        [
            w("in", OBJ),
            w("m", OBJ),
            w("a", OBJ),
            w("b", OBJ),
            w("c", OBJ),
        ],
        [
            e("delta_top", ["in"], ["a", "m"], DELTA),
            e("delta_right", ["m"], ["b", "c"], DELTA),
        ],
        [inp("in")],
        [out("a"), out("b"), out("c")],
    );
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();
    (rule, lhs, rhs)
}

fn ba_distributivity_rule_named() -> (
    RewriteRule<VecKind, i32, i32>,
    NamedOpenGraph,
    NamedOpenGraph,
) {
    // BA interaction forward step: μ;δ expands to the 4-edge crossing shape.
    let lhs = make_named_open_hypergraph(
        [
            w("a", OBJ),
            w("b", OBJ),
            w("m", OBJ),
            w("x", OBJ),
            w("y", OBJ),
        ],
        [
            e("mu", ["a", "b"], ["m"], MU),
            e("delta", ["m"], ["x", "y"], DELTA),
        ],
        [inp("a"), inp("b")],
        [out("x"), out("y")],
    );
    let rhs = make_named_open_hypergraph(
        [
            w("in_l", OBJ),
            w("in_r", OBJ),
            w("l_mid", OBJ),
            w("l_out", OBJ),
            w("r_mid", OBJ),
            w("r_out", OBJ),
            w("out_l", OBJ),
            w("out_r", OBJ),
        ],
        [
            e("delta_l", ["in_l"], ["l_out", "l_mid"], DELTA),
            e("delta_r", ["in_r"], ["r_out", "r_mid"], DELTA),
            e("mu_l", ["l_mid", "r_mid"], ["out_l"], MU),
            e("mu_r", ["l_out", "r_out"], ["out_r"], MU),
        ],
        [inp("in_l"), inp("in_r")],
        [out("out_l"), out("out_r")],
    );
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();
    (rule, lhs, rhs)
}

fn delete_single_edge_rule_named(
    label: i32,
) -> (
    RewriteRule<VecKind, i32, i32>,
    NamedOpenGraph,
    NamedOpenGraph,
) {
    let lhs = make_named_open_hypergraph(
        [w("a", OBJ), w("b", OBJ)],
        [e("drop", ["a"], ["b"], label)],
        [],
        [],
    );
    let rhs_wires: [NamedWire<'static>; 0] = [];
    let rhs_edges: [NamedEdge<'static>; 0] = [];
    let rhs_inputs: [BoundaryPort<'static>; 0] = [];
    let rhs_outputs: [BoundaryPort<'static>; 0] = [];
    let rhs = make_named_open_hypergraph(rhs_wires, rhs_edges, rhs_inputs, rhs_outputs);
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();
    (rule, lhs, rhs)
}

fn injective_maps(domain: usize, target: usize) -> Vec<Vec<usize>> {
    fn backtrack(
        domain: usize,
        target: usize,
        used: &mut [bool],
        current: &mut Vec<usize>,
        out: &mut Vec<Vec<usize>>,
    ) {
        if current.len() == domain {
            out.push(current.clone());
            return;
        }
        for i in 0..target {
            if !used[i] {
                used[i] = true;
                current.push(i);
                backtrack(domain, target, used, current, out);
                current.pop();
                used[i] = false;
            }
        }
    }

    let mut out = Vec::new();
    let mut used = vec![false; target];
    let mut current = Vec::with_capacity(domain);
    backtrack(domain, target, &mut used, &mut current, &mut out);
    out
}

fn isomorphic_with_boundary(
    expected: &OpenHypergraph<VecKind, i32, i32>,
    actual: &OpenHypergraph<VecKind, i32, i32>,
) -> bool {
    if expected.h.w.len() != actual.h.w.len()
        || expected.h.x.len() != actual.h.x.len()
        || expected.s.source() != actual.s.source()
        || expected.t.source() != actual.t.source()
    {
        return false;
    }

    let all_w_maps = injective_maps(expected.h.w.len(), actual.h.w.len());
    let all_x_maps = injective_maps(expected.h.x.len(), actual.h.x.len());

    let expected_s: Vec<Vec<usize>> = expected
        .h
        .s
        .clone()
        .into_iter()
        .map(|f| f.table.0)
        .collect();
    let expected_t: Vec<Vec<usize>> = expected
        .h
        .t
        .clone()
        .into_iter()
        .map(|f| f.table.0)
        .collect();
    let actual_s: Vec<Vec<usize>> = actual.h.s.clone().into_iter().map(|f| f.table.0).collect();
    let actual_t: Vec<Vec<usize>> = actual.h.t.clone().into_iter().map(|f| f.table.0).collect();

    for w_map in &all_w_maps {
        let w = make_map(w_map, actual.h.w.len());

        let wires_ok = (&w >> &actual.h.w)
            .map(|mapped| mapped == expected.h.w)
            .unwrap_or(false);
        if !wires_ok {
            continue;
        }

        let s_ok = (&expected.s >> &w)
            .map(|mapped| mapped == actual.s)
            .unwrap_or(false);
        let t_ok = (&expected.t >> &w)
            .map(|mapped| mapped == actual.t)
            .unwrap_or(false);
        if !(s_ok && t_ok) {
            continue;
        }

        for x_map in &all_x_maps {
            let x = make_map(x_map, actual.h.x.len());
            let ops_ok = (&x >> &actual.h.x)
                .map(|mapped| mapped == expected.h.x)
                .unwrap_or(false);
            if !ops_ok {
                continue;
            }

            let mut incidence_ok = true;
            for (e_exp, &e_act) in x.table.0.iter().enumerate() {
                let exp_src = &expected_s[e_exp];
                let exp_tgt = &expected_t[e_exp];
                let act_src = &actual_s[e_act];
                let act_tgt = &actual_t[e_act];
                if exp_src.len() != act_src.len() || exp_tgt.len() != act_tgt.len() {
                    incidence_ok = false;
                    break;
                }
                for (u_exp, u_act) in exp_src.iter().zip(act_src.iter()) {
                    if w.table.0[*u_exp] != *u_act {
                        incidence_ok = false;
                        break;
                    }
                }
                if !incidence_ok {
                    break;
                }
                for (u_exp, u_act) in exp_tgt.iter().zip(act_tgt.iter()) {
                    if w.table.0[*u_exp] != *u_act {
                        incidence_ok = false;
                        break;
                    }
                }
                if !incidence_ok {
                    break;
                }
            }
            if incidence_ok {
                return true;
            }
        }
    }
    false
}

#[test]
fn apply_rewrite_replaces_matched_edge() {
    let host = make_named_open_hypergraph(
        [w("in", 10), w("out", 11)],
        [e("edge", ["in"], ["out"], 20)],
        [inp("in")],
        [out("out")],
    );
    let lhs = make_named_open_hypergraph(
        [w("in", 10), w("out", 11)],
        [e("edge", ["in"], ["out"], 20)],
        [inp("in")],
        [out("out")],
    );
    let rhs = make_named_open_hypergraph(
        [w("in", 10), w("out", 11)],
        [e("edge_new", ["in"], ["out"], 21)],
        [inp("in")],
        [out("out")],
    );
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();

    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &lhs,
        &host,
        &[("in", "in"), ("out", "out")],
        &[("edge", "edge")],
        &host_ma,
    );

    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&rhs.graph, &out));
}

#[test]
fn apply_rewrite_removes_matched_subgraph_with_empty_rhs() {
    let host = make_named_open_hypergraph(
        [w("a", 10), w("b", 11)],
        [e("edge", ["a"], ["b"], 20)],
        [],
        [],
    );
    let lhs = make_named_open_hypergraph(
        [w("a", 10), w("b", 11)],
        [e("edge", ["a"], ["b"], 20)],
        [],
        [],
    );
    let rhs_wires: [NamedWire<'static>; 0] = [];
    let rhs_edges: [NamedEdge<'static>; 0] = [];
    let rhs_inputs: [BoundaryPort<'static>; 0] = [];
    let rhs_outputs: [BoundaryPort<'static>; 0] = [];
    let rhs = make_named_open_hypergraph(rhs_wires, rhs_edges, rhs_inputs, rhs_outputs);
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();

    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &lhs,
        &host,
        &[("a", "a"), ("b", "b")],
        &[("edge", "edge")],
        &host_ma,
    );

    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&rhs.graph, &out));
}

#[test]
fn apply_rewrite_fs1_reassociates_mu_tree() {
    let (rule, lhs, rhs) = fs1_associativity_rule_named();
    let host = lhs;
    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &host,
        &host,
        &[
            ("a", "a"),
            ("b", "b"),
            ("c", "c"),
            ("m", "m"),
            ("out", "out"),
        ],
        &[("mu_left", "mu_left"), ("mu_top", "mu_top")],
        &host_ma,
    );

    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&rhs.graph, &out));
}

#[test]
fn apply_rewrite_fs2_reassociates_delta_tree() {
    let (rule, lhs, rhs) = fs2_coassociativity_rule_named();
    let host = lhs;
    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &host,
        &host,
        &[("in", "in"), ("m", "m"), ("a", "a"), ("b", "b"), ("c", "c")],
        &[("delta_top", "delta_top"), ("delta_left", "delta_left")],
        &host_ma,
    );

    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&rhs.graph, &out));
}

#[test]
fn apply_rewrite_ba_distributivity_expands_to_expected_shape() {
    let (rule, lhs, rhs) = ba_distributivity_rule_named();
    let host = lhs;
    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &host,
        &host,
        &[("a", "a"), ("b", "b"), ("m", "m"), ("x", "x"), ("y", "y")],
        &[("mu", "mu"), ("delta", "delta")],
        &host_ma,
    );

    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&rhs.graph, &out));
}

#[test]
fn apply_rewrite_delete_edge_in_context_keeps_rest() {
    let (rule, lhs, _rhs_empty) = delete_single_edge_rule_named(40);

    // Host has two edges; rule should delete only "drop_edge" and preserve
    // "keep_edge" and the open boundary.
    let host = make_named_open_hypergraph(
        [w("in", OBJ), w("mid", OBJ), w("out", OBJ)],
        [
            e("keep_edge", ["in"], ["mid"], 30),
            e("drop_edge", ["mid"], ["out"], 40),
        ],
        [inp("in")],
        [out("out")],
    );
    let expected = make_open_hypergraph_named(
        [w("in", OBJ), w("mid", OBJ), w("out", OBJ)],
        [e("keep_edge", ["in"], ["mid"], 30)],
        [inp("in")],
        [out("out")],
    );

    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &lhs,
        &host,
        &[("a", "mid"), ("b", "out")],
        &[("drop", "drop_edge")],
        &host_ma,
    );

    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&expected, &out));
}

#[test]
fn frobenius_semi_algebra_example45_two_disjoint_matches_diverge() {
    // This test encodes the core behavior discussed in Example 45:
    // two disjoint FS-rule applications from the same host produce distinct
    // rewrite results.
    //
    // Reference:
    // Bonchi et al., arXiv:2104.14686v2, Section 5.1, Example 45.
    let rules = frobenius_semi_algebra_rules();

    // Host (2 -> 2): two δ edges feeding two μ edges.
    // We choose wire ordering so each FS rule matches a different pair of
    // edges (disjoint edge sets), mirroring the setup in Example 45.
    let host = frobenius_example45_host_named();
    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();

    // FS3 match described declaratively by named wire/edge correspondences.
    let m_fs3 = named_match_witness(
        &rules.fs3_lhs,
        &host,
        &[
            ("a", "in_l"),
            ("b", "r_mid"),
            ("m", "l_mid"),
            ("x", "l_out"),
            ("y", "out_l"),
        ],
        &[("delta", "delta_l"), ("mu", "mu_l")],
        &host_ma,
    );
    let h1 = apply_rewrite(&rules.fs3, &host_ma, &m_fs3).unwrap();

    // FS4 match described declaratively by named wire/edge correspondences.
    let m_fs4 = named_match_witness(
        &rules.fs4_lhs,
        &host,
        &[
            ("a", "l_out"),
            ("b", "in_r"),
            ("m", "r_out"),
            ("x", "out_r"),
            ("y", "r_mid"),
        ],
        &[("delta", "delta_r"), ("mu", "mu_r")],
        &host_ma,
    );
    let h2 = apply_rewrite(&rules.fs4, &host_ma, &m_fs4).unwrap();

    let expected_h1 = expected_example45_after_fs3();
    let expected_h2 = expected_example45_after_fs4();

    assert!(isomorphic_with_boundary(&expected_h1, &h1));
    assert!(isomorphic_with_boundary(&expected_h2, &h2));
    assert!(!isomorphic_with_boundary(&h1, &h2));
}

#[test]
fn circuit_apply_rewrite_and_one_simplifies_to_identity() {
    // x AND 1 -> x
    // Host circuit: x --AND(1)--> y
    // Expected circuit: identity on x (input x, output x).
    let r = circuit_rule_and_one();

    let host = make_named_open_hypergraph(
        [cw("x"), cw("one"), cw("y")],
        [g_const1("k1", "one"), g_and("and", "x", "one", "y")],
        [inp("x")],
        [out("y")],
    );
    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &r.lhs,
        &host,
        &[("x", "x"), ("one", "one"), ("y", "y")],
        &[("k1", "k1"), ("and", "and")],
        &host_ma,
    );

    let out = apply_rewrite(&r.rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&r.rhs.graph, &out));
}

#[test]
fn circuit_apply_rewrite_xor_zero_simplifies_to_identity() {
    // x XOR 0 -> x
    // Host circuit: x --XOR(0)--> y
    // Expected circuit: identity on x (input x, output x).
    let r = circuit_rule_xor_zero();

    let host = make_named_open_hypergraph(
        [cw("x"), cw("zero"), cw("y")],
        [g_const0("k0", "zero"), g_xor("xor", "x", "zero", "y")],
        [inp("x")],
        [out("y")],
    );
    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &r.lhs,
        &host,
        &[("x", "x"), ("zero", "zero"), ("y", "y")],
        &[("k0", "k0"), ("xor", "xor")],
        &host_ma,
    );

    let out = apply_rewrite(&r.rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&r.rhs.graph, &out));
}

#[test]
fn circuit_apply_rewrite_double_not_eliminates() {
    // NOT(NOT(x)) -> x
    // Host circuit: x -> NOT -> NOT -> y
    // Expected circuit: identity on x (input x, output x).
    let r = circuit_rule_double_not();

    let host = make_named_open_hypergraph(
        [cw("x"), cw("m"), cw("y")],
        [g_not("n1", "x", "m"), g_not("n2", "m", "y")],
        [inp("x")],
        [out("y")],
    );
    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &r.lhs,
        &host,
        &[("x", "x"), ("m", "m"), ("y", "y")],
        &[("n1", "n1"), ("n2", "n2")],
        &host_ma,
    );

    let out = apply_rewrite(&r.rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&r.rhs.graph, &out));
}

#[test]
fn circuit_apply_rewrite_and_associativity_reassociates() {
    // (a AND b) AND c -> a AND (b AND c)
    // Host circuit: left-associated AND tree with three inputs.
    // Expected circuit: right-associated AND tree with same 3-input/1-output behavior.
    let lhs = make_named_open_hypergraph(
        [cw("a"), cw("b"), cw("c"), cw("m"), cw("out")],
        [g_and("and1", "a", "b", "m"), g_and("and2", "m", "c", "out")],
        [inp("a"), inp("b"), inp("c")],
        [out("out")],
    );
    let rhs = make_named_open_hypergraph(
        [cw("a"), cw("b"), cw("c"), cw("m"), cw("out")],
        [g_and("and1", "b", "c", "m"), g_and("and2", "a", "m", "out")],
        [inp("a"), inp("b"), inp("c")],
        [out("out")],
    );
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();

    let host = make_named_open_hypergraph(
        [cw("a"), cw("b"), cw("c"), cw("m"), cw("out")],
        [g_and("and1", "a", "b", "m"), g_and("and2", "m", "c", "out")],
        [inp("a"), inp("b"), inp("c")],
        [out("out")],
    );
    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &lhs,
        &host,
        &[
            ("a", "a"),
            ("b", "b"),
            ("c", "c"),
            ("m", "m"),
            ("out", "out"),
        ],
        &[("and1", "and1"), ("and2", "and2")],
        &host_ma,
    );

    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&rhs.graph, &out));
}

#[test]
fn circuit_apply_rewrite_and_one_in_context() {
    // Rule: x AND 1 -> x
    // Host circuit:
    //   x --AND(1)--> y --NOT--> out
    // Expected circuit:
    //   x -----------NOT-------> out
    // (the matched redex is internal; NOT context is preserved)
    let lhs = make_named_open_hypergraph(
        [cw("x"), cw("one"), cw("y")],
        [g_const1("k1", "one"), g_and("and", "x", "one", "y")],
        [inp("x")],
        [out("y")],
    );
    let rhs = make_named_open_hypergraph([cw("x")], [], [inp("x")], [out("x")]);
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();

    let host = make_named_open_hypergraph(
        [cw("x"), cw("one"), cw("y"), cw("out")],
        [
            g_const1("k1", "one"),
            g_and("and", "x", "one", "y"),
            g_not("n", "y", "out"),
        ],
        [inp("x")],
        [out("out")],
    );
    let expected = make_open_hypergraph_named(
        [cw("x"), cw("out")],
        [g_not("n", "x", "out")],
        [inp("x")],
        [out("out")],
    );

    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &lhs,
        &host,
        &[("x", "x"), ("one", "one"), ("y", "y")],
        &[("k1", "k1"), ("and", "and")],
        &host_ma,
    );

    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&expected, &out));
}

#[test]
fn circuit_apply_rewrite_xor_zero_in_context() {
    // Rule: x XOR 0 -> x
    // Host circuit:
    //   x --XOR(0)--> y --AND(c)--> out
    // Expected circuit:
    //   x ------------AND(c)------> out
    // (the matched redex is internal; downstream AND context is preserved)
    let lhs = make_named_open_hypergraph(
        [cw("x"), cw("zero"), cw("y")],
        [g_const0("k0", "zero"), g_xor("xor", "x", "zero", "y")],
        [inp("x")],
        [out("y")],
    );
    let rhs = make_named_open_hypergraph([cw("x")], [], [inp("x")], [out("x")]);
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();

    let host = make_named_open_hypergraph(
        [cw("x"), cw("zero"), cw("y"), cw("c"), cw("out")],
        [
            g_const0("k0", "zero"),
            g_xor("xor", "x", "zero", "y"),
            g_and("and", "y", "c", "out"),
        ],
        [inp("x"), inp("c")],
        [out("out")],
    );
    let expected = make_open_hypergraph_named(
        [cw("x"), cw("c"), cw("out")],
        [g_and("and", "x", "c", "out")],
        [inp("x"), inp("c")],
        [out("out")],
    );

    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &lhs,
        &host,
        &[("x", "x"), ("zero", "zero"), ("y", "y")],
        &[("k0", "k0"), ("xor", "xor")],
        &host_ma,
    );

    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&expected, &out));
}

#[test]
fn circuit_apply_rewrite_multiple_rules_pipeline_matches_expected_stubs() {
    // Host circuit pipeline:
    //   x --XOR(0)--> u --AND(1)--> v --NOT--> w --NOT--> out
    //
    // Apply rules in sequence:
    //   (1) XOR-zero, (2) AND-one, (3) double-NOT
    // and check against explicit expected stubs after each stage.
    let r_xor0 = circuit_rule_xor_zero();
    let r_and1 = circuit_rule_and_one();
    let r_dnot = circuit_rule_double_not();

    let host0 = make_named_open_hypergraph(
        [
            cw("x"),
            cw("zero"),
            cw("u"),
            cw("one"),
            cw("v"),
            cw("w"),
            cw("out"),
        ],
        [
            g_const0("k0", "zero"),
            g_xor("xor", "x", "zero", "u"),
            g_const1("k1", "one"),
            g_and("and", "u", "one", "v"),
            g_not("n1", "v", "w"),
            g_not("n2", "w", "out"),
        ],
        [inp("x")],
        [out("out")],
    );

    let expected1 = make_open_hypergraph_named(
        [cw("x"), cw("one"), cw("v"), cw("w"), cw("out")],
        [
            g_const1("k1", "one"),
            g_and("and", "x", "one", "v"),
            g_not("n1", "v", "w"),
            g_not("n2", "w", "out"),
        ],
        [inp("x")],
        [out("out")],
    );

    let host0_ma = MonogamousAcyclicHost::new(&host0.graph).unwrap();
    let m0 = named_match_witness(
        &r_xor0.lhs,
        &host0,
        &[("x", "x"), ("zero", "zero"), ("y", "u")],
        &[("k0", "k0"), ("xor", "xor")],
        &host0_ma,
    );
    let out1 = apply_rewrite(&r_xor0.rule, &host0_ma, &m0).unwrap();
    assert!(isomorphic_with_boundary(&expected1, &out1));

    let host1 = make_named_open_hypergraph(
        [cw("x"), cw("one"), cw("v"), cw("w"), cw("out")],
        [
            g_const1("k1", "one"),
            g_and("and", "x", "one", "v"),
            g_not("n1", "v", "w"),
            g_not("n2", "w", "out"),
        ],
        [inp("x")],
        [out("out")],
    );
    let expected2 = make_open_hypergraph_named(
        [cw("x"), cw("w"), cw("out")],
        [g_not("n1", "x", "w"), g_not("n2", "w", "out")],
        [inp("x")],
        [out("out")],
    );

    let host1_ma = MonogamousAcyclicHost::new(&host1.graph).unwrap();
    let m1 = named_match_witness(
        &r_and1.lhs,
        &host1,
        &[("x", "x"), ("one", "one"), ("y", "v")],
        &[("k1", "k1"), ("and", "and")],
        &host1_ma,
    );
    let out2 = apply_rewrite(&r_and1.rule, &host1_ma, &m1).unwrap();
    assert!(isomorphic_with_boundary(&expected2, &out2));

    let host2 = make_named_open_hypergraph(
        [cw("x"), cw("w"), cw("out")],
        [g_not("n1", "x", "w"), g_not("n2", "w", "out")],
        [inp("x")],
        [out("out")],
    );
    let expected3 = make_open_hypergraph_named([cw("x")], [], [inp("x")], [out("x")]);

    let host2_ma = MonogamousAcyclicHost::new(&host2.graph).unwrap();
    let m2 = named_match_witness(
        &r_dnot.lhs,
        &host2,
        &[("x", "x"), ("m", "w"), ("y", "out")],
        &[("n1", "n1"), ("n2", "n2")],
        &host2_ma,
    );
    let out3 = apply_rewrite(&r_dnot.rule, &host2_ma, &m2).unwrap();
    assert!(isomorphic_with_boundary(&expected3, &out3));
}

#[test]
fn program_apply_rewrite_dead_code_elimination_in_context() {
    // Dead code elimination rule (Markov-style):
    //   assign(u -> v); discard(v) -> discard(u)
    //
    // Host program graph (context + dead assignment):
    //   live: out = add(a, b)
    //   dead: t1 = assign(d); discard(t1)
    //
    // Expected optimized graph:
    //   keep live computation and replace assign+discard by discard(d).
    let (rule, lhs, _rhs) = program_rule_assign_then_discard();

    let host = make_named_open_hypergraph(
        [pw("a"), pw("b"), pw("out"), pw("d"), pw("t1")],
        [
            p_add("live_add", "a", "b", "out"),
            p_assign("assign", "d", "t1"),
            p_discard("discard_v", "t1"),
        ],
        [inp("a"), inp("b")],
        [out("out")],
    );
    let expected = make_open_hypergraph_named(
        [pw("a"), pw("b"), pw("out"), pw("d")],
        [
            p_add("live_add", "a", "b", "out"),
            p_discard("discard_u", "d"),
        ],
        [inp("a"), inp("b")],
        [out("out")],
    );

    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &lhs,
        &host,
        &[("u", "d"), ("v", "t1")],
        &[("assign", "assign"), ("discard_v", "discard_v")],
        &host_ma,
    );
    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&expected, &out));
}

#[test]
fn program_apply_rewrite_dead_code_elimination_multiple_steps() {
    // Apply the same dead-elimination rule repeatedly through a dead chain:
    //
    // start:
    //   t1 = assign(d)
    //   t2 = assign(t1)
    //   discard(t2)
    //
    // step 1 (on assign(t1->t2);discard(t2)):
    //   t1 = assign(d)
    //   discard(t1)
    //
    // step 2 (on assign(d->t1);discard(t1)):
    //   discard(d)
    //
    // The live context `out = add(a,b)` is preserved across both steps.
    let (rule, lhs, _rhs) = program_rule_assign_then_discard();

    let host0 = make_named_open_hypergraph(
        [pw("a"), pw("b"), pw("out"), pw("d"), pw("t1"), pw("t2")],
        [
            p_add("live_add", "a", "b", "out"),
            p_assign("assign_1", "d", "t1"),
            p_assign("assign", "t1", "t2"),
            p_discard("discard_v", "t2"),
        ],
        [inp("a"), inp("b")],
        [out("out")],
    );
    let expected1 = make_open_hypergraph_named(
        [pw("a"), pw("b"), pw("out"), pw("d"), pw("t1")],
        [
            p_add("live_add", "a", "b", "out"),
            p_assign("assign_1", "d", "t1"),
            p_discard("discard_u", "t1"),
        ],
        [inp("a"), inp("b")],
        [out("out")],
    );

    let host0_ma = MonogamousAcyclicHost::new(&host0.graph).unwrap();
    let m0 = named_match_witness(
        &lhs,
        &host0,
        &[("u", "t1"), ("v", "t2")],
        &[("assign", "assign"), ("discard_v", "discard_v")],
        &host0_ma,
    );
    let out1 = apply_rewrite(&rule, &host0_ma, &m0).unwrap();
    assert!(isomorphic_with_boundary(&expected1, &out1));

    let host1 = make_named_open_hypergraph(
        [pw("a"), pw("b"), pw("out"), pw("d"), pw("t1")],
        [
            p_add("live_add", "a", "b", "out"),
            p_assign("assign", "d", "t1"),
            p_discard("discard_v", "t1"),
        ],
        [inp("a"), inp("b")],
        [out("out")],
    );
    let expected2 = make_open_hypergraph_named(
        [pw("a"), pw("b"), pw("out"), pw("d")],
        [
            p_add("live_add", "a", "b", "out"),
            p_discard("discard_u", "d"),
        ],
        [inp("a"), inp("b")],
        [out("out")],
    );

    let host1_ma = MonogamousAcyclicHost::new(&host1.graph).unwrap();
    let m1 = named_match_witness(
        &lhs,
        &host1,
        &[("u", "d"), ("v", "t1")],
        &[("assign", "assign"), ("discard_v", "discard_v")],
        &host1_ma,
    );
    let out2 = apply_rewrite(&rule, &host1_ma, &m1).unwrap();
    assert!(isomorphic_with_boundary(&expected2, &out2));
}

#[test]
fn program_apply_rewrite_constant_propagation_in_context() {
    // Constant propagation rule:
    //   c = const1();  y = add(c, x)
    //   --------------------------------
    //   y = add_const1(x)
    //
    // Host program graph (larger context):
    //   c = const1()
    //   y = add(c, x)
    //   z = assign(y)        // downstream context must stay connected
    //
    // Expected optimized graph:
    //   y = add_const1(x)
    //   z = assign(y)
    let lhs = make_named_open_hypergraph(
        [pw("c"), pw("x"), pw("y")],
        [p_const1("k1", "c"), p_add("add", "c", "x", "y")],
        [inp("x")],
        [out("y")],
    );
    let rhs = make_named_open_hypergraph(
        [pw("x"), pw("y")],
        [p_add_const1("addc1", "x", "y")],
        [inp("x")],
        [out("y")],
    );
    let rule = RewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();

    let host = make_named_open_hypergraph(
        [pw("c"), pw("x"), pw("y"), pw("z")],
        [
            p_const1("k1", "c"),
            p_add("add", "c", "x", "y"),
            p_assign("use_y", "y", "z"),
        ],
        [inp("x")],
        [out("z")],
    );
    let expected = make_open_hypergraph_named(
        [pw("x"), pw("y"), pw("z")],
        [p_add_const1("addc1", "x", "y"), p_assign("use_y", "y", "z")],
        [inp("x")],
        [out("z")],
    );

    let host_ma = MonogamousAcyclicHost::new(&host.graph).unwrap();
    let m = named_match_witness(
        &lhs,
        &host,
        &[("c", "c"), ("x", "x"), ("y", "y")],
        &[("k1", "k1"), ("add", "add")],
        &host_ma,
    );
    let out = apply_rewrite(&rule, &host_ma, &m).unwrap();
    assert!(isomorphic_with_boundary(&expected, &out));
}
