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

fn make_open_hypergraph(
    sources: &[Vec<usize>],
    targets: &[Vec<usize>],
    w_labels: Vec<i32>,
    x_labels: Vec<i32>,
    s_map: &[usize],
    t_map: &[usize],
) -> OpenHypergraph<VecKind, i32, i32> {
    let h = make_hypergraph(sources, targets, w_labels, x_labels);
    let s = FiniteFunction::new(VecArray(s_map.to_vec()), h.w.len()).unwrap();
    let t = FiniteFunction::new(VecArray(t_map.to_vec()), h.w.len()).unwrap();
    OpenHypergraph::new(s, t, h).unwrap()
}

fn make_open_hypergraph_empty_boundary(
    sources: &[Vec<usize>],
    targets: &[Vec<usize>],
    w_labels: Vec<i32>,
    x_labels: Vec<i32>,
) -> OpenHypergraph<VecKind, i32, i32> {
    let h = make_hypergraph(sources, targets, w_labels, x_labels);
    let s = FiniteFunction::initial(h.w.len());
    let t = FiniteFunction::initial(h.w.len());
    OpenHypergraph::new(s, t, h).unwrap()
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
