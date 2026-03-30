use crate::array::vec::{VecArray, VecKind};
use crate::category::Arrow;
use crate::finite_function::FiniteFunction;
use crate::indexed_coproduct::IndexedCoproduct;
use crate::semifinite::SemifiniteFunction;
use crate::strict::hypergraph::Hypergraph;
use crate::strict::open_hypergraph::{
    FrobeniusRewriteMatch, FrobeniusRewriteRule, OpenHypergraph, SmcRewriteMatch, SmcRewriteRule,
};
use std::collections::HashMap;

pub(super) const OBJ: i32 = 0;
pub(super) const MU: i32 = 1;
pub(super) const DELTA: i32 = 2;

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
pub(super) struct NamedEdge<'a> {
    logical_name: &'a str,
    sources: Vec<&'a str>,
    targets: Vec<&'a str>,
    label: i32,
}

#[derive(Clone, Copy)]
pub(super) struct NamedWire<'a> {
    logical_name: &'a str,
    label: i32,
}

#[derive(Clone, Copy)]
pub(super) struct BoundaryPort<'a> {
    logical_name: &'a str,
}

pub(super) fn w<'a>(logical_name: &'a str, label: i32) -> NamedWire<'a> {
    NamedWire {
        logical_name,
        label,
    }
}

pub(super) fn inp<'a>(logical_name: &'a str) -> BoundaryPort<'a> {
    BoundaryPort { logical_name }
}

pub(super) fn out<'a>(logical_name: &'a str) -> BoundaryPort<'a> {
    BoundaryPort { logical_name }
}

pub(super) fn e<'a, const S: usize, const T: usize>(
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

pub(super) fn make_open_hypergraph_named<'a, W, E, I, O>(
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

pub(super) fn make_map(indices: &[usize], target: usize) -> FiniteFunction<VecKind> {
    FiniteFunction::new(VecArray(indices.to_vec()), target).unwrap()
}

pub(super) struct NamedOpenGraph {
    pub(super) graph: OpenHypergraph<VecKind, i32, i32>,
    wire_ix: HashMap<String, usize>,
    edge_ix: HashMap<String, usize>,
}

pub(super) fn make_named_open_hypergraph<'a, W, E, I, O>(
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

pub(super) fn named_match_witness<'a>(
    rule: &'a SmcRewriteRule<VecKind, i32, i32>,
    lhs: &NamedOpenGraph,
    host: &NamedOpenGraph,
    wire_pairs: &[(&str, &str)],
    edge_pairs: &[(&str, &str)],
    host_graph: &'a OpenHypergraph<VecKind, i32, i32>,
) -> SmcRewriteMatch<'a, VecKind, i32, i32> {
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
    SmcRewriteMatch::new(rule, host_graph, w, x).unwrap()
}

pub(super) fn named_frobenius_match_witness<'a>(
    rule: &'a FrobeniusRewriteRule<i32, i32>,
    lhs: &NamedOpenGraph,
    host: &NamedOpenGraph,
    wire_pairs: &[(&str, &str)],
    edge_pairs: &[(&str, &str)],
    host_graph: &'a OpenHypergraph<VecKind, i32, i32>,
) -> FrobeniusRewriteMatch<'a, i32, i32> {
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
    FrobeniusRewriteMatch::new(rule, host_graph, w, x).unwrap()
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

pub(super) fn isomorphic_with_boundary(
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
