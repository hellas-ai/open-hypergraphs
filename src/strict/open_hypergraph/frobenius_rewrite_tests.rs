use crate::category::Arrow;
use crate::strict::open_hypergraph::{
    apply_frobenius_rewrite, try_apply_frobenius_rewrite, FrobeniusRewriteApplyError,
    FrobeniusRewriteRule,
};

use super::test_utils::{
    e, inp, isomorphic_with_boundary, make_named_open_hypergraph, named_frobenius_match_witness,
    out, w, DELTA, MU, OBJ,
};

#[test]
fn frobenius_apply_rewrite_enumerates_multiple_complements() {
    let lhs = make_named_open_hypergraph([w("m", OBJ)], [], [inp("m")], [out("m")]);
    let rhs = make_named_open_hypergraph(
        [w("a", OBJ), w("b", OBJ), w("c", OBJ)],
        [e("f", ["a"], ["b"], MU), e("g", ["b"], ["c"], DELTA)],
        [inp("a")],
        [out("c")],
    );
    let rule = FrobeniusRewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();

    let host = make_named_open_hypergraph(
        [
            w("w1", OBJ),
            w("w2", OBJ),
            w("w3", OBJ),
            w("w4", OBJ),
            w("w5", OBJ),
        ],
        [
            e("f0", ["w1"], ["w2"], MU),
            e("g0", ["w2"], ["w3"], DELTA),
            e("f1", ["w1"], ["w4"], MU),
            e("g1", ["w4"], ["w5"], DELTA),
        ],
        [],
        [],
    );

    let m = named_frobenius_match_witness(&rule, &lhs, &host, &[("m", "w4")], &[], &host.graph);

    let out = apply_frobenius_rewrite(&m);
    assert_eq!(out.len(), 5);
    assert!(out
        .iter()
        .all(|graph| graph.s.source() == 0 && graph.t.source() == 0));
}

#[test]
fn frobenius_apply_rewrite_enumerates_distinct_complements() {
    let lhs = make_named_open_hypergraph([w("m", OBJ)], [], [inp("m")], [out("m")]);
    let rhs = make_named_open_hypergraph(
        [w("a", OBJ), w("b", OBJ), w("c", OBJ)],
        [e("f", ["a"], ["b"], MU), e("g", ["b"], ["c"], DELTA)],
        [inp("a")],
        [out("c")],
    );
    let rule = FrobeniusRewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();

    let host = make_named_open_hypergraph(
        [
            w("w1", OBJ),
            w("w2", OBJ),
            w("w3", OBJ),
            w("w4", OBJ),
            w("w5", OBJ),
        ],
        [
            e("f0", ["w1"], ["w2"], MU),
            e("g0", ["w2"], ["w3"], DELTA),
            e("f1", ["w1"], ["w4"], MU),
            e("g1", ["w4"], ["w5"], DELTA),
        ],
        [],
        [],
    );

    let m = named_frobenius_match_witness(&rule, &lhs, &host, &[("m", "w4")], &[], &host.graph);
    let out = apply_frobenius_rewrite(&m);

    for i in 0..out.len() {
        for j in (i + 1)..out.len() {
            assert!(
                !isomorphic_with_boundary(&out[i], &out[j]),
                "frobenius complements {i} and {j} should be distinct",
            );
        }
    }
}

#[test]
fn frobenius_rewrite_rejects_match_on_host_boundary_for_non_boundary_redex_wire() {
    let lhs = make_named_open_hypergraph([w("m", OBJ)], [], [], []);
    let rhs = make_named_open_hypergraph([], [], [], []);
    let rule = FrobeniusRewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();

    let host = make_named_open_hypergraph([w("w", OBJ)], [], [inp("w")], [out("w")]);
    let m = named_frobenius_match_witness(&rule, &lhs, &host, &[("m", "w")], &[], &host.graph);

    assert!(apply_frobenius_rewrite(&m).is_empty());
}

#[test]
fn frobenius_rewrite_reports_dangling_reason() {
    let lhs = make_named_open_hypergraph([w("m", OBJ)], [], [], []);
    let rhs = make_named_open_hypergraph([], [], [], []);
    let rule = FrobeniusRewriteRule::new(lhs.graph.clone(), rhs.graph.clone()).unwrap();

    let host = make_named_open_hypergraph([w("w", OBJ)], [], [inp("w")], [out("w")]);
    let m = named_frobenius_match_witness(&rule, &lhs, &host, &[("m", "w")], &[], &host.graph);

    let err = try_apply_frobenius_rewrite(&m).unwrap_err();
    assert_eq!(err, FrobeniusRewriteApplyError::DanglingConditionFailed);
}
