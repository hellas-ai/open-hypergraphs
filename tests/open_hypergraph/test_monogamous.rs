use open_hypergraphs::array::vec::{VecArray, VecKind};
use open_hypergraphs::category::{Arrow, SymmetricMonoidal};
use open_hypergraphs::finite_function::FiniteFunction;
use open_hypergraphs::semifinite::SemifiniteFunction;
use open_hypergraphs::strict::hypergraph::Hypergraph;
use open_hypergraphs::strict::open_hypergraph::OpenHypergraph;

use crate::theory::meaningless::{Arr, Obj};

#[test]
fn test_in_out_nodes_are_images() {
    let w = SemifiniteFunction(VecArray(vec![0i8, 1i8, 2i8]));
    let h: Hypergraph<VecKind, Obj, Arr> = Hypergraph::discrete(w);
    let s = FiniteFunction::new(VecArray(vec![0, 2, 2]), 3).unwrap();
    let t = FiniteFunction::new(VecArray(vec![1, 1]), 3).unwrap();

    let g = OpenHypergraph::new(s, t, h).unwrap();

    assert_eq!(g.in_nodes(), vec![0, 2]);
    assert_eq!(g.out_nodes(), vec![1]);
}

#[test]
fn test_is_monogamous_true() {
    let f: OpenHypergraph<VecKind, Obj, Arr> = OpenHypergraph::singleton(
        0u8,
        SemifiniteFunction(VecArray(vec![0i8])),
        SemifiniteFunction(VecArray(vec![1i8])),
    );

    assert!(f.is_monogamous());
}

#[test]
fn test_is_monogamous_false_non_mono_interface() {
    let h = Hypergraph::discrete(SemifiniteFunction(VecArray(vec![0i8, 1i8])));
    let s = FiniteFunction::new(VecArray(vec![0, 0]), 2).unwrap();
    let t = FiniteFunction::new(VecArray(vec![1]), 2).unwrap();

    let g: OpenHypergraph<VecKind, Obj, Arr> = OpenHypergraph::new(s, t, h).unwrap();

    assert!(!g.is_monogamous());
}

#[test]
fn test_is_monogamous_false_boundary_has_degree() {
    let base: OpenHypergraph<VecKind, Obj, Arr> = OpenHypergraph::singleton(
        0u8,
        SemifiniteFunction(VecArray(vec![0i8])),
        SemifiniteFunction(VecArray(vec![1i8])),
    );
    let s = FiniteFunction::new(VecArray(vec![0, 1]), 2).unwrap();
    let t = FiniteFunction::new(VecArray(vec![1]), 2).unwrap();

    let g: OpenHypergraph<VecKind, Obj, Arr> = OpenHypergraph::new(s, t, base.h).unwrap();

    assert!(!g.is_monogamous());
}

#[test]
fn test_identity_is_monogamous() {
    let h: OpenHypergraph<VecKind, Obj, Arr> =
        OpenHypergraph::identity(SemifiniteFunction(VecArray(vec![0i8, 1i8])));

    assert!(h.is_monogamous());
}

#[test]
fn test_symmetry_is_monogamous() {
    let h: OpenHypergraph<VecKind, Obj, Arr> = OpenHypergraph::twist(
        SemifiniteFunction(VecArray(vec![0i8])),
        SemifiniteFunction(VecArray(vec![1i8])),
    );

    assert!(h.is_monogamous());
}

#[test]
fn test_composition_preserves_monogamous() {
    let f: OpenHypergraph<VecKind, Obj, Arr> =
        OpenHypergraph::identity(SemifiniteFunction(VecArray(vec![0i8, 1i8])));
    let g: OpenHypergraph<VecKind, Obj, Arr> = OpenHypergraph::twist(
        SemifiniteFunction(VecArray(vec![0i8])),
        SemifiniteFunction(VecArray(vec![1i8])),
    );

    let composed = Arrow::compose(&f, &g).expect("matching boundaries");

    assert!(composed.is_monogamous());
}
