use open_hypergraphs::array::vec::*;
use open_hypergraphs::finite_function::*;
use open_hypergraphs::indexed_coproduct::*;
use open_hypergraphs::layer::{converse, indegree, layer, operation_adjacency};
use open_hypergraphs::open_hypergraph::*;
use open_hypergraphs::semifinite::*;

#[derive(Clone, PartialEq, Debug)]
pub enum Arr {
    F,
    G,
    H,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Obj {
    A,
}

////////////////////////////////////////
// Main methods

#[test]
fn test_layer_singleton() {
    use Arr::*;
    use Obj::*;

    let x = SemifiniteFunction(VecArray(vec![A, A]));
    let y = SemifiniteFunction(VecArray(vec![A]));
    let f = OpenHypergraph::<VecKind, _, _>::singleton(F, x.clone(), y.clone());

    let (layer, _) = layer::<VecKind, Obj, Arr>(&f);
    assert_eq!(layer.table, VecArray(vec![0]));
}

#[test]
fn test_layer_f_f_op() {
    use Arr::*;
    use Obj::*;

    let x = SemifiniteFunction(VecArray(vec![A, A]));
    let y = SemifiniteFunction(VecArray(vec![A]));
    let f = OpenHypergraph::<VecKind, _, _>::singleton(F, x.clone(), y.clone());
    let f_op = OpenHypergraph::<VecKind, _, _>::singleton(F, y, x);

    let (layer, _) = layer::<VecKind, Obj, Arr>(&(&f >> &f_op).unwrap());
    assert_eq!(layer.table, VecArray(vec![0, 1]));
}

#[test]
fn test_layer_g_tensor_g_f() {
    use Arr::*;
    use Obj::*;

    let x = SemifiniteFunction(VecArray(vec![A, A]));
    let y = SemifiniteFunction(VecArray(vec![A]));

    let f = OpenHypergraph::<VecKind, _, _>::singleton(F, x.clone(), y.clone());
    let g = OpenHypergraph::singleton(G, y.clone(), y.clone());
    let h = (&(&g | &g) >> &f).unwrap();

    let (layer, _) = layer::<VecKind, Obj, Arr>(&h);
    assert_eq!(layer.table, VecArray(vec![0, 0, 1]));
}

#[test]
fn test_layer_fh_tensor_gh() {
    use Arr::*;
    use Obj::*;

    let x = SemifiniteFunction(VecArray(vec![A]));

    let f = OpenHypergraph::singleton(F, x.clone(), x.clone());
    let g = OpenHypergraph::singleton(G, x.clone(), x.clone());
    let h = OpenHypergraph::singleton(H, x.clone(), x.clone());

    let z = &(&f >> &h).unwrap() | &(&g >> &h).unwrap();

    let (layer, unvisited) = layer::<VecKind, Obj, Arr>(&z);
    assert_eq!(layer.table, VecArray(vec![0, 1, 0, 1]));
    assert!(!unvisited.0.iter().any(|x| *x == 1));
}

#[test]
fn test_layer_f_op_f() {
    use Arr::*;
    use Obj::*;

    let x = SemifiniteFunction(VecArray(vec![A, A]));
    let y = SemifiniteFunction(VecArray(vec![A]));
    let f = OpenHypergraph::<VecKind, _, _>::singleton(F, x.clone(), y.clone());
    let f_op = OpenHypergraph::<VecKind, _, _>::singleton(F, y, x);

    let (layer, _) = layer::<VecKind, Obj, Arr>(&(&f_op >> &f).unwrap());
    assert_eq!(layer.table, VecArray(vec![0, 1]));
}

// TODO: test a non-monogamous-acyclic diagram

#[test]
fn test_indegree() {
    use Arr::*;
    use Obj::*;

    let x = SemifiniteFunction(VecArray(vec![A, A]));
    let y = SemifiniteFunction(VecArray(vec![A]));

    // There are no operations adjacent to f
    //      ┌───┐
    // ●────│   │
    //      │ f │────●
    // ●────│   │
    //      └───┘
    println!("singleton");
    let f = OpenHypergraph::<VecKind, _, _>::singleton(F, x.clone(), y.clone());
    let a = operation_adjacency(&f);
    let i = indegree(&a);
    assert_eq!(i.table, VecArray(vec![0]));

    // both copies of g are adjacent to f, and f is adjacent to nothing
    //      ┌───┐
    // ●────│ g │────┐    ┌───┐
    //      └───┘    ●────│   │
    //                    │ f │────●
    //      ┌───┐    ●────│   │
    // ●────│ g │────┘    └───┘
    //      └───┘
    println!("(g | g) >> f");
    let g = OpenHypergraph::singleton(G, y.clone(), y.clone());
    let h = (&(&g | &g) >> &f).unwrap();
    let a = operation_adjacency(&h);
    let i = indegree(&a);
    assert_eq!(i.table, VecArray(vec![0, 0, 2]));

    // the lhs f is adjacent to the rhs, but not vice-versa.
    //
    //      ┌───┐     ┌───┐
    // ●────│   │     │   │────●
    //      │ f │──●──│ f │
    // ●────│   │     │   │────●
    //      └───┘     └───┘
    //
    println!("f >> f_op");
    let f_op = OpenHypergraph::singleton(F, y.clone(), x.clone());
    let h = (&f >> &f_op).unwrap();
    let a = operation_adjacency(&h);
    let i = indegree(&a);
    assert_eq!(i.table, VecArray(vec![0, 1]));

    // LHS f is adjacent to RHS f in *two distinct ways*!
    //    ┌───┐         ┌───┐
    //    │   │────●────│   │
    // ●──│ f │         │ f │──●
    //    │   │────●────│   │
    //    └───┘         └───┘
    println!("f_op >> f");
    let h = (&f_op >> &f).unwrap();
    let a = operation_adjacency(&h);
    let i = indegree(&a);
    assert_eq!(i.table, VecArray(vec![0, 2]));
}

#[test]
fn test_converse() {
    let sources = SemifiniteFunction::<VecKind, usize>(VecArray(vec![2, 0, 2]));
    let values = FiniteFunction::new(VecArray(vec![4, 4, 0, 1]), 5).unwrap();
    let c = IndexedCoproduct::from_semifinite(sources, values).unwrap();

    let actual = converse(&c);

    let sources = SemifiniteFunction::<VecKind, usize>(VecArray(vec![1, 1, 0, 0, 2]));
    let values = FiniteFunction::new(VecArray(vec![2, 2, 0, 0]), 3).unwrap();
    let expected = IndexedCoproduct::from_semifinite(sources, values).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_operation_adjacency() {
    use Arr::*;
    use Obj::*;

    let x = SemifiniteFunction(VecArray(vec![A, A]));
    let y = SemifiniteFunction(VecArray(vec![A]));

    // There are no operations adjacent to f
    //      ┌───┐
    // ●────│   │
    //      │ f │────●
    // ●────│   │
    //      └───┘
    let f = OpenHypergraph::<VecKind, _, _>::singleton(F, x.clone(), y.clone());
    let result = operation_adjacency::<VecKind, Obj, Arr>(&f);
    assert_eq!(result.sources.table, VecArray(vec![0]));
    assert_eq!(result.values.table, VecArray(vec![]));

    // both copies of g are adjacent to f, and f is adjacent to nothing
    //      ┌───┐
    // ●────│ g │────┐    ┌───┐
    //      └───┘    ●────│   │
    //                    │ f │────●
    //      ┌───┐    ●────│   │
    // ●────│ g │────┘    └───┘
    //      └───┘
    let g = OpenHypergraph::singleton(G, y.clone(), y.clone());
    let h = (&(&g | &g) >> &f).unwrap();
    let result = operation_adjacency::<VecKind, Obj, Arr>(&h);
    assert_eq!(result.sources.table, VecArray(vec![1, 1, 0]));
    assert_eq!(result.values.table, VecArray(vec![2, 2]));

    // the lhs f is adjacent to the rhs, but not vice-versa.
    //
    //      ┌───┐     ┌───┐
    // ●────│   │     │   │────●
    //      │ f │──●──│ f │
    // ●────│   │     │   │────●
    //      └───┘     └───┘
    //
    let f_op = OpenHypergraph::singleton(F, y.clone(), x.clone());
    let h = (&f >> &f_op).unwrap();
    let result = operation_adjacency(&h);
    assert_eq!(result.sources.table, VecArray(vec![1, 0]));
    assert_eq!(result.values.table, VecArray(vec![1]));

    // LHS f is adjacent to RHS f in *two distinct ways*!
    //    ┌───┐         ┌───┐
    //    │   │────●────│   │
    // ●──│ f │         │ f │──●
    //    │   │────●────│   │
    //    └───┘         └───┘
    let h = (&f_op >> &f).unwrap();
    let result = operation_adjacency(&h);
    assert_eq!(result.sources.table, VecArray(vec![2, 0]));
    assert_eq!(result.values.table, VecArray(vec![1, 1]));
}
