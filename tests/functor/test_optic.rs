use open_hypergraphs::array::vec::*;
use open_hypergraphs::finite_function::*;
use open_hypergraphs::functor::*;
use open_hypergraphs::indexed_coproduct::*;
use open_hypergraphs::open_hypergraph::*;
use open_hypergraphs::operations::*;
use open_hypergraphs::semifinite::*;

use num_traits::Zero;

// A test theory where every arrow label is arbitrarily polymorphic (and therefore has a dagger).

#[allow(dead_code)]
#[derive(PartialEq, Eq, Clone, Debug)]
enum Ob {
    A,
    B,
    C,
}

#[allow(dead_code)]
#[derive(PartialEq, Eq, Clone, Debug)]
enum Op {
    F,
    G,
    H,
}

// The reverse map is the dagger functor: which just swaps source and target type of each operation.
struct Dagger;

impl SpiderFunctor<VecKind, Ob, Op, Ob, Op> for Dagger {
    // Dagger is identity on objects...
    fn map_object(
        a: &SemifiniteFunction<VecKind, Ob>,
    ) -> IndexedCoproduct<VecKind, SemifiniteFunction<VecKind, Ob>> {
        IndexedCoproduct::elements(a.clone())
    }

    // ... and swaps source/target type of each operation.
    fn map_operations(ops: Operations<VecKind, Ob, Op>) -> OpenHypergraph<VecKind, Ob, Op> {
        OpenHypergraph::tensor_operations(
            Operations::new(ops.x, ops.b, ops.a).expect("safe by construction"),
        )
    }
}

struct DaggerOptic;

impl Optic<VecKind, Ob, Op, Ob, Op> for DaggerOptic {
    type F = Identity;
    type R = Dagger;

    fn residual(
        ops: &Operations<VecKind, Ob, Op>,
    ) -> IndexedCoproduct<VecKind, SemifiniteFunction<VecKind, Ob>> {
        // The residual object for each operation is the unit object;
        // so we map each operation to the empty array.
        let sources = FiniteFunction::terminal(ops.len());
        let values = SemifiniteFunction::zero();
        IndexedCoproduct::new(sources, values).unwrap()
    }
}

#[test]
fn test_dagger_optic_identity() {
    use Ob::*;
    let w = SemifiniteFunction::<VecKind, Ob>(VecArray(vec![A, B]));
    //let id = OpenHypergraph::<VecKind, Ob, Op>::identity(w.clone());

    let fw = DaggerOptic::map_object(&w);

    // Check that for each "sublist" in the indexed coproduct fw, there is a single generating
    // object in w.
    assert_eq!(fw.len(), w.len());

    // In this particular case, each sublist should have length 2
    assert_eq!(fw.values.len(), w.len() * 2);

    // The functor outputs "interleaved" values F(A₀) ● R(A₀) ... F(An) ● R(An)
    assert_eq!(fw.values.0, VecArray(vec![A, A, B, B]));
}
