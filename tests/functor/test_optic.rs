use open_hypergraphs::array::vec::*;
use open_hypergraphs::operations::*;
use open_hypergraphs::strict::functor::identity::Identity;
use open_hypergraphs::strict::functor::*;
use open_hypergraphs::strict::*;

use core::fmt::Debug;
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

impl<O: PartialEq + Clone, A: PartialEq + Clone> Functor<VecKind, O, A, O, A> for Dagger {
    // Dagger is identity on objects...
    fn map_object(
        &self,
        a: &SemifiniteFunction<VecKind, O>,
    ) -> IndexedCoproduct<VecKind, SemifiniteFunction<VecKind, O>> {
        IndexedCoproduct::elements(a.clone())
    }

    // ... and swaps source/target type of each operation.
    fn map_operations(&self, ops: Operations<VecKind, O, A>) -> OpenHypergraph<VecKind, O, A> {
        OpenHypergraph::tensor_operations(
            Operations::new(ops.x, ops.b, ops.a).expect("safe by construction"),
        )
    }

    fn map_arrow(&self, f: &OpenHypergraph<VecKind, O, A>) -> OpenHypergraph<VecKind, O, A> {
        define_map_arrow(self, f)
    }
}

fn dagger_optic<O: Clone + PartialEq + Debug, A: Clone + PartialEq + Debug>(
) -> Optic<Identity, Dagger, VecKind, O, A, O, A> {
    Optic::new(
        Identity,
        Dagger,
        Box::new(|ops: &Operations<VecKind, O, A>| {
            // The residual object for each operation is the unit object;
            // so we map each operation to the empty array.
            let sources = FiniteFunction::terminal(ops.len());
            let values = SemifiniteFunction::zero();
            IndexedCoproduct::new(sources, values).unwrap()
        }),
    )
}

#[test]
fn test_dagger_optic_identity() {
    use Ob::*;
    let w = SemifiniteFunction::<VecKind, Ob>(VecArray(vec![A, B]));
    let id = OpenHypergraph::<VecKind, Ob, Op>::identity(w.clone());

    let dagger_optic = dagger_optic();
    let fw = dagger_optic.map_object(&w);

    // Check that for each "sublist" in the indexed coproduct fw, there is a single generating
    // object in w.
    assert_eq!(fw.len(), w.len());

    // In this particular case, each sublist should have length 2
    assert_eq!(fw.values.len(), w.len() * 2);

    // The functor outputs "interleaved" values F(A₀) ● R(A₀) ... F(An) ● R(An)
    assert_eq!(fw.values.0, VecArray(vec![A, A, B, B]));

    // transposing interleaves the F/R objects
    let t = FiniteFunction::transpose(w.len(), 2);
    assert_eq!((&t >> &fw.values).unwrap().0, VecArray(vec![A, B, A, B]));

    let ff = dagger_optic.map_arrow(&id);
    assert_eq!(ff.source(), SemifiniteFunction(VecArray(vec![A, A, B, B])));
    assert_eq!(ff.target(), SemifiniteFunction(VecArray(vec![A, A, B, B])));
}

#[test]
fn test_dagger_optic_singleton() {
    use Ob::*;
    use Op::*;

    let x = SemifiniteFunction::<VecKind, Ob>(VecArray(vec![A, B]));
    let y = SemifiniteFunction::<VecKind, Ob>(VecArray(vec![C]));
    let dagger_optic = dagger_optic();

    let f = OpenHypergraph::singleton(F, x.clone(), y.clone());

    // transposing interleaves the F/R objects
    let f_x = dagger_optic.map_object(&x);
    let t_x = FiniteFunction::transpose(x.len(), 2);
    assert_eq!((&t_x >> &f_x.values).unwrap(), (&x + &x).unwrap());

    let f_y = dagger_optic.map_object(&y);
    let t_y = FiniteFunction::transpose(y.len(), 2);
    assert_eq!((&t_y >> &f_y.values).unwrap(), (&y + &y).unwrap());

    let ff = dagger_optic.map_arrow(&f);
    assert_eq!(ff.source(), f_x.values);
    assert_eq!(ff.target(), f_y.values);
}

use crate::theory::meaningless::*;
use proptest::proptest;

proptest! {
    #[test]
    fn test_dagger_optic_type(f in arb_open_hypergraph()) {
        let dagger_optic = dagger_optic::<Obj, Arr>();
        let ff = dagger_optic.map_arrow(&f);

        // TODO: is there a nicer way to annotate the types here?
        // Only the `Arr` types are not inferable.
        let f_a = <_ as Functor<_, _, Arr, _, Arr>>::map_object(&dagger_optic.fwd, &f.source());
        let r_a = <_ as Functor<_, _, Arr, _, Arr>>::map_object(&dagger_optic.rev, &f.source());

        let f_b = <_ as Functor<_, _, Arr, _, Arr>>::map_object(&dagger_optic.fwd, &f.target());
        let r_b = <_ as Functor<_, _, Arr, _, Arr>>::map_object(&dagger_optic.rev, &f.target());

        assert_eq!(f_a.len(), r_a.len());
        assert_eq!(f_b.len(), r_b.len());

        let p_a = f_a.coproduct(&r_a).unwrap().sources.injections(&FiniteFunction::transpose(f_a.len(), 2)).unwrap();
        let p_b = f_b.coproduct(&r_b).unwrap().sources.injections(&FiniteFunction::transpose(f_b.len(), 2)).unwrap();

        assert_eq!((&p_a >> &ff.source()).unwrap(), f_a.values + r_a.values);
        assert_eq!((&p_b >> &ff.target()).unwrap(), f_b.values + r_b.values);
    }
}
