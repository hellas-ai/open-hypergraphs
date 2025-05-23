use crate::array::*;
use crate::category::*;
use crate::finite_function::*;
use crate::indexed_coproduct::*;
use crate::operations::*;
use crate::semifinite::*;

use crate::strict::open_hypergraph::*;

use super::traits::*;

use core::fmt::Debug;
use num_traits::One;

type ResidualFn<K, O1, A1, O2> =
    dyn Fn(&Operations<K, O1, A1>) -> IndexedCoproduct<K, SemifiniteFunction<K, O2>>;

/// An optic is composed of forward and reverse functors along with a residual object
pub struct Optic<
    F: Functor<K, O1, A1, O2, A2>,
    R: Functor<K, O1, A1, O2, A2>,
    K: ArrayKind,
    O1,
    A1,
    O2,
    A2,
> {
    pub fwd: F,
    pub rev: R,
    pub residual: Box<ResidualFn<K, O1, A1, O2>>,
    _phantom: std::marker::PhantomData<A2>,
}

impl<
        F: Functor<K, O1, A1, O2, A2>,
        R: Functor<K, O1, A1, O2, A2>,
        K: ArrayKind,
        O1,
        A1,
        O2,
        A2,
    > Optic<F, R, K, O1, A1, O2, A2>
{
    pub fn new(
        fwd: F,
        rev: R,
        residual: Box<ResidualFn<K, O1, A1, O2>>,
    ) -> Optic<F, R, K, O1, A1, O2, A2> {
        let _phantom = std::marker::PhantomData;
        Optic {
            fwd,
            rev,
            residual,
            _phantom,
        }
    }
}

impl<F, R, K: ArrayKind + Debug, O1, A1, O2, A2> Functor<K, O1, A1, O2, A2>
    for Optic<F, R, K, O1, A1, O2, A2>
where
    F: Functor<K, O1, A1, O2, A2>,
    R: Functor<K, O1, A1, O2, A2>,
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O1>: Array<K, O1>,
    K::Type<A1>: Array<K, A1>,
    K::Type<O2>: Array<K, O2> + Debug,
    K::Type<A2>: Array<K, A2>,
{
    fn map_object(
        &self,
        a: &SemifiniteFunction<K, O1>,
    ) -> IndexedCoproduct<K, SemifiniteFunction<K, O2>> {
        // Each object A is mapped to F(A) ● R(A)
        let fa = self.fwd.map_object(a);
        let ra = self.rev.map_object(a);

        assert_eq!(fa.len(), ra.len());
        let n = fa.len();

        // Create paired function similar to Python's FA + RA
        let paired = fa
            .coproduct(&ra)
            .expect("Coproduct of SemifiniteFunction always succeeds");
        let p = FiniteFunction::transpose(K::I::one() + K::I::one(), n);

        let sources = FiniteFunction::new(
            fa.sources.table + ra.sources.table,
            fa.sources.target + ra.sources.target - K::I::one(),
        )
        .unwrap();
        let values = paired.indexed_values(&p).unwrap();
        IndexedCoproduct::new(sources, values).unwrap()
    }

    fn map_operations(&self, ops: Operations<K, O1, A1>) -> OpenHypergraph<K, O2, A2> {
        // Forward and reverse maps
        let fwd = self.fwd.map_operations(ops.clone());
        let rev = self.rev.map_operations(ops.clone());

        // Get mapped objects
        let fa = self.fwd.map_object(&ops.a.values);
        let fb = self.fwd.map_object(&ops.b.values);
        let ra = self.rev.map_object(&ops.a.values);
        let rb = self.rev.map_object(&ops.b.values);

        let m = (self.residual)(&ops);

        // Create interleavings
        let fwd_interleave = interleave_blocks(&ops.b.flatmap_sources(&fb), &m).dagger();
        let rev_cointerleave = interleave_blocks(&m, &ops.b.flatmap_sources(&rb));

        debug_assert_eq!(fwd.target(), fwd_interleave.source());
        debug_assert_eq!(rev_cointerleave.target(), rev.source());

        let i_fb = OpenHypergraph::identity(fb.values.clone());
        let i_rb = OpenHypergraph::identity(rb.values.clone());

        // Compose the diagram parts
        let lhs = fwd.compose(&fwd_interleave).unwrap().tensor(&i_rb);
        let rhs = i_fb.tensor(&rev_cointerleave.compose(&rev).unwrap());
        let c = lhs.compose(&rhs).unwrap();

        // Partial dagger to bend wires
        let d = partial_dagger(&c, &fa, &fb, &ra, &rb);

        // Final interleaving
        let lhs = interleave_blocks(&fa, &ra).dagger();
        let rhs = interleave_blocks(&fb, &rb);

        lhs.compose(&d).unwrap().compose(&rhs).unwrap()
    }

    fn map_arrow(&self, f: &OpenHypergraph<K, O1, A1>) -> OpenHypergraph<K, O2, A2> {
        define_map_arrow(self, f)
    }
}

impl<F, R, K: ArrayKind + Debug, O1, A1, O2, A2> Optic<F, R, K, O1, A1, O2, A2>
where
    F: Functor<K, O1, A1, O2, A2>,
    R: Functor<K, O1, A1, O2, A2>,
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O1>: Array<K, O1>,
    K::Type<A1>: Array<K, A1>,
    K::Type<O2>: Array<K, O2> + Debug,
    K::Type<A2>: Array<K, A2>,
{
    pub fn adapt(
        &self,
        c: &OpenHypergraph<K, O2, A2>,
        a: &SemifiniteFunction<K, O1>,
        b: &SemifiniteFunction<K, O1>,
    ) -> OpenHypergraph<K, O2, A2> {
        let fa = self.fwd.map_object(a);
        let fb = self.fwd.map_object(b);
        let ra = self.rev.map_object(a);
        let rb = self.rev.map_object(b);

        // Uninterleave to get d : FA●RA → FB●RB
        let lhs = interleave_blocks(&fa, &ra);
        let rhs = interleave_blocks(&fb, &rb).dagger();
        let d = lhs.compose(c).unwrap().compose(&rhs).unwrap();

        // Verify source/target
        // NOTE: unwrap() because coproduct of semifinite functions always succeeds.
        debug_assert_eq!(d.source(), fa.coproduct(&ra).unwrap().values);
        debug_assert_eq!(d.target(), fb.coproduct(&rb).unwrap().values);

        // Partial dagger to get d : FA●RB → FB●RA
        partial_dagger(&d, &fa, &fb, &rb, &ra)
    }
}

fn interleave_blocks<K: ArrayKind, O, A>(
    a: &IndexedCoproduct<K, SemifiniteFunction<K, O>>,
    b: &IndexedCoproduct<K, SemifiniteFunction<K, O>>,
) -> OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    if a.len() != b.len() {
        panic!("Can't interleave types of unequal lengths");
    }

    let ab = a
        .coproduct(b)
        .expect("Coproduct of SemifiniteFunction always succeeds");

    let two = K::I::one() + K::I::one();
    let s = FiniteFunction::identity(ab.values.len());
    let t = ab
        .sources
        .injections(&FiniteFunction::transpose(two, a.len()))
        .unwrap();

    OpenHypergraph::spider(s, t, ab.values.clone()).unwrap()
}

/// Helper function to perform partial dagger operation
fn partial_dagger<K: ArrayKind + Debug, O, A>(
    c: &OpenHypergraph<K, O, A>,
    fa: &IndexedCoproduct<K, SemifiniteFunction<K, O>>,
    fb: &IndexedCoproduct<K, SemifiniteFunction<K, O>>,
    ra: &IndexedCoproduct<K, SemifiniteFunction<K, O>>,
    rb: &IndexedCoproduct<K, SemifiniteFunction<K, O>>,
) -> OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    let s = {
        let s_i = FiniteFunction::inj0(fa.values.len(), rb.values.len())
            .compose(&c.s)
            .unwrap();

        let s_o = FiniteFunction::inj1(fb.values.len(), ra.values.len())
            .compose(&c.t)
            .unwrap();

        s_i.coproduct(&s_o).unwrap()
    };

    let t = {
        let t_i = FiniteFunction::inj0(fb.values.len(), ra.values.len())
            .compose(&c.t)
            .unwrap();
        let t_o = FiniteFunction::inj1(fa.values.len(), rb.values.len())
            .compose(&c.s)
            .unwrap();

        t_i.coproduct(&t_o).unwrap()
    };

    OpenHypergraph::new(s, t, c.h.clone()).unwrap()
}
