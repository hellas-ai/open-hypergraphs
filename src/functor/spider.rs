//! Symmetric Monoidal Hypergraph Functors on Open Hypergraphs
use crate::{
    array::*, category::*, finite_function::*, indexed_coproduct::*, open_hypergraph::*,
    operations::*, semifinite::*,
};

/// Strict symmetric monoidal hypergraph functors
pub trait Functor<K: ArrayKind, O1, A1, O2, A2> {
    /// Action on objects
    fn map_object(a: &SemifiniteFunction<K, O1>) -> IndexedCoproduct<K, SemifiniteFunction<K, O2>>;

    /// Action on arrows
    fn map_arrow(a: &OpenHypergraph<K, O1, A1>) -> OpenHypergraph<K, O2, A2>;
}

/// The identity functor, which implements [`Functor`] for any signature.
pub struct Identity;

impl<K: ArrayKind, O, A> Functor<K, O, A, O, A> for Identity
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    fn map_object(a: &SemifiniteFunction<K, O>) -> IndexedCoproduct<K, SemifiniteFunction<K, O>> {
        IndexedCoproduct::elements(a.clone())
    }

    fn map_arrow(f: &OpenHypergraph<K, O, A>) -> OpenHypergraph<K, O, A> {
        f.clone()
    }
}

////////////////////////////////////////////////////////////////////////////////
// Spider Functors

pub fn to_operations<K: ArrayKind, O, A>(f: &OpenHypergraph<K, O, A>) -> Operations<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    Operations {
        x: f.h.x.clone(),
        a: f.h.s.map_semifinite(&f.h.w).unwrap(),
        b: f.h.t.map_semifinite(&f.h.w).unwrap(),
    }
}

fn map_half_spider<K: ArrayKind, O>(
    w: &IndexedCoproduct<K, SemifiniteFunction<K, O>>,
    f: &FiniteFunction<K>,
) -> FiniteFunction<K> {
    w.sources.injections(f).unwrap()
}

/// A [`SpiderFunctor`] is a [`Functor`] implemented in terms of its action on a tensoring of
/// [`Operations`].
/// This is (generally) easier to implement than [`Functor`] directly.
///
// NOTE: [`SpiderFunctor`] does not imply Functor because of the orphan instances rule.
pub trait SpiderFunctor<K: ArrayKind, O1, A1, O2, A2>: Sized
where
    K::Type<K::I>: NaturalArray<K>,

    K::Type<O1>: Array<K, O1>,
    K::Type<A1>: Array<K, A1>,

    K::Type<O2>: Array<K, O2>,
    K::Type<A2>: Array<K, A2>,
{
    /// Action on objects
    fn map_object(a: &SemifiniteFunction<K, O1>) -> IndexedCoproduct<K, SemifiniteFunction<K, O2>>;

    /// Often, it's easier to map a list of operations f, g, h into their tensoring F(f) ● F(g) ●
    /// F(h).
    /// This efficiently generalises to the implementation of map_arrow.
    fn map_operations(ops: Operations<K, O1, A1>) -> OpenHypergraph<K, O2, A2>;

    fn map_arrow(f: &OpenHypergraph<K, O1, A1>) -> OpenHypergraph<K, O2, A2> {
        // Compute the tensoring of operations
        // Fx = F(x₀) ● F(x₁) ● ... ● F(x_n)
        let fx = Self::map_operations(to_operations(f));

        // Compute the tensoring of objects
        // Fw = F(w₀) ● F(w₁) ● ... ● ... F(w_n)
        let fw = Self::map_object(&f.h.w);

        spider_map_arrow::<K, O1, A1, O2, A2>(f, fw, fx)
    }
}

// NOTE: this implementation is factored outside the trait impl so we can use it in the Optic
// functor implementation as well.
pub(crate) fn spider_map_arrow<K: ArrayKind, O1, A1, O2, A2>(
    f: &OpenHypergraph<K, O1, A1>,
    fw: IndexedCoproduct<K, SemifiniteFunction<K, O2>>,
    fx: OpenHypergraph<K, O2, A2>,
) -> OpenHypergraph<K, O2, A2>
where
    K::Type<K::I>: NaturalArray<K>,

    K::Type<O1>: Array<K, O1>,
    K::Type<A1>: Array<K, A1>,

    K::Type<O2>: Array<K, O2>,
    K::Type<A2>: Array<K, A2>,
{
    // Construct the identity map on the wires of F(w)
    let i = OpenHypergraph::<K, O2, A2>::identity(fw.values.clone());

    let sx = {
        let fs = map_half_spider(&fw, &f.s);
        let e_s = map_half_spider(&fw, &f.h.s.values);
        OpenHypergraph::<K, O2, A2>::spider(fs, i.t.coproduct(&e_s).unwrap(), i.h.w.clone())
            .unwrap()
    };

    let yt = {
        let ft = map_half_spider(&fw, &f.t);
        let fe_t = map_half_spider(&fw, &f.h.t.values);
        OpenHypergraph::<K, O2, A2>::spider(i.s.coproduct(&fe_t).unwrap(), ft, i.h.w.clone())
            .unwrap()
    };

    // Construct the diagram
    //
    //          sx
    //   _______|___________
    //   |                 |
    //
    //
    //           /-----------------------------\
    //  -- F(s)--                               --- F(t) ---
    //           \---F(e_s)---F(x)----F(e_t)---/
    //
    //
    //                               |_______________________|
    //                                           |
    //                                           yt

    sx.compose(&i.tensor(&fx)).unwrap().compose(&yt).unwrap()
}

impl<K: ArrayKind, O, A> SpiderFunctor<K, O, A, O, A> for Identity
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    fn map_object(a: &SemifiniteFunction<K, O>) -> IndexedCoproduct<K, SemifiniteFunction<K, O>> {
        // same action on objects
        <Self as Functor<K, O, A, O, A>>::map_object(a)
    }

    fn map_operations(ops: Operations<K, O, A>) -> OpenHypergraph<K, O, A> {
        OpenHypergraph::tensor_operations(ops)
    }
}
