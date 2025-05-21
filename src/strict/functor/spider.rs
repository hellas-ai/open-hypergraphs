//! Symmetric Monoidal Hypergraph Functors on Open Hypergraphs
use crate::array::*;
use crate::category::*;
use crate::finite_function::*;
use crate::indexed_coproduct::*;
use crate::operations::*;
use crate::semifinite::*;

use crate::strict::open_hypergraph::*;

/// Strict symmetric monoidal hypergraph functors
pub trait Functor<K: ArrayKind, O1, A1, O2, A2> {
    /// Action on objects
    fn map_object(
        &self,
        a: &SemifiniteFunction<K, O1>,
    ) -> IndexedCoproduct<K, SemifiniteFunction<K, O2>>;

    /// Action on arrows
    /// This is generally easier to implement in terms of a functors' action on a tensoring of [`Operations`].
    /// Use [`functor_map_arrow`] for that.
    fn map_arrow(&self, a: &OpenHypergraph<K, O1, A1>) -> OpenHypergraph<K, O2, A2>;
}

/// Define a functor (a mapping on [`OpenHypergraph`]s) by its action on [`Operations`].
/// This is typically easier than implementing `map_arrow` directly.
pub fn define_map_arrow<K: ArrayKind, O1, A1, O2, A2, F: Functor<K, O1, A1, O2, A2>>(
    functor: &F,
    f: &OpenHypergraph<K, O1, A1>,
    map_operations: impl Fn(Operations<K, O1, A1>) -> OpenHypergraph<K, O2, A2>,
) -> OpenHypergraph<K, O2, A2>
where
    K::Type<K::I>: NaturalArray<K>,

    K::Type<O1>: Array<K, O1>,
    K::Type<A1>: Array<K, A1>,

    K::Type<O2>: Array<K, O2>,
    K::Type<A2>: Array<K, A2>,
{
    // Compute the tensoring of operations
    // Fx = F(x₀) ● F(x₁) ● ... ● F(x_n)
    let fx = map_operations(to_operations(f));

    // Compute the tensoring of objects
    // Fw = F(w₀) ● F(w₁) ● ... ● ... F(w_n)
    let fw = functor.map_object(&f.h.w);

    spider_map_arrow::<K, O1, A1, O2, A2>(f, fw, fx)
}

// Given an arrow `f`, and a functor's action on its objects `fw` and operations `fx`,
// return the value of the functor applied to `f`.
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

pub(crate) fn to_operations<K: ArrayKind, O, A>(f: &OpenHypergraph<K, O, A>) -> Operations<K, O, A>
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

pub(crate) fn map_half_spider<K: ArrayKind, O>(
    w: &IndexedCoproduct<K, SemifiniteFunction<K, O>>,
    f: &FiniteFunction<K>,
) -> FiniteFunction<K> {
    w.sources.injections(f).unwrap()
}
