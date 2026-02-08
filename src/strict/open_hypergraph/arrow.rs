use crate::array::*;
use crate::category::*;
use crate::finite_function::*;
use crate::operations::*;
use crate::semifinite::*;
use crate::strict::hypergraph::{arrow::HypergraphArrow, Hypergraph, InvalidHypergraph};

use core::fmt::Debug;
use core::ops::{BitOr, Shr};
use num_traits::Zero;

impl<K: ArrayKind> From<InvalidHypergraph<K>> for InvalidOpenHypergraph<K> {
    fn from(value: InvalidHypergraph<K>) -> Self {
        InvalidOpenHypergraph::InvalidHypergraph(value)
    }
}

#[derive(Debug)]
pub enum InvalidOpenHypergraph<K: ArrayKind> {
    CospanSourceType(K::I, K::I),
    CospanTargetType(K::I, K::I),
    InvalidHypergraph(InvalidHypergraph<K>),
}

/// Open Hypergraphs
///
/// # Invariants
///
/// We must have the following invariants:
///
/// These are checked by the [`OpenHypergraph::validate`] method
///
/// # Panics
///
/// Most operations assume the invariants hold, and will panic if not.
pub struct OpenHypergraph<K: ArrayKind, O, A> {
    pub s: FiniteFunction<K>,
    pub t: FiniteFunction<K>,
    pub h: Hypergraph<K, O, A>,
}

impl<K: ArrayKind, O, A> OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    pub fn new(
        s: FiniteFunction<K>,
        t: FiniteFunction<K>,
        h: Hypergraph<K, O, A>,
    ) -> Result<Self, InvalidOpenHypergraph<K>> {
        let f = OpenHypergraph { s, t, h };
        f.validate()
    }

    pub fn validate(self) -> Result<Self, InvalidOpenHypergraph<K>> {
        let h = self.h.validate()?;
        let w_source = h.w.0.len();
        let s_target = self.s.target();
        let t_target = self.t.target();

        // TODO: validate hypergraph as well
        if s_target != w_source {
            Err(InvalidOpenHypergraph::CospanSourceType(s_target, w_source))
        } else if t_target != w_source {
            Err(InvalidOpenHypergraph::CospanTargetType(t_target, w_source))
        } else {
            Ok(OpenHypergraph {
                s: self.s,
                t: self.t,
                h,
            })
        }
    }

    pub fn singleton(
        x: A,
        a: SemifiniteFunction<K, O>,
        b: SemifiniteFunction<K, O>,
    ) -> OpenHypergraph<K, O, A> {
        Self::tensor_operations(Operations::singleton(x, a, b))
    }

    pub fn tensor_operations(operations: Operations<K, O, A>) -> OpenHypergraph<K, O, A> {
        let h = Hypergraph::tensor_operations(operations);
        let t = h.t.values.clone();
        let s = h.s.values.clone();
        OpenHypergraph { s, t, h }
    }

    ////////////////////////////////////////
    // Category methods

    pub fn source(&self) -> SemifiniteFunction<K, O> {
        // NOTE: invalid OpenHypergraph will panic!
        (&self.s >> &self.h.w).expect("invalid open hypergraph: cospan source has invalid codomain")
    }

    pub fn target(&self) -> SemifiniteFunction<K, O> {
        (&self.t >> &self.h.w).expect("invalid open hypergraph: cospan target has invalid codomain")
    }

    pub fn identity(w: SemifiniteFunction<K, O>) -> OpenHypergraph<K, O, A> {
        let s = FiniteFunction::<K>::identity(w.0.len());
        let t = FiniteFunction::<K>::identity(w.0.len());
        let h = Hypergraph::<K, O, A>::discrete(w);
        OpenHypergraph { s, t, h }
    }

    pub fn spider(
        s: FiniteFunction<K>,
        t: FiniteFunction<K>,
        w: SemifiniteFunction<K, O>,
    ) -> Option<Self> {
        if s.target() != w.len() || t.target() != w.len() {
            return None;
        }

        let h = Hypergraph::discrete(w);
        Some(OpenHypergraph { s, t, h })
    }

    pub fn is_monogamous(&self) -> bool
    where
        K::Type<bool>: Array<K, bool>,
    {
        // TODO: implement monogamy check
        // Note: when implemented, monogamy should also imply boundary legs are injective.
        debug_assert!(self.s.is_injective() && self.t.is_injective());
        true
    }

    pub fn is_acyclic(&self) -> bool {
        // TODO: implement acyclicity check
        true
    }
}

impl<K: ArrayKind, O, A> OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A>,
{
    fn compose(&self, other: &Self) -> Option<Self> {
        if self.target() != other.source() {
            return None;
        }

        // compute coequalizer q
        let q_lhs = self.t.inject0(other.h.w.0.len());
        let q_rhs = other.s.inject1(self.h.w.0.len());
        let q = q_lhs.coequalizer(&q_rhs).expect("Invalid OpenHypergraph");

        // Compute cospan legs
        // NOTE: we don't return None here because composition should only fail on a type mismatch.
        // If the compositions for s and t give None, it means there's a bug in the library!
        let s = self.s.inject0(other.h.w.0.len()).compose(&q).unwrap();
        let t = other.t.inject1(self.h.w.0.len()).compose(&q).unwrap();

        // Tensor self and other, then unify wires on the boundaries.
        // NOTE: this should never fail for a valid open hypergraph
        let h = self.tensor(other).h.coequalize_vertices(&q).unwrap();

        Some(OpenHypergraph { s, t, h })
    }
}

impl<K: ArrayKind, O, A> OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
{
    /// Build the boundary arrow `n+m -> G` where `n+m` is discrete.
    pub fn boundary_arrow(&self) -> HypergraphArrow<K, O, A> {
        let boundary = self.source().coproduct(&self.target());
        let source = Hypergraph::discrete(boundary);
        let w = (&self.s + &self.t).expect("invalid open hypergraph: cospan legs not coproductible");
        let x = FiniteFunction::<K>::initial(self.h.x.len());
        HypergraphArrow::new(source, self.h.clone(), w, x)
            .expect("invalid open hypergraph: boundary arrow not natural")
    }
}

impl<K: ArrayKind, O, A> Arrow for OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A>,
{
    // TODO: should be SemifiniteFunction?
    type Object = SemifiniteFunction<K, O>;

    fn source(&self) -> Self::Object {
        self.source()
    }

    fn target(&self) -> Self::Object {
        self.target()
    }

    fn identity(w: Self::Object) -> Self {
        Self::identity(w)
    }

    fn compose(&self, other: &Self) -> Option<Self> {
        self.compose(other)
    }
}

impl<K: ArrayKind, O, A> Monoidal for OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A>,
{
    fn unit() -> Self::Object {
        SemifiniteFunction::<K, O>::zero()
    }

    fn tensor(&self, other: &Self) -> Self {
        OpenHypergraph {
            s: &self.s | &other.s,
            t: &self.t | &other.t,
            h: &self.h + &other.h,
        }
    }
}

impl<K: ArrayKind, O, A> SymmetricMonoidal for OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A>,
{
    fn twist(a: Self::Object, b: Self::Object) -> Self {
        let s = FiniteFunction::twist(a.len(), b.len());
        let t = FiniteFunction::identity(a.len() + b.len());

        // NOTE: because the *source* map is twist, the internal labelling of wires
        // is `b + a` instead of `a + b`. This matters!
        let h = Hypergraph::discrete(b + a);
        OpenHypergraph { s, t, h }
    }
}

impl<K: ArrayKind, O, A> Spider<K> for OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A>,
{
    fn dagger(&self) -> Self {
        OpenHypergraph {
            s: self.t.clone(),
            t: self.s.clone(),
            h: self.h.clone(),
        }
    }

    fn spider(s: FiniteFunction<K>, t: FiniteFunction<K>, w: Self::Object) -> Option<Self> {
        OpenHypergraph::spider(s, t, w)
    }
}

// Syntactic sugar for composition and tensor
impl<K: ArrayKind, O, A> Shr<&OpenHypergraph<K, O, A>> for &OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A>,
{
    type Output = Option<OpenHypergraph<K, O, A>>;
    fn shr(self, rhs: &OpenHypergraph<K, O, A>) -> Option<OpenHypergraph<K, O, A>> {
        self.compose(rhs)
    }
}

// Parallel composition
impl<K: ArrayKind, O, A> BitOr<&OpenHypergraph<K, O, A>> for &OpenHypergraph<K, O, A>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A>,
{
    type Output = OpenHypergraph<K, O, A>;
    fn bitor(self, rhs: &OpenHypergraph<K, O, A>) -> OpenHypergraph<K, O, A> {
        self.tensor(rhs)
    }
}

impl<K: ArrayKind, O, A> Clone for OpenHypergraph<K, O, A>
where
    K::Type<O>: Clone,
    K::Type<A>: Clone,
    K::Type<K::I>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            s: self.s.clone(),
            t: self.t.clone(),
            h: self.h.clone(),
        }
    }
}

impl<K: ArrayKind, O: Debug, A: Debug> Debug for OpenHypergraph<K, O, A>
where
    K::Index: Debug,
    K::Type<K::I>: Debug,
    K::Type<O>: Debug,
    K::Type<A>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OpenHypergraph")
            .field("s", &self.s)
            .field("t", &self.t)
            .field("h", &self.h)
            .finish()
    }
}
