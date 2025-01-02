use crate::array::*;
use crate::category::*;
use crate::finite_function::*;
use crate::hypergraph::Hypergraph;
use crate::semifinite::*;

use core::ops::{BitOr, Shr};
use num_traits::Zero;

pub enum ValidationError<K: ArrayKind> {
    CospanSourceType(K::I, K::I),
    CospanTargetType(K::I, K::I),
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
    K::I: Into<usize>, // TODO: remove this!
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    pub fn new(
        s: FiniteFunction<K>,
        t: FiniteFunction<K>,
        h: Hypergraph<K, O, A>,
    ) -> Result<Self, ValidationError<K>> {
        let f = OpenHypergraph { s, t, h };
        f.validate()
    }

    pub fn validate(self) -> Result<Self, ValidationError<K>> {
        // TODO: used isomorphism of K::I <> usize
        let w_source = self.h.w.0.len();
        let s_target = self.s.target();
        let t_target = self.t.target();

        // TODO: validate hypergraph as well

        if s_target != w_source {
            Err(ValidationError::CospanSourceType(s_target, w_source))
        } else if t_target != w_source {
            Err(ValidationError::CospanTargetType(t_target, w_source))
        } else {
            Ok(self)
        }
    }
}

impl<K: ArrayKind, O, A> Arrow for OpenHypergraph<K, O, A>
where
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    // TODO: should be SemifiniteFunction?
    type Object = SemifiniteFunction<K, O>;

    fn source(&self) -> Self::Object {
        // NOTE: invalid OpenHypergraph will panic!
        (&self.s >> &self.h.w).expect("invalid open hypergraph: cospan source has invalid codomain")
    }

    fn target(&self) -> Self::Object {
        (&self.t >> &self.h.w).expect("invalid open hypergraph: cospan target has invalid codomain")
    }

    fn identity(w: Self::Object) -> Self {
        let s = FiniteFunction::<K>::identity(w.0.len());
        let t = FiniteFunction::<K>::identity(w.0.len());
        let h = Hypergraph::<K, O, A>::discrete(w);
        OpenHypergraph { s, t, h }
    }

    fn compose(&self, other: &Self) -> Option<Self> {
        if self.target() != other.source() {
            return None;
        }

        // compute coequalizer q
        let q_lhs = self.t.inject0(other.h.w.0.len());
        let q_rhs = other.s.inject1(self.h.w.0.len());
        let q = q_lhs.coequalizer(&q_rhs);

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

impl<K: ArrayKind, O, A> Monoidal for OpenHypergraph<K, O, A>
where
    K::Type<O>: Array<K, O>,
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
    K::Type<O>: Array<K, O>,
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

// Syntactic sugar for composition and tensor
impl<K: ArrayKind, O, A> Shr<&OpenHypergraph<K, O, A>> for &OpenHypergraph<K, O, A>
where
    K::Type<O>: Array<K, O>,
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
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    type Output = OpenHypergraph<K, O, A>;
    fn bitor(self, rhs: &OpenHypergraph<K, O, A>) -> OpenHypergraph<K, O, A> {
        self.tensor(rhs)
    }
}
