//! Implement [`crate::category`] traits for [`crate::lax::OpenHypergraph`]
use crate::array::vec::VecKind;
use crate::category::*;
use crate::lax::*;

impl<O: Clone + PartialEq, A: Clone> Arrow for OpenHypergraph<O, A> {
    type Object = Vec<O>;

    fn source(&self) -> Self::Object {
        self.sources
            .iter()
            .map(|i| self.hypergraph.nodes[i.0].clone())
            .collect()
    }

    fn target(&self) -> Self::Object {
        self.targets
            .iter()
            .map(|i| self.hypergraph.nodes[i.0].clone())
            .collect()
    }

    fn identity(a: Self::Object) -> Self {
        OpenHypergraph::identity(a)
    }

    fn compose(&self, other: &Self) -> Option<Self> {
        if self.target() != other.source() {
            return None;
        }
        self.lax_compose(other)
    }
}

impl<O: Clone, A: Clone> OpenHypergraph<O, A> {
    /// Compose two open hypergraphs, unifying the boundary nodes without checking labels.
    ///
    /// Returns None when the boundary arities do not match.
    pub fn lax_compose(&self, other: &Self) -> Option<Self> {
        if self.targets.len() != other.sources.len() {
            return None;
        }

        let n = self.hypergraph.nodes.len();
        let mut f = self.tensor(other);
        for (u, v) in self.targets.iter().zip(other.sources.iter()) {
            f.unify(*u, NodeId(v.0 + n));
        }

        f.sources.truncate(self.sources.len());
        f.targets = f.targets[self.targets.len()..].to_vec();

        Some(f)
    }
}

impl<O: Clone + PartialEq, A: Clone> Monoidal for OpenHypergraph<O, A> {
    fn unit() -> Self::Object {
        vec![]
    }

    fn tensor(&self, other: &Self) -> Self {
        OpenHypergraph::tensor(self, other)
    }
}

use crate::array::vec::VecArray;
use crate::semifinite::*;

impl<O: Clone + PartialEq, A: Clone + PartialEq> SymmetricMonoidal for OpenHypergraph<O, A> {
    fn twist(a: Self::Object, b: Self::Object) -> Self {
        let f = crate::strict::open_hypergraph::OpenHypergraph::twist(
            SemifiniteFunction(VecArray(a)),
            SemifiniteFunction(VecArray(b)),
        );
        OpenHypergraph::from_strict(f)
    }
}

impl<O: Clone + PartialEq, A: Clone + PartialEq> Spider<VecKind> for OpenHypergraph<O, A> {
    fn dagger(&self) -> Self {
        let mut f = self.clone();
        f.sources = self.targets.clone();
        f.targets = self.sources.clone();
        f
    }

    fn spider(
        s: crate::finite_function::FiniteFunction<VecKind>,
        t: crate::finite_function::FiniteFunction<VecKind>,
        w: Self::Object,
    ) -> Option<Self> {
        OpenHypergraph::spider(s, t, w)
    }
}

use core::ops::{BitOr, Shr};

impl<O: Clone + PartialEq, A: Clone> Shr<&OpenHypergraph<O, A>> for &OpenHypergraph<O, A> {
    type Output = Option<OpenHypergraph<O, A>>;

    fn shr(self, rhs: &OpenHypergraph<O, A>) -> Self::Output {
        self.compose(rhs)
    }
}

impl<O: Clone + PartialEq, A: Clone> BitOr<&OpenHypergraph<O, A>> for &OpenHypergraph<O, A> {
    type Output = OpenHypergraph<O, A>;

    fn bitor(self, rhs: &OpenHypergraph<O, A>) -> Self::Output {
        self.tensor(rhs)
    }
}
