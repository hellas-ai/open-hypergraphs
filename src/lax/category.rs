//! Cospans of Hypergraphs.
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
        let mut f = OpenHypergraph::empty();
        let node_ids: Vec<NodeId> = a.iter().map(|o| f.new_node(o.clone())).collect();
        f.sources = node_ids.clone();
        f.targets = node_ids.clone();
        f
    }

    fn compose(&self, other: &Self) -> Option<Self> {
        if self.target() != other.source() {
            return None;
        }

        let n = other.hypergraph.nodes.len();
        let mut f = self.tensor(other);
        for (u, v) in self.targets.iter().zip(other.sources.iter()) {
            f.unify(*u, NodeId(v.0 + n));
        }

        f.sources = f.sources[..self.sources.len()].to_vec();
        f.targets = f.targets[self.targets.len()..].to_vec();

        Some(f)
    }
}

impl<O: Clone + PartialEq, A: Clone> Monoidal for OpenHypergraph<O, A> {
    fn unit() -> Self::Object {
        vec![]
    }

    fn tensor(&self, other: &Self) -> Self {
        let hypergraph = self.hypergraph.coproduct(&other.hypergraph);

        // renumber all nodes
        let n = self.hypergraph.nodes.len();

        let sources = self
            .sources
            .iter()
            .cloned()
            .chain(other.sources.iter().map(|&i| NodeId(i.0 + n)))
            .collect();

        let targets = self
            .targets
            .iter()
            .cloned()
            .chain(other.targets.iter().map(|&i| NodeId(i.0 + n)))
            .collect();

        OpenHypergraph {
            sources,
            targets,
            hypergraph,
        }
    }
}

use crate::array::vec::VecArray;
use crate::semifinite::*;

impl<O: Clone + PartialEq, A: Clone + PartialEq> SymmetricMonoidal for OpenHypergraph<O, A> {
    fn twist(a: Self::Object, b: Self::Object) -> Self {
        let f = crate::open_hypergraph::OpenHypergraph::twist(
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
        let w = SemifiniteFunction(VecArray(w));
        let f = crate::open_hypergraph::OpenHypergraph::spider(s, t, w)?;
        Some(OpenHypergraph::from_strict(f))
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
