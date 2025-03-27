use {
    super::traits::*,
    crate::{array::*, finite_function::*},
};

/// Categories with [hypergraph structure](https://ncatlab.org/nlab/show/hypergraph+category).
/// We call this `Spider` to avoid confusion with the [crate::hypergraph::Hypergraph] struct.
pub trait Spider<K: ArrayKind>: Arrow {
    /// Given an `Arrow` with type `f : A → B`,
    /// construct its dagger `f† : B → A`
    /// by using Hypergraph structure to bend sources to targets and vice-versa.
    fn dagger(&self) -> Self;

    /// Construct a spider using a type `w`, and source `s` and target `t` interface maps.
    fn spider(s: FiniteFunction<K>, t: FiniteFunction<K>, w: Self::Object) -> Option<Self>;

    /// Construct a "half-spider": a spider whose `t` leg is identity.
    fn half_spider(s: FiniteFunction<K>, w: Self::Object) -> Option<Self> {
        let t = FiniteFunction::<K>::identity(s.target());
        Self::spider(s, t, w)
    }
}
