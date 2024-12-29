pub trait Arrow: Sized {
    type Object;

    fn source(&self) -> Self::Object;
    fn target(&self) -> Self::Object;

    /// the identity morphism on `a`
    fn identity(a: &Self::Object) -> Self;

    /// Compose morphisms in diagrammatic order: `self ; other`
    ///
    /// # Errors
    ///
    /// Returns None if `self.target() != other.source()`.
    fn compose(&self, other: &Self) -> Option<Self>;
}

pub trait Coproduct: Arrow {
    /// Construct the initial arrow `initial_a : 0 → a` from some object `a`
    fn initial(a: &Self::Object) -> Self;

    /// Construct the coproduct of two arrows
    fn coproduct(&self, other: &Self) -> Self;
}

pub trait Monoidal: Arrow {
    /// the monoidal unit
    fn unit() -> Self;

    /// `f \otimes g` of two morphisms
    fn tensor(&self, other: &Self) -> Self;
}

pub trait SymmetricMonoidal: Monoidal {
    /// Construct the symmetry `\sigma_{a,b}` from `a` and `b`.
    fn twist(a: &Self::Object, b: &Self::Object) -> Self;
}
