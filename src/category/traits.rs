// NOTE: in general, objects are taken "owned", but arrows are not.
// This is because objects are typically owned by the actual result,
// whereas composition, coproduct etc. creates a *new* result.
pub trait Arrow: Sized {
    type Object;

    fn source(&self) -> Self::Object;
    fn target(&self) -> Self::Object;

    /// the identity morphism on `a`
    fn identity(a: Self::Object) -> Self;

    /// Compose morphisms in diagrammatic order: `self ; other`
    ///
    /// # Errors
    ///
    /// Returns None if `self.target() != other.source()`.
    fn compose(&self, other: &Self) -> Option<Self>;
}

pub trait Coproduct: Arrow {
    fn initial_object() -> Self::Object;

    /// Construct the initial arrow `initial_a : 0 → a` from some object `a`
    fn initial(a: Self::Object) -> Self;

    // Construct the left injection map from two objects
    fn inj0(a: Self::Object, b: Self::Object) -> Self;

    // Construct the left injection map from two objects
    fn inj1(a: Self::Object, b: Self::Object) -> Self;

    /// Construct the coproduct of two arrows, or None if they didn't share a codomain.
    fn coproduct(&self, other: &Self) -> Option<Self>;
}

pub trait Monoidal: Arrow {
    /// the monoidal unit object
    fn unit() -> Self::Object;

    /// `f \otimes g` of two morphisms
    fn tensor(&self, other: &Self) -> Self;
}

pub trait SymmetricMonoidal: Monoidal {
    /// Construct the symmetry `\sigma_{a,b}` from `a` and `b`.
    fn twist(a: Self::Object, b: Self::Object) -> Self;
}
