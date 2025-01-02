use crate::array::*;
use crate::category::*;
use crate::finite_function::FiniteFunction;

use std::marker::PhantomData;
use std::ops::Shr;

/// A function whose *source* is finite, but whose *target* may be non-finite.
/// This is really just an array!
#[derive(PartialEq, Eq)]
pub struct SemifiniteFunction<K: ArrayKind, T>(pub K::Type<T>);

/// Arrows in the category of semifinite functions
pub enum SemifiniteArrow<K: ArrayKind, T> {
    Identity, // Identities on types T
    Finite(FiniteFunction<K>),
    Semifinite(SemifiniteFunction<K, T>),
}

impl<K: ArrayKind, T> From<FiniteFunction<K>> for SemifiniteArrow<K, T> {
    fn from(val: FiniteFunction<K>) -> Self {
        SemifiniteArrow::Finite(val)
    }
}

impl<K: ArrayKind, T> From<SemifiniteFunction<K, T>> for SemifiniteArrow<K, T> {
    fn from(val: SemifiniteFunction<K, T>) -> Self {
        SemifiniteArrow::Semifinite(val)
    }
}

/// Objects of arrows
#[derive(PartialEq, Eq)]
pub enum SemifiniteObject<K: ArrayKind, T> {
    Finite(K::I),         // all sources are natural numbers
    Type(PhantomData<T>), // Targets are arbitrary types
}

fn compose_semifinite<K: ArrayKind, T>(
    lhs: &FiniteFunction<K>,
    rhs: &SemifiniteFunction<K, T>,
) -> Option<SemifiniteFunction<K, T>>
where
    K::Type<T>: Array<K, T>,
{
    if lhs.target != rhs.0.len() {
        return None;
    }

    // TODO: had to reimplement composition of finite functions again.
    let table = rhs.0.gather(lhs.table.get_range(..));
    Some(SemifiniteFunction(table))
}

/// A [`FiniteFunction`] can be precomposed with a [`SemifiniteFunction`] to re-index it.
impl<K: ArrayKind, T> Shr<&SemifiniteFunction<K, T>> for &FiniteFunction<K>
where
    K::Type<T>: Array<K, T>,
{
    type Output = Option<SemifiniteFunction<K, T>>;

    fn shr(self, other: &SemifiniteFunction<K, T>) -> Option<SemifiniteFunction<K, T>> {
        compose_semifinite(self, other)
    }
}

impl<K: ArrayKind, T> Arrow for SemifiniteArrow<K, T>
where
    K::Type<T>: Array<K, T>,
{
    type Object = SemifiniteObject<K, T>;

    fn source(&self) -> Self::Object {
        //SemifiniteObject::Finite(self.0.len())
        match self {
            SemifiniteArrow::Finite(f) => SemifiniteObject::Finite(f.source()),
            _ => SemifiniteObject::Type(PhantomData),
        }
    }

    fn target(&self) -> Self::Object {
        match self {
            SemifiniteArrow::Finite(f) => SemifiniteObject::Finite(f.target()),
            _ => SemifiniteObject::Type(PhantomData),
        }
    }

    fn identity(a: &Self::Object) -> Self {
        match a {
            SemifiniteObject::Finite(a) => FiniteFunction::identity(a).into(),
            SemifiniteObject::Type(_) => SemifiniteArrow::Identity,
        }
    }

    fn compose(&self, other: &Self) -> Option<Self> {
        // LHS must always be finite otherwise this is not composable.
        if let SemifiniteArrow::Finite(f) = self {
            match other {
                SemifiniteArrow::Finite(g) => (f >> g).map(SemifiniteArrow::Finite),
                SemifiniteArrow::Semifinite(g) => {
                    let result: Option<SemifiniteFunction<K, T>> = compose_semifinite(f, g);
                    result.map(SemifiniteArrow::Semifinite)
                    //(f >> g).map(|x| SemifiniteArrow::Semifinite(x))
                }
                _ => None, // Identity is only for types!
            }
        } else {
            None
        }
    }
}
