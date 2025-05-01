use {
    super::types::*,
    crate::{array::*, category::*, finite_function::*},
};

use {core::marker::PhantomData, num_traits::Zero};

/// Arrows in the category of semifinite functions
pub enum SemifiniteArrow<K: ArrayKind, T> {
    Identity, // Identities on types T
    Finite(FiniteFunction<K>),
    Semifinite(SemifiniteFunction<K, T>),
}

/// Objects of arrows
#[derive(PartialEq, Eq)]
pub enum SemifiniteObject<K: ArrayKind, T> {
    Finite(K::I),        // all sources are natural numbers
    Set(PhantomData<T>), // Targets are arbitrary types
}

impl<K: ArrayKind, T> Arrow for SemifiniteArrow<K, T>
where
    K::Type<T>: Array<K, T>,
{
    type Object = SemifiniteObject<K, T>;

    fn source(&self) -> Self::Object {
        //SemifiniteObject::Finite(self.0.len())
        match self {
            Self::Finite(f) => SemifiniteObject::Finite(f.source()),
            Self::Semifinite(f) => SemifiniteObject::Finite(f.0.len()),
            _ => SemifiniteObject::Set(PhantomData),
        }
    }

    fn target(&self) -> Self::Object {
        match self {
            Self::Finite(f) => SemifiniteObject::Finite(f.target()),
            _ => SemifiniteObject::Set(PhantomData),
        }
    }

    fn identity(a: Self::Object) -> Self {
        match a {
            SemifiniteObject::Finite(a) => FiniteFunction::identity(a).into(),
            SemifiniteObject::Set(_) => Self::Identity,
        }
    }

    fn compose(&self, other: &Self) -> Option<Self> {
        // LHS must always be finite otherwise this is not composable.
        let Self::Finite(f) = self else {
            return None;
        };

        match other {
            Self::Finite(g) => (f >> g).map(Self::Finite),
            Self::Semifinite(g) => {
                compose_semifinite(f, g).map(Self::Semifinite)
                //(f >> g).map(|x| SemifiniteArrow::Semifinite(x))
            }
            _ => None, // Identity is only for types!
        }
    }
}

impl<K: ArrayKind, T> Coproduct for SemifiniteArrow<K, T>
where
    K::Type<T>: Array<K, T>,
{
    fn initial_object() -> Self::Object {
        SemifiniteObject::Finite(K::I::zero())
    }

    fn initial(_a: Self::Object) -> Self {
        todo!()
    }

    fn inj0(_a: Self::Object, _b: Self::Object) -> Self {
        todo!()
    }

    fn inj1(_a: Self::Object, _b: Self::Object) -> Self {
        todo!()
    }

    fn coproduct(&self, _other: &Self) -> Option<Self> {
        todo!()
    }
}

impl<K: ArrayKind, T> From<FiniteFunction<K>> for SemifiniteArrow<K, T> {
    fn from(val: FiniteFunction<K>) -> Self {
        Self::Finite(val)
    }
}

impl<K: ArrayKind, T> From<SemifiniteFunction<K, T>> for SemifiniteArrow<K, T> {
    fn from(val: SemifiniteFunction<K, T>) -> Self {
        Self::Semifinite(val)
    }
}

impl<K: ArrayKind, T> TryFrom<SemifiniteArrow<K, T>> for SemifiniteFunction<K, T> {
    type Error = ();

    fn try_from(value: SemifiniteArrow<K, T>) -> Result<Self, Self::Error> {
        match value {
            SemifiniteArrow::Semifinite(f) => Ok(f),
            _ => Err(()),
        }
    }
}
