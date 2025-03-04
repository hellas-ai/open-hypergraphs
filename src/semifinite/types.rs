use crate::array::*;
use crate::category::*;
use crate::finite_function::FiniteFunction;

use core::fmt::Debug;
use core::ops::{Add, Shr};
use num_traits::{One, Zero};

/// A function whose *source* is finite, but whose *target* may be non-finite.
/// This is really just an array!
pub struct SemifiniteFunction<K: ArrayKind, T>(pub K::Type<T>);

impl<K: ArrayKind, T> Clone for SemifiniteFunction<K, T>
where
    K::Type<T>: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<K: ArrayKind, T> SemifiniteFunction<K, T>
where
    K::Type<T>: Array<K, T>,
{
    pub fn new(x: K::Type<T>) -> Self {
        Self(x)
    }

    pub fn len(&self) -> K::I {
        self.0.len()
    }

    // An array of length 1, containing the element x.
    pub fn singleton(x: T) -> SemifiniteFunction<K, T> {
        SemifiniteFunction(K::Type::<T>::fill(x, K::I::one()))
    }

    pub fn coproduct(&self, other: &Self) -> Self {
        SemifiniteFunction(self.0.concatenate(&other.0))
    }
}

/// As a special case, a we can precompose a [`FiniteFunction`] with a [`SemifiniteFunction`].
/// This is also overloaded with the `>>` syntax.
pub fn compose_semifinite<K: ArrayKind, T>(
    lhs: &FiniteFunction<K>,
    rhs: &SemifiniteFunction<K, T>,
) -> Option<SemifiniteFunction<K, T>>
where
    K::Type<T>: Array<K, T>,
{
    if lhs.target() != rhs.0.len() {
        return None;
    }

    // TODO: had to reimplement composition of finite functions again.
    let table = rhs.0.gather(lhs.table.get_range(..));
    Some(SemifiniteFunction(table))
}

// NOTE: we can't derive PartialEq because it will introduce an unnecessary `T: PartialEq` bound
impl<K: ArrayKind, T> PartialEq<SemifiniteFunction<K, T>> for SemifiniteFunction<K, T>
where
    K::Type<T>: PartialEq,
{
    fn eq(&self, other: &SemifiniteFunction<K, T>) -> bool {
        self.0 == other.0
    }
}

impl<K: ArrayKind, T> Debug for SemifiniteFunction<K, T>
where
    K::Type<T>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("SemifiniteFunction").field(&self.0).finish()
    }
}

impl<K: ArrayKind, T> Add<&SemifiniteFunction<K, T>> for &SemifiniteFunction<K, T>
where
    K::Type<T>: Array<K, T>,
{
    type Output = Option<SemifiniteFunction<K, T>>; // NOTE: redundant Option, always succeeds

    fn add(self, rhs: &SemifiniteFunction<K, T>) -> Self::Output {
        Some(self.coproduct(rhs))
    }
}

impl<K: ArrayKind, T> Add<SemifiniteFunction<K, T>> for SemifiniteFunction<K, T>
where
    K::Type<T>: Array<K, T>,
{
    type Output = SemifiniteFunction<K, T>;

    fn add(self, rhs: Self) -> Self::Output {
        self.coproduct(&rhs)
    }
}

impl<K: ArrayKind, T> Zero for SemifiniteFunction<K, T>
where
    K::Type<T>: Array<K, T>,
{
    fn zero() -> Self {
        SemifiniteFunction(K::Type::<T>::empty())
    }

    fn is_zero(&self) -> bool {
        self.0.is_empty()
    }
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
