use crate::array::*;
use crate::finite_function::FiniteFunction;

use core::ops::{Add, Shr};
use num_traits::Zero;

/// A function whose *source* is finite, but whose *target* may be non-finite.
/// This is really just an array!
pub struct SemifiniteFunction<K: ArrayKind, T>(pub K::Type<T>);

impl<K: ArrayKind, T> SemifiniteFunction<K, T>
where
    K::Type<T>: Array<K, T>,
{
    pub fn len(&self) -> K::I {
        self.0.len()
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
    if lhs.target != rhs.0.len() {
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

impl<K: ArrayKind, T> Add for SemifiniteFunction<K, T>
where
    K::Type<T>: Array<K, T>,
{
    type Output = SemifiniteFunction<K, T>;

    fn add(self, rhs: Self) -> Self::Output {
        SemifiniteFunction(K::Type::<T>::concatenate(&self.0, &rhs.0))
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
