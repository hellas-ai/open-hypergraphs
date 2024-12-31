use super::types::FiniteFunction;
use crate::array::{Array, ArrayKind};
use std::ops::Shr;

/// A function whose *source* is finite, but whose *target* may be non-finite.
#[derive(PartialEq, Eq)]
pub struct SemifiniteFunction<K: ArrayKind, T>(pub K::Type<T>);

/// A [`FiniteFunction`] can be precomposed with a [`SemifiniteFunction`] to re-index it.
impl<K: ArrayKind, T> Shr<&SemifiniteFunction<K, T>> for &FiniteFunction<K>
where
    K::Type<T>: Array<K, T>,
{
    type Output = Option<SemifiniteFunction<K, T>>;

    fn shr(self, other: &SemifiniteFunction<K, T>) -> Option<SemifiniteFunction<K, T>> {
        // TODO: used usize/K::I isomorphism
        if self.target != other.0.len().into() {
            return None;
        }

        // TODO: had to reimplement composition of finite functions again.
        let table = other.0.gather(self.table.get_range(..));
        Some(SemifiniteFunction(table))
    }
}
