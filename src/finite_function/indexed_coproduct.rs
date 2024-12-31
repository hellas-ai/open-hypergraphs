use crate::array::*;
use crate::category::*;
use crate::finite_function::semifinite::*;

pub struct IndexedCoproduct<K: ArrayKind, F: Coproduct> {
    pub sources: SemifiniteFunction<K, K::I>,
    pub values: F,
}
