use crate::array::*;
use crate::category::*;
use crate::finite_function::semifinite::*;
use crate::finite_function::types::*;

pub struct IndexedCoproduct<K: ArrayKind, F: Coproduct> {
    pub sources: SemifiniteFunction<K, K::I>,
    pub values: F,
}
