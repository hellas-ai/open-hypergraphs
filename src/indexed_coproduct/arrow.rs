use crate::array::*;
use crate::semifinite::*;

pub struct IndexedCoproduct<K: ArrayKind, F> {
    pub sources: SemifiniteFunction<K, K::I>,
    pub values: F,
}
