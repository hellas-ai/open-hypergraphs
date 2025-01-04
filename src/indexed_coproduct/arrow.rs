use crate::array::*;
use crate::semifinite::*;

pub struct IndexedCoproduct<K: ArrayKind, F: Clone> {
    pub sources: SemifiniteFunction<K, K::I>,
    pub values: F,
}

impl<K: ArrayKind, F: Clone> Clone for IndexedCoproduct<K, F>
where
    K::Type<K::I>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            sources: self.sources.clone(),
            values: self.values.clone(),
        }
    }
}
