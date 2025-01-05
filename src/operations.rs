use crate::array::*;
use crate::indexed_coproduct::*;
use crate::semifinite::*;

/// Column-oriented storage of operations and their types.
pub struct Operations<K: ArrayKind, O, A> {
    x: SemifiniteFunction<K, A>,
    s: IndexedCoproduct<K, SemifiniteFunction<K, O>>,
    t: IndexedCoproduct<K, SemifiniteFunction<K, O>>,
}

impl<K: ArrayKind, O, A> Operations<K, O, A>
where
    K::Type<A>: Array<K, A>,
    K::Type<O>: Array<K, O>,
    K::Type<K::I>: NaturalArray<K>, // TODO: remove this bound; required only for singleton.
{
    pub fn new(
        x: SemifiniteFunction<K, A>,
        s: IndexedCoproduct<K, SemifiniteFunction<K, O>>,
        t: IndexedCoproduct<K, SemifiniteFunction<K, O>>,
    ) -> Option<Self> {
        todo!()
    }

    /// A single operation (or a tensoring of length 1)
    pub fn singleton(x: A, a: SemifiniteFunction<K, O>, b: SemifiniteFunction<K, O>) -> Self {
        Self {
            x: SemifiniteFunction::singleton(x),
            s: IndexedCoproduct::<K, SemifiniteFunction<K, O>>::singleton(a),
            t: IndexedCoproduct::<K, SemifiniteFunction<K, O>>::singleton(b),
        }
    }

    pub fn len(&self) -> K::I {
        self.x.len()
    }

    pub fn validate(&self) -> Option<Self> {
        todo!()
    }
}
