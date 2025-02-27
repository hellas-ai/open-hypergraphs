//! Tensorings of operations
use crate::array::*;
use crate::indexed_coproduct::*;
use crate::semifinite::*;

/// Column-oriented storage of operations and their types.
#[non_exhaustive] // force construction via new
pub struct Operations<K: ArrayKind, O, A> {
    pub x: SemifiniteFunction<K, A>,
    pub a: IndexedCoproduct<K, SemifiniteFunction<K, O>>,
    pub b: IndexedCoproduct<K, SemifiniteFunction<K, O>>,
}

impl<K: ArrayKind, O, A> Operations<K, O, A>
where
    K::Type<A>: Array<K, A>,
    K::Type<O>: Array<K, O>,
    K::Type<K::I>: NaturalArray<K>, // TODO: can we remove this bound? required only for singleton.
{
    /// Safely create a list of operations which is guaranteed to have the right number of source
    /// and target types for each operation.
    pub fn new(
        x: SemifiniteFunction<K, A>,
        a: IndexedCoproduct<K, SemifiniteFunction<K, O>>,
        b: IndexedCoproduct<K, SemifiniteFunction<K, O>>,
    ) -> Option<Self> {
        (Operations { x, a, b }).validate()
    }

    /// Ensure this list of operations has a source and target type for each operation label.
    pub fn validate(self) -> Option<Self> {
        let n = self.x.len();
        if n != self.a.len() || n != self.b.len() {
            None
        } else {
            Some(self)
        }
    }

    /// A single operation (or a tensoring of length 1)
    pub fn singleton(x: A, a: SemifiniteFunction<K, O>, b: SemifiniteFunction<K, O>) -> Self {
        Self {
            x: SemifiniteFunction::singleton(x),
            a: IndexedCoproduct::<K, SemifiniteFunction<K, O>>::singleton(a),
            b: IndexedCoproduct::<K, SemifiniteFunction<K, O>>::singleton(b),
        }
    }

    // TODO: use `indexed_coproduct::HasLen`?
    pub fn len(&self) -> K::I {
        self.x.len()
    }
}

impl<K: ArrayKind, O, A> std::fmt::Debug for Operations<K, O, A>
where
    K::Type<A>: std::fmt::Debug,
    K::Type<O>: std::fmt::Debug,
    K::Index: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Operations")
            .field("x", &self.x)
            .field("a", &self.a)
            .field("b", &self.b)
            .finish()
    }
}

impl<K: ArrayKind, O, A> Clone for Operations<K, O, A>
where
    K::Type<A>: Array<K, A>,
    K::Type<O>: Array<K, O>,
    K::Type<K::I>: NaturalArray<K>,
{
    fn clone(&self) -> Self {
        Self {
            x: self.x.clone(),
            a: self.a.clone(),
            b: self.b.clone(),
        }
    }
}
