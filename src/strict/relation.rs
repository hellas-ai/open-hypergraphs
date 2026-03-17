//! Indexed coproducts as relations
use crate::array::{Array, ArrayKind, NaturalArray, OrdArray};
use crate::finite_function::FiniteFunction;
use crate::indexed_coproduct::{HasLen, IndexedCoproduct};
use num_traits::{One, Zero};

/// Compute the *converse* of an [`IndexedCoproduct`] thought of as a "multirelation".
///
/// An [`IndexedCoproduct`] `c : Σ_{x ∈ X} s(x) → Q` can equivalently be thought of as `c : X →
/// Q*`, i.e. a mapping from X to finite lists of elements in Q.
///
/// Such a list defines a (multi-)relation as the multiset of pairs
///
/// `R = { ( x, f(x)_i ) | x ∈ X, i ∈ len(f(x)) }`
///
/// This function computes the *converse* of that relation as an indexed coproduct
/// `converse(c) : Q → X*`, or more precisely
/// `converse(c) : Σ_{q ∈ Q} s(q) → X`.
///
/// NOTE: An indexed coproduct does not uniquely represent a 'multirelation', since *order* of the
/// elements matters.
/// The result of this function is only unique up to permutation of the sublists.
pub fn converse<K: ArrayKind>(
    r: &IndexedCoproduct<K, FiniteFunction<K>>,
) -> IndexedCoproduct<K, FiniteFunction<K>>
where
    K::Type<K::I>: NaturalArray<K>,
{
    let values_table = {
        let arange = K::Index::arange(&K::I::zero(), &r.sources.len());
        let unsorted_values = r.sources.table.repeat(arange.get_range(..));
        unsorted_values.sort_by(&r.values.table)
    };

    let sources_table =
        (r.values.table.as_ref() as &K::Type<K::I>).bincount(r.values.target.clone());

    let sources = FiniteFunction::new(sources_table, r.values.table.len() + K::I::one()).unwrap();
    let values = FiniteFunction::new(values_table, r.len()).unwrap();

    IndexedCoproduct::new(sources, values).unwrap()
}
