//! # Forgetful functors for `Var`.
//!
//! The [`Forget`] functor "forgets" operations created by the [`Var`] interface while preserving connectivity.
//! Concretely, any operation labeled [`HasVar::var`] is mapped to a *spider*:
//! An [`OpenHypergraph`] with the same interfaces, but whose hypergraph is a single node. If the
//! operation has no sources and targets, it's mapped to the empty [`OpenHypergraph`].
use crate::category::*;
use crate::finite_function::FiniteFunction;
use crate::lax::functor::*;
use crate::lax::var::*;
use crate::lax::*;

/// Replace operations labeled `HasVar::var()` with spiders, or the empty diagram.
pub fn forget<
    O: Clone + PartialEq + std::fmt::Debug,
    A: Clone + PartialEq + HasVar + std::fmt::Debug,
>(
    f: &OpenHypergraph<O, A>,
) -> OpenHypergraph<O, A> {
    Forget.map_arrow(f)
}

/// Replace `1 → 1` operations labeled `HasVar::var()` with spiders, or the empty diagram.
pub fn forget_monogamous<
    O: Clone + PartialEq + std::fmt::Debug,
    A: Clone + PartialEq + HasVar + std::fmt::Debug,
>(
    f: &OpenHypergraph<O, A>,
) -> OpenHypergraph<O, A> {
    ForgetMonogamous.map_arrow(f)
}

// TODO: ideally we could get the user to prove their A type is isomorphism to
// `Either VarLabel Other`, then use this to return an OpenHypergraph over the `Other` type.
// However, we need Clone, and this would seem to require putting a closure in the Functor struct.
// This can be worked around by using `Hypergraph::map_nodes`.

/// Functor to replace operations labeled `HasVar::var()` with spiders, or the empty diagram.
///
/// More precisely: if an operation labeled `HasVar::var()` has `m` sources and `n` targets, all labeled `a`,
/// then it will be replaced with a `m → 1 ← n` spider, with all nodes labeled `a`.
/// If `m = 0 = n`, the operation will be replaced by the *empty* diagram:
/// this corresponds to the Extra-special frobenius axiom, which says that 0 → 0 Frobenius maps are
/// the empty OpenHypergraph.
/// See p2 <https://arxiv.org/pdf/1601.02307> - the "extra law".
#[derive(Clone)]
pub struct Forget;

impl<O: Clone + PartialEq, A: HasVar + Clone + PartialEq> Functor<O, A, O, A> for Forget {
    // Identity-on-objects
    fn map_object(&self, o: &O) -> impl ExactSizeIterator<Item = O> {
        std::iter::once(o.clone())
    }

    fn map_operation(&self, a: &A, source: &[O], target: &[O]) -> OpenHypergraph<O, A> {
        // Eliminate var-labeled operations which have all their sources + targets the same type.
        if *a == HasVar::var() && all_elements_equal(source, target) {
            // Extra-special frobenius axiom: 0 → 0 copy is the empty diagram
            if source.is_empty() && target.is_empty() {
                return OpenHypergraph::empty();
            }

            // At least one must have a value
            let label = {
                if source.is_empty() {
                    target[0].clone()
                } else {
                    source[0].clone()
                }
            };

            let s = FiniteFunction::terminal(source.len());
            let t = FiniteFunction::terminal(target.len());

            return OpenHypergraph::<O, A>::spider(s, t, vec![label]).unwrap();
        }
        OpenHypergraph::singleton(a.clone(), source.to_vec(), target.to_vec())
    }

    fn map_arrow(&self, f: &OpenHypergraph<O, A>) -> OpenHypergraph<O, A> {
        define_map_arrow(self, f)
    }
}

// Are all elements of both lists equal? (not pairwise- equivalent to reduce(equal, concat(s, t)))
fn all_elements_equal<T: PartialEq>(a: &[T], b: &[T]) -> bool {
    a.iter()
        .chain(b.iter())
        .all(|x| *x == *a.first().unwrap_or(x))
}

// not public: no use for this except via forget_monogamous
#[derive(Clone)]
struct ForgetMonogamous;

impl<O: Clone + PartialEq + std::fmt::Debug, A: HasVar + Clone + PartialEq + std::fmt::Debug>
    Functor<O, A, O, A> for ForgetMonogamous
{
    // Identity-on-objects
    fn map_object(&self, o: &O) -> impl ExactSizeIterator<Item = O> {
        std::iter::once(o.clone())
    }

    fn map_operation(&self, a: &A, source: &[O], target: &[O]) -> OpenHypergraph<O, A> {
        if source.len() != 1 || target.len() != 1 {
            return OpenHypergraph::singleton(a.clone(), source.to_vec(), target.to_vec());
        }

        // Eliminate var-labeled operations which have all their sources + targets the same type.
        if *a == HasVar::var() && all_elements_equal(source, target) {
            // Extra-special frobenius axiom: 0 → 0 copy is the empty diagram
            if source.is_empty() && target.is_empty() {
                return OpenHypergraph::empty();
            }

            // At least one must have a value
            let label = {
                if source.is_empty() {
                    target[0].clone()
                } else {
                    source[0].clone()
                }
            };

            let s = FiniteFunction::terminal(source.len());
            let t = FiniteFunction::terminal(target.len());

            return OpenHypergraph::<O, A>::spider(s, t, vec![label]).unwrap();
        }
        OpenHypergraph::singleton(a.clone(), source.to_vec(), target.to_vec())
    }

    fn map_arrow(&self, f: &OpenHypergraph<O, A>) -> OpenHypergraph<O, A> {
        define_map_arrow(self, f)
    }
}
