///!
use crate::category::*;
use crate::finite_function::FiniteFunction;
use crate::lax::functor::*;
use crate::lax::var::*;
use crate::lax::*;

// TODO: ideally we could get the user to prove their A type is isomorphism to
// `Either VarLabel Other`, then use this to return an OpenHypergraph over the `Other` type.
// For example, if my labels are `Either Copy Signature`, then this functor should be able to
//
// However, this seems to require putting a closure in the struct, and that can't work

/// Forget the
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
            // Extra-special frobenius axiom: 0 → 0 Frobenius maps are the empty OpenHypergraph.
            // Follows from p2 https://arxiv.org/pdf/1601.02307 - the "extra law".
            if source.is_empty() && target.is_empty() {
                return OpenHypergraph::empty();
            }

            let s = FiniteFunction::terminal(source.len());
            let t = FiniteFunction::terminal(target.len());
            return OpenHypergraph::<O, A>::spider(s, t, vec![source[0].clone()]).unwrap();
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
