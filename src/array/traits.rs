//! The operations which an array type must support to implement open hypergraphs
use core::ops::{Bound, Range, RangeBounds};
use std::ops::Sub;

/// Array *kinds*.
/// For example, [`super::vec::VecKind`] is the set of types [`Vec<T>`] for all `T`.
pub trait ArrayKind {
    /// The type of arrays containing elements type T
    type Type<T>;
    /// The type of *index* elements. For [`super::vec::VecKind`], this is [`usize`].
    type I;

    /// a `Slice` is a read-only view into another array's data.
    /// For `VecKind` this is `&[T]`.
    type Slice<'a, T: 'a>; // part of an array
}

// Clamp a single [`usize`] bound `b` into the range `start <= b <= end`.
fn clamp_bound(b: Bound<usize>, start: usize, end: usize) -> usize {
    match b {
        Bound::Included(x) => x.clamp(start, end),
        Bound::Excluded(x) => (x - 1).clamp(start, end),
        Bound::Unbounded => end,
    }
}

/// Arrays of elements T for some [`ArrayKind`] `K`.
///
/// # Panics
///
/// Any operation using an index out of range for the given array will panic.
pub trait Array<K: ArrayKind, T>
where
    K::I: From<usize>,
{
    /// The empty array
    fn empty() -> Self;

    /// Length of an array
    fn len(&self) -> usize;

    /// Test if an array is empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Clamp any `R: RangeBounds<usize>` into the range of valid indices for this array.
    fn to_range<R: RangeBounds<usize>>(&self, r: R) -> Range<usize> {
        let start = clamp_bound(r.start_bound().cloned(), 0, self.len() - 1);
        let end = clamp_bound(r.end_bound().cloned(), 0, self.len() - 1);
        Range { start, end }
    }

    /// Concatenate two arrays
    fn concatenate(&self, other: &Self) -> Self;

    /// `fill(x, n)` returns the array length n containing repeated element x.
    fn fill(x: T, n: usize) -> Self;

    /// Retrieve a single element by its index.
    fn get(&self, i: &K::I) -> T;

    /// Get a contiguous range of the underlying array as a slice.
    fn get_range<R: RangeBounds<usize>>(&self, rb: R) -> K::Slice<'_, T>;

    /// Write to a contiguous range of data in an array
    fn set_range<R: RangeBounds<usize>>(&mut self, rb: R, v: &K::Type<T>); // mutate self

    /// Gather elements of this array according to the indices.
    /// <https://en.wikipedia.org/wiki/Gather/scatter_(vector_addressing)#Gather>
    /// ```rust
    /// x = y.gather(idx)  // x[i] = y[idx[i]]
    /// ```
    fn gather(&self, idx: K::Slice<'_, K::I>) -> Self;

    /// Scatter elements of array `x` into this array at indices `idx`
    /// ```rust
    /// self.scatter(idx, x)  // self[idx[i]] = x[i]
    /// ```
    fn scatter(&mut self, idx: K::Slice<'_, K::I>, x: &Self);
}

/// Arrays which also support numeric operations.
pub trait SemiringArray<K: ArrayKind, T>: Array<K, T> + Sized + Sub<Self, Output = Self>
where
    K::I: From<usize>,
{
    /// An inclusive-and-exclusive cumulative sum
    /// For an input of size `N`, returns an array `x` of size `N+1` where `x[0] = 0` and `x[-1] = sum(x)`
    fn cumulative_sum(&self) -> Self;
}

/// Arrays of indices, thought of as natural numbers.
pub trait NaturalArray<K: ArrayKind>: SemiringArray<K, K::I>
where
    // Required because we need to convert a sum of indices into a usize to pass to arange...
    K::I: From<usize>,
{
    /// Indices from start to stop
    fn arange(start: &K::I, stop: &K::I) -> Self;

    /// Repeat each element of the given slice.
    /// self and x must be equal lengths.
    fn repeat(&self, x: K::Slice<'_, K::I>) -> Self;

    /// Segmented sum of input.
    /// For example, for `self = [1 2 0]`,
    /// `self.segmented_sum([1 | 2 3]) = [1 5 0]`.
    fn segmented_sum<T, A: SemiringArray<K, T>>(&self, x: &A) -> A {
        let segment_sizes = self;
        let ptr = segment_sizes.cumulative_sum();
        let sum = x.cumulative_sum();

        let n = sum.len();

        // NOTE: we do two allocations for both `gather`s here, but avoiding this
        // would require complicating the API quite a bit!
        sum.gather(ptr.get_range(1..)) - sum.gather(ptr.get_range(..n - 1))
    }

    /// Given an array of *sizes* compute the concatenation of `arange` arrays of each size.
    /// For example,
    /// `[2 3 0 5] ⇒ [ 0 1 | 0 1 2 | | 0 1 2 3 4 ]`
    /// where `|` illustrates a "segment boundary"
    fn segmented_arange(&self) -> Self {
        // How this works, by example:
        //   input   = [ 2 3 0 5 ]
        //   output  = [ 0 1 | 0 1 2 | | 0 1 2 3 4 ]
        // compute ptrs
        //   p       = [ 0 2 5 5 ]
        //   r       = [ 0 0 | 2 2 2 | | 5 5 5 5 5 ]
        //   i       = [ 0 1   2 3 4     5 6 7 8 9 ]
        //   i - r   = [ 0 1 | 0 1 2 | | 0 1 2 3 4 ]
        // Note: r is computed as repeat(p, n)
        //
        // Complexity
        //   O(n)     sequential
        //   O(log n) PRAM CREW (cumsum is log n)
        let ptr = self.cumulative_sum();
        let sum = &ptr.get(&(ptr.len() - 1).into());
        let r = self.repeat(ptr.get_range(..ptr.len() - 1));
        Self::arange(&0.into(), sum) - r
    }
}
