//! The operations which an array type must support to implement open hypergraphs
use core::ops::{Add, Sub};
use core::ops::{Bound, Range, RangeBounds};
use num_traits::{One, Zero};

/// Array *kinds*.
/// For example, [`super::vec::VecKind`] is the set of types [`Vec<T>`] for all `T`.
pub trait ArrayKind: Sized {
    /// The type of arrays containing elements type T
    type Type<T>;

    /// The type of index *elements*. For [`super::vec::VecKind`], this is [`usize`].
    type I: Clone
        + PartialEq
        + Ord
        + One
        + Zero
        + Add<Output = Self::I>
        + Sub<Output = Self::I>
        // NOTE: this last constraint that an index can add with rhs an Index array is a hack that
        // lets us implement `tensor` for finite functions without unnecessary cloning.
        + for<'a> Add<&'a Self::Index, Output = Self::Index>;

    /// Arrays of indices (isomorphic to `Type<I>`) must implement NaturalArray
    type Index: NaturalArray<Self> + Into<Self::Type<Self::I>> + From<Self::Type<Self::I>>;

    /// a `Slice` is a read-only view into another array's data.
    /// For `VecKind` this is `&[T]`.
    type Slice<'a, T: 'a>; // part of an array
}

// Clamp a single bound `b` into the range `start <= b <= end`.
fn clamp_bound<I: Sub<Output = I> + One + Ord>(b: Bound<I>, start: I, end: I) -> I {
    match b {
        Bound::Included(x) => x.clamp(start, end),
        Bound::Excluded(x) => (x - I::one()).clamp(start, end),
        Bound::Unbounded => end,
    }
}

/// Arrays of elements T for some [`ArrayKind`] `K`.
///
/// # Panics
///
/// Any operation using an index out of range for the given array will panic.
pub trait Array<K: ArrayKind, T>: Clone + PartialEq<Self> {
    /// The empty array
    fn empty() -> Self;

    /// Length of an array
    fn len(&self) -> K::I;

    /// Test if an array is empty
    fn is_empty(&self) -> bool {
        self.len() == K::I::zero()
    }

    /// Clamp any `R: RangeBounds<K::I>` into the range of valid indices for this array.
    fn to_range<R: RangeBounds<K::I>>(&self, r: R) -> Range<K::I> {
        let z = K::I::zero();
        let n = self.len() - K::I::one();
        let start = clamp_bound(r.start_bound().cloned(), z.clone(), n.clone());
        let end = clamp_bound(r.end_bound().cloned(), z.clone(), n.clone());
        Range { start, end }
    }

    /// Concatenate two arrays
    fn concatenate(&self, other: &Self) -> Self;

    /// `fill(x, n)` returns the array length n containing repeated element x.
    fn fill(x: T, n: K::I) -> Self;

    /// Retrieve a single element by its index.
    fn get(&self, i: K::I) -> T;

    /// Get a contiguous range of the underlying array as a slice.
    fn get_range<R: RangeBounds<K::I>>(&self, rb: R) -> K::Slice<'_, T>;

    /// Write to a contiguous range of data in an array
    fn set_range<R: RangeBounds<K::I>>(&mut self, rb: R, v: &K::Type<T>); // mutate self

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

/// Arrays of natural numbers.
/// This is used for computing with *indexes* and *sizes*.
pub trait NaturalArray<K: ArrayKind>:
    Array<K, K::I> + Sized + Sub<Self, Output = Self> + AsRef<K::Index>
{
    /// An inclusive-and-exclusive cumulative sum
    /// For an input of size `N`, returns an array `x` of size `N+1` where `x[0] = 0` and `x[-1] = sum(x)`
    fn cumulative_sum(&self) -> Self;

    /// Indices from start to stop
    fn arange(start: &K::I, stop: &K::I) -> Self;

    /// Repeat each element of the given slice.
    /// self and x must be equal lengths.
    fn repeat(&self, x: K::Slice<'_, K::I>) -> Self;

    /// Segmented sum of input.
    /// For example, for `self = [1 2 0]`,
    /// `self.segmented_sum([1 | 2 3]) = [1 5 0]`.
    fn segmented_sum(&self, x: &Self) -> Self {
        let segment_sizes = self;
        let ptr = segment_sizes.cumulative_sum();
        let sum = x.cumulative_sum();

        let n = sum.len();

        // NOTE: we do two allocations for both `gather`s here, but avoiding this
        // would require complicating the API quite a bit!
        let one = K::I::one();
        sum.gather(ptr.get_range(one.clone()..)) - sum.gather(ptr.get_range(..n - one))
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
        let sum = &ptr.get(ptr.len() - K::I::one());
        let r = self.repeat(ptr.get_range(..ptr.len() - K::I::one()));
        Self::arange(&K::I::zero(), sum) - r
    }
}
