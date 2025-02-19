//! The operations which an array type must support to implement open hypergraphs
use core::fmt::Debug;
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
        + Debug
        + One
        + Zero
        + Add<Output = Self::I>
        + Sub<Output = Self::I>
        // NOTE: this last constraint that an index can add with rhs an Index array is a hack that
        // lets us implement `tensor` for finite functions without unnecessary cloning.
        + for<'a> Add<&'a Self::Index, Output = Self::Index>;

    /// Arrays of indices (isomorphic to `Type<I>`) must implement NaturalArray
    type Index: NaturalArray<Self>
        + Into<Self::Type<Self::I>>
        + From<Self::Type<Self::I>>
        + AsRef<Self::Type<Self::I>>;

    /// a `Slice` is a read-only view into another array's data.
    /// For `VecKind` this is `&[T]`.
    type Slice<'a, T: 'a>; // part of an array
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

    fn from_slice<'a>(slice: K::Slice<'a, T>) -> Self;

    /// Clamp any `R: RangeBounds<K::I>` into the range of valid indices for this array.
    fn to_range<R: RangeBounds<K::I>>(&self, r: R) -> Range<K::I> {
        let n = self.len();
        let start = match r.start_bound().cloned() {
            Bound::Included(i) => i,
            Bound::Excluded(i) => i + K::I::one(),
            Bound::Unbounded => K::I::zero(),
        };

        // NOTE: Range is *exclusive* of end, so for Included(i) we need to increment!.
        let end = match r.end_bound().cloned() {
            Bound::Included(i) => i + K::I::one(),
            Bound::Excluded(i) => i,
            Bound::Unbounded => n,
        };

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
    /// ```text
    /// x = self.gather(idx) // equivalent to x[i] = self[idx[i]]
    /// ```
    fn gather(&self, idx: K::Slice<'_, K::I>) -> Self;

    /// Scatter elements of `self` into a new array at indices `idx`.
    /// ```text
    /// x = self.scatter(idx) // equivalent to x[idx[i]] = self[i]
    /// ```
    ///
    /// # Panics
    ///
    /// If there is any `i ≥ n` in `idx`
    fn scatter(&self, idx: K::Slice<'_, K::I>, n: K::I) -> Self;
}

pub trait OrdArray<K: ArrayKind, T>: Clone + PartialEq<Self> + Array<K, T> {
    /// Produce an array of indices which sorts `self`.
    /// That is, `self.gather(self.argsort())` is monotonic.
    fn argsort(&self) -> K::Index;

    /// Sort this array by the given keys
    ///
    /// ```rust
    /// use open_hypergraphs::array::{*, vec::*};
    /// let values = VecArray(vec![10, 20, 30, 40]);
    /// let keys = VecArray(vec![3, 1, 0, 2]);
    /// let expected = VecArray(vec![30, 20, 40, 10]);
    /// let actual = values.sort_by(&keys);
    /// assert_eq!(expected, actual);
    /// ```
    fn sort_by(&self, key: &Self) -> Self {
        self.gather(key.argsort().get_range(..))
    }
}

/// Arrays of natural numbers.
/// This is used for computing with *indexes* and *sizes*.
pub trait NaturalArray<K: ArrayKind>:
    OrdArray<K, K::I> + Sized + Sub<Self, Output = Self> + Add<Self, Output = Self> + AsRef<K::Index>
{
    fn max(&self) -> Option<K::I>;

    /// An inclusive-and-exclusive cumulative sum
    /// For an input of size `N`, returns an array `x` of size `N+1` where `x[0] = 0` and `x[-1] = sum(x)`
    fn cumulative_sum(&self) -> Self;

    // NOTE: we can potentially remove this if IndexedCoproduct moves to using pointers instead of
    // segment sizes.
    #[must_use]
    fn sum(&self) -> K::I {
        if self.len() == K::I::zero() {
            K::I::zero()
        } else {
            self.cumulative_sum().get(self.len())
        }
    }

    /// Indices from start to stop
    ///
    /// ```rust
    /// use open_hypergraphs::array::{*, vec::*};
    /// let x0 = VecArray::arange(&0, &3);
    /// assert_eq!(x0, VecArray(vec![0, 1, 2]));
    ///
    /// let x1 = VecArray::arange(&0, &0);
    /// assert_eq!(x1, VecArray(vec![]));
    /// ```
    fn arange(start: &K::I, stop: &K::I) -> Self;

    /// Repeat each element of the given slice.
    /// self and x must be equal lengths.
    fn repeat(&self, x: K::Slice<'_, K::I>) -> Self;

    /// Compute the arrays (self%denominator, self/denominator)
    ///
    /// # Panics
    ///
    /// When d == 0.
    fn quot_rem(&self, d: K::I) -> (Self, Self);

    /// Compute `self * c + x`, where `c` is a constant (scalar) and `x` is an array.
    ///
    /// # Panics
    ///
    /// When self.len() != x.len().
    fn mul_constant_add(&self, c: K::I, x: &Self) -> Self;

    /// Compute the connected components of a graph with `n` nodes.
    /// Edges are stored as a pair of arrays of nodes `(sources, targets)`
    /// meaning that for each `i` there is an edge `sources[i] → targets[i]`.
    ///
    /// Since `n` is the number of nodes in the graph, the values in `sources` and `targets` must
    /// be less than `n`.
    ///
    /// # Returns
    ///
    /// Returns a pair `(cc_ix, k)`, where `cc_ix[i]` is the connected component for the `i`th
    /// node, and `k` is the total number of components.
    ///
    /// # Panics
    ///
    /// * Inequal lengths: `sources.len() != targets.len()`
    /// * Indexes are out of bounds: `sources[i] >= n` or `targets[i] >= n`.
    fn connected_components(sources: &Self, targets: &Self, n: K::I) -> (Self, K::I);

    /// Segmented sum of input.
    /// For example, for `self = [1 2 0]`,
    /// `self.segmented_sum([1 | 2 3]) = [1 5 0]`.
    ///
    /// # Panics
    ///
    /// When `self.sum() != x.len()`
    fn segmented_sum(&self, x: &Self) -> Self {
        let segment_sizes = self;

        // cumulative sum of segments, including total size (last element)
        // [ 2 4 ] → [ 0 2 6 ]
        let ptr = segment_sizes.cumulative_sum();

        // Cumulative sum of values
        // [ 1 2 3 4 5 6 ] → [ 0 1 3 6 10 15 21 ]
        let sum = x.cumulative_sum();

        // total number of pointers (num segments + 1)
        let n = ptr.len();

        // NOTE: we do two allocations for both `gather`s here, but avoiding this
        // would require complicating the API quite a bit!
        sum.gather(ptr.get_range(K::I::one()..)) - sum.gather(ptr.get_range(..n - K::I::one()))
    }

    /// Given an array of *sizes* compute the concatenation of `arange` arrays of each size.
    ///
    /// ```rust
    /// use open_hypergraphs::array::{*, vec::*};
    /// let x = VecArray::<usize>(vec![2, 3, 0, 5]);
    /// let y = VecArray::<usize>(vec![0, 1, 0, 1, 2, 0, 1, 2, 3, 4]);
    /// assert_eq!(x.segmented_arange(), y)
    /// ```
    ///
    /// Default implementation has time complexity:
    ///
    /// - Sequential: `O(n)`
    /// - PRAM CREW: `O(log n)`
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
        let p = self.cumulative_sum();
        let last_idx = p.len() - K::I::one();
        let sum = p.get(last_idx.clone());

        let r = self.repeat(p.get_range(..last_idx));
        let i = Self::arange(&K::I::zero(), &sum);
        i - r
    }
}
