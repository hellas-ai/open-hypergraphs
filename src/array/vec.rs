//! [`Vec<T>`]-backed arrays
use crate::array::*;
use core::ops::{Add, Deref, DerefMut, Index, RangeBounds, Sub};

/// Arrays backed by a [`Vec<T>`].
#[derive(PartialEq, Eq, Debug)]
pub struct VecKind {}

impl ArrayKind for VecKind {
    type Type<T> = VecArray<T>;
    type I = usize;
    type Index = VecArray<usize>;

    // A Slice for Vec is just a rust slice
    type Slice<'a, T: 'a> = &'a [T];
}

/// A newtype wrapper for [`Vec<T>`] allowing pointwise arithmetic operations.
pub struct VecArray<T>(Vec<T>);

// VecArray is a newtype wrapper, so we can just treat it like a regular old Vec.
impl<T> Deref for VecArray<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for VecArray<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Clone> Array<VecKind, T> for VecArray<T> {
    fn empty() -> Self {
        VecArray(Vec::default())
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn concatenate(&self, other: &Self) -> Self {
        let mut result: Vec<T> = Vec::with_capacity(self.len() + other.len());
        result.extend_from_slice(self);
        result.extend_from_slice(other);
        VecArray(result)
    }

    fn fill(x: T, n: usize) -> Self {
        VecArray(vec![x; n])
    }

    fn get(&self, i: usize) -> T {
        self[i].clone()
    }

    fn get_range<R: RangeBounds<usize>>(&self, rb: R) -> &[T] {
        self.index(self.to_range(rb))
    }

    fn set_range<R: RangeBounds<usize>>(&mut self, rb: R, v: &<VecKind as ArrayKind>::Type<T>) {
        let r = self.to_range(rb);
        self[r].clone_from_slice(v)
    }

    fn gather(&self, idx: &[usize]) -> Self {
        VecArray(idx.iter().map(|i| self.0[*i].clone()).collect())
    }

    fn scatter(&mut self, idx: &[usize], v: &Self) {
        for i in idx {
            self[*i] = v[*i].clone();
        }
    }
}

impl Add<&VecArray<usize>> for usize {
    type Output = VecArray<usize>;

    fn add(self, rhs: &VecArray<usize>) -> Self::Output {
        VecArray(rhs.iter().map(|x| x + self).collect())
    }
}

impl<T: Clone + Sub<Output = T>> Sub<VecArray<T>> for VecArray<T> {
    type Output = VecArray<T>;

    fn sub(self, rhs: VecArray<T>) -> VecArray<T> {
        assert_eq!(self.len(), rhs.len());
        VecArray(
            self.iter()
                .zip(rhs.iter())
                .map(|(x, y)| x.clone() - y.clone())
                .collect(),
        )
    }
}

impl NaturalArray<VecKind> for VecArray<usize> {
    fn cumulative_sum(&self) -> Self {
        VecArray(
            self.iter()
                .scan(0, |acc, x| {
                    *acc += *x;
                    Some(*acc)
                })
                .collect(),
        )
    }

    fn arange(start: &usize, stop: &usize) -> Self {
        assert!(stop >= start);
        let n = stop - start;
        let mut v = Vec::with_capacity(n);
        for i in 0..n {
            v.push(start + i);
        }
        VecArray(v)
    }

    fn repeat(&self, x: &[usize]) -> VecArray<usize> {
        let mut v: Vec<usize> = Vec::new();
        for (k, xi) in self.iter().zip(x) {
            v.extend(std::iter::repeat(xi).take(*k))
        }
        VecArray(v)
    }
}
