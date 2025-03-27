use crate::array::*;
use crate::category::*;

use core::fmt::Debug;
use core::ops::{Add, BitOr, Shr};
use num_traits::{One, Zero};

/// A finite function is an array of indices in a range `{0..N}` for some `N ∈ Nat`
#[derive(Eq)]
pub struct FiniteFunction<K: ArrayKind> {
    pub table: K::Index,
    pub target: K::I,
}

// Can't use derived PartialEq because it introduces unwanted bound `K: PartialEq`.
impl<K: ArrayKind> PartialEq for FiniteFunction<K> {
    fn eq(&self, other: &Self) -> bool {
        self.table == other.table && self.target == other.target
    }
}

// Ad-hoc methods for finite functions
impl<K: ArrayKind> FiniteFunction<K> {
    /// Construct a FiniteFunction from a table of indices
    pub fn new(table: K::Index, target: K::I) -> Option<Self> {
        // If table was nonempty and had a value larger or equal to codomain, this is invalid.
        // TODO: should check that table min is greater than zero!
        if let Some(true) = table.max().map(|m| m >= target) {
            return None;
        }
        Some(Self { table, target })
    }

    /// The length-`a` array of zeroes `!_a : a → 1`.
    /// TODO: doctest
    pub fn terminal(a: K::I) -> Self {
        let table = K::Index::fill(K::I::zero(), a);
        let target = K::I::one();
        Self { table, target }
    }

    /// Construct the constant finite function `f : a → x + 1 + b`,
    /// an array of length `a` mapping all elements to `x`.
    ///
    /// Note that the *target* of the finite function must be at least `x+1` to be valid.
    /// This is equivalent to `!_a ; ι₁ ; ι₀`, where
    ///
    /// - `!_a : a → 1` is the terminal map
    /// - `ι₁ : 1 → x+1` and `ι₀ : x+1 → x+1+b` are injections.
    ///
    /// ```rust
    /// # use open_hypergraphs::category::*;
    /// # use open_hypergraphs::array::vec::*;
    /// # use open_hypergraphs::finite_function::*;
    /// let (x, a, b) = (2, 3, 2);
    ///
    /// let actual = FiniteFunction::<VecKind>::constant(a, x, b);
    /// let expected = FiniteFunction::new(VecArray(vec![2, 2, 2]), x + b + 1).unwrap();
    /// assert_eq!(actual, expected);
    ///
    /// // Check equal to `!_a ; ι₁ ; ι₀`
    /// let i1 = FiniteFunction::<VecKind>::inj1(x, 1);
    /// let i0 = FiniteFunction::inj0(x + 1, b);
    /// let f  = FiniteFunction::terminal(a);
    /// let h  = f.compose(&i1).expect("b").compose(&i0).expect("c");
    /// assert_eq!(actual, h);
    /// ```
    pub fn constant(a: K::I, x: K::I, b: K::I) -> Self {
        let table = K::Index::fill(x.clone(), a);
        let target = x + b + K::I::one(); // We need the +1 to ensure entries in range.
        Self { table, target }
    }

    /// Directly construct `f ; ι₀` instead of computing by composition.
    ///
    /// ```rust
    /// # use open_hypergraphs::array::vec::*;
    /// # use open_hypergraphs::category::*;
    /// # use open_hypergraphs::finite_function::*;
    /// # let f = FiniteFunction::<VecKind>::identity(5);
    /// # let b = 3;
    /// # let i0 = FiniteFunction::<VecKind>::inj0(f.target(), b);
    /// assert_eq!(Some(f.inject0(b)), &f >> &i0);
    /// ```
    pub fn inject0(&self, b: K::I) -> FiniteFunction<K> {
        Self {
            table: self.table.clone(),
            target: b + self.target(),
        }
    }

    /// Directly construct `f ; ι₁` instead of computing by composition.
    ///
    /// ```rust
    /// # use open_hypergraphs::array::vec::*;
    /// # use open_hypergraphs::category::*;
    /// # use open_hypergraphs::finite_function::*;
    /// # let f = FiniteFunction::<VecKind>::identity(5);
    /// # let a = 3;
    /// # let i1 = FiniteFunction::<VecKind>::inj1(a, f.target());
    /// assert_eq!(Some(f.inject1(a)), &f >> &i1);
    /// ```
    pub fn inject1(&self, a: K::I) -> Self {
        Self {
            table: a.clone() + &self.table,
            target: a + self.target.clone(),
        }
    }

    /// Given a finite function `f : A → B`, return the initial map `initial : 0 → B`.
    pub fn to_initial(&self) -> Self {
        Self::initial(self.target.clone())
    }

    pub fn coequalizer(&self, other: &Self) -> Option<Self> {
        // if self is parallel to other
        if self.source() != other.source() || self.target() != other.target() {
            return None;
        }

        let (table, target) =
            K::Index::connected_components(&self.table, &other.table, self.target());
        Some(Self { table, target })
    }

    pub fn coequalizer_universal(&self, f: &Self) -> Option<Self>
    where
        K::Type<K::I>: Array<K, K::I>,
    {
        let table = coequalizer_universal(self, f.table.as_ref())?.into();
        let target = f.target();
        Some(FiniteFunction { table, target })
    }

    /// `transpose(a, b)` is the "transposition permutation" for an `a → b` matrix stored in
    /// row-major order.
    ///
    /// Let M be an `a*b`-dimensional input thought of as a matrix in row-major order -- having `b`
    /// rows and `a` columns.
    /// Then `transpose(a, b)` computes the "target indices" of the transpose.
    /// So for matrices `M : a → b` and `N : b → a`, setting the indices `N[transpose(a, b)] = M`
    /// is the same as writing `N = M.T`.
    pub fn transpose(a: K::I, b: K::I) -> Self {
        if a.is_zero() {
            return Self::initial(a);
        }

        let n = b.clone() * a.clone();
        let i = K::Index::arange(&K::I::zero(), &n);
        let (q, r) = i.quot_rem(a);
        Self {
            target: n,
            // r * b + q
            table: r.mul_constant_add(b, &q),
        }
    }

    /// Given a finite function `s : N → K`
    /// representing the objects of the finite coproduct
    /// `Σ_{n ∈ N} s(n)`
    /// whose injections have the type
    /// `ι_x : s(x) → Σ_{n ∈ N} s(n)`,
    /// and given a finite map
    /// `a : A → N`,
    /// compute the coproduct of injections
    /// ```text
    /// injections(s, a) : Σ_{x ∈ A} s(x) → Σ_{n ∈ N} s(n)
    /// injections(s, a) = Σ_{x ∈ A} ι_a(x)
    /// ```
    /// So that `injections(s, id) == id`
    ///
    /// Note that when a is a permutation,
    /// injections(s, a) is a "blockwise" version of that permutation with block
    /// sizes equal to s.
    /// """
    pub fn injections(&self, a: &Self) -> Option<Self> {
        let s = self;
        let p = self.table.cumulative_sum();

        // TODO: better errors!
        let k = (a >> s)?;
        let r = k.table.segmented_arange();

        let repeats = k.table;
        let values = p.gather(a.table.get_range(..));
        let z = repeats.repeat(values.get_range(..));

        Some(Self {
            table: r + z,
            target: p.get(p.len() - K::I::one()),
        })
    }

    /// Given a finite function `f : A → B`, compute the cumulative sum of `f`, a finite function
    /// `cumulative_sum(f) : A → sum_i(f())`
    ///
    /// ```rust
    /// # use open_hypergraphs::category::*;
    /// # use open_hypergraphs::array::vec::*;
    /// # use open_hypergraphs::finite_function::*;
    /// let f = FiniteFunction::<VecKind>::new(VecArray(vec![3, 0, 1, 4]), 5).unwrap();
    /// let c = f.cumulative_sum();
    /// assert_eq!(c.table, VecArray(vec![0, 3, 3, 4]));
    /// assert_eq!(c.target(), 8);
    ///
    /// let f = FiniteFunction::<VecKind>::new(VecArray(vec![]), 5).unwrap();
    /// let c = f.cumulative_sum();
    /// assert_eq!(c.table, VecArray(vec![]));
    /// assert_eq!(c.target(), 0);
    /// ```
    pub fn cumulative_sum(&self) -> Self {
        let extended_table = self.table.cumulative_sum();
        let target = extended_table.get(self.source());
        let table = Array::from_slice(extended_table.get_range(..self.source()));
        Self { table, target }
    }
}

/// Compute the universal map for a coequalizer `q : B → Q` and arrow `f : B → T`, generalised to
/// the case where `T` is an arbitrary set (i.e., `f` is an array of `T`)
pub fn coequalizer_universal<K: ArrayKind, T>(
    q: &FiniteFunction<K>,
    f: &K::Type<T>,
) -> Option<K::Type<T>>
where
    K::Type<T>: Array<K, T>,
{
    if q.source() != f.len() {
        return None;
    }

    // Compute table by scattering
    let table = f.scatter(q.table.get_range(..), q.target());

    // TODO: FIXME: we only need SemifiniteFunction to check this is a coequalizer;
    // we use the >> implementation to check, which is implemented for SemifiniteFunction
    use crate::semifinite::SemifiniteFunction;
    let u = SemifiniteFunction(table);

    // NOTE: we expect() here because composition is *defined* for self and u by construction;
    // if it panics, there is a library bug.
    let f_prime = (q >> &u).expect("by construction");
    if f_prime.0 == *f {
        Some(u.0)
    } else {
        None
    }
}

impl<K: ArrayKind> Arrow for FiniteFunction<K> {
    type Object = K::I;

    fn source(&self) -> Self::Object {
        self.table.len()
    }

    fn target(&self) -> Self::Object {
        self.target.clone()
    }

    fn identity(a: Self::Object) -> Self {
        let table = K::Index::arange(&K::I::zero(), &a);
        let target = a.clone();
        Self { table, target }
    }

    fn compose(&self, other: &Self) -> Option<Self> {
        if self.target == other.source() {
            let table = other.table.gather(self.table.get_range(..));
            let target = other.target.clone();
            Some(Self { table, target })
        } else {
            None
        }
    }
}

impl<K: ArrayKind> Coproduct for FiniteFunction<K> {
    fn initial_object() -> Self::Object {
        K::I::zero()
    }

    fn initial(a: Self::Object) -> Self {
        Self {
            table: K::Index::empty(),
            target: a.clone(),
        }
    }

    fn coproduct(&self, other: &Self) -> Option<Self> {
        if self.target != other.target {
            return None;
        }

        Some(Self {
            table: self.table.concatenate(&other.table),
            target: self.target.clone(),
        })
    }

    /// Coproduct injection 0.
    ///
    /// As an array, the indices `0..a`
    fn inj0(a: Self::Object, b: Self::Object) -> Self {
        let table = K::Index::arange(&K::I::zero(), &a);
        let target = a.clone() + b.clone();
        Self { table, target }
    }

    /// Coproduct injection 1.
    ///
    /// As an array, the indices `a..(a+b)`
    ///
    /// ```rust
    /// # use open_hypergraphs::category::*;
    /// # use open_hypergraphs::finite_function::*;
    /// # use open_hypergraphs::array::vec::*;
    /// assert_eq!(
    ///     FiniteFunction::<VecKind>::inj1(3, 5).table,
    ///     VecArray(vec![3,4,5,6,7]),
    ///     )
    /// ```
    fn inj1(a: Self::Object, b: Self::Object) -> Self {
        let target = a.clone() + b.clone();
        let table = K::Index::arange(&a, &target);
        Self { table, target }
    }
}

impl<K: ArrayKind> Monoidal for FiniteFunction<K> {
    // the unit object
    fn unit() -> Self::Object {
        K::I::zero()
    }

    fn tensor(&self, other: &Self) -> Self {
        // NOTE: this uses the `Add<&K::Index>` bound on `K::I` to compute offset piece without
        // unnecessary cloning.
        let table = self
            .table
            .concatenate(&(self.target.clone() + &other.table));
        let target = self.target.clone() + other.target.clone();
        Self { table, target }
    }
}

impl<K: ArrayKind> SymmetricMonoidal for FiniteFunction<K> {
    fn twist(a: K::I, b: K::I) -> Self {
        // This is more efficiently expressed as arange + b `mod` target,
        // but this would require adding an operation add_mod(a, b, n) to the array trait.
        let target = a.clone() + b.clone();
        let lhs = K::Index::arange(&b, &target);
        let rhs = K::Index::arange(&K::I::zero(), &b);
        let table = lhs.concatenate(&rhs);
        Self { table, target }
    }
}

// Syntactic sugar for finite function composition
impl<K: ArrayKind> Shr<&FiniteFunction<K>> for &FiniteFunction<K> {
    type Output = Option<FiniteFunction<K>>;

    fn shr(self, rhs: &FiniteFunction<K>) -> Option<FiniteFunction<K>> {
        self.compose(rhs)
    }
}

// Sugar for coproduct
impl<K: ArrayKind> Add<&FiniteFunction<K>> for &FiniteFunction<K> {
    type Output = Option<FiniteFunction<K>>;

    fn add(self, rhs: &FiniteFunction<K>) -> Option<FiniteFunction<K>> {
        self.coproduct(rhs)
    }
}

// Tensor product (parallel composition)
impl<K: ArrayKind> BitOr<&FiniteFunction<K>> for &FiniteFunction<K> {
    type Output = FiniteFunction<K>;
    fn bitor(self, rhs: &FiniteFunction<K>) -> FiniteFunction<K> {
        self.tensor(rhs)
    }
}

impl<K: ArrayKind> Clone for FiniteFunction<K> {
    fn clone(&self) -> Self {
        Self {
            table: self.table.clone(),
            target: self.target.clone(),
        }
    }
}

impl<K: ArrayKind> Debug for FiniteFunction<K>
where
    K::Index: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FiniteFunction")
            .field("table", &self.table)
            .field("target", &self.target)
            .finish()
    }
}
