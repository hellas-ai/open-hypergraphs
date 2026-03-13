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
    pub fn new(table: K::Index, target: K::I) -> Option<FiniteFunction<K>> {
        // If table was nonempty and had a value larger or equal to codomain, this is invalid.
        // TODO: should check that table min is greater than zero!
        if let Some(true) = table.max().map(|m| m >= target) {
            return None;
        }
        Some(FiniteFunction { table, target })
    }

    /// The length-`a` array of zeroes `!_a : a → 1`.
    /// TODO: doctest
    pub fn terminal(a: K::I) -> Self {
        let table = K::Index::fill(K::I::zero(), a);
        let target = K::I::one();
        FiniteFunction { table, target }
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
        FiniteFunction { table, target }
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
        FiniteFunction {
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
    pub fn inject1(&self, a: K::I) -> FiniteFunction<K> {
        FiniteFunction {
            table: a.clone() + &self.table,
            target: a + self.target.clone(),
        }
    }

    /// Given a finite function `f : A → B`, return the initial map `initial : 0 → B`.
    pub fn to_initial(&self) -> FiniteFunction<K> {
        Self::initial(self.target.clone())
    }

    pub fn coequalizer(&self, other: &Self) -> Option<FiniteFunction<K>> {
        // if self is parallel to other
        if self.source() != other.source() || self.target() != other.target() {
            return None;
        }

        let (table, target) =
            K::Index::connected_components(&self.table, &other.table, self.target());
        Some(FiniteFunction { table, target })
    }

    pub fn coequalizer_universal(&self, f: &Self) -> Option<Self>
    where
        K::Type<K::I>: Array<K, K::I> + PartialEq,
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
    pub fn transpose(a: K::I, b: K::I) -> FiniteFunction<K> {
        if a.is_zero() {
            return Self::initial(a);
        }

        let n = b.clone() * a.clone();
        let i = K::Index::arange(&K::I::zero(), &n);
        let (q, r) = i.quot_rem(a);
        FiniteFunction {
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
    pub fn injections(&self, a: &FiniteFunction<K>) -> Option<Self> {
        let s = self;
        let p = self.table.cumulative_sum();

        // TODO: better errors!
        let k = (a >> s)?;
        let r = k.table.segmented_arange();

        let repeats = k.table;
        let values = p.gather(a.table.get_range(..));
        let z = repeats.repeat(values.get_range(..));

        Some(FiniteFunction {
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
        FiniteFunction { table, target }
    }
}

impl<K: ArrayKind> FiniteFunction<K>
where
    K::Type<K::I>: NaturalArray<K>,
{
    /// Check if this finite function is injective.
    pub fn is_injective(&self) -> bool {
        if self.source().is_zero() {
            return true;
        }

        let counts = self.table.bincount(self.target.clone());
        counts.max().map_or(true, |m| m <= K::I::one())
    }

    /// Check whether `self` and `other` have disjoint images in a common codomain.
    ///
    /// Domains may differ. Returns `true` exactly when
    /// `image(self) ∩ image(other) = ∅`.
    ///
    /// Returns `false` when codomains differ.
    pub fn has_disjoint_image(&self, other: &Self) -> bool {
        if self.target != other.target {
            return false;
        }

        let mut seen = K::Index::fill(K::I::zero(), self.target.clone());
        if !self.table.is_empty() {
            seen.scatter_assign_constant(&self.table, K::I::one());
        }

        let other_seen = seen.gather(other.table.get_range(..));
        other_seen.max().is_none_or(|m| m < K::I::one())
    }

    /// Build the canonical injection of the complement of `image(self)` in the codomain.
    ///
    /// For `self : A -> B`, returns `k : (B \\ image(self)) -> B`.
    /// The domain may be empty.
    pub(crate) fn image_complement_injection(&self) -> Option<Self> {
        let mut marker = K::Index::fill(K::I::zero(), self.target.clone());
        if !self.table.is_empty() {
            marker.scatter_assign_constant(&self.table, K::I::one());
        }
        let kept_ix = marker.zero();
        FiniteFunction::new(kept_ix, self.target.clone())
    }

    /// Build the canonical injection of `image(self)` into the codomain.
    ///
    /// For `self : A -> B`, returns `i : image(self) -> B`.
    pub(crate) fn canonical_image_injection(&self) -> Option<Self> {
        let (unique, _) = self.table.sparse_bincount();
        FiniteFunction::new(unique, self.target.clone())
    }

    /// Build the coproduct of parallel maps into a common codomain.
    ///
    /// For parallel maps `self, f_i : A_i -> B`, returns
    /// `[self, f_1, ..., f_n] : A + A_1 + ... + A_n -> B`.
    pub(crate) fn coproduct_many(&self, others: &[&Self]) -> Option<Self> {
        let target = self.target.clone();
        for m in others {
            if m.target != target {
                return None;
            }
        }

        let mut tables = Vec::<&K::Index>::with_capacity(others.len() + 1);
        tables.push(&self.table);
        for m in others {
            tables.push(&m.table);
        }

        Some(FiniteFunction {
            table: K::Index::concatenate_many(&tables),
            target,
        })
    }

    /// Build a total inverse for an injective map by choosing a fill value outside its image.
    ///
    /// For `f : A -> B` injective, returns `f_inv : B -> A` such that:
    /// - `f_inv ; f = id_A` on `image(f)` (left-inverse property),
    /// - for `b` not in `image(f)`, `f_inv(b) = fill`.
    ///
    /// This is useful when callers only apply `f_inv` to values known to be in `image(f)`,
    /// while still requiring a total finite function at the type level.
    ///
    /// Returns `None` when:
    /// - `self` is not injective, or
    /// - `fill` is out of bounds for `A`, or
    /// - `A` is empty and `B` is non-empty (no total map `B -> A` exists).
    pub(crate) fn inverse_with_fill(&self, fill: K::I) -> Option<Self> {
        if !self.is_injective() {
            return None;
        }

        if self.source().is_zero() {
            if self.target().is_zero() {
                return Some(FiniteFunction {
                    table: K::Index::empty(),
                    target: K::I::zero(),
                });
            }
            return None;
        }

        if fill >= self.source() {
            return None;
        }

        let mut inverse = K::Index::fill(fill, self.target());
        let values = K::Index::arange(&K::I::zero(), &self.source());
        inverse.scatter_assign(&self.table, values);
        Some(FiniteFunction {
            table: inverse,
            target: self.source(),
        })
    }

    /// Factor `self : A -> C` through an injective map `inj : B -> C`.
    ///
    /// Returns the unique `g : A -> B` such that `self = g ; inj`.
    ///
    pub(crate) fn factor_through_injective(&self, inj: &Self) -> Self {
        assert_eq!(
            self.target(),
            inj.target(),
            "factor_through_injective requires parallel maps"
        );
        assert!(
            inj.is_injective(),
            "factor_through_injective requires an injective map"
        );

        // Build a left-inverse table on `inj`'s image and reindex `self`.
        let values: K::Type<K::I> = K::Index::arange(&K::I::zero(), &inj.source()).into();
        let inverse: K::Type<K::I> = values.scatter(inj.table.get_range(..), inj.target());
        let table: K::Index = inverse.gather(self.table.get_range(..)).into();
        let factored = FiniteFunction {
            table,
            target: inj.source(),
        };

        let recomposed = factored
            .compose(inj)
            .expect("factor_through_injective: invalid codomain after factoring");
        assert!(
            recomposed == *self,
            "factor_through_injective requires image(self) subset image(inj)"
        );

        factored
    }
}

/// Compute the universal map for a coequalizer `q : B → Q` and arrow `f : B → T`, generalised to
/// the case where `T` is an arbitrary set (i.e., `f` is an array of `T`)
pub fn coequalizer_universal<K: ArrayKind, T>(
    q: &FiniteFunction<K>,
    f: &K::Type<T>,
) -> Option<K::Type<T>>
where
    K::Type<T>: Array<K, T> + PartialEq,
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
        FiniteFunction { table, target }
    }

    fn compose(&self, other: &Self) -> Option<Self> {
        if self.target == other.source() {
            let table = other.table.gather(self.table.get_range(..));
            let target = other.target.clone();
            Some(FiniteFunction { table, target })
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

#[cfg(test)]
mod tests {
    use super::FiniteFunction;
    use crate::array::vec::{VecArray, VecKind};
    use crate::category::Arrow;
    use proptest::prelude::{Just, Strategy};
    use proptest::{prop_assert, prop_assert_eq, proptest};

    fn ff(table: Vec<usize>, target: usize) -> FiniteFunction<VecKind> {
        FiniteFunction::new(VecArray(table), target).expect("valid finite function")
    }

    fn finite_function_strategy(
        max_source: usize,
        max_target: usize,
    ) -> impl Strategy<Value = FiniteFunction<VecKind>> {
        (0..=max_source, 0..=max_target).prop_flat_map(|(source, target)| {
            let source = if target == 0 { 0 } else { source };
            proptest::collection::vec(0..target, source).prop_map(move |table| ff(table, target))
        })
    }

    fn injective_function_strategy(
        max_source: usize,
        max_target: usize,
    ) -> impl Strategy<Value = FiniteFunction<VecKind>> {
        (0..=max_target).prop_flat_map(move |target| {
            (0..=core::cmp::min(max_source, target)).prop_flat_map(move |source| {
                proptest::sample::subsequence((0..target).collect::<Vec<_>>(), source)
                    .prop_map(move |table| ff(table, target))
            })
        })
    }

    fn parallel_maps_strategy(
        max_maps: usize,
        max_source: usize,
        max_target: usize,
    ) -> impl Strategy<Value = (FiniteFunction<VecKind>, Vec<FiniteFunction<VecKind>>)> {
        (0..=max_target, 1usize..=max_maps).prop_flat_map(move |(target, n_maps)| {
            if target == 0 {
                let base = ff(vec![], 0);
                let others = vec![ff(vec![], 0); n_maps - 1];
                Just((base, others)).boxed()
            } else {
                proptest::collection::vec(
                    proptest::collection::vec(0..target, 0..=max_source),
                    n_maps,
                )
                .prop_map(move |tables| {
                    let mut it = tables.into_iter();
                    let base = ff(it.next().expect("at least one map"), target);
                    let others = it.map(|t| ff(t, target)).collect();
                    (base, others)
                })
                .boxed()
            }
        })
    }

    fn factorable_through_injective_strategy(
        max_source: usize,
        max_target: usize,
    ) -> impl Strategy<
        Value = (
            FiniteFunction<VecKind>,
            FiniteFunction<VecKind>,
            FiniteFunction<VecKind>,
        ),
    > {
        injective_function_strategy(max_source, max_target).prop_flat_map(move |inj| {
            let b = inj.source();
            if b == 0 {
                let g = ff(vec![], 0);
                let self_map = (&g >> &inj).expect("typed composition");
                Just((self_map, inj, g)).boxed()
            } else {
                proptest::collection::vec(0..b, 0..=max_source)
                    .prop_map(move |table| {
                        let g = ff(table, b);
                        let self_map = (&g >> &inj).expect("typed composition");
                        (self_map, inj.clone(), g)
                    })
                    .boxed()
            }
        })
    }

    proptest! {
        #[test]
        fn canonical_image_injection_characterizes_image(f in finite_function_strategy(8, 8)) {
            let i = f.canonical_image_injection().expect("always valid");
            prop_assert!(i.is_injective());
            prop_assert_eq!(i.target(), f.target());

            let mut used = vec![false; f.target()];
            for &x in &f.table.0 {
                used[x] = true;
            }

            let mut seen = vec![false; f.target()];
            for &x in &i.table.0 {
                prop_assert!(used[x]);
                prop_assert!(!seen[x]);
                seen[x] = true;
            }
            prop_assert_eq!(used.clone(), seen);
            prop_assert_eq!(i.source(), used.iter().filter(|&&b| b).count());
        }

        #[test]
        fn has_disjoint_image_matches_set_disjointness(
            f in finite_function_strategy(8, 8),
            g in finite_function_strategy(8, 8),
        ) {
            if f.target() != g.target() {
                prop_assert!(!f.has_disjoint_image(&g));
                return Ok(());
            }

            let mut seen_f = vec![false; f.target()];
            for &x in &f.table.0 {
                seen_f[x] = true;
            }
            let mut seen_g = vec![false; g.target()];
            for &x in &g.table.0 {
                seen_g[x] = true;
            }

            let expected = (0..f.target()).all(|i| !(seen_f[i] && seen_g[i]));
            prop_assert_eq!(f.has_disjoint_image(&g), expected);
        }

        #[test]
        fn image_complement_injection_is_exact_complement(f in finite_function_strategy(8, 8)) {
            let k = f.image_complement_injection().expect("always valid");
            prop_assert!(k.is_injective());
            prop_assert_eq!(k.target(), f.target());

            let mut used = vec![false; f.target()];
            for &x in &f.table.0 {
                used[x] = true;
            }

            let mut seen = vec![false; f.target()];
            for &x in &k.table.0 {
                prop_assert!(!used[x]);
                prop_assert!(!seen[x]);
                seen[x] = true;
            }

            for i in 0..f.target() {
                prop_assert_eq!(seen[i], !used[i]);
            }

            // Set-theoretic disjointness of images: no value appears in both images.
            for i in 0..f.target() {
                prop_assert!(!(used[i] && seen[i]));
            }
        }

        #[test]
        fn coproduct_many_matches_iterated_coproduct((f0, others) in parallel_maps_strategy(4, 6, 8)) {
            let refs: Vec<&FiniteFunction<VecKind>> = others.iter().collect();
            let actual = f0.coproduct_many(&refs).expect("parallel maps");
            let expected = others.iter().fold(f0.clone(), |acc, m| {
                (&acc + m).expect("parallel maps")
            });
            prop_assert_eq!(actual, expected);
        }

        #[test]
        fn inverse_with_fill_left_inverts_injective_maps(
            f in injective_function_strategy(8, 8),
            fill in 0usize..8,
        ) {
            if f.source() == 0 {
                prop_assert_eq!(f.inverse_with_fill(fill), if f.target() == 0 { Some(ff(vec![], 0)) } else { None });
            } else if fill < f.source() {
                let inv = f.inverse_with_fill(fill).expect("valid inverse");
                prop_assert_eq!(&f >> &inv, Some(FiniteFunction::<VecKind>::identity(f.source())));

                let mut preimage = vec![None; f.target()];
                for (i, &y) in f.table.0.iter().enumerate() {
                    preimage[y] = Some(i);
                }
                for (y, &x) in inv.table.0.iter().enumerate() {
                    match preimage[y] {
                        Some(i) => prop_assert_eq!(x, i),
                        None => prop_assert_eq!(x, fill),
                    }
                }
            } else {
                prop_assert_eq!(f.inverse_with_fill(fill), None);
            }
        }

        #[test]
        fn factor_through_injective_recovers_original_factor(
            (self_map, inj, g) in factorable_through_injective_strategy(8, 8),
        ) {
            let factored = self_map.factor_through_injective(&inj);

            prop_assert_eq!(factored.clone(), g);
            prop_assert_eq!(&factored >> &inj, Some(self_map));
        }
    }

    #[test]
    fn coproduct_many_returns_none_on_target_mismatch() {
        let f = ff(vec![0, 1], 3);
        let g = ff(vec![0], 2);
        assert!(f.coproduct_many(&[&g]).is_none());
    }

    #[test]
    fn inverse_with_fill_rejects_non_injective_map() {
        let f = ff(vec![0, 0], 2);
        assert_eq!(f.inverse_with_fill(0), None);
    }
}
