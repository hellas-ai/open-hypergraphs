use crate::array::{Array, ArrayKind, NaturalArray};
use crate::finite_function::FiniteFunction;
use crate::strict::hypergraph::matching::{
    find_subgraph_matches as find_hypergraph_subgraph_matches, HypergraphMatch, MatchOptions,
};
use crate::strict::open_hypergraph::OpenHypergraph;

#[derive(Clone, PartialEq)]
pub struct OpenHypergraphMatch<K: ArrayKind> {
    pub w: crate::finite_function::FiniteFunction<K>,
    pub x: crate::finite_function::FiniteFunction<K>,
}

impl<K: ArrayKind> OpenHypergraphMatch<K> {
    pub fn source_image<O, A>(
        &self,
        pattern: &OpenHypergraph<K, O, A>,
    ) -> Option<crate::finite_function::FiniteFunction<K>> {
        &pattern.s >> &self.w
    }

    pub fn target_image<O, A>(
        &self,
        pattern: &OpenHypergraph<K, O, A>,
    ) -> Option<crate::finite_function::FiniteFunction<K>> {
        &pattern.t >> &self.w
    }
}

pub(crate) fn smc_boundary_images<K: ArrayKind, O, A>(
    pattern: &OpenHypergraph<K, O, A>,
    w: &FiniteFunction<K>,
) -> Option<(FiniteFunction<K>, FiniteFunction<K>)> {
    Some(((&pattern.s >> w)?, (&pattern.t >> w)?))
}

pub(crate) fn is_smc_open_match<K: ArrayKind, O, A>(
    pattern: &OpenHypergraph<K, O, A>,
    host: &OpenHypergraph<K, O, A>,
    w: &FiniteFunction<K>,
) -> bool
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O>,
    K::Type<A>: Array<K, A>,
{
    if !host.is_monogamous() || !host.is_acyclic() {
        return false;
    }

    let Some((inputs_in_host, outputs_in_host)) = smc_boundary_images(pattern, w) else {
        return false;
    };

    inputs_in_host.is_injective()
        && outputs_in_host.is_injective()
        && inputs_in_host.has_disjoint_image(&outputs_in_host)
}

pub fn find_subgraph_matches<K: ArrayKind, O, A>(
    pattern: &OpenHypergraph<K, O, A>,
    host: &OpenHypergraph<K, O, A>,
    options: &MatchOptions,
) -> Vec<OpenHypergraphMatch<K>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
    O: PartialEq,
    A: PartialEq,
    K::I: Into<usize> + core::convert::TryFrom<usize>,
    for<'a> K::Slice<'a, K::I>: From<&'a [K::I]>,
{
    find_hypergraph_subgraph_matches(&pattern.h, &host.h, options)
        .into_iter()
        .map(|HypergraphMatch { w, x }| OpenHypergraphMatch { w, x })
        .collect()
}

pub fn find_smc_matches<K: ArrayKind, O, A>(
    pattern: &OpenHypergraph<K, O, A>,
    host: &OpenHypergraph<K, O, A>,
    options: &MatchOptions,
) -> Vec<OpenHypergraphMatch<K>>
where
    K::Type<K::I>: NaturalArray<K>,
    K::Type<O>: Array<K, O> + PartialEq,
    K::Type<A>: Array<K, A> + PartialEq,
    O: PartialEq,
    A: PartialEq,
    K::I: Into<usize> + core::convert::TryFrom<usize>,
    for<'a> K::Slice<'a, K::I>: From<&'a [K::I]>,
{
    find_subgraph_matches(pattern, host, options)
        .into_iter()
        .filter(|m| is_smc_open_match(pattern, host, &m.w))
        .collect()
}
