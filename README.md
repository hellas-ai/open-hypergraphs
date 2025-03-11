# Open Hypergraphs

An implementation of [Data Parallel Algorithms for String Diagrams](https://arxiv.org/pdf/2305.01041).
See also the [Python implementation](https://github.com/statusfailed/open-hypergraphs/).

Features:

- Algebraic construction by tensor and composition
- Functors, including optic transformation for reverse differentiation of morphisms
- Data-parallel diagram layering

Examples:

An example for defining a simple expression language ([polynomial
circuits](https://www.sciencedirect.com/science/article/pii/S2352220823000469))
and evaluating its terms is given [here](./examples/polycirc.rs).
