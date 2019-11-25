![CSIRO's Data61 Logo](https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png)

# `consy`

Implementing the `Data.List` API using `Control.Lens.Cons` without sacrificing
performance.

Instances for `[a]` should compile down to core similar to the native `Data.List`
functions. Rewrite rules are used for other instances to rewrite to their efficient
implementations.

There are inspection tests and benchmarks in `test/`
