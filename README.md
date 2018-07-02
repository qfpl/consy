![Data61](http://i.imgur.com/uZnp9ke.png)

# `consy`

Implementing the `Data.List` API using `Control.Lens.Cons` without sacrificing
performance.

Instances for `[a]` should compile down to core similar to the native `Data.List`
functions. Rewrite rules are used for other instances to rewrite to their efficient
implementations.

There are inspection tests and benchmarks in `test/`
