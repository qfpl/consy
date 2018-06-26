Implementing the `Data.List` API using `Control.Lens.Cons`, aiming for equivalent
performance or better.

Shortcut fusion is implemented for the API, and there are rewrite rules to change
the Consy API functions into their more specialized versions, like
`filter -> Data.Text.filter`.

There are inspection tests, benchmarks, and allocation tests in `test/`
