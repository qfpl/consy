Implementing the `Data.List` API using `Control.Lens.Cons`, aiming for equivalent
performance or better.

Shortcut fusion is implemented for the API, and there are rewrite rules to change
the Consy API functions into their more specialized versions, like
`filter -> Data.Text.filter`.

There are inspection tests, benchmarks, and allocation tests in `test/`

---

I had this idea after seeing the [TextualMonoid](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Textual.html)
class. It seems basically equivalent to

```haskell
class (Monoid s, Cons s s Char Char) => TextualMonoid s where
```

with appropriate laws regarding the interactions between `mappend`, `mempty`, `cons`,
and `uncons`. 
