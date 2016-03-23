---
title: How to read arrow combinators
tags: Arrow
author: Kwang Yul Seo
---

Arrows, like monads and applicative functors, express computations that happen within a context. In Haskell, arrows are represented with a constructor class `Arrow`:

```haskell
class Arrow a where
  arr :: (b -> c) -> a b c
  -- Each function may be treated as a computation.

  (>>>) :: a b c -> a c d -> a b d
  -- Computations may be composed, by connecting the output of the first to the input of the second.

  first :: a b c -> a (b,d) (c,d)
  -- A computation may be applied to part of the input, with the rest copied through to the output.
  ...
```

As usual, it takes some time to comprehend what arrow is and why and how it is useful. If you are looking for a good arrow tutorial, I strongly recommend John Hughes's [Programming with Arrows][arrow].

However, it is not necessary to understand arrow just to read some code written with arrow combinators because arrows are often used even when there is no compelling reason to do so. `(->)` is an instance of `Arrow` and some Haskell programmers just prefer arrow style even though combinators of `Data.Bifunctor` are often more readable.

```haskell
instance Arrow (->) where
    arr f = f
    (***) f g ~(x,y) = (f x, g y)
```

So here is my tip. If you encounter code written with arrow combinators in the context of `(->)`, use the following [translation table][table] to decode what the code means.

| Combinator | Meaning                   | Alternatives                     |
|------------|---------------------------|----------------------------------|
| (>>>)      | flip (.)                  |                                  |
| first      | \f (x, y) -> (f x, y)     | first (Data.Bifunctor)           |
| second     | \f (x, y) -> (x, f y)     | second (Data.Bifunctor)          |
| (***)      | \f g (x, y) -> (f x, g y) | bimap (Data.Bifunctor)           |
| (&&&)      | \f g x -> (f x, g x)      | liftA2 (,) (Control.Applicative) |
| left       | Maps over Left case       | first (Data.Bifunctor)           |
| right      | Maps over Right case      | second (Data.Bifunctor)          |
| (+++)      | Maps over both cases      | bimap (Data.Bifunctor)           |
| (\|\|\|)   | Eliminates Either         | either (Data.Either)             |
| app        | \(f, x) -> f x            | uncurry ($)                      |

In case of `second` and `right`, we can also use `fmap` because both `(,)` and `Either` is functorial on the second type. But I personally don't recommend `fmap` in this case because these types arbitrarily map the second type instead of the first one.

[arrow]: http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf
[table]: https://en.wikibooks.org/wiki/Haskell/Understanding_arrows#Arrow_combinators_crop_up_in_unexpected_places
