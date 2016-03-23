---
title: Non empty list
tags: non empty list, prelude
author: Kwang Yul Seo
---
Haskell is well known for its safety. A well-typed Haskell program never goes wrong. Is it true? Unfortunately, no. The type system of Haskell is great and it does catch many bugs at compile time, but Haskell's `Prelude` is full of partial functions.

For example, `head` and `tail` functions of `Data.List` throws an error when an empty list is given as an argument.

```haskell
λ> head []
*** Exception: Prelude.head: empty list
```

That's why we have a separate [safe][safe] package which provides alternative safe functions such as [headMay][headMay] and [tailMay][tailMay].

```haskell
λ> headMay []
Nothing
it :: Maybe a
```

What if you know that your list is never empty? Checking the return value of `headMay` or `tailMay` soon becomes cumbersome.

Fortunately, Haskell `Prelude` provides [NonEmpty][NonEmpty] data type which guarantees that the list is not empty. You can use `head` and `tail` functions without worrying about the partiality. It also provides many list functions such as `map`, `reverse` and `length`.

```haskell
infixr 5 :|, <|
data NonEmpty a = a :| [a]

head :: NonEmpty a -> a
```

[safe]: https://hackage.haskell.org/package/safe
[headMay]: https://www.stackage.org/haddock/lts-7.12/safe-0.3.10/Safe.html#v:headMay
[tailMay]: https://www.stackage.org/haddock/lts-7.12/safe-0.3.10/Safe.html#v:tailMay
[NonEmpty]: https://www.stackage.org/haddock/lts-7.12/base-4.9.0.0/Data-List-NonEmpty.html
