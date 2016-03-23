---
title: Difference List for Writer Monad
tags: Haskell, difference list, writer monad
author: Kwang Yul Seo
---

In writing a compiler in Haskell, it is conventional to use the [Writer
monad][writer] to accumulate emitted code.

The class declaration of `MonadWriter` is

```haskell
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
```

Here `w` must be a [Monoid][monoid] instance, so that `MonadWriter` can combine
the outputs of the subcomputations using [mappend][mappend].

A naive implementation of a compiler can use `MonadWriter [String]` to
accumulate emitted code. Haskell list is an instance of `Monoid` and its
`mappend` function is implemented with `(++)` which appends two lists.

This looks okay at first, but it is not an efficient way to use the `Writer`
monad because the time complexity of `(++)` is O(n) on the length of the first
operand.  Because the first operand gets bigger as `MonadWriter` accumulates
more code, the time complexity of `MonadWriter [a]` becomes quadratic.

A [Difference list][difference-list] is the rescue because it is a `Monoid`
instance which supports O(1) `append` and `snoc` operations on lists. So we can
use `MonadWriter (DList Instruction)` instead of `MonadWriter [Instruction]` to
accumulate emitted code and convert it to a list using `DList.toList`.

Wikipedia explains the term difference list as in the following:

> In the second approach, difference lists are implemented as single-argument
> functions, which take a list as argument and prepend to that list. As a
> consequence, concatenation of difference lists of the second type is
> implemented essentially as function composition, which is O(1). However, of
> course the list still has to be constructed eventually (assuming all of its
> elements are needed), which is plainly at least O(n).

There is a chapter on Real World Haskell about the difference list: [Chapter 13.
Data Structures][real-world-chap13]. Learn you Haskell for a Great Good also
provides a chapter on the difference list: [For a Few Monads More][lyhgg].

[writer]: https://hackage.haskell.org/package/mtl-2.0.1.0/docs/Control-Monad-Writer-Lazy.html
[monoid]: http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Monoid.html
[mappend]: http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Monoid.html#v:mappend
[difference-list]: https://en.wikipedia.org/wiki/Difference_list
[real-world-chap13]: http://book.realworldhaskell.org/read/data-structures.html
[lyhgg]: http://learnyouahaskell.com/for-a-few-monads-more
