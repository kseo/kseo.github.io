---
title: The power of lazy evaluation
tags: Haskell, lazy evaluation, backtracking, exception handling
author: Kwang Yul Seo
---

[How to replace failure by a list of successes][wadler-1985] is a classic paper
which shows the power of lazy evaluation with respect to exception handling,
backtracking and pattern matching. It says we don’t need special language
constructs because these features can be emulated only with lazy evaluation.

The idea is simple. Each term that may raise an exception of backtrack is
replaced by a term that returns a list of values; `[]` for failure and a
singleton list for success.

* (failure) -> []
* (success) -> [v]

A term that might return many values through backtracking is replaced by a term
that returns a list of those values.

* (backtracking) -> [v1, v2, …]

There are two ways of combining terms that may raise exceptions or backtrack.

> "or" combination is a way of combining two terms so that evaluation of the
> resulting term succeeds whenever evaluation of the first term *or* the second
> term succeeds.

"or" combination can be implemented with list append, `(++)`.

> "and" combination is a way of combining two terms so that evaluation of the
> resulting term succeeds whenever evaluation of the first term *and* the second
> term succeeds.

"and" combination can be implemented with cartesian product.

Let’s look at an example. `assoc` is a function that looks up entries in an
association list. Given a list of pairs `xys` and a value `x`, the call `assoc
xys x` returns `y` such that the pair `(x, y)` is in the list `xys`. If there is
no such `y`, the call should raise an exception. `assoc` can be written in
Haskell using list comprehension:

```haskell
assoc xys x = [y | (x', y) <- xys, x' == x]
```

Here are the results of some experiments on assoc.

* assoc [("a", 1), ("a", 3)] "a" = [1,3]
* assoc [("a", 1), ("b", 2)] "b" = [2]
* assoc [("a", 1), ("b", 2)] "c" = [0]

"Or" combinator `(assoc xys1 x) ? (assoic xys2 x)` can be written

```haskell
assoc xys1 x ++ assoc xys2 x
```

and "and" combinator `(assoc xys x) + (assoic xys2 x)` can be written

```haskell
[y1 + y2 | [y1 <- assoic xys1 x, y2 <- assoc xys2 x]
```

Backtracking usually requires special language features such as coroutines or
generators because evaluation of an expression needs to be *suspended* after it
has returned one value, and then may be *resumed* later if more values are
needed. Lazy evaluation provides this property without any additional language
features. That is, evaluation of a list is already performed only when more
elements are needed.

[wadler-1985]: https://rkrishnan.org/files/wadler-1985.pdf
