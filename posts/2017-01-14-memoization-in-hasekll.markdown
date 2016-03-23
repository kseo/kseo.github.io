---
title: Memoization in Haskell
tags: memoization
author: Kwang Yul Seo
---
*Memoization* is an optimization technique used to speed up a function by caching its previously computed results. In impure programming languages, a mutable map is used to cache computed results.

<!--more-->

For example, `fib` function in Python

```python
def fib(n):
    if n < 2: return 1
    return fib(n-1) + fib(n-2)
```

can be speed up by memoization:

```python
fib_memo = {}
def fib(n):
    if n < 2: return 1
    if not fib_memo.has_key(n):
        fib_memo[n] = fib(n-1) + fib(n-2)
    return fib_memo[n]
```

`fib_memo` dictionary caches the previous computed results, so `fib(n)` does not need to repeat the same calculation again for the same `n`.

This implementation technique of memoization is used widely in many programming languages, but it can't be applied directly to Haskell because Haskell is pure and we don't want to introduce impurity just to memoize a function. Fortunately, it is possible to memoize a function without side effects thanks to Haskell's nature of *lazy evaluation*.

The following `memoize` function takes a function of type `Int -> a` and returns a memoized version of the same function. The trick is to turn a function into a value because, in Haskell, *functions* are not memoized but *values* are. `memoize` converts a function `f :: Int -> a` into an infinite list `[a]` whose `n`th element contains the value of `f n`. Thus each element of the list is evaluated when it is first accessed and cached automatically by the Haskell runtime thanks to lazy evaluation.

```
memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)
```

Let's define a memoized version of `fib` using `memoize`.

```haskell
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibMemo = memoize fib
```

Does `fibMemo` work properly? Sadly no, because `fib` is a recursive function which calls itself. When we call `fibMemo 10` twice, the second call is returned immediately because the first result is cached. However, intermediate `fib` calls used to evaluate `fib 10` are not cached at all because the body of `fib` calls itself directly without using `fibMemo`.

We can fix this issue by factoring out recursion from `fib`.

```haskell
import Data.Function (fix)

fib :: (Int -> Integer) -> Int -> Integer
fib f 0 = 0
fib f 1 = 1
fib f n = f (n - 1) + f (n - 2)

fibMemo :: Int -> Integer
fibMemo = fix (memoize . fib)
```

Now every call to `fib` is memoized because `memoize . fib` is used every time `fib` recursively calls itself.

So far, I explained how to memoize a function whose domain is `Int`. Of course, we can generalize this technique so that an arbitrary function can be memoized. The basic idea is the same. A function is converted into a large data structure which contains the same information so that memoization is performed by lazy evaluation.

Interested readers are referred to

* Section 3 of [Fun with type functions][fun] explains the type-directed memoization technique using type families.
* Ralf Hinze's [Memo funtions, polytypially!][ralf]

Conal Elliott's articles on memoization:

* [Memoizing polymorphic functions – part one][elliott1]
* [Memoizing polymorphic functions – part two][elliott2]
* [Memoizing polymorphic functions via unmemoization][elliott3]
* [Elegant memoization with functional memo tries][elliott4]

[MemoTrie][MemoTrie] provides a basis for memoized functions over some domains, using tries.

[fun]: https://wiki.haskell.org/Simonpj/Talk:FunWithTypeFuns
[elliott1]: http://conal.net/blog/posts/memoizing-polymorphic-functions-part-one
[elliott2]: http://conal.net/blog/posts/memoizing-polymorphic-functions-part-two
[elliott3]: http://conal.net/blog/posts/memoizing-polymorphic-functions-via-unmemoization
[elliott4]: http://conal.net/blog/posts/elegant-memoization-with-functional-memo-tries
[ralf]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.43.3272&rep=rep1&type=pdf
[MemoTrie]: https://hackage.haskell.org/package/MemoTrie-0.6.7
