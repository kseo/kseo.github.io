---
title: Short cut fusion
tags: short cut fusion, optimization
author: Kwang Yul Seo
---
Let's start with a simple programming task. How do you add integers from 1 to 10? In Java-like language, we use a loop to calculate the sum.

```java
int sum = 0;
for (int i = 1; i <= 10; i++) {
  sum += i;
}
```

In Haskell, we can succinctly performs the same calculation using *list comprehension* and `foldr`.

```haskell
foldr (+) 0 [1..10]
```

The code is short and elegant, but novice Haskell programmers are often concerned with the performance of this program. Indeed, it looks very inefficient on the surface because it first generates a list of integers from 1 to 10 by allocating each con cells, and subsequently deallocates them by folding over the list.

So functional programming is nice, but it is not performant in real world? The answer is no! Our Haskell compiler, GHC is smart enough to optimize this pattern into a simple loop which does not require an explicit list structure. This compiler optimization technique is called [short cut fusion][wiki].

Let's see how our example is actually transformed into a simple loop step by step.

First, the list comprehension expression `[1..10]` is a syntactic sugar for `from 1 10`. The definition of `from` is as follows:

```haskell
from :: (Ord a, Num a) => a -> a -> [a]
from a b = if a > b
           then []
           else a : from (a + 1) b
```

`from` is a list producing function and we can abstract the definition over *cons* and *nil* by parameterizing both as arguments.

```haskell
from' :: (Ord a, Num a) => a -> a -> (a -> b -> b) -> b -> b
from' a b = \c n -> if a > b
                    then n
                    else c a (from' (a + 1) b c n)
```

The original `from` can be obtained in terms of `build`:

```haskell
build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

from a b = build (from' a b )
```

Here `build` is a dual of `foldr` which produces a list. You can think of `from'` as a program with 'holes' for constructors, and `build` plugs those holes with actual constructors.

The key idea of the *short cut fusion* is that `build` and `foldr` cancel each other. To put it another way, when we produce a list that will be consumed immediately, we can fuse them together.

```haskell
foldr k z (build g) = g k z
```

We now can see that the evaluation of `foldr (+) 0 [1..10]` does not produce an intermediate list.

```
foldr (+) 0 (from 1 10)
=> foldr (+) 0 (build (from' 1 10))
=> from' 1 10 (+) 0
=> \c n -> (if 1 > 10
            then n
            else c 1 (from' 2 10 c n)) (+) 0
=> if 1 > 10
   then 0
   else 1 + (from' 2 10 (+) 0)
=> 1 + 2 + ... + 9 + 10 + 0
=> 55
```

Interested readers might like to take a look at the [original paper][paper] for further information.

[wiki]: https://wiki.haskell.org/Short_cut_fusion
[paper]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/deforestation-short-cut.pdf
