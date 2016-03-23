---
title: Type-level insertion sort
tags: insertion sort, type-level, closed type families
author: Kwang Yul Seo
---
*Multi-parameter type classes* and *functional dependencies* made type-level programming possible. Back in 2000, Thomas Hallgren showed [an implementation of insertion sort][wm01] as an example of static computation using functional dependencies. The code has a strong resemblance to logic programming which looks bizarre to most functional programmers. In this post, I will show you a more "functional-style" implementation of insertion sort using *closed type families*.

<!--more-->

# Term-level insertion sort

Here's an implementation of insertion sort we all know.

```haskell
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x : xs) = insert x (sort xs)

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = x : []
insert x (y : ys) = insert' (compare x y) x y ys

insert' :: (Ord a) => Ordering -> a -> a -> [a] -> [a]
insert' LT  x y ys = x : (y : ys)
insert' _   x y ys = y : insert x ys

l = [1, 3, 2, 4, 7, 9, 5]
```

`sort l` sorts the given list.

```
λ> sort l
[1,2,3,4,5,7,9]
```

To implement insertion sort in type-level, we must be able to define

1. naturals, booleans and lists
1. functions

in type-level. For the basics of type-level programming, readers are referred to [Type-level functions using closed type families][type-level].

# Insertion sort

Here's an implementation of type-level insertion sort. One can see the strong similarity with the term-level insertion sort.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

type family Sort xs where
  Sort '[] = '[]
  Sort (x ': xs) = Insert x (Sort xs)

type family Insert x xs where
  Insert x '[] = x ': '[]
  Insert x (y ': ys) = Insert' (CmpNat x y) x y ys

type family Insert' b x y ys where
  Insert' 'LT  x y ys = x ': (y ': ys)
  Insert' _    x y ys = y ': Insert x ys

type L = [1, 3, 2, 4, 7, 9, 5]
```

In this simple scenario, converting a term-level function into a type-level function is almost mechanical. Just a few rules suffice.

* sort -> type family Sort
* [] -> '[]
* (x : xs) -> (x ': xs)
* compare -> CmpNat

We can evaluate `Sort L` using GHCi's `kind!` command.

```
λ> :kind! Sort L
Sort L :: [Nat]
= '[1, 2, 3, 4, 5, 7, 9]
```

[wm01]: http://www.cse.chalmers.se/~hallgren/Papers/wm01.html
[type-level]: https://kseo.github.io/posts/2017-01-16-type-level-functions-using-closed-type-families.html
