---
title: Type-level functions using closed type families
tags: dependent type, type-level, closed type families
author: Kwang Yul Seo
---
In this post, we will see how to write basic type-level functions using [closed type families][closed].

Before we start, let's declare a bunch of GHC language extensions required to use type-level functions.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
```

Also import required modules. `GHC.TypeLits` provides type-level natural numbers and symbols.

```haskell
import GHC.TypeLits
import Data.Proxy
```

# Literals

Datatype promotion allows us to use `True` and `False` as type constructors whose kind is `Bool`. The quotes are used to emphasize the promotion, but can be omitted when syntactically unambiguous.

```
λ> :set -XDataKinds
λ> :kind 'True
'True :: Bool
λ> :kind 'False
'False :: Bool
```

We can also use numbers such as `1` and `2` as types. The kind of these numbers is `Nat`.

```
λ> :kind 1
1 :: Nat
```

# Type-level Function

`If` takes three arguments `c`, `t` and `e` and returns `t` if `c` is `True`, returns `e` otherwise. The kind of `If` is `Bool -> * -> * -> *`.

```haskell
type family If c t e where
  If 'True  t e = t
  If 'False t e = e
```

We can use GHCi's `kind!` command to evaluate type functions.

```
λ> :kind! If 'True Bool Char
If 'True Bool Char :: *
= Bool
λ> :kind! If 'False Int Double
If 'False Int Double :: *
= Double
```

# Type-level List

As we can promote types like `Bool`, we can also promote lists and treat `[]` as a *kind constructor*, and `[]` and `(:)` as *types*.

When `(:)` is seen as a type constructor, it has kind

```
λ> :kind (:)
(:) :: a -> [a] -> [a]
```

It means `(:)` is *kind-polymorphic*.

So we can create a type-level list of booleans as well as naturals.

```
λ> :kind [True, False]
[True, False] :: [Bool]
λ> :kind [1, 2]
[1,2] :: [Nat]
```

# Type-level List Function

The definition of type-level function `Length` is the same as the value level `length` function. If it an empty list returns 0. If it not empty, add 1 to the length of the tail.

```haskell
type family Length xs where
   Length '[]       = 0
   Length (x ': xs) = 1 + Length xs
```

`0` and `1` are types of `Nat` kind and `(+)` is a type-level add function defined in `GHC.TypeLits`. We can even use `(:)` as a pattern here.

```
λ> :kind! Length [Char,Bool,Int]
Length [Char,Bool,Int] :: Nat
= 3
```

It seems `Length` is almost identical to the value level function `length`, but `Length` function is not kind-polymorphic by default. Thus passing `[1, 2, 3]` to `Length` causes an error.

```
λ> :kind! Length [1,2,3]

<interactive>:1:8: error:
    • Expected kind ‘[*]’, but ‘'[1, 2, 3]’ has kind ‘[Nat]’
    • In the first argument of ‘Length’, namely ‘'[1, 2, 3]’
      In the type ‘Length '[1, 2, 3]’
```

To make it poly-kinded, we need to turn on `PolyKind` extension. The kind is inferred automatically, but we can also specify the kind with `k`.

```haskell
type family Length (xs :: [k]) where
   Length '[]       = 0
   Length (x ': xs) = 1 + Length xs
```

`Head` and `Tail` are defined in a similar manner. Note that the kind of `xs` is explicitly annotated with `[*]` because they only work on type-level lists.

```haskell
type family Head (xs :: [*]) where
   Head (x ': xs) = x

type family Tail (xs :: [*]) where
  Tail (x ': xs) = xs
```

We can see `Head` and `Tail` work as expected.

```
λ> :kind! Head [Char, Bool, Int]
Head [Char, Bool, Int] :: *
= Char
*Main
λ> :kind! Tail [Char, Bool, Int]
Tail [Char, Bool, Int] :: [*]
= '[Bool, Int]
```

One notable thing here is that both `Head` and `Tail` are partially defined. What if we pass `'[]` to `Head` or `Tail`?

```
λ> :kind! Head '[]
Head '[] :: GHC.Types.*
= Head '[]
```

It seems GHC treats `Head '[]` as a valid type instead of emitting a type error. It is a bit mysterious, but at least we can see that type-level functions in Haskell can be partial and the behavior is not intuitive. Interested readers are referred to Richard Eisenberg's [What are type families?][type-families] which discusses this issue in details.

# Higher-order Type-level List Function

It is even possible to define type-level *map* function. `Map` takes a type-level function `f` and a type-level list `xs`. It applies `f` to every element of `xs` and returns a new type-level list containing the results.

```haskell
type family Map (f :: * -> *) (xs :: [*]) where
   Map f '[]       = '[]
   Map f (x ': xs) = f x ': Map f xs
```

```
λ> :kind! Map MakePair [Char,Bool,Int]
Map MakePair [Char,Bool,Int] :: [GHC.Types.*]
= '[(Char, Char), (Bool, Bool), (Int, Int)]
```

where the definition of `MakePair` is

```haskell
type family MakePair (x :: *) where
  MakePair x = (x, x)
```

# Wrap-up

So far we've covered only the basics of type-level datatypes and functions. Recent additions to GHC make it possible to explore the whole new world of dependent type programming in Haskell. Interested readers might want to take a look at the publications of [Richard A. Eisenberg][Eisenberg] whose current work is to add dependent types to Haskell.

[closed]: https://github.com/ghc/ghc/commit/569b26526403df4d88fe2a6d64c7dade09d003ad
[type-families]: https://typesandkinds.wordpress.com/2015/09/09/what-are-type-families/
[Eisenberg]: http://cs.brynmawr.edu/~rae/
