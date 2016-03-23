---
title: Higher rank polymorphism
tags: higher rank polymorphism, let bound polymorphism, RankNTypes, ScopedTypeVariables
author: Kwang Yul Seo
---
The type system of Haskell is based on *Hindley-Milner*, so it has so called *let-bound polymorphism*. It means identifiers bound using a `let` or `where` clause can be polymorphic. On the contrary, lambda-bound identifiers are monomorphic.

For example, the following program is illegal in Haskell:

```haskell
foo :: (Int, Char)
foo = (\f -> (f 1, f 'a')) id
```

The [Typing Pitfalls][pitfalls] section of "A Gentle Introduction to Haskell, Version 98" also mentions a similar case.

```haskell
let f g  =  (g [], g 'a')                       -- ill-typed expression
in f (\x->x)
```

Thanks to *let-polymorphism*, we can easily make `foo` type-check by moving `f` to `where` clause.

```haskell
foo :: (Int, Char)
foo = (f 1, f 'a')
  where f = id
```

Or by binding `f` with `let`.

```haskell
foo :: (Int, Char)
foo = let f = id
       in (f 1, f 'a')
```

This is rather unfortunate because all these forms represent the same program, but only one of them fails to type check.

If Haskell can't infer the type for us, let's bite the bullet and perform the type inference by ourselves. What's the type of `f`? It is `forall a. a -> a`. Thus the type of `foo` is `(forall a. a -> a) -> (Int, Char)`.

Aha! This is a higher-rank type (rank 2 in this case) and we can specify the type of `f` using two GHC extensions; `RankNTypes` and `ScopedTypeVariables`.

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

foo :: (Int, Char)
foo = (\(f :: forall a. a -> a) -> (f 1, f 'a')) id
```

`RankNTypes` allows us to express higher-rank types and `ScopeTypeVariables` allows free type variables to be re-used in the scope of a function

Now our program is well-typed!

[pitfalls]: https://www.haskell.org/tutorial/pitfalls.html
