---
title: Encoding existentials
tags: existential type, Church encoding
author: Kwang Yul Seo
---
Existential types are important because [Abstract Types Have Existential Type][mitch-plotkin-88]. Haskell supports existential types though a GHC extension named `ExistentialQuantification`.

Here's is an example. This code below creates an abstract data type named `Obj`. Clients of `Obj` can use only `show` function because clients can't know the hidden representation of `Obj`.

```haskell
{-# LANGUAGE ExistentialQuantification #-}

data Obj = forall a. (Show a) => Obj a

obj1 :: Obj
obj1 = Obj "hello"

obj2 :: Obj
obj2 = Obj 1

app :: Obj -> String
app (Obj x) = show x
```

The code is simple, but confusing because it uses *forall* instead of *exists* quantifier. It becomes more clear when we rewrite the definition of `Obj` in GADT syntax.

```haskell
{-# LANGUAGE GADTs #-}

data Obj where
  Obj :: (Show a) => a -> Obj
```

As the type variable `a` no longer appears on the right hand side, it is considered to be existentially quantified. Also you no longer need `ExistentialQuantification`. Existentials are subsumed by GADTs.

There is another way to encode existential types without `ExistentialQuantification`. Because an existential type is a pair of type and a value, we can use the [Church encoding][church] for a pair to represent existentials.

```
{∃X,T} = ∀Y. (∀X. T→Y) → Y
```

In Haskell, we need to enable `RankNTypes`.

```haskell
{-# LANGUAGE RankNTypes #-}

type Obj = forall y. (forall x. (Show x) => x -> y) -> y

obj :: Obj
obj f = f "hello"

app :: Obj -> String
app obj = obj (\x -> show x)
```

[mitch-plotkin-88]: http://theory.stanford.edu/~jcm/papers/mitch-plotkin-88.pdf
[church]: https://en.wikipedia.org/wiki/Church_encoding
