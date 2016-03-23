---
title: Encodings of lens
tags: Lens
author: Kwang Yul Seo
---

There are variety of libraries offering lens on the hackage. These include [data-accessor][data-acessor], [fclabels][fclabels], [lenses][lenses], [data-lens][data-lens] and [lens][lens]. Though they represent the same idea, their encodings of lens are not compatible. This article is an attempt to understand various encodings used to represent the idea of lens.

# Lens as a getter and a setter

The most intuitive way to encode lens is to represent it as a collection of a *getter* and a *setter*. `Lens s a` represents a *focus* to an `a` inside an `s` structure.

```haskell
data Lens s a = Lens
  { get :: s -> a
  , set :: a -> s -> s
  }
```

We can define a lens to the first element of a pair as follows:

```haskell
fstL :: Lens (a,b) a
fstL = Lens {
    get = \(x,y) -> x
  , set = \x (_,y) -> (x,y)
  }
```

Similarly, we can define `snd`:

```haskell
sndL :: Lens (a,b) b
sndL = Lens {
    get = \(x,y) -> y
  , set = \y (x,_) -> (x,y)
  }
```

Having defined this, it is easy to access and modify a pair:

```
> get fstL (1,2)
1
> set fstL 3 (1,2)
(3,2)
```

We can also implement `update` in terms of `get` and `set` by getting the value, applying a function and setting it again.

```haskell
update :: Lens s a -> (a -> a) -> s -> s
update l f s = set l (f (get l s)) s
```

This `update` function works fine for a small data structure such as pairs, but becomes expensive for large data structures as it needs to visit all the way down to the *focus* twice every time it updates.

```haskell
> update fstL (+1) (1,2)
(2,2)
```

Lenses are composable with `compose` function:

```haskell
compose :: Lens b c -> Lens a b -> Lens a c
compose bc ab = Lens
  { get = get bc . get ab
  , set = update ab . set bc
  }
```

We can now modify nested pairs:

```
> update (sndL `compose` fstL) (*2) ((3,4),5)
((3,8),5)
```

# Lens as a getter and an updater

A slight variation to the previous lens encoding is to represent a lens with a *getter* and an *updater* instead of a *getter* and a *setter*.

```haskell
data Lens s a = Lens
  { get :: s -> a
  , update :: (a -> a) -> s -> s
  }
```

The performance of this encoding is better than the previous one because we no longer need to visit the *focus* twice for an single update. `fstL` and `sndL` directly implement `update` as follows:

```haskell
fstL :: Lens (a,b) a
fstL = Lens {
    get = \(x,y) -> x
  , update = \f (x,y) -> (f x,y)
  }

sndL :: Lens (a,b) b
sndL = Lens {
    get = \(x,y) -> y
  , update = \f (x,y) -> (x, f y)
  }
```

But what about `set`? We can easily define `set` in terms of `update` using `const` function:

```haskell
set :: Lens s a -> a -> s -> s
set l a s = update l (const a) s
```

Composition of two lenses are similarly defined:

```haskell
compose :: Lens b c -> Lens a b -> Lens a c
compose bc ab = Lens
  { get = get bc . get ab
  , update = update ab . update bc
  }
```

# CPS based lens (aka van Laarhoven lens)

This encoding is called *a CPS based lens* as the type looks like a continuation passing style function, which would be `(b -> r) -> (a -> r)` though the return type is `f a` instead of any `r`. It is also called [van Laarhoven lens][cps] named after **Twan van Laarhoven** who first discovered this encoding.

This is the most popular lens encoding as it is used by the famous Edward Kmett's [lens][lens] library.

```haskell
{-# LANGUAGE Rank2Types #-}

type Lens s a = forall f. Functor f => (a -> f a) -> (s -> f s)
```

It is hard to understand the type at first sight. There are no longer identifiable setter or getter in the type signature. Instead, we have a functor `f` in the signature.

As expected from the clue, we can derive `get` and `update` by applying different functors to the type.

First, we derive `get` by applying `Const` functor:

```haskell
import Control.Applicative

get :: Lens s a -> s -> a
get l = getConst . l Const
```

Also we can derive `update` by applying `Identity` functor:

```haskell
import Control.Monad.Identity

update :: Lens s a -> (a -> a) -> s -> s
update l m = runIdentity . l (Identity . m)
```

`set` function can be implemented in terms of `get` and `update` as usual:

```haskell
set :: Lens s a -> a -> s -> s
set l b a = update l (const b) a
```

Fortunately, defining a lens in var Laarhoven lens encoding is not hard. For example, we can define `fstL` using `fmap`:

```haskell
fstL :: Lens (a,b) a
fstL f (x,y) = fmap (\a -> (a, y)) (f x)
```

Or we can define `sndL` more succinctly using `TupleSections` and `(<$>)` operator:

```haskell
sndL :: Lens (a,b) b
sndL x (a,b) = (a,) <$> x b
```

As this job of defining a lens is so mechanical, lens library provides a [TemplateHaskell macro][makeLenses] to generate lenses from a record.

Let's see how `get` works by actually evaluating `get fstL (1,2)`:

```
get fstL (1,2)
=> getConst . (fstL Const) (1,2)
=> getConst . (\x (a,b) -> (,b) <$> x a) (1,2)
=> getConst . (\(a,b) -> (,b) <$> Const a) (1,2)
=> getConst . ((,2) <$> Const 1)
=> getConst . (Const 1)
=> 1
```

One particularly nice property of var Laarhoven lens is that composition of two lens is just the function composition operator `(.)`.

```haskell
compose :: Lens b c -> Lens a b -> Lens a c
compose r s = s . r
```

So we can simply use the `(.)` to compose two lenses instead of custom `compose` function.

```haskell
> update (fstL . sndL) (*2) ((3,4),5)
((3,8),5)
```

However, the order of composition is reversed due to the nature of CPS based encoding. This is a bit confusing at first, but it makes sense when we think of it as a property referencing in an OO language.

```
pair.(fstL.sndL) *= 2
```

This style of composition is also possible with previous two encodings if we use Haskell's type classes to overload `(.)` and `id` as described in [Overloading functional references][overloading]. But van Laarhoven lens is still simpler as we don't need to hide the `(.)` operator of the `Prelude`.

# Isomorphism lens

There is a yet another encoding of lens called [Isomorphism lens][iso] which was also discovered by van Laarhoven.

> A lens from type s to a is a bijection between s and a pair of a and some residual r.

To put it more succinctly,

```
type Lens s a = exists r. s <-> (a,r)
```

Because Haskell has no `exists` keyword, we can represent the encoding with a `newtype` wrapper:

```haskell
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}

-- Isomorphisms/bijections between type @a@ and @b@
data Iso a b = Iso { fw :: a -> b, bw :: b -> a }

-- Lenses with a data wrapper, in practice you might want to unpack the Iso type
data Lens s a = forall r. Lens (Iso s (a,r))
```

This encoding captures the intuition of what lens does extremely well. Suppose we have a record type `s`, and want to take out a field of type `a`. When we do that, there is some residual, which is `s - a`. So `s` and `(a, s - a)` is must be isomorphic.

We can define `get` and `update` as follows:

```haskell
import Data.Bifunctor

get :: Lens s a -> s -> a
get (Lens l) = fst . fw l

update :: Lens s a -> (a -> a) -> (s -> s)
update (Lens l) f = bw l . first f . fw l
```

`set` function is the same as before:

```haskell
set :: Lens s a -> a -> s -> s
set l b a = update l (const b) a
```

Isomorphism lens has the nice property that we can derive lens laws directly from the isomorphism law.

> fw i . bw i = bw i . fw i = id

See [van Laarhoven's article][iso] to see how we can derive three lens law.

[data-acessor]: https://hackage.haskell.org/package/data-accessor
[fclabels]: https://hackage.haskell.org/package/fclabels
[lenses]: https://hackage.haskell.org/package/lenses
[data-lens]: https://hackage.haskell.org/package/data-lens
[lens]: https://www.stackage.org/haddock/lts-7.12/lens-4.14/Control-Lens.html
[overloading]: http://www.twanvl.nl/blog/haskell/overloading-functional-references
[cps]: http://www.twanvl.nl/blog/haskell/cps-functional-references
[iso]: http://www.twanvl.nl/blog/haskell/isomorphism-lenses
[makeLenses]: https://hackage.haskell.org/package/lens-4.15.1/docs/Control-Lens-TH.html#v:makeLenses
