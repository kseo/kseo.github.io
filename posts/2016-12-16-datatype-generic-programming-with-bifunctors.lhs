---
title: Datatype-generic programming with bifunctors
tags: datatype-generic, bifunctors
author: Kwang Yul Seo
---
In the *origami* style of programming, higher-order recursion operators such as *map*, *fold* and *unfold* captures the structure of programs. These operators have two aspects: *mapping* and *accumulating*.

[The Essence of the Iterator Pattern][iterator] by Jeremy Gibbons and Bruno C. d. S. Oliveira show that *applicative functors* and the corresponding `traverse` operator capture the essence of the *ITERATOR* pattern providing both mapping and accumulating. This explains why Haskell's `Applicative` and `Traversable` work so well for many data types!

But in this post, instead of reiterating the paper, we are going to review one of the earlier approach which provides recursion operators in datatype-generic way. Surprisingly, what we need is only [bifunctors][bifunctors].

This post is in literate Haskell, so let's start with a list of GHC extensions and imports:

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE TemplateHaskell #-}
>
> import Data.Bifunctor
> import Data.Bifunctor.TH

`Data.Bifunctor.TH` provides a `TemplateHaskell` macro `deriveBifunctor`, which automatically derives the `Bifunctor` instance. This is possible because all sum-of-product data types induce bifunctors. Here's our favorite list data type.

> data ListF a r = Nil | Cons a r deriving Functor
> deriveBifunctor ''ListF

`Fix` is the fixed point of a `Bifunctor`.

> newtype Fix s a = In { out :: s a (Fix s a) }

Then we define `List` as a fixed point of `ListF`.

> type List = Fix ListF

To map over an arbitrary data type defined by `Fix`, we should be able to define a `Functor` instance of `Fix s`. It seems like a hard problem at first, but with enough patience and time it is actually possible to define `fmap` in terms of `bimap` as follows:

> instance Bifunctor s => Functor (Fix s) where
>   fmap f = In . bimap f (fmap f) . out

This looks magical, but we can comprehend the definition by inspecting the types of its components.

* out :: Fix s a -> s a (Fix s a)
* In :: s a (Fix s a) -> Fix s a
* fmap :: (a -> b) -> Fix s a -> Fix s b
* bimap :: (a -> b) -> (c -> d) -> s a c -> s b d

The type of `fmap f` is `Fix s a -> Fix s b`, so the type of `bimap f (fmap f)` is `s a (Fix s a) -> s b (Fix s b)`. Now we can compose these:

* out :: Fix s a -> s a (Fix s a)
* bimap f (fmap f) :: s a -> s a (Fix s a) -> s b (Fix s b)
* In :: s b (Fix s b) -> Fix s b

Thus,

* In . bitmap f (fmap f) . out :: Fix s a -> Fix s b

`fold` and `unfold` can be defined similiarly:

> fold :: Bifunctor s => (s a b -> b) -> Fix s a -> b
> fold f = f . bimap id (fold f) . out
>
> unfold :: Bifunctor s => (b -> s a b) -> b -> Fix s a
> unfold f = In . bimap id (unfold f) . f

Here's how we use `fmap` on `List`:

> nil :: List a
> nil = In Nil
>
> cons :: a -> List a -> List a
> cons x xs = In (Cons x xs)
>
> l :: List Int
> l = fmap (+1) (cons 3 (cons 4 nil))

Tada! These recursive operators are indeed datatype-generic because the defintion of `fmap`, `fold` and `unfold` never use the specific data type we defined. They use only `bimap` which is parameterized by the shape `s` of the data. It means we can reuse these functions for other data types without reimplementing them for each type. For example, here's a definition of `Tree`:

> data TreeF a r = Leaf | Branch a r r deriving Functor
> deriveBifunctor ''TreeF
> type Tree = Fix TreeF
>
> leaf :: Tree a
> leaf = In Leaf
>
> branch :: a -> Tree a -> Tree a -> Tree a
> branch x l r= In (Branch x l r)

To map over a tree, we can just use the same `fmap` function we defined above!

> t :: Tree Int
> t = fmap (+1) (branch 3 leaf (branch 4 leaf leaf))

This technique of using *bifunctors* to implement datatype-generic recursive functions is mostly superseded by `Applicative` and `Traversable` in Haskell, but I think it is still a good example which shows the real power of *bifunctors*!

[bifunctors]: https://hackage.haskell.org/package/bifunctors
[iterator]: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf
