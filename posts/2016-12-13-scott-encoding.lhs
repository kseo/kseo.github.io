---
title: Scott encoding of Algebraic Data Types
tags: scott encoding, algebraic data type
author: Kwang Yul Seo
---
This article is written in literate Haskell.

It is well known that algebraic data types can be encoded in a functional programming language by higher order functions. The Church encoding is the most famous one, but there is a lesser known encoding, called Scott encoding. The Scott encoding is generally considered as [better][ifl2014_submission_13].

In this article, I will show you some examples of Scott-encoded algebraic data types. Here I will use named functions intead of anonymous functions because the named function makes the notation of recursive algorithms easier.

Before move on, we need to turn on GHC extension `RankNTypes`. If you want to know how Rank-N types are related to the Scott encoding, see [24 Days of GHC Extensions: Rank N Types][rankn].

> {-# LANGUAGE RankNTypes #-}

= Pair

`Pair` is a simplest example of a container type. Because it is a non-recursive type, the Church and Scott encoding overlap in this case. This is the standard encoding used for pairs in λ-calculus courses.

> newtype PairS a b = PairS { unpairS :: forall r. (a -> b -> r) -> r }

Containers can be expressed by using closures (partial applications). `pairS` takes 3 arguments. We have a closure by applying only 2 arguments.

> pairS :: a -> b -> PairS a b
> pairS a b = PairS (\p -> p a b)

Now it is time to define selection functions. `fstS` and `sndS` are implemented by passing a continuation (the function in which the continuation continues). It is a 2 argument function which returns either the first or the second argument. `fstS` returns the first argument and `sndS` returns the second argument.

> fstS :: PairS a b -> a
> fstS (PairS p) = p (\x _ -> x)
>
> sndS :: PairS a b -> b
> sndS (PairS p) = p (\_ y -> y)

Other functions such as `swapS` can be implemented in terms of `pairS`, `fstS` and `sndS`.

> swapS :: PairS a b -> PairS b a
> swapS p = pairS (sndS p) (fstS p)

= Peano numbers

`NumS` is the simplest recursive data type which represents Peano numbers.

> newtype NumS = NumS { unnumS :: forall r. (NumS -> r) -> r -> r }

`NumS` has two constructors. `zeroS` is the non recursive constructor that represents the value zero. `succS` is the recursive constructor which yields the successor of such a Peano number.

> zeroS :: NumS
> zeroS = NumS (\s z -> z)
>
> succS :: NumS -> NumS
> succS n = NumS (\s z -> s n)

`unnumS` is the deconstructor which takes 2 continuations and a `NumS`. The continuations determine what we reduce the `NumS` into depending on which constructor is found.

For convinence, `unnumS'` is defined to have the `NumS` argument be the last arugment to the `unnumS` function.

> unnumS' :: (NumS -> r) -> r -> NumS -> r
> unnumS' s z (NumS f) = f s z

When we find the num is a successor, then we know that the num is not empty, so we reduce it to `False`. When we find it is the zero, we reduce it to `True`.

> isZero :: NumS -> Bool
> isZero = unnumS' (\_ -> False) True

`addS` is slightly more complex, but it can also be defined using the same techinque. You can recognize that it is a pattern mathcing in disguse.

> addS :: NumS -> NumS -> NumS
> addS n m =
>     unnumS' (\s -> succS (addS s m))
>             m n

= List

We can apply the same transformation to `ListS` type.

> newtype ListS a =
>    ListS {
>      unconsS :: forall r. (a -> ListS a -> r) -> r -> r
>    }
>
> nilS :: ListS a
> nilS = ListS (\co ni -> ni)
>
> consS :: a -> ListS a -> ListS a
> consS x xs = ListS (\co ni -> co x xs)
>
> unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
> unconsS' co ni (ListS f) = f co ni
>
> isNullS :: ListS a -> Bool
> isNullS = unconsS' (\_ _ -> False) True
>
> mapS :: (a -> b) -> ListS a -> ListS b
> mapS f =
>   unconsS' (\x xs -> consS (f x) (mapS f xs))
>            nilS

= References

Interested readers might like to take a look at the following papers for more information:

* [Comprehensive Encoding of Data Types and Algorithms in the λ-Calculus (Functional Pearl)][jmjansenLambdapaper] by JAN MARTIN JANSEN
* [Church Encoding of Data Types Considered Harmful for Implementations (Functional Pearl)][ifl2014_submission_13] by Pieter Koopman, Rinus Plasmeijer and Jan Martin Jansen

[rankn]: https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html
[jmjansenLambdapaper]: http://www.nlda-tw.nl/janmartin/papers/jmjansenLambdapaper.pdf
[ifl2014_submission_13]: https://ifl2014.github.io/submissions/ifl2014_submission_13.pdf
