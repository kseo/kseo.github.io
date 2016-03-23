---
title: Data.Typeable and Data.Dynamic in Haskell
tags: Haskell, Data.Typeable, Data.Dynamic, dynamic typing
author: Kwang Yul Seo
---
This article is written in literate Haskell.

[Data.Typeable][typeable] is a way to implement dynamic (delayed) type checking
in Haskell using a [universal type][universal-type].

For example, you can implement a heterogenous list in Haskell. `toDyn` converts
any `Typeable` instance into `Dynamic` which is similar to Java `Object` type.
Any type that is an instance of `Typeable` class can be wrapped with `Dynamic`
type.

> import Data.Dynamic
> import Data.Maybe

> hlist :: [Dynamic]
> hlist = [ toDyn ("string" :: String)
>         , toDyn (7 :: Int)
>         , toDyn (pi :: Double)
>         , toDyn 'x'
>         , toDyn (((), Just "foo") :: ((), Maybe String))
>         ]

> dyn :: Dynamic
> dyn = hlist !! 1

To be precise, `hlist` is not actually a heterogenous list from the point of
Haskell type system. It is just a homogenous list of `Dynamic`. The chapter 20,
“Untyped Means Uni-Typed” of Harper’s textbook also emphasizes this observation:
dynamic types (with typeable representations) are statically typed languages
with only one type.

You can convert a `Dynamic` object back into an ordinary Haskell value using
`fromDynamic`. Type checking is dynamic because it is delayed to runtime.

> v :: Int
> v = case fromDynamic dyn of
>         Nothing -> error "Type mismatch"
>         Just x  -> x

You can make any type `Typeable` by adding `deriving Data.Typeable`. In GHC, you
need to turn on `-XDeriveDataTypeable` option to make GHC automatically derive
the instance for you.

The `Data.Typeable` class is used primarily for generic programming in the "Scrap
Your Boilerplate (SYB)" style. I will write more on this later.

= References

* Martín Abadi, Luca Cardelli, Benjamin Pierce and Gordon Plotkin, "[Dynamic
  Typing in a Statically Typed Language][SRC-RR-47]", ACM Transactions on
  Programming Languages and Systems (TOPLAS), 1991.
* James Cheney and Ralf Hinze, "[A lightweight implementation of generics and
  dynamics][HW02]", Haskell ’02: Proceedings of the 2002 ACM SIGPLAN Workshop on
  Haskell, 2002.
* Lammel, Ralf and Jones, Simon Peyton, "[Scrap your boilerplate: a practical
  design pattern for generic programming][hmap]", TLDI ’03: Proceedings of the
  2003 ACM SIGPLAN International Workshop on Types in Languages Design and
  Implementation, 2003
* Harper, 2011, Practical Foundations for Programming Languages.

[typeable]: http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Typeable.html
[universal-type]: http://www.haskell.org/haskellwiki/Heterogenous_collections#A_Universal_type
[SRC-RR-47]: http://www.hpl.hp.com/techreports/Compaq-DEC/SRC-RR-47.pdf
[HW02]: https://www.cs.ox.ac.uk/people/ralf.hinze/publications/HW02.pdf
[hmap]: http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/hmap.ps
