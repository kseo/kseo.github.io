---
title: Type inference algorithms of Haskell-like languages
tags: type inference, type system, Haskell
author: Kwang Yul Seo
---
I collected papers on the type inference algorithms used by Haskell-like languages.

# Haskell

Haskell supports advanced type system features such as GADTs, type classes and type families. The current type checker implemented by GHC is described in [OutsideIn(X): Modular type inference with local assumptions][outsideinx].

[outsideinx]: https://www.microsoft.com/en-us/research/publication/outsideinx-modular-type-inference-with-local-assumptions/

# PureScript

The type checker of PureScript is inspired by the following papers. It supports type classes, row polymorphism, higher kinded polymorphism (rank N types). There are no soundness proofs yet.

* [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism][bidir]
* [HMF: Simple Type Inference for First-Class Polymorphism][hmf]
* [Koka: Programming with Row-polymorphic Effect Types][koka]

[bidir]: https://people.mpi-sws.org/~neelk/bidir.pdf
[hmf]: https://www.microsoft.com/en-us/research/publication/hmf-simple-type-inference-for-first-class-polymorphism/
[koka]: https://www.microsoft.com/en-us/research/publication/koka-programming-with-row-polymorphic-effect-types/

# Elm

Elm's type checker is an implementation of Pottier and Rem's [The Essence of ML Type Inference][emlti] with two extensions:

* A limited set of built-in type classes (i.e. number, appendable, comparable)
* Records based on [Extensible records with scoped labels][extensible]

There is no support for type classes or higher kinded polymorphism yet.

[emlti]: http://gallium.inria.fr/~fpottier/publis/emlti-final.pdf
[extensible]: https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/
