---
title: Objects versus Abstract Data Types
tags: objects, abstract data types
author: Kwang Yul Seo
---

William R. Cook's essay titled [On Understanding Data Abstraction,
Revisited][essay] explains the differences between two forms of data
abstraction, *objects* and *abstract data types*. His another paper,
[Object-Oriented Programming Versus Abstract Data Types][CookOOPvsADT90] shows
why distinguishing these two data abstractions are important:

> Object-oriented programming and abstract data types can also be viewed as
> complementary implementation techniques: objects are centered around the
> constructors of a data abstraction, while abstract data types are organized
> around the operations. These differences have consequences relating to
> extensibility, efficiency, typing, and verification; in many cases the
> strengths of one paradigm are the weaknesses of the other. Most
> object-oriented programming languages support aspects of both techniques, not
> a unification of them, so an understanding of their relative merits is useful
> in designing programs.

Haskell type classes are also discussed in Cook's essay. He described Haskell
type classes as a powerful mechanism for parameterization and extensibility
because type classes are unique in that they have properties of both objects and
abstract data types:

* Type classes do not allow different instances to interoperate, but they allow
  multiple type class instances within a single program.
* Type classes are similar to object interfaces in allowing a method to operate
  on any value that has the necessary operations.
* Type classes are based on algebraic signatures as in abstract data types, but
  they do not enforce any hiding of representations.

[essay]: http://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf
[CookOOPvsADT90]: https://www.cs.utexas.edu/users/wcook/papers/OOPvsADT/CookOOPvsADT90.pdf

