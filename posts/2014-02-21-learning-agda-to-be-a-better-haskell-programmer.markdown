---
title: Learning Agda to be a better Haskell programmer
tags: Haskell, Agda, dependent type
author: Kwang Yul Seo
---

In my previous post [Learning Prolog to be a better Haskell
programmer][learn-prolog], I advocated learning Prolog is quite helpful to get
more intuitions on Haskell type-level programming.

I think a good next step is to learn a dependent typed programming language such
as [Agda][agda] or [Epigram][epigram]. As learning Haskell is a good way to
develop oneself as a better Java programmer, learning a dependent typed
programming language is a good way to develop oneself as a better Haskell
programmer.

Among many dependent typed programming languages, I recommend Agda simply
because its surface syntax is quite similar to that of Haskell. Because
dependent typed programming languages in general are not mature enough to
perform day-to-day programming task and most of them are more or less equivalent
in powers, choosing a syntactically familiar language helps you understand more
advanced type system behind the syntax.

As the name "dependent type" implies, the biggest difference lies in the type
system. While Haskell’s type system strictly splits values and types, Agda
blurs the distinction between types and values. Type level programming in
Haskell with type families or functional dependencies is esoteric at best, but
type level programming in Agda is a norm.

For example, it is possible to define a type of lists of a certain length. In
this setting, it is a type error to pass an empty list to head.

```agda
data Vec (A : Set) : Nat -> Set where
     [] : Vec A zero
     _::_ : {n : Nat} -> A -> Vec A n -> Vec A (suc n)
 
head : {A : Set}{n : Nat} -> Vec A (suc n) -> A
head (x :: xs) = x
```

Please refer to [Dependently Typed Programming in Agda][AgdaIntro] and [Daniel
Peebles’s introduction on Agda][peebles] for more information on Agda.

[learn-prolog]: https://kseo.github.io/posts/2014-02-17-learning-prolog-to-be-a-better-haskell-programmer.html
[agda]: http://wiki.portal.chalmers.se/agda/pmwiki.php
[epigram]: https://code.google.com/archive/p/epigram/
[AgdaIntro]: http://www.cse.chalmers.se/~ulfn/darcs/AFP08/LectureNotes/AgdaIntro.pdf
[peebles]: https://www.youtube.com/playlist?p=B7F836675DCE009C

