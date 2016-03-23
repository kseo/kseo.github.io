---
title: Defects of von Neumann languages
tags: Haskell, von Neumann, John Backus, FP
author: Kwang Yul Seo
---

[John Backus][backus], the inventor of Fortran criticized the defects of von
Neumann languages (aka, imperative languages) in his [1977 Turing Award
Lecture][CanProgrammingBeLiberated]. Most of his points still remain valid until
today!

The defects can be summarized:

* close coupling of semantics to state transitions
* division of programming into a world of expressions and a world of statements
* inability to effectively use powerful combining forms for building new
  programs from existing ones
* lack of mathematical properties for reasoning about programs

He described conventional programming languages as *fat and flabby* because each
successive programming language adds more and more features with little
cleaning up. Even though most conventional languages are similar as they simply
add higher level constructs to the same underlying von Neumann computing model,
learning a new language still takes long time due to many subtle corner cases in
their semantics.

He evaluated von Neumann model with 4 criteria:

* _Foundations_: complex, bulky, not useful compared to Turing machines, various
  automata, Church’s lambda calculus or Curry’s system of combinators
* _History sensitivity_: have storage, are history sensitive
* _Type of semantics_: state transition with complex states (not simple as in
  automata)
* _Clarity and conceptual usefulness of programs_: programs can be moderately
  clear, are not very useful conceptually

What’s the von Neumann computer? It has three parts: CPU, a store and a
connecting tube that can transmit a single word between the CPU and the store.
Programming on this machine is to change the contents of the store by repeatedly
transferring single words between CPU and the store through the tube. So he
called this tube the von Neumann bottleneck.

By the von Neumann bottleneck, he does not mean the limited bandwidth of the
tunnel (bus). The real problem is that it becomes an intellectual bottleneck
that keeps us tied to word-at-a-time thinking instead of larger conceptual
thinking. The assignment statement in von Neumann languages corresponds to the
von Neumann bottleneck which keeps us thinking in word-at-a-time terms in much
the same way the computer’s bottleneck does.

So he proposed Functional Programming (FP) system as an alternative to von
Neumann languages. FP is a functional programming language in point-free style.
Please read the original paper for the details of the language. If you are
interested in the implementation of FP, [FP][fp] is my Haksell implementation of
FP.

[backus]: https://en.wikipedia.org/wiki/John_Backus
[CanProgrammingBeLiberated]: http://worrydream.com/refs/Backus-CanProgrammingBeLiberated.pdf
[fp]: https://github.com/kseo/fp

