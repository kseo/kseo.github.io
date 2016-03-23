---
title: Hindley-Milner type inference with constraints
tags: type inference, Hindley-Milner, constraints
author: Kwang Yul Seo
---
Algorithm W is the best known algorithm for implementing Hindley-Milner type inference. But it is a bit complicated as it intermingles two separate processes: constraint generation and solving.

There is an alternative approach based on *constraints generation*. In this approach, constraints are collected by bottom-up traversal, and then solved independently. Heeren's [Generalizing Hindley-Milner Type Inference Algorithms][Heeren] paper describes the algorithm in details.

Here's [my implementation][poly_constraints] of Heeren's algorithm. I forked Stephen Diehls's [Poly][poly] and modified [the type checker][infer] to use the Heeren's algorithm.

[Heeren]: http://soft.vub.ac.be/~cfscholl/Capita-Selecta-2015/papers/2002%20Heeren.pdf
[poly]: https://github.com/sdiehl/write-you-a-haskell/tree/master/chapter7/poly_constraints
[poly_constraints]: https://github.com/kseo/poly_constraints
[infer]: https://github.com/kseo/poly_constraints/blob/master/src/Infer.hs
