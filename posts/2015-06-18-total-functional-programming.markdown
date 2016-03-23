---
title: Total functional programming
tags: total functional programming
author: Kwang Yul Seo
---

This is my summary of D.A. Turner's [Total Functional Programming][total].

# Introduction

The author suggests a simple discipline of *total functional programming* designed to exclude the possibility of non-termination (thus not Turing complete too).

A mathematical function must be total, but functions of Haskell and SML are partial because these languages allow unrestricted recursion. For example,

```haskell
loop :: Int->Int
loop n = 1 + loop n
```

Passing `0` to `loop`, we get

```haskell
loop 0 = 1 + loop 0
```

By subtracting `loop 0` from both sides, we get `0 = 1`. This non-sense happens because `loop 0` is not an integer despite being of type `Int`. It is ⊥, the undefined integer.

# Total functional programming

In total functional programming, ⊥ does not exist. If we have an expression `e` of type `Int`, we can be sure that evaluation of `e` will terminate with an integer value. There are no run-time errors. There are three main advantages of total functional programming:

1. Simpler Proof Theory

Because every function is a mathematical function, we can do equational reasoning without taking ⊥ into consideration.

2. Simpler Language Design

The presence of ⊥ makes the language complicated because we have a fundamental language design choice - whether to make functional application strict in the argument.

```haskell
f ⊥ = ⊥
```

SML says yes to this while Haskell uses lazy evaluation as the norm. Another example is the `&` operator on `Bool`.

```haskell
True & True = True
True & False = False
False & True = False
False & False = False
⊥ & y = ?
x & ⊥ = ?
```

Most language chose left-strict `&`, but this choice is arbitrary and breaks the symmetry which `&` has in logic.

In total function programming, these semantic choices go away. A split between strict and non-strict languages no longer exists and the evaluation order won't affect the outcome.

3. Flexibility of Implementation

In total functional programming, reduction is *strongly Church-Rosser*, meaning *every reduction sequence leads to a normal form and normal forms are unique*. This gives much greater freedom for implementor to choose an efficient evaluation strategy without affecting the outcome.

However, total functional programming has disadvantages in that programs are no longer Turing complete and there is no way to write an operating system. The author suggests *elementary total functional programming* with codata as a solution for these disadvantages.

# Elementary total functional programming

Elementary means

* Type structure no more complicated than Hindley/Milner
* Programs and proofs will be kept separate

It is a strongly terminating subset of Haskell (or SML) by imposing three essential restrictions to maintain totality

1. All case analysis must be complete.

Where a function is defined by pattern matching, every constructor of the argument type must be covered.

Also make all built-in operations total. For example,

```haskell
0 / 0 = 0
```

Make `hd` total by supplying an extra argument, which is the value to be returned if the list is empty. Or simply don't use `hd` and always do a case analysis.

2. Type recursion must be covariant.

It means type recursion through the left hand side of → is prohibited. Otherwise, we can obtain a value, `foo` of type `X`, which is equivalent to `loop 0` above.

```haskell
data Silly = Very (Silly->X) ||not allowed!

bad :: Silly -> X
bad (Very f) = f (Very f)
foo :: X
foo = bad (Very bad)
```

3. Each recursive function call must be on a syntactic subcomponent of its formal parameter.

Unrestricted general recursion brings back ⊥. So we allow only *structural recursion*, which is guaranteed to terminate. The author claims that many common algorithms can be written in primitive recursion though some of them need style changes or intermediate data structures.

# Programming with Codata

To write an operating system in total functional programming, we need infinite lists. But being total means it is not possible to define infinite data structures. Codata is the solution for this dilemma.

```haskell
codata Colist a = Conil | a <> Colist a
```

The type `Colist` contains all the infinite lists as well as finite ones. We can get the infinite ones by omitting `Conil` alternative.

We do *primitive corecursion* on codata. Ordinary recursion on codata is not legal because it might not terminate. Conversely corecursion is not legal on data because data must be finite.

```haskell
ones :: Colist Nat
ones = 1 <> ones

fibs :: Colist Nat
fibs = f 0 1
       where
       f a b = a <> f b (a+b)
```

All these infinite structures are total. Every expression whose principle operator is a coconstructor is in normal form.

# Coinduction

The definition of bisimilarity

```haskell
x ≈ y ⇒ hd x = hd y ∧ tl x ≈ tl y
```

In other words, two pieces of codata are bisimilar if:

* their finite parts are equal, and
* their infinite parts are bisimilar.

We can take it as the definition of equality on infinite objects, and perform equational reasoning in proofs.

# Beyond structural recursion

The author also mentions *Walther recursion*, a generalisation of primitive recursion.

# Observations and Concluding Remarks

It may be time to reconsider the decision to choose universality (a language in which we can write all terminating programs and silly programs which fail to terminate) over security.

[total]: https://uf-ias-2012.wikispaces.com/file/view/turner.pdf
