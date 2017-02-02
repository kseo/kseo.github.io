---
title: Context reduction
tags: context reduction
author: Kwang Yul Seo
---
Hello, Haskellers! Today I am going to explain what *context reduction* is and why it is necessary.

<!--more-->

# Quiz

Let's start with a quick quiz. What's the type of `f`?

```haskell
f xs y  =  xs == [y]
```

The return type `f` must be `Bool` because the type of `==` is `Eq a => a -> a -> Bool`. If we assume the type of `y` is `t`, the type of `xs` must be `[t]` because two operands of `==` must have the same type. The type constraint must be `Eq [t]` because two lists are compared for equality. So we expect the type of `f` should be `Eq [t] => [t] -> t -> Bool`.

Let's check the type in GHCi.

```
λ> f xs y  =  xs == [y]
f :: Eq t => [t] -> t -> Bool
```

Surprisingly, the context is `Eq t` instead of `Eq [t]`. Even though the equality is taken at the list type, the context must be simplified. This is called *context reduction* and is specified in [Haskell 2010 Language Report][haskell2010] (also in Haskell 98).

# Context reduction

[Type Classes and Constraint Handling Rules][0006034] mentions two reasons why context reduction in Haskell is important.

1.  Syntactically, context reduction allows the type checker to present type class constraints to the programmer in a more readable form.
1. Operationally, context reduction allows the type checker to put type class constraints into a more efficient form. Type class constraints are translated into dictionaries. Hence, simplifying type class constraints may allow a more efficient translation.

Let's visit each reason with concrete examples.

# Readability

What's the type of `g`?

```haskell
g a b = [show (a,a), show (a,b), show (b,a), show(b,b)]
```

If the type checker infers the type without simplification, it will be

```haskell
g :: (Show (a,a), Show(b,b), Show (a,b), Show (b, a)) => a -> b -> [String]
```

But Haskell simplifies the context to

```haskell
g :: (Show b, Show a) => a -> b -> [String]
```

The inferred type looks simpler to programmers.

Surprisingly, GHCi reports the simplified type even though I explicitly annotate the type with the former.

```
λ> :type g
g :: (Show b, Show a) => a -> b -> [String]
```

# Efficient translation

GHC implements type classes as dictionary passing. Readers are referred to Section 4 of [How to make ad-hoc polymorphism less ad hoc][wadler88] for the details.

Let's see how type classes are actually translated by dumping the GHC simplifier output.

```
ghc -ddump-simpl -ddump-to-file -c a.hs
```

```haskell
{-# NOINLINE f #-}
f :: (Eq a, Ord a) => a -> a -> Bool
f x y = x > y

main = print $ f 1 2
```

`g_rn6` takes two dictionary arguments though the first one is never used (marked as `Dead`).

```
f_rn6
  :: forall a_aoY. (Eq a_aoY, Ord a_aoY) => a_aoY -> a_aoY -> Bool
[GblId, Arity=4, Caf=NoCafRefs, Str=DmdType]
f_rn6 =
  \ (@ a_a1vN)
    _ [Occ=Dead]
    ($dOrd_a1vP :: Ord a_a1vN)
    (x_a1rc :: a_a1vN)
    (y_a1rd :: a_a1vN) ->
    > @ a_a1vN $dOrd_a1vP x_a1rc y_a1rd
```

Call sites of `g` must create and pass these dictionary arguments when they call `g`.

```
main :: IO ()
[GblId, Str=DmdType]
main =
  print
    @ Bool
    GHC.Show.$fShowBool
    (f_rn6
       @ Integer
       integer-gmp-1.0.0.1:GHC.Integer.Type.$fEqInteger
       integer-gmp-1.0.0.1:GHC.Integer.Type.$fOrdInteger
       1
       2)
```

Simplifying type class constraints allow a more efficient translation because it removes redundant dictionary arguments.

```haskell
{-# NOINLINE f #-}
f x y = x > y

main = print $ f 1 2
```

is translated to

```
f_rn6 :: forall a_a1vz. Ord a_a1vz => a_a1vz -> a_a1vz -> Bool
[GblId, Arity=3, Caf=NoCafRefs, Str=DmdType]
f_rn6 =
  \ (@ a_a1vz)
    ($dOrd_a1zP :: Ord a_a1vz)
    (x_aoY :: a_a1vz)
    (y_aoZ :: a_a1vz) ->
    > @ a_a1vz $dOrd_a1zP x_aoY y_aoZ
```

`g_rn6` takes only one dictionary argument `$dOrd_a1zP` because context reduction merged `(Eq a, Ord a)` into `Ord a`. This is a valid simplification because `Ord a` implies `Eq a`.

# Formal semantics

The Haskell report provides only informal hints about context reduction.

Fortunately, Section 7.4 of Mark P. Jones' [Typing Haskell in Haskell][thih] gives us the formal semantics of context reduction in Haskell. Section 3.2 of [Type classes: exploring the design space][type-classes] also discusses context reduction. Interested readers are referred to both papers.

[haskell2010]: https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-620004
[0006034]: https://arxiv.org/pdf/cs/0006034.pdf
[wadler88]: http://people.csail.mit.edu/dnj/teaching/6898/papers/wadler88.pdf
[thih]: http://web.cecs.pdx.edu/~mpj/thih/thih.pdf
[type-classes]: https://research.microsoft.com/en-us/um/people/simonpj/Papers/type-class-design-space/
