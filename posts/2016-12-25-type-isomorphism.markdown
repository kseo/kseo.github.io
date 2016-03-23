---
title: Type Isomorphism
tags: type isomorphism
author: Kwang Yul Seo
---
Type isomorphisms are a general notion of conversion between types. We say that type `A` and `B` are isomorphic, if we have conversion functions `f :: A -> B` and `g :: B -> A` such that

```
f . g = idB
g . f = idA
```

Type isomorphisms imply that we can safely convert without loss of information between types.

# Motivation

There always exist multiple types that can represent the same values we want to represent. The problem is that they are not compatible because the type system does not automatically recognize them as equal. So we might not be able to reuse the existing libraries as the types we use in our program are not compatible with the types these libraries use. Type isomorphisms provide wrapping/unwrapping functions that can safely convert between these types.

Type isomorphisms also help us understand various transformations used in equational reasoning and API design.

# Basics

Haskell programmers already use the type isomorphism to reason about programs. For example, we know that pair `(a,b)` is isomorphic to `(b,a)` because `swap` is the conversion function in both directions.

```haskell
swap :: (a,b) -> (b,a)
swap (a, b) = (b, a)
```

Another example of type isomorphism is `a` and `() -> a`. We can define conversion functions as follows:

```haskell
f :: a -> () -> a
f = \x _ -> x

g :: (() -> a) -> a
g k = k ()
```

What about `a -> ()`? This type is unsurprisingly isomorphic to `()` because `a -> ()` type has only one inhabitant which discards the argument and returns `()`. Conversion functions are:

```haskell
f :: (a -> ()) -> ()
f _ = ()

g :: () -> a -> ()
g = \_ _ -> ()
```

# Currying and uncurrying

In functional programming, *currying* transforms a function that takes multiple arguments via a pair, into a function that accepts the first argument of the pair, and returns a function that accepts the second argument, before returning the result. *uncurrying* performs transformation in the opposite direction.

Here are the types of `curry` and `uncurry` functions.

```haskell
curry :: ((a, b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a, b) -> c
```

From the signature of these functions, we can see that `(a, b) -> c` and `a -> b -> c` are isomorphic by `curry` and `uncurry` functions.

# Continuation

`a` can be converted to `(a -> r) -> r` by CPS transformation. A CPS term can be converted back to the direct style by applying `id` function.

```
{-# LANGUAGE RankNTypes #-}

f :: a -> (a -> r) -> r
f a b = b a

g :: (forall r. (a -> r) -> r) -> a
g a = a id
```

`RankNTypes` extension is necessary to represent the type of `g`.

# Algebraic data types

Every algebraic data type can be represented with combinations of *product* and *sum* types. This is why these types are called *algebraic data types*.

In Haskell, products are encoded by `(a, b)` and sums are encoded by `Either a b`. Thus an algebraic data type of Haskell is isomorphic to some combinations of `(a, b)` and `Either a b`. Let's see a few examples.

`Bool` is isomorphic to `Either () ()` because we can define conversion functions `f` and `g`:

```haskell
f :: Bool -> Either () ()
f True = Left ()
f False = Right ()

g :: Either () () -> Bool
g (Left ()) = True
g (Right ()) = False
```

`Maybe a` is isomorphic to `Either a ()` or `Either () a`.

```haskell
f :: Maybe a -> Either () a
f (Just x) = Right x
f Nothing = Left ()

g :: Either () a -> Maybe a
g (Left ()) = Nothing
g (Right x) = Just x
```

# Playing with type isomorphism

*unfold* is the categorical dual of *fold*. It means we can get the type of `unfold` by reversing arrows of `fold`.

Here are the type signatures of `foldr` and `unfoldr` taken from the `base` library.

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
unfoldr :: (b -> Maybe (a, b)) -> (b -> [a])
```

It is not obvious how `foldr` and `unfoldr` are related. But we can apply the types isomorphism we've learned to derive the type of `unfoldr` from `foldr`.

```
(a → b → b) → b → ([a] → b)
=== ((a, b) → b) → b → ([a] → b)
=== ((a, b) → b) → (() -> b) → ([a] → b)
=== (((a, b) → b), (() -> b)) → ([a] → b)
=== ((Either (a, b) ()) → b) → ([a] → b)
=== (Maybe (a, b) -> b) → ([a] → b)
```

We used the following type isomorphisms:

* `a -> b -> c ~= (a, b) -> c`
* `a ~= () -> a`
* `((a -> c), (b -> c)) ~= Either a b -> c`
* `Either a () ~= Maybe a`.

Finally, by reversing the arrows of `foldr`, we get `unfoldr`.

```
foldr   :: (Maybe (a, b) -> b           ) -> ([a] -> b  )
unfoldr :: (b            -> Maybe (a, b)) -> (b   -> [a])
```

Interested readers might want to take a look at my previous post [unfold and fold][unfold] and Conal Elliott's [Folds and unfolds all around us][folds-and-unfolds] for the details.

# Existentials

`{∃X,T}` and `∀Y. (∀X. T→Y) → Y` are isomorphic types. My previous post [Encoding existentials][existentials] shows how we can encode existential types using only `RankNTypes` and *forall* quantifier.

[existentials]: https://kseo.github.io/posts/2016-12-19-encoding-existentials.html
[unfold]: https://kseo.github.io/posts/2016-12-12-unfold-and-fold.html
[folds-and-unfolds]: http://conal.net/talks/folds-and-unfolds.pdf
