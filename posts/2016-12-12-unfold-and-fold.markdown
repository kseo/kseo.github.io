---
title: unfold and fold
tags: unfold, fold, category theory
author: Kwang Yul Seo
---

# unfold

Every functional programmer loves *fold*. fold is [universal and expressive][fold]. But *fold* has a secret twin brother named *unfold* which undoes what *fold* does. In this post, we will see what *unfold* is and how it is related to *fold*.

`unfoldr` builds a list from a seed value while `foldr` reduces a list to a summary value.

```haskell
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
```

`unfoldr` takes the element and returns `Nothing` if it is done producing the list or returns `Just (a, b)`, in which case, `a` is a prepended to the list and `b` is used as the next element in a recursive call.

For example, we can define `iterate` as follows:

```haskell
iterate f == unfoldr (\x -> Just (x, f x))
```

Another simple use of `unfoldr`:

```haskell
> unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
[10,9,8,7,6,5,4,3,2,1]
```

As the name suggests, *unfold* is the categorical dual of *fold*. (Maybe it should be *cofold* instead of *unfold*.) It means we can get the signature of `foldr` by reversing the arrows of `unfoldr`, and vice versa.

Let's try this.

```haskell
unfoldr :: (b -> Maybe (a, b)) -> (b -> [a])
foldr   :: (Maybe (a, b) -> b) -> ([a] -> b)
```

Oops! It is not our beloved `foldr` function whose signature is:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

# Type isomorphisms

But don't be disappointed! We can show that they represent the same thing by using type isomorphisms:

```
(a → b → b) → b → ([a] → b)
```
> by a -> b -> c ~= (a, b) -> c
```
((a, b) → b) → b → ([a] → b)
```
> by a ~= () -> a
```
((a, b) → b) → (() -> b) → ([a] → b)
```
> by a -> b -> c ~= (a, b) -> c
```
(((a, b) → b), (() -> b)) → ([a] → b)
```
> by ((a -> c), (b -> c)) ~= Either a b -> c
```
((Either (a, b) ()) → b) → ([a] → b)
```
> by Either a () ~= Maybe a
```
(Maybe (a, b) -> b) → ([a] → b)
```

Now we can clearly see that *unfold* is the dual of *fold*. If you want to learn more on the relationship between *fold* and *unfold*, see Conal Elliott's [Folds and unfolds all around us][conal].

# Implementation

Here's an implementation of `unfoldr`.

```haskell
unfoldr :: (b -> Maybe (a, b)) -> (b -> [a])
unfoldr f b = case f b of
                Just (a, b') -> a : unfoldr f b'
                Nothing -> []
```

[fold]: http://www.cs.nott.ac.uk/~pszgmh/fold.pdf
[conal]: http://conal.net/talks/folds-and-unfolds.pdf
