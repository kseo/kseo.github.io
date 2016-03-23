---
title: Lazy vs Strict State Monad
tags: state monad, lazy, strict
author: Kwang Yul Seo
---
[mtl][mtl] (or its underlying [transformers][transformers]) package provides two types of `State` monad; `Control.Monad.State.Strict` and `Control.Monad.State.Lazy`. `Control.Monad.State` re-exports `Control.Monad.State.Lazy`.

The difference between these two state monads does not matter in most cases, but it may cause unexpected surprises when infinite lists are involved. In this post, I am going to explain the subtle difference between lazy and strict state monads.

Let's start the discussion with a simple example. The program shown below returns an infinite list of integers `[1..]` in a lazy state monad. Running the program prints `[1,2,3,4,5]` as expected.

```haskell
import Control.Monad.State.Lazy

foo :: State () [Int]
foo = traverse pure [1..]

main = print $ take 5 (evalState foo ())
```

However, when we replace the import with `Control.Monad.State.Strict`, the program hangs up.

```haskell
import Control.Monad.State.Strict

foo :: State () [Int]
foo = traverse pure [1..]

main = print $ take 5 (evalState foo ())
```

What happened here? The definition of `traverse` might give us a hint.

```haskell
instance Traversable [] where
  traverse f = List.foldr cons_f (pure [])
    where cons_f x ys = (:) <$> f x <*> ys
```

From the definition of `traverse`, we can see that `traverse return [1..]` expands to

```
(:) <$> (return 1) <*> ((:) <$> (return 2) <*> ((:) <$> (return 3) <*> (...)))
```

`(<$>)` and `(<*>)` operators are used to combine values. `(<$>)` are `(<*>)` are defined in `Functor` and `Applicative` instances of `State` monad respectively.

Let's compare the definitions of these operators.

* Control.Monad.Trans.State.Lazy

```haskell
instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    StateT mf <*> StateT mx = StateT $ \ s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s'')
```

* Control.Monad.Trans.State.Strict

```haskell
instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ (a, s') -> (f a, s')) $ runStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    StateT mf <*> StateT mx = StateT $ \ s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        return (f x, s'')
```    

The two definitions are almost the same except for a small difference in pattern matching. Did you find it? Yes, the lazy version uses a tilde `~` in pattern matching on a pair. It is a [lazy pattern][lazypattern] matching.

Here's the secret. In the strict version, the pattern matches on the pair forces its evaluation. So `traverse pure [1..]` never returns until its evaluation is finished. The lazy version avoids this evaluation of the pair using an irrefutable pattern `~(a,w)`. Evaluation is forced later when the pair is actually needed. This is why we can manipulate infinite lists in a lazy state monad.

But this observation does not imply that we should always prefer the lazy version of state monad because the lazy state monad often builds up large thunks and causes space leaks due to its laziness.

[mtl]: https://www.stackage.org/lts-7.12/package/mtl-2.2.1
[transformers]: https://www.stackage.org/lts-7.12/package/transformers-0.5.2.0
[lazypattern]: https://wiki.haskell.org/Lazy_pattern_match
