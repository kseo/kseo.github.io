---
title: Constraint Kinds
tags: constraints, kinds
author: Kwang Yul Seo
---
In this blog post, I will show some examples of using the [ConstraintKinds][1] GHC extension.

# Constraints

Constraints in Haskell mean one of the following things:

* Class constraints, e.g. `Show a`
* *Implicit parameter constraints*, e.g. `?x::Int` (with the `-XImplicitParams` flag)
* *Equality constraints*, e.g. `a ~ Int` (with the `-XTypeFamilies` or `-XGADTs` flag)
* Tuples of any of the above, e.g., `(Show a, a ~ Int)`

These constraints can only occur to the left of => arrow in standard Haskell.

# Kinds

In Haskell, types have types which are called *kinds*.

* The kind `*` is for types that have terms (even though not every type of kind `*` needs be inhabited if we exclude ⊥).
* `k -> l` forms a function kind to indicate types of kind `l` that are parameterized over types of kind `k`.

Examples:

* kind `*`: `Char`, `Bool`, `Char -> Bool`, `Maybe Int`, `[Int]`
* kind `* -> *`: `Maybe`, `[]`
* kind `* -> * -> *`: `Either`, `(,)`

While *data* introduces types of kind `*`, *classes* introduce types of kind `Constraint`.

* Classes such as `Show`, `Eq` or `Ord` are of kind `* -> Constraint`. They form a class constraint when applied to types of kind `*`.
* Classes such as `Functor` or `Monad` are of kind `(* -> *) -> Constraint`. They form a class constraint when applied to type constructors of kind `* -> *`.

We can use tuple syntax to create empty constraints and combine constraints.

```haskell
type NoConstraint = (() :: Constraint)
type Text a = (Read a, Show a)
```

# ConstraintKinds

Classes and contexts were not first-class citizens in Haskell, but the introduction of the *Constraint kind* has changed this and allows them to be used as parameters of types.

```haskell
{-# LANGUAGE ConstraintKinds #-}
```

With this extension, constraints can be used in new ways:

* Aliases of classes, partially applied classes, and contexts can be defined using type declarations
* Families of classes, partially applied classes, and contexts can be defined using type synonym families

Let's visit each use case with concrete examples.

# Constraint synonyms

Since constraints are now just types, type synonyms can be reused on constraints.

```haskell
type Text a = (Show a, Read a)
```

Here is a real world example of constraint synonyms defined in the [Haskell Tool Stack][StackT].

```haskell
-- | Constraint synonym for all of the common environment instances
type HasEnv r = (HasLogOptions r, HasTerminal r, HasReExec r, HasSticky r)

-- | Constraint synonym for constraints commonly satisifed by monads used in stack.
type StackM r m =
    (MonadReader r m, MonadIO m, MonadBaseControl IO m, MonadLoggerIO m, MonadMask m, HasEnv r)
```

Or we can define `Func` in terms of two type arguments `ctx` and `a` where `ctx` is a context of kind `* -> Constraint`.

```haskell
type Func cxt a = cxt a => a -> a

inc :: Func Num a
inc = (+1)
```

# Constraint families

Constraint families allow constraints to be indexed by a type in the same way that type families and data families allow types to be indexed by types.

For example, constraint families let us define a generalized version of the `Monad` class where we can impose some constraints on the element it can contain. Wolfgang Jeltsch's [The Constraint kind][2] and Max Bolingbroke's [Constraint Kinds for GHC][3] explain this classic example in details.

[rmonad][rmonad] provides a collection of restricted monads based on associated datatypes.

```haskell
class RMonad m where
  type RMonadCtxt m a :: Constraint
  type RMonadCtxt m a = ()

  return :: RMonadCtxt m a => a -> m a
  (>>=) :: (RMonadCtxt m a, RMonadCtxt m b) => m a -> (a -> m b) -> m b

instance RMonad S.Set where
  type RMonadCtxt S.Set a = Ord a
  return = S.singleton
  mx >>= fxmy = S.fromList [y | x <- S.toList mx, y <- S.toList (fxmy x)]      

instance RMonad [] where
  return x = [x]
  (>>=) = flip concatMap
```

[1]: https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#the-constraint-kind
[2]: https://jeltsch.wordpress.com/2013/02/14/the-constraint-kind/
[3]: http://blog.omega-prime.co.uk/?p=127
[StackT]: https://github.com/commercialhaskell/stack/blob/master/src/Stack/Types/StackT.hs#L67
[rmonad]: https://hackage.haskell.org/package/rmonad
