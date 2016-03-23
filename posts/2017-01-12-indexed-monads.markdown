---
title: Indexed Monads
tags: indexed, monad
author: Kwang Yul Seo
---
In this post, I am going to introduce *indexed monads*, which generalize monads with additional type parameters carrying the information about the computational effects.

# Motivation

The State monad represents computations with a state that can be queried and updated. For example, an `Int` state is queried and updated during the computation of `c` in the following example. While the value of the state is changed from `0` to `1`, the type of the state remains the same during the entire computation.

```haskell
import Control.Monad.State

test1 = runState c (0::Int) where
         c = do
             v <- get
             put (succ v)
             return v
-- (0, 1)
```

This is okay in most cases, but we sometimes want to express a computation where not only the value but also the type of the state can be changed. The vanilla State monad is not general enough to express this requirement.

# Indexed Monads

Indexed monads are a generalization of monads that index each monadic type by an initial (type)state and a final (type)state. `m` is a type constructor for three type arguments, `p`, `q` and `a`. The argument `a` is the type of values produced by the monadic computation. `p` and `q` represent the types of the state before and after the computation.

```haskell
class IxMonad m where
    ireturn :: a -> m p p a
    ibind :: m p q a -> (a -> m q r b) -> m p r b
```

`ireturn` and `ibind` must meet the monad laws as the ordinary monads do. `ibind` is required to be associative and `ireturn` to be the left and the right unit of `ibind`.

All ordinary monads can be injected into `IxMonad` with a newtype wrapper `MW`. It is a *phantom type* as the type parameters `p` and `q` are not used on the right hand-side of `MW`.

```haskell
newtype MW m p q a = MW { unMW:: m a }

instance Monad m => IxMonad (MW m) where
    ireturn = MW . return
    ibind (MW m) f = MW (m >>= unMW . f)
```

Here is an example of using the ordinary `State` monad wrapped with `MW`. `iget` and `iput` wraps the result with `MW` newtype wrapper.

```haskell
iget :: (MonadState s m) => MW m s s s
iget = MW get

iput :: (MonadState s m) => s -> MW m s s ()
iput = MW . put

test2 = runState (unMW c) (0::Int) where
         c = iget `ibind` (
               \v -> iput (succ v) `ibind` (
                 \_ -> ireturn v))
-- (0, 1)
```

# Indexed State Monad

`IxStateT` defines an indexed state monad where `si` and `so` represents the input and the output state type respectively. The definition of `IxStateT` is similar to that of `StateT` except that the type of the state can be changed during the computation.

```haskell
newtype IxStateT m si so v = IxStateT { runIxStateT:: si -> m (so,v) }

instance Monad m => IxMonad (IxStateT m) where
  ireturn x = IxStateT (\si -> return (si,x))
  ibind (IxStateT m) f = IxStateT (\si -> m si >>= (\ (sm,x) -> runIxStateT (f x) sm))

vsget :: Monad m => IxStateT m si si si
vsget = IxStateT (\si -> return (si,si))

vsput :: Monad m => so -> IxStateT m si so ()
vsput x = IxStateT (\si -> return (x,()))
```

The following example gets an `Int` from the state and puts a `String` into the state. We can see that the type of the state is changed from `Int` to `String`.

```haskell
test3 = runIxStateT c (0::Int) >>= print where
         c = vsget `ibind` (
               \v -> vsput (show v) `ibind` (
                 \_ -> vsget `ibind` (
                   \v' -> ireturn (v,v'))))
-- ("0",(0,"0"))
```

# Do notation

The `IxMonad` examples above looks ugly as we couldn't use the do notation. Fortunately, `-XRebindableSyntax` extension allows us to overload the do-notation by providing alternative definitions that are local to the module.

```haskell
{-# LANGUAGE RebindableSyntax #-}

import Prelude hiding ((>>=), (>>), return)
import IxState

return :: (Monad m) => a -> IxStateT m si si a
return = ireturn

(>>=) :: (Monad m) => IxStateT m p q a -> (a -> IxStateT m q r b) -> IxStateT m p r b
(>>=) = ibind

(>>) :: (Monad m) => IxStateT m p q a -> IxStateT m q r b -> IxStateT m p r b
v >> w = v >>= \_ -> w

c :: (Monad m) => IxStateT m Int String (Int, String)
c = do
  v <- vsget
  vsput (show v)
  v' <- vsget
  return (v, v')
```

# Other definitions

There are multiple ways to define indexed monads. The one used here is from Robert Atkey's [Parameterised Notions of Computation][paramnotions].

Other definitions include:

* McBride: [Kleisli Arrows of Outrageous Fortune][Kleisli]
* Orchard: [Fun with indexed monads][ixmonad-fita14]

# References

1. Oleg Kiselyov's [Parameterized `monad'][param]
2. [Indexed Monad section][ix] of Stephen Diehl's What I Wish I Knew When Learning Haskell


[param]: http://okmij.org/ftp/Computation/monads.html#param-monad
[ix]: http://dev.stephendiehl.com/hask/#indexed-monads
[paramnotions]: http://bentnib.org/paramnotions-jfp.pdf
[Kleisli]: https://personal.cis.strath.ac.uk/conor.mcbride/Kleisli.pdf
[ixmonad-fita14]: http://www.cl.cam.ac.uk/~dao29/ixmonad/ixmonad-fita14.pdf
