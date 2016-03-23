---
title: Write your own stream processing library Part1
tags: coroutine, generator, iteratee, pipeline, conduit, pipes
author: Kwang Yul Seo
---
[pipes][pipes] and [conduit][conduit] are two competing libraries for handling stream data processing in Haskell. Though both libraries provide excellent tutorials on the usage of the libraries, the implementation details are impenetrable to most Haskell programmers.

The best way to understand how these streaming libraries work is to write a minimalistic version by ourselves. In this post, I will show you how we can write a small streaming data library with *coroutine*. Our implementation is based on Mario Blazevic's excellent article [Coroutine Pipelines][issue19].

<!--more-->

# Generator

`Generator` is a monad transformer which allows the base monad to pause its computation and yield a value. This corresponds to `Producer` of pipes or `Source` of conduit.

```haskell
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Control.Monad.Trans.Class

newtype Generator a m x =
  Generator { bounceGen :: m (Either (a, Generator a m x) x) }
```

`Generator a m x` represents a computation which yields values of type `a` on top of the base monad `m` and returns a value of type `x`.

`Either` indicates that `Generator` has two cases:

* `(a, Generator a m x)`: A pair of a yielded value and a suspension to be resumed.
* `x`: A return value `x`.

The enclosing `m` allows us to perform monadic actions while running the generator.

The definition of `Monad` instance for `Generator` is as follows:

```haskell
instance Monad m => Monad (Generator a m) where
  return  = Generator . return . Right
  t >>= f = Generator $ bounceGen t
                      >>= \case Left (a, cont) -> return $ Left (a, cont >>= f)
                                Right x -> bounceGen (f x)

instance MonadTrans (Generator a) where
  lift = Generator . liftM Right

yield :: Monad m => a -> Generator a m ()
yield a = Generator (return $ Left (a, return ()))
```

`>>=` operator has two cases to consider. If `t` is a suspension (`Left` case), it yields `a` and combines the remaining computation `cont` with `f`. If `t` is a value `x` (`Right` case), it continues the computation by passing the value to `f`. Once we define `>>=` this way, the definition of `yield` is straightforward. It yields a value and does nothing more.

To run a `Generator`, we need `runGenerator` function which collects the yielded values while executing the generator. `run'` uses a [difference list][dlist] to collect yielded values and converts it to the normal list by applying `[]` at the end.

```haskell
runGenerator :: Monad m => Generator a m x -> m ([a], x)
runGenerator = run' id where
  run' f g = bounceGen g
             >>= \case Left (a, cont) -> run' (f.(a:)) cont
                       Right x -> return (f [], x)
```

Now we are ready to create generators. `triple` is a generator which yields the given value three times.

```haskell
triple :: Monad m => a -> Generator a m ()
triple x = do
    yield x
    yield x
    yield x
```

Running `triple 3` returns `([3, 3, 3], ())` as expected.

```
λ> runGenerator $ triple 3
([3,3,3],())
```

When the base monad is `IO`, we can interleave IO actions. For example, `loop` yields the line input from the stdin until an empty string is read.

```haskell
loop :: Generator String IO ()
loop = do
    str <- lift getLine
    when (str /= "") $ do
      yield str
      loop
```

```
λ> runGenerator loop
Hello
world!

(["Hello","world!"],())
```

It is even possible to mix two generators by alternating each generator.

```haskell
alternate :: Monad m => Generator a m () -> Generator a m () -> Generator a m ()
alternate g1 g2 = Generator $ liftM2 go (bounceGen g1) (bounceGen g2)
  where
    go (Left (a, cont)) (Left (b, cont')) = Left  (a, Generator $ return $ Left (b, alternate cont cont'))
    go (Left (a, cont)) (Right _)         = Left  (a, cont)
    go (Right _)        (Left (b, cont))  = Left  (b, cont)
    go (Right _)        (Right _)         = Right ()
```

We can see that the outputs of `triple 1` and `triple 2` are intermingled.

```
λ> runGenerator $ alternate (triple 1) (triple 2)
([1,2,1,2,1,2],())
```

Part 2 of this post will continue the discussion with *Iteratees*.

[pipes]: https://hackage.haskell.org/package/pipes
[conduit]: https://hackage.haskell.org/package/conduit
[issue19]: https://themonadreader.files.wordpress.com/2011/10/issue19.pdf
[dlist]: https://wiki.haskell.org/Difference_list