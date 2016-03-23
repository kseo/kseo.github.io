---
title: Writer monad
tags: writer monad, state monad, reverse state monad
author: Kwang Yul Seo
---
The *Writer monad* represents computations which produce a stream of data in addition to the computed values. It is commonly used by code generators to emit code.

<!--more-->

[transformers][transformers] provides both the strict and lazy versions of `WriterT` monad transformer. The definition of bind operator `>>=` reveals how the Writer monad works.

```haskell
instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = writer (a, mempty)
    m >>= k  = WriterT $ do
        (a, w)  <- runWriterT m
        (b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
```

`runWriterT` returns a pair whose second element is the output to accumulate. Because the output value is a `Monoid` instance, we can merge two outputs `w` and `w'` using `mappend` and return the combined output.

Here is a simple example of the Writer monad. It accumulates `LogEntry`s in a list. (CAUTION: Do not use `WriterT` for plain logging in real world applications. It unnecessarily keeps the entire logs in memory. I recommend [fast-logger][fast-logger] for logging.)

```haskell
import Control.Monad
import Control.Monad.Trans.Writer.Strict

data LogEntry = LogEntry { msg::String }
  deriving (Eq, Show)

calc :: Writer [LogEntry] Integer
calc = do
  output "start"
  let x = sum [1..10000000]
  output (show x)
  output "done"
  return x

output :: String -> Writer [LogEntry] ()
output x = tell [LogEntry x]

test = mapM_ print $ execWriter calc
```

The code looks innocuous, but its performance deteriorates when the accumulated log gets bigger because the `Monoid` instance of `[]` uses `(++)` to append two lists and the concatenations are left-nested.

```
do { tell [1]; tell [2]; tell [3]; tell[4]; tell [5] }
=>
(((([1] ++ [2]) ++ [3]) ++ [4]) ++ [5])
```

`(++)` is [known to perform poorly][2028189] when applications of `(++)` are left-nested.

# Difference List

One well-known solution is to use *the difference list* instead of an ordinary list. `DList` provides O(1) `append` and `snoc` operations on lists. [Demystifying DList][dlist] explains how `DList` works in details.

The code is almost the same except we replaced `[LogEntry]` with `DList LogEntry`, but it scales well as the accumulated log gets bigger.

```haskell
import Data.DList

calc :: Writer (DList LogEntry) Integer
calc = ...

output :: String -> Writer (DList LogEntry) ()
output x = tell (singleton (LogEntry x))

test = mapM_ print $ toList (execWriter calc)
```

# Endo

Another option is to use `Endo` wrapper from `Data.Monoid`. It is an endomorphism from type `a` to `a`.

```haskell
newtype Endo a = Endo { appEndo :: a -> a }
               deriving (Generic)
```

Surprisingly, it is an instance of `Monoid`. `mempty` is the *identity* function and `mappend` is the *composition* of two functions.

```haskell
instance Monoid (Endo a) where
        mempty = Endo id
        Endo f `mappend` Endo g = Endo (f . g)
```

But how can I output a log? We need a function of type `[LogEntry] -> [LogEntry]` to make an `Endo` value. The trick is to create a section `([LogEntry x]<>)` which prepends a log entry to the list.

```haskell
calc :: Writer (Endo [LogEntry]) Integer
calc = ...

output :: String -> Writer (Endo [LogEntry]) ()
output x = tell $ Endo ([LogEntry x]<>)

test = mapM_ print $ appEndo (execWriter calc) []
```

But why does this use of `Endo` perform well? To see why, we need to see how the following code is actually evaluated.

```
do { tell [1]; tell [2]; tell [3]; tell[4]; tell [5] }
```

is translated to

```
([1]++) . ([2]++) . ([3]++) . ([4]++) . ([5]++)
```

This is a composition of functions whose type is `[Int] -> [Int]`. We can obtain the final result by applying `[]`.

```
([1]++) . ([2]++) . ([3]++) . ([4]++) . ([5]++) $ []
=>
[1] ++ ([2] ++ ([3] ++ ([4] ++ ([5] ++ []))))
```

We can see that `(++)` operators are right-nested.

This also explains why `DList` in the previous section performs well because `DList` is just `Endo` specialized to lists.

```haskell
newtype DList a = DL { unDL :: [a] -> [a] }

instance Monoid (DList a) where
    mempty  = DL id
    mappend xs ys = DL (unDL xs . unDL ys)
```

# State Monad

It is possible to implement the Writer monad in terms of the *State monad*. We can store the accumulated logs in the state and update it by appending a new log.

```haskell
import Control.Monad.Trans.State
import Data.Monoid ((<>))

calc :: State [LogEntry] Integer
calc = ...

output :: String -> State [LogEntry] ()
output x = modify (<> [LogEntry x])

test = mapM_ print $ execState calc []
```

Unfortunately, this version has the same performance issue with the initial version because applications of `(++)` are left-nested.

But there is a magical trick that can change this situation.

# Backward State Monad

The section "2.8 Variation six: Backwards state" of Philip Wadler's [The essence of functional programming][essence] briefly mentions the *Backwards state* monad (also known as *reverse state monad*). This is a strange variant of the State monad where the state is propagated backward.

```haskell
newtype RState s a = RState { runRState :: s -> (a,s) }

instance Monad (RState s) where
    return x = RState $ (,) x
    RState sf >>= f = RState $ \s ->
        let (a,s'') = sf s'
            (b,s') = runRState (f a) s
        in (b,s'')

rget = RState $ \s -> (s,s)
rmodify f = RState $ \s -> ((),f s)
rput = rmodify . const

execRState f s = snd (runRState f s)
```

In the definition of `>>=`, the state `s` is passed to the second expression and its result `s'` is passed back to the first expression. This seems impossible because two expressions are mutually recursive, but Haskell's lazy evaluation makes it possible. In the backward state monad, `rget` reads the state from the future!

With this in mind, we can implement the Writer monad by prepending the log to the state. Because the state contains all the future logs, we can simply prepend our log to it.

```haskell
calc :: RState [LogEntry] Integer
calc = ...

output :: String -> RState [LogEntry] ()
output x = rmodify ([LogEntry x]<>)

test = mapM_ print $ execRState calc []
```

Applications of `(++)` are right-nested because logs are accumulated backward from the end.

Readers who would like to know more about the backward state monads are referred to:

* [Mindfuck: The Reverse State Monad][reverse-state-monad] shows how to compute the fibonacci number using the reverse state monad.
* [tardis][tardis] package - a combination of both a forwards and a backwards state transformer.

[fast-logger]: https://hackage.haskell.org/package/fast-logger
[transformers]: https://www.stackage.org/lts-7.12/package/transformers-0.5.2.0
[2028189]: http://stackoverflow.com/a/13879693/2028189
[dlist]: http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/
[essence]: https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/the-essence-of-functional-programming.pdf
[reverse-state-monad]: https://lukepalmer.wordpress.com/2008/08/10/mindfuck-the-reverse-state-monad/
[tardis]: https://hackage.haskell.org/package/tardis-0.4.1.0
