---
title: Strict Identity Monad
tags: identity monad, lazy, strict
author: Kwang Yul Seo
---
In my [previous post][state-monad], I explained the difference between the lazy and strict state monads. What I didn't mention in the post is that the state monad is not special in this regard. Other monads also can have both lazy and strict variants. For example, [transformers][transformers] package provides both flavors of monads for `RWS` and `Writer` monads too.

It is also possible to have identity monads with either lazy or strict semantics. *transformers* package provides only the lazy variant, but it is not hard to image a strict variant of the identity monad. Here's the definition of `Control.Monad.StrictIdentity` defined in [strict-identity][strict-identity] package.

```haskell
newtype StrictIdentity a =  StrictIdentity {runStrictIdentity_ :: a }

instance Monad StrictIdentity where
    return !a = StrictIdentity $! a
    (!m) >>= (!k)  = k $! runStrictIdentity  m
```

The strict identity monad when bound to a function always evaluate its argument. We can see that both [BangPatterns][BangPatterns] and the strict application operator `($!)` are used to enforce strict evaluation of arguments.

In this sense, [Eval][Eval] monad from `Control.Parallel.Strategies` is also an identity monad. It enforces the strict evaluation of the argument with pattern matching.

```haskell
data Eval a = Done a

instance Monad Eval where
  return x = Done x
  Done x >>= k = k x   -- Note: pattern 'Done x' makes '>>=' strict
```

In [Implementing a call-by-value interpreter in Haskell][call-by-value], I mistakenly used the lazy identity monad to force the evaluation order. We need to keep in mind that transforming a program into a monadic form does not automatically guarantees the evaluation order unless the monad is strict.

**UPDATE**: Calling arbitrary monads lazy or strict is not appropriate as each monad has varying degree of strictness. For example, the `StrictIdentity` is more strict than the `Eval` monad. See [the Reddit discussion thread][reddit] for details.

```
λ> Control.Monad.StrictIdentity.runStrictIdentity $ do x <- return undefined; return 1
*** Exception: Prelude.undefined
λ> Control.Parallel.Strategies.runEval $ do x <- return undefined; return 1
1
```

[state-monad]: https://kseo.github.io/posts/2016-12-28-lazy-vs-strict-state-monad.html
[transformers]: https://www.stackage.org/lts-7.12/package/transformers-0.5.2.0
[strict-identity]: https://hackage.haskell.org/package/strict-identity
[BangPatterns]: https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html
[call-by-value]: https://kseo.github.io/posts/2017-01-05-implementing-a-call-by-value-interpreter-in-haskell.html
[Eval]: https://www.stackage.org/haddock/lts-7.12/parallel-3.2.1.0/Control-Parallel-Strategies.html#t:Eval
[reddit]: https://www.reddit.com/r/haskell/comments/5mp9e8/kwangs_haskell_blog_strict_identity_monad/
