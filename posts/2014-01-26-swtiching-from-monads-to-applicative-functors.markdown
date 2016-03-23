---
title: Switching from monads to applicative functors
tags: Haskell, monad, applicative functor
author: Kwang Yul Seo
---

Many applications of monads actually do not require monads but only applicative
functors. Monads allow us to run actions depending on the results of earlier
actions, but not all applications of monads need this extended functionality.

It is better to use applicative functors where possible because there are some
advantages of applicative functors. [Applicative functor on the Haskell
Wiki][applicative] mentions two:

* Code that uses only on the `Applicative` interface are more general than ones
  uses the `Monad` interface, because there are more applicative functors than
  monads. The `ZipList` is an applicative functor on lists, where `liftA2` is
  implemented by `zipWith`. It is a typical example of an applicative functor
  that is not a monad.

* Programming with `Applicative` has a more applicative/functional feel.
  Especially for newbies, it may encourage functional style even when
  programming with effects. `Monad` programming with do notation encourages a
  more sequential & imperative style.

There is another advantage. Applicative functors do not need special
transformers because they can be combined in a generic way.

But there is a problem. It is usually not easy to decide if we need monads or
applicative functors up front. You ambitiously start with applicative functors
and find later that you actually needed monads. Sad!

Here is my tip. I start with monads but use only `return`, `ap`, `liftM`, `liftM2`, …
instead of `do`, `>>=`. The most common pattern is

```haskell
do x <- fx
   y <- fy
   return (g x y)
```

This can be rewritten as `liftM2 g fx fy`. Once you are sure that you need only
those monad methods, you can mechanically switch from monads to applicative
functors using the following translation table:

* `import Control.Monad` -> `import Control.Applicative`
* `return` -> `pure`
* `ap` -> -> `(<*>)`
* `liftM` -> `liftA` or `(<$>)`
* `liftM2` -> `liftA2`
* `(Monad m =>)` -> `(Applicative f =>)`

[applicative]: http://www.haskell.org/haskellwiki/Applicative_functor#Some_advantages_of_applicative_functors

