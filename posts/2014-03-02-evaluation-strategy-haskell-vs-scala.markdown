---
title: "Evaluation Strategy: Haskell vs Scala"
tags: Haskell, Scala, evaluation strategy
author: Kwang Yul Seo
---

Haskell is a non-strict language, and GHC uses a strategy called laziness which
combines non-strictness and sharing for efficiency.

Thus, you can easily implement [const][const] which never uses the second
argument.

```haskell
const x y = x
```

With this definition, it is okay to pass `undefined` as the second argument of
`const` because `y` is not never evaluated. But in Haskell, you can also make an
argument strict using the [BangPatterns GHC extension][BangPatterns].

```haskell
const x !y = x
```

Interestingly, the situation is reversed in Scala whose default evaluation
strategy is strict.

```scala
def const(x: Int, y:Int) = x
```

You can make an argument non-strict by putting the `=>` symbol between the
variable name and the type.

```scala
def const(x: Int, y: => Int) = x
```

[const]: http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#v:const
[BangPatterns]: https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html
