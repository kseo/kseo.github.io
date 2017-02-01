---
title: Simple benchmarking with GHCi
tags: GHCi, option
author: Kwang Yul Seo
---
GHCi has a lesser known option [:set +s][ghci]. When turned on, GHCi displays some stats for each expression evaluated.

<!--more-->

Let's experiment with the option.

```
λ> :set +s
```

`:set +s` displays the elapsed time and number of bytes allocated after evaluating each expression.

```
λ> fibs = 0 : scanl (+) 1 fibs
(0.00 secs, 0 bytes)
```

The number of bytes allocated is zero for `fibs` because no GC has occurred.

> NOTE: the allocation figure is only accurate to the size of the storage manager’s allocation area, because it is calculated at every GC. Hence, you might see values of zero if no GC has occurred.

```
λ> fibs !! 100
354224848179261915075
(0.01 secs, 110,440 bytes)
```

`fibs !! 100` took 0.01 seconds and allocated 110,440 bytes of memory.

This is a quick-and-dirty way to get a feel for the performance of a function. If you need a serious benchmark, please use [criterion][criterion] instead.

[ghci]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-cmd-:set +s
[criterion]: http://www.serpentine.com/criterion/tutorial.html
