---
title: Implementing Union-Find algorithms in Haskell
tags: Haskell, ST monad, union find
author: Kwang Yul Seo
---

The union/find algorithm makes critical use of updatable states. Its efficiency
relies on the set representations being simplified each time the structure is
examined. Purely functional languages, which lack updatable states seem
inherently inefficient to implement the algorithm.

Fortunately, we can implement the algorithm efficiently in Haskell with the help
of `ST` monad which provides support for strict state threads. It is described in
the PLDI ’94 paper by John Launchbury and Simon Peyton Jones [Lazy Functional
State Threads][state].

```haskell
data UnionFind s = UnionFind {
    ids:: STUArray s Int Int
  , szs:: STUArray s Int Int
  }
 
newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n = liftM2 UnionFind (newListArray (0, n-1) [0..n-1]) (newArray (0, n-1) 1)
 
find :: (UnionFind s) -> Int -> Int -> ST s Bool
find uf p q = liftM2 (==) (root uf p) (root uf q)
 
root :: (UnionFind s) -> Int -> ST s Int
root uf i = do
    id <- readArray (ids uf) i
    if (id /= i)
        then do
            gpid <- readArray (ids uf) id
            writeArray (ids uf) i gpid
            root uf id
        else return i
 
unite :: (UnionFind s) -> Int -> Int -> ST s ()
unite uf p q = do
    i <- root uf p
    j <- root uf q
    szi <- readArray (szs uf) i
    szj <- readArray (szs uf) j
    if (szi < szj)
        then do
            writeArray (ids uf) i j
            writeArray (szs uf) j (szi + szj)
        else do
            writeArray (ids uf) j i
            writeArray (szs uf) i (szj + szi)
```

The code above implements the weighted quick-union with path compression
specified in [Union-Find Algorithms][01UnionFind]. You can see that the code is
almost the same to the Java code. Because [ST monad][st] uses mutable memory
internally, the behavior is also the same to that of imperative languages.

You can run the code with `runST` function.

```haskell
main = print $ runST $ do
    uf <- newUnionFind 10
    unite uf 3 4 -- 0, 1, 2, {3, 4}, 5, 6, 7, 8, 9
    unite uf 4 9 -- 0, 1, 2, {3, 4, 9}, 5, 6, 7, 8
    unite uf 8 0 -- {0, 8}, 1, 2, {3, 4, 9}, 5, 6, 7, 8
    unite uf 2 3 -- {0, 8}, 1, {2, 3, 4, 9}, 5, 6, 7
    unite uf 5 6 -- {0, 8}, 1, {2, 3, 4, 9}, {5, 6}, 7
    unite uf 5 9 -- {0, 8}, 1, {2, 3, 4, 5, 6, 9}, 7
    unite uf 7 3 -- {0, 8}, 1, {2, 3, 4, 5, 6, 7, 9}
    unite uf 4 8 -- 1, {0, 2, 3, 4, 5, 6, 7, 8, 9}
    find uf 1 2 -- False
```

The complete source code is [here][gist].

[state]: http://research.microsoft.com/~simonpj/Papers/lazy-functional-state-threads.ps.Z
[st]: http://www.haskell.org/haskellwiki/Monad/ST
[01UnionFind]: http://www.cs.princeton.edu/~rs/AlgsDS07/01UnionFind.pdf
[gist]: https://gist.github.com/kseo/8693028

