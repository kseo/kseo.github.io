---
title: Data.Map vs Data.IntMap
tags: Haskell, Data.Map, Data.IntMap
author: Kwang Yul Seo
---

A map is the one of most widely used data structures in many applications. Thus,
many language runtimes provide an efficient implementation of a map. In a purely
functional programming language, map is usually implemented as a balanced binary
tree. Haskell is no exception here and the implementation of Haskell’s
`Data.Map` is based on size balanced binary trees described in

* Stephen Adams, ["Efficient sets: a balancing act"][bb], Journal of Functional Programming 3(4):553-562, October 1993, .
* J. Nievergelt and E.M. Reingold, "Binary search trees of bounded balance", SIAM journal of computing 2(1), March 1973.

`Data.Map` is parameterized over key and value types, so that you can use any type
you want as long as key is an instance of `Ord` type class. So, for example, you
can use `Int` as the key type and store any type you want.

However, Haskell also provides a special version `Data.IntMap` for `Int` key. It
seems redundant at first, but `Data.IntMap` is different from `Data.Map` in that
it supports efficient merging of two maps. The implementation of `Data.IntMap`
is described in

* Chris Okasaki and Andy Gill, ["Fast Mergeable Integer Maps"][okasaki98fast], Workshop on ML, September 1998, pages 77-86,
* D.R. Morrison, "/PATRICIA — Practical Algorithm To Retrieve Information Coded In Alphanumeric/", Journal of the ACM, 15(4), October 1968, pages 514-534.

The author of `Data.IntMap` mentions that insertions and deletions of
`Data.IntMap` when compared to a generic size-balanced map implementation are
also much faster. This observation suggests that we should use `Data.IntMap`
whenever possible whether or not we need union or intersection of twp maps.

[okasaki98fast]: http://citeseer.ist.psu.edu/okasaki98fast.html
[bb]: http://www.swiss.ai.mit.edu/~adams/BB/
