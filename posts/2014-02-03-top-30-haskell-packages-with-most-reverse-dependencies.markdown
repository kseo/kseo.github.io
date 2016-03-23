---
title: Top 30 Haskell packages with most reverse dependencies
tags: Haskell, package, reverse dependencies
author: Kwang Yul Seo
---

[Reverse Dependencies][reverse] shows the reverse dependencies of each Haskell
package (those packages that depend upon it). I [sorted the list][sort] to check
the packages with most reverse dependencies. Here are top 30 Haskell packages
among 2348 packages.

1. base 5961
1. containers 2110
1. bytestring 2018
1. mtl 1664
1. directory 1068
1. text 979
1. transformers 971
1. filepath 934
1. time 723
1. array 696
1. process 628
1. QuickCheck 621
1. network 581
1. parsec 577
1. random 537
1. HUnit 491
1. template-haskell 488
1. vector 429
1. binary 399
1. test-framework 379
1. utf8-string 334
1. unix 331
1. old-locale 325
1. deepseq 323
1. pretty 288
1. stm 274
1. test-framework-quickcheck2 270
1. test-framework-hunit 264
1. attoparsec 263
1. aeson 254

Though the list does not show the importance of each package, there is high
chance that you need to use at least one of these packages when you write your
own package. So to become a seasoned Haskell programmer, you need to get
accustomed to these packages.

[reverse]: http://packdeps.haskellers.com/reverse
[sort]: https://docs.google.com/spreadsheets/d/1-wbCAyoloUaTXV-WOzwdhyMQV6AiwUwTgphCODMau4k/pub?output=html
