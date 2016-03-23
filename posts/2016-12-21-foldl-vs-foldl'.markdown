---
title: foldl vs foldl'
tags: fold, recursion
author: Kwang Yul Seo
---
Chris Allen [mentioned][Haskell-Pitfalls] `foldl` as one of the newbie traps in Haskell.

> foldl' is always what you want, don’t use foldl!

Because `foldl` always has to examine the whole list, there is no reason to make it lazy. It just uses more memory to do the same thing as `foldl'`.

[Real World Haskell][rwh] also recommends using `foldl'` instead of `foldl`.

> Due to the thunking behavior of foldl, it is wise to avoid this function in real programs: even if it doesn't fail outright, it will be unnecessarily inefficient. Instead, import Data.List and use foldl'

[Haskell Wiki][wiki] compares `foldr`, `foldl` and `foldl'` and recommends using either `foldr` or `foldl'`.

> foldl' is the more efficient way to arrive at that result because it doesn't build a huge thunk.

But here comes a question. If `foldl'` is almost always better than `foldl`, why do we have `foldl` anyway? It makes sense only when the combining function is non-strict in its *first* argument. (The example is taken from the Haskell Wiki.)

```haskell
(?) :: Int -> Int -> Int
_ ? 0 = 0
x ? y = x*y

list :: [Int]
list = [2,3,undefined,5,0]

okey = foldl (?) 1 list
boom = foldl' (?) 1 list
```

Evaluation of `okey`:

```
okey -->
foldl (?) 1 [2,3,undefined,5,0] -->
foldl (?) (1 ? 2) [3,undefined,5,0] -->
foldl (?) ((1 ? 2) ? 3) [undefined,5,0] -->
foldl (?) (((1 ? 2) ? 3) ? undefined) [5,0] -->
foldl (?) ((((1 ? 2) ? 3) ? undefined) ? 5) [0] -->
foldl (?) (((((1 ? 2) ? 3) ? undefined) ? 5) ? 0) [] -->
((((1 ? 2) ? 3) ? undefined) ? 5) ? 0 -->
0
```

Evaluation of `boom`:

```
boom -->
foldl' (?) 1 [2,3,undefined,5,0] -->
    1 ? 2 --> 2
foldl' (?) 2 [3,undefined,5,0] -->
    2 ? 3 --> 6
foldl' (?) 6 [undefined,5,0] -->
    6 ? undefined -->
*** Exception: Prelude.undefined
```

This example actually shows why `foldl` is so useless because it is hard to find a function which is non-strict in its *first* argument.

Many functions in Haskell are non-strict in its *second* argument and this is why `foldr` is useful. For example, `(&&)` is non-strict in its *second* argument and `and` can be efficiently defined using `foldr`.

```haskell
(&&)                    :: Bool -> Bool -> Bool
True  && x              =  x
False && _              =  False

and                     :: [Bool] -> Bool
and                     =  foldr (&&) True
```

In conclusion, we should use `foldl'` by default unless we have a very compelling reason to use `foldl` instead.

But, wait! Let's check how our beloved `sum` function is written. Because `(+)` is strict in both of its arguments, `foldl'` should have been used. But here's [the actual code][sum] taken from the `base` package.

```haskell
sum                     :: (Num a) => [a] -> a
{-# INLINE sum #-}
sum                     =  foldl (+) 0
```

OMG! There is a historical accident here. Interested readers are referred to [Fixing foldl][fixing] article from Well-Typed.

[Haskell-Pitfalls]: http://lorepub.com/post/2016-12-17-Haskell-Pitfalls
[rwh]: http://book.realworldhaskell.org/read/functional-programming.html
[wiki]: https://wiki.haskell.org/Foldr_Foldl_Foldl'#Foldl
[sum]: https://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.List.html#sum
[fixing]: http://www.well-typed.com/blog/90/
