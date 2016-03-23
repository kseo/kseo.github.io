---
title: DoAndIfThenElse language extension
tags: Haskell, DoAndIfThenElse, language extension
author: Kwang Yul Seo
---

Have you ever encountered "Unexpected semi-colons in conditional" errors while
building your project with Cabal and wondered why? This blog post explains about
this puzzling error.

Here is a simple program which prints the given command line arguments.

```haskell
import System.Environment
 
main = do
    args <- getArgs
    if length args < 1 then
        putStrLn "Usage: DoAndIfThenElse [args]"
    else
        putStrLn $ concat args
```

You can build this Haskell program with `ghc –make`:

```
ghc --make DoAndIfThenElse.hs
[1 of 1] Compiling Main             ( DoAndIfThenElse.hs, DoAndIfThenElse.o )
Linking DoAndIfThenElse ...
```

Okay. Then let’s create a Cabal build script and build this program with Cabal.

```
name:            DoAndIfThenElse
version:         0.0.1
cabal-version:   >= 1.8
build-type:      Simple
 
executable DoAndIfThenElse
  hs-source-dirs:    src
  main-is:           DoAndIfThenElse.hs
  build-depends:     base
```

The source code is exactly the same, but now GHC suddenly complains about the
unexpected semi-colons we never inserted anyway.

```
src/DoAndIfThenElse.hs:5:8:
    Unexpected semi-colons in conditional:
        if length args < 1 then putStrLn
                                  "Usage: DoAndIfThenElse [args]"; else putStrLn $ concat args
    Perhaps you meant to use -XDoAndIfThenElse?
```

You can fix this problem by adding `DoAndIfThenElse` language pragma at the
beginning:

```haskell
{-# LANGUAGE DoAndIfThenElse #-}
```

Or you can fix it by changing the indentation of *then* and *else* of *if*
expression.

```haskell
if length args < 1
    then putStrLn "Usage: DoAndIfThenElse [args]"
    else putStrLn $ concat args
```

So the problem is on the indentation. You have to keep *then* and *else* at
deeper indentation levels than the *if* block they belong.

`ghc –make` is okay because GHC automatically turns on the syntax extension
`DoAndIfThenElse`. However, Cabal is more picky, so you have to turn it on
manually either at the top of your code files, or in your Cabal files.

There is also a StackOverflow question on this issue, [Unexpected semi-colons in
conditional][stackoverflow].

[stackoverflow]: http://stackoverflow.com/questions/10076318/unexpected-semi-colons-in-conditional
