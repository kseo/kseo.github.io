---
title: GHC typecheck build (no codegen)
tags: Haskell, GHC, build
author: Kwang Yul Seo
---

When programming in Haskell, I rebuild my program as frequently as possible to
check if there is any type error. This is a good practice because Haskell finds
most of bugs just by type checking.

However, as a program gets bigger, the longer it takes to build the program.
This is problematic for a large project. Fortunately, GHC provides `-fno-code`
option. When this option is given, GHC omits code generation and only performs
type checking.

In combination with `-fforce-recomp`, one can force typecheck build with the
following command:

```
cabal build --ghc-options="-fforce-recomp -fno-code"
```

