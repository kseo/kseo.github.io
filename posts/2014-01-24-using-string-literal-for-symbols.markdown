---
title: Using string literal for symbols
tags: Haskell, string literal
author: Kwang Yul Seo
---

In writing a compiler or an interpreter, it is common to define a symbol or
identifier type which is distinct from `String` to make it more type-safe.

```haskell
newtype Symbol = Symbol String
```

Now `String` and `Symbol` are distinct types and we can’t accidentally mix two.
But it also means that we have to use `Symbol` constructor whenever we need to
create a symbol from a string literal.

```haskell
Symbol "main"
```

You can avoid this labor using the GHC extension,
[OverloadedStrings][OverloadedStrings]. By making `Symbol` an instance of
`IsString`, GHC automatically coerces a string literal into a symbol.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.String

instance IsString Symbol where
    fromString = Symbol . fromString

s :: Symbol
s = "hello"
```

[OverloadedStrings]: https://kseo.github.io/posts/2014-01-15-overloaded-string-literals.html
