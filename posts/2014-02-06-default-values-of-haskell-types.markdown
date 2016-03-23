---
title: Default values of Haskell Types
tags: Haskell, default value
author: Kwang Yul Seo
---

Java types have default values. If you don’t explicitly assign a value in a
field declaration, it gets its default value of the given type. For example, the
default value of `int` is `0` and the default value of `boolean` is `false`.

On the contrary, Haskell types do not provide default values. This is natural
because Haskell does not destructively update a variable initialized with the
the default value.

However, it is handy to have a default value in some cases such as a record with
many fields. Haskell libraries often provide a default value for such a record
for the ease of construction. For example,
[llvm-general-pure][llvm-general-pure] provides `defaultModule`, which is the
default value of `Module`. The data constructor `Module` has 4 fields:

* `moduleName :: String`
* `moduleDataLayout :: Maybe DataLayout` -- a DataLayout, if specified, must match that
   of the eventual code generator
* `moduleTargetTriple :: Maybe String`
* `moduleDefinitions :: [Definition]`

Using `defaultModule`, you don’t need to supply all these fields. You can
construct a `Module` value using Haskell record update syntax as in the following:

```haskell
defaultModule { moduleName="mymodule" }
```

The [data-default][data-default] package provides a type class `Default`, which
is useful for this purpose. If a given type is an instance of `Default`, you can
get its default value using `def` method. Instances are provided for `()`,
`Set`, `Map`, `Int`, `Integer`, `Float`, `Double`, and many others.

```haskell
Prelude Data.Default> def :: Int
0
Prelude Data.Default> def :: [a]
[]
Prelude Data.Default> def :: Double
0.0
```

[llvm-general-pure]: http://bscarlet.github.io/llvm-general/3.3.8.2/doc/html/llvm-general-pure/index.html
[data-default]: http://hackage.haskell.org/package/data-default
