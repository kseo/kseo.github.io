---
title: Type defaulting in Haskell
tags: type defaulting
author: Kwang Yul Seo
---
Type defaulting is introduced to solve the ambiguous type problems caused by type classes.

Consider the following classic example:

```haskell
show :: Show a => a -> String
read :: Read a => String -> a

f :: String -> String
f s = show (read s)
```

`f` appears to be well-typed, but GHC complains about the ambiguous type variable. The problem is that there is nothing to specify the type of the intermediate subexpression `(read s)`. `read` can parse an `Int`, a `String` or any other type that is an instance of `Read`. The compiler can't arbitrarily choose a type because the choice of the type affects the program behavior. We can avoid the ambiguity by specifying the type explicitly:

```haskell
f :: String -> String
f s = show (read s :: Int)
```

However, the restriction above becomes quite cumbersome when we need to handle numeric types. For example,

```haskell
negate :: Num a => a -> a
show :: Show a => a -> String

(show (negate 4))
```

The expression `(show (negate 4))` is ambiguous because the literal `4` is of `Num a => a` type in Haskell. `4` can be an `Int`, a `Float` or any other type that is an instance of `Num`, so the compiler can't choose any particular type for the same reason above. But the Haskell Committee thought that this is too much restriction. After much debates, they compromised and added an ad-hoc rule for choosing a particular default type. [A History of Haskell][history] mentions the rationale behind this decision:

> Performing numerical calculations on constants is one of the very first things a Haskell programmer does, and furthermore there is more reason to expect numeric operations to behave in similar ways for different types than there is for non-numeric operations.

The default type of `Num` is `Integer`, so the Haskell compiler infers the type of `(negate 4)` as `Integer` instead of rejecting it as an invalid program.

Haskell default rule can be summarized as:

```
default Num Integer
default Real Integer
default Enum Integer
default Integral Integer
default Fractional Double
default RealFrac Double
default Floating Double
default RealFloat Double
```

The type defaulting rule is very conservative. Defaults are limited to Prelude numeric classes and cannot be applied to user-defined classes. Our first example `f` is invalid because `Read` is not a numeric type class. But, sometimes, we would like to change the default type for the type classes. So Haskell lets us specify a list of types in a special top-level *default* declaration.

```haskell
default (Int, Float)
```

The restriction is still too tight. According to the rule, even innocuously looking `show []` is not typeable because GHC can't decide the type of `[a]`. So compiling the following program causes an ambiguous type error.

```haskell
f = show []
```

But every Haskell programmer knows that `show []` works fine in GHCi from experience. We don't know the exact type chosen because the output is always `"[]"` regardless of the type. But it works anyway. What happened here?

```
λ> show []
"[]"
```

In fact, GHCi extends Haskell's type-defaulting rules by including `Show`, `Eq` and `Ord` in addition to numeric type classes. [Type defaulting in GHCi][ghci] explains the details. It also adds the unit type `()` to the standard list of types which are tried when doing type defaulting. So the type of `[]` in `show []` becomes `[()]` according to the extended type defaulting rules in GHCi.

Type defaulting in Haskell is one of the controversial language features. It is confusing to novice Haskell programmers because the defaulting rule is very ad-hoc. Interested readers might want to take a look several enhancement [proposals][Defaulting] being discussed in Haskell Prime Wiki.

[history]: https://research.microsoft.com/en-us/um/people/simonpj/papers/history-of-haskell/history.pdf
[ghci]: https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/interactive-evaluation.html
[Defaulting]: https://prime.haskell.org/wiki/Defaulting
