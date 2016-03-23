---
title: Visible Type Application in GHC 8
tags: TypeApplications
author: Kwang Yul Seo
---
GHC 8.0 introduced a new GHC extension named [TypeApplications][TypeApplications] which allows us to give explicit type arguments to a polymorphic function.

To see what `TypeApplications` does, we need to understand how polymorphism in Haskell is implemented under the hood. In Haskell, a polymorphic function is translated into a function which takes both type arguments and value arguments. For example,

```haskell
id :: a -> a
id x = x
```

is translated into

```haskell
id :: forall a . a -> a
id @a x = x
```

Here `@a` is the type argument. The specialization of `id` to `idString` is represented by passing `@String` type argument to `id` function.

```haskell
idString :: String -> String
idString = id @String
```

This is not an ad-hoc way to implement polymorphism. The trick of passing around type parameters as ordinary function arguments was devised by *System F* (as known as the *polymorphic lambda calculus*) and GHC uses System F as its internal representation. An interested reader might want to take a look at Gabriel Gonzalez's [Polymorphism for dummies][polymorphism] for other examples.

Before GHC 8.0, the type application was invisible. There was no way to pass the type parameter such as `@String` and `@Int` explicitly. GHC infers the type from the argument type, or we had to specify the type using type annotations.

```
λ> id "a"
"a"
λ> id (3 :: Int)
3
```

`TypeApplications` extension allows us to give explicit type arguments.

```
λ> :set -XTypeApplications
λ> id @String "a"
"a"
λ> id @Int 3
3
```

This is useful in resolving ambiguity in type classes or type families. The show/read problem from [Type defaulting in Haskell][type-defaulting] was not typeable due to ambiguity, but we can easily remove ambiguity by giving an explicit type argument.

```haskell
{-# LANGUAGE TypeApplications #-}

f :: String -> String
f s = show (read @Int s)
```

The type argument is not limited to concrete types. As we can pass a variable to a function as an argument, it is possible to pass a type variable to a function as a type argument if it is explicitly quantified with `ExplicitForAll`.

```haskell
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE TypeApplications    #-}

incShow :: forall a . (Read a, Show a, Num a) => String -> String
incShow = show . (+1) . read @a
```

```
λ> incShow @Double "3.0"
"4.0"`
```

In the following example, `g False` would be ill-typed because GHC can't infer the proper type. Adding the explicit type `@Char` resolves the problem.

```
type family F a
type instance F Char = Bool

g :: F a -> a
g _ = undefined

f :: Char
f = g True

h = g False -- will cause an error
h' = g @Char False
```

Some of these cases can be solved with type annotations, but it can be cumbersome in complicated examples. Visible type applications generally provide a more succinct way to resolve ambiguity.

If you would like to know the technical details of `TypeApplications`, please refer to [Visible Type Application (Extended version)][type-app-extended].

[TypeApplications]: https://ghc.haskell.org/trac/ghc/wiki/TypeApplication
[polymorphism]: http://www.haskellforall.com/2015/10/polymorphism-for-dummies.html
[type-defaulting]: https://kseo.github.io/posts/2017-01-04-type-defaulting-in-haskell.html
[type-app-extended]: https://www.seas.upenn.edu/~sweirich/papers/type-app-extended.pdf
