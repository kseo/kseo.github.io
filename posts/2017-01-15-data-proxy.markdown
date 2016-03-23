---
title: Data.Proxy
tags: Proxy
author: Kwang Yul Seo
---
[Proxy][Proxy] is a mysterious type residing in `Data.Proxy` module. The definition of `Proxy` tells nothing about its nature.

```haskell
-- | A concrete, poly-kinded proxy type
data Proxy t = Proxy
```

The documentation of `Proxy` says two hints. It is a (1) *concrete* and (2) *poly-kinded* proxy type.

# Poly-kinded

The kind of `Proxy` is `forall k. k -> *`.

```
λ> :k Proxy
Proxy :: k -> *
```

Here `k` is [poly-kinded][poly] so we can pass types of any kind to `Proxy`.

* `Proxy Char` where `k` is `*`.
* `Proxy (,)` where `k` is `* -> *`
* `Proxy Show` where `k` is `* -> Constraint`
* `Proxy Monad` where `k` is `(* -> *) -> Constraint`

# Concrete value

In Haskell, we can create a value of any type we want by annotating `undefined` with the type.

```
λ> let p = undefined :: Int
```

However, we can't use this trick if the kind of the type is not `*`, For example, we can't annotate `undefined` with type `(,)` because its kind is `* -> * -> *`.

```
λ> let q = undefined :: (,)

<interactive>:4:22: error:
    • Expecting two more arguments to ‘(,)’
      Expected a type, but ‘(,)’ has kind ‘* -> * -> *’
    • In an expression type signature: (,)
      In the expression: undefined :: (,)
      In an equation for ‘q’: q = undefined :: (,)
```

`Proxy` lets us to overcome this limitation. We can create a proxy value representing the type by annotating `Proxy` data constructor.

```
λ> import Data.Proxy
λ> let p = Proxy :: Proxy (,)
λ> :t p
p :: Proxy (,)
```

We can think of `Proxy :: Proxy (,)` as a reified value of the type `(,)`.

# Type Application

The read/show problem below is ill-typed because of ambiguity.

```
f :: String -> String
f s = show (read s)
```

We can fix this issue by explicitly passing the type as a value argument. As you can see the wild card pattern `_`, the value is not used anywhere in the definition of `f`. Only its type is used.

```haskell
f :: forall proxy a. (Read a, Show a) => proxy a -> String -> String
f _ = (show :: a -> String) . read
```

```
λ> f (Proxy :: Proxy Int) "3"
"3"
```

As a side note, [TypeApplications][TypeApplications] extension introduced in GHC 8 provides an alternative way to fix this.

# Typeable

Another application of `Proxy` is `Typeable`. Before kind polymorphism was introduced in GHC, there was a lot of code duplication in the way `Typeable` is implemented because `t` can represent only a specific kind.

```haskell
class Typeable (t :: *) where
  typeOf :: t -> TypeRep

class Typeable1 (t :: * -> *) where
  typeOf1 :: t a -> TypeRep

class Typeable2 (t :: * -> * -> *) where
  typeOf2 :: t a b -> TypeRep
```

`Proxy` allows us to merge all these classes into one:

```haskell
class Typeable t where
  typeOf :: Proxy t -> TypeRep

instance Typeable Int  where typeOf _ = TypeRep
instance Typeable []   where typeOf _ = TypeRep
```

# Other use cases of Proxy

* [json-schema][json-schema] uses `Proxy` to obtain the JSON representation for the given type.

```haskell
class JSONSchema a where
  schema :: Proxy a -> Schema
```

* Edward Kmett's [reflection][reflection] package shows an advanced usage of `Proxy`. Austin Seipp's [Reflecting values to types and back][using-reflection] explains how to reify arbitrary terms into types that can be reflected back into terms.

[Proxy]: https://www.stackage.org/haddock/lts-7.12/base-4.9.0.0/Data-Proxy.html
[poly]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XPolyKinds
[TypeApplications]: https://kseo.github.io/posts/2017-01-08-visible-type-application-ghc8.html
[json-schema]: https://www.stackage.org/lts-7.12/package/json-schema-0.7.4.1
[reflection]: https://www.stackage.org/lts-7.12/package/reflection-2.1.2
[using-reflection]: https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
