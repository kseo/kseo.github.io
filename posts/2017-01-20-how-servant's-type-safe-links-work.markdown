---
title: How Servant's type-safe links work
tags: servant, safe link, type families
author: Kwang Yul Seo
---
Many Haskell web frameworks provide so called *type-safe links*. Servant is no exception and it provides one of the strongest safety check. It statically guarantees that all links are valid endpoints of the server!

In servant, we can create a type-safe URI using `safeLink` function. It takes both *the whole API* and *the API endpoint* we would like to point to. It signals a type error if the endpoint does not belong to the whole api.

<!--more-->

```haskell
-- | Create a valid (by construction) relative URI with query params.
--
-- This function will only typecheck if `endpoint` is part of the API `api`
safeLink
    :: forall endpoint api. (IsElem endpoint api, HasLink endpoint)
    => Proxy api      -- ^ The whole API that this endpoint is a part of
    -> Proxy endpoint -- ^ The API endpoint you would like to point to
    -> MkLink endpoint
```

To better explain how type-safe links work in Servant, let's create a mini web DSL that is a small subset of Servant DSL.

```haskell
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

import Data.Proxy
import GHC.Exts (Constraint)
import GHC.TypeLits

data a :<|> b = a :<|> b
infixr 8 :<|>

data (path :: k) :> a
infixr 9 :>

data Method = Get | Post
```

* `(:<|>)` is the union of two APIs.
* `(:>)` specifies the path.
* Support only two methods `Get` and `Post`

Here's the API and a few endpoints defined using our mini web DSL.

```haskell
type Api = "item" :> Get
      :<|> "item" :> Post
      :<|> "user" :> Get

type ValidEndpoint = "item" :> Get

type InvalidEndpoint1 = "user" :> Post
type InvalidEndpoint2 = "none" :> Get
```

For simplicity, our `safeLink` function is also revised to return `()` instead of creating an actual URI. Creating a URI from the endpoint is straightforward.

Our mini `safeLink` throws an type error if `endpoint` does not belong to `api`. But how? The body of `safeLink` is just `()`!

```haskell
safeLink :: forall endpoint api. (IsElem endpoint api) => Proxy api -> Proxy endpoint -> ()
safeLink _ _ = ()
```

An acute reader might have noticed, the magic is hidden in the `IsElem` type function. `IsElem endpoint api` checks if `endpoint` is an element of `api`.

Before diving into the definition of `IsElem`, we need an auxiliary type-level function `Or`. It is defined using *closed type families*.

```haskell
-- | If either a or b produce an empty constraint, produce an empty constraint.
type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
    -- This works because of:
    -- https://ghc.haskell.org/trac/ghc/wiki/NewAxioms/CoincidentOverlap
    Or () b       = ()
    Or a ()       = ()
```

`Or` takes two constraints as arguments and produces an empty constraint if either one of two arguments produces an empty constraint.

`IsElm` is defined in terms of `Or`.

```haskell
-- | Closed type family, check if endpoint is within api
type family IsElem endpoint api :: Constraint where
    IsElem e (sa :<|> sb)                   = Or (IsElem e sa) (IsElem e sb)
    IsElem (e :> sa) (e :> sb)              = IsElem sa sb
    IsElem e e                              = ()
```

The rules are straightforward:

1. If `api` consists of multiple endpoints, check each endpoint and combines the result using `Or`.
1. If both `api` and `endpoint` start with the same path `e`, check the remaining parts.
1. If both `api` and `endpoint` is the same, return an empty constraint.

If `endpoint` does not belong to `api`, GHC can't deduce the type and emits an type error. The actual implementation of Servant is more complicated because it needs to handle other components such as `Capture` and `QueryParam`, but the core idea is the same!

Now we can test if our `safeLink` actually works.

`getItem` is well-typed because `ValidEndpoint` is in fact a valid endpoint of `Api` using `safeLink`.

```haskell
getItem  = safeLink (Proxy :: Proxy Api) (Proxy :: Proxy ValidEndpoint)
postUser = safeLink (Proxy :: Proxy Api) (Proxy :: Proxy InvalidEndpoint1)
```

`postUser` is ill-typed because `InvalidEndpoint1` is not a valid endpoint of `Api`. We can see that GHC actually throws an error!

```
• Could not deduce: Or
                      (IsElem ("user" :> 'Post) ("item" :> 'Get))
                      (Or
                         (IsElem ("user" :> 'Post) ("item" :> 'Post))
                         (IsElem ("user" :> 'Post) ("user" :> 'Get)))
    arising from a use of ‘safeLink’
• In the expression:
    safeLink (Proxy :: Proxy Api) (Proxy :: Proxy InvalidEndpoint1)
  In an equation for ‘postUser’:
      postUser
        = safeLink (Proxy :: Proxy Api) (Proxy :: Proxy InvalidEndpoint1)
```

[servant]: https://haskell-servant.github.io/
