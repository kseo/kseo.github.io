---
title: Reified dictionaries
tags: reified dictionaries, constraints, GADTs
author: Kwang Yul Seo
---
GADT allows us to reify a constraint as an explicit dictionary. With *ConstraintKinds*, we can further generalize this trick. In this post, I will explain how this trick works.

<!--more-->

In his article, [Constraint Kinds for GHC][ConstraintKinds], Max Bolingbroke showed a trick of reifying a constraint as an explicit dictionary using a GADT:

```haskell
{-# LANGUAGE GADTs #-}

data ShowDict a where
  ShowDict :: Show a => ShowDict a

showish :: ShowDict a -> a -> String
showish ShowDict x = show x

use_showish :: String
use_showish = showish ShowDict 10
```

How does this trick work? *GADTs* extension plays an essential role here. When GADTs is enabled, a type-class context given in the constructor is available by pattern matching. In this example above, pattern matching on `ShowDict` makes the `Show a` type-class context available in the body of the `showish` function.

Operationally, the `ShotDict` constructor has a hidden field that stores the `(Show a)` dictionary that is passed to `ShowDict`; so when pattern matching that dictionary becomes available for the right-hand side of the match. [Section 9.4.7 of the GHC user guide][ghc] explains this behavior in details.

We can observe the `(Show a)` dictionary instance hidden in the constructor by dumping the GHC simplifier output. Pattern matching on the constructor reveals the hidden dictionary `$dShow_aKG` as follows.

```
showish_roY :: forall a_ayV. ShowDict a_ayV -> a_ayV -> String
[GblId, Arity=2, Caf=NoCafRefs, Str=DmdType]
showish_roY =
  \ (@ a_aKE) (ds_d10M :: ShowDict a_aKE) (x_ayW :: a_aKE) ->
    case ds_d10M of _ [Occ=Dead] { ShowDict $dShow_aKG ->
    show @ a_aKE $dShow_aKG x_ayW
    }
```

With *ConstraintKinds* extension, we can further generalize this idea by passing an arbitrary context to the constructor.

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

data Dict ctxt where
  Dict :: ctxt => Dict ctxt

showish' :: Dict (Show a) -> a -> String
showish' Dict x = show x

use_showish' :: String
use_showish' = showish' Dict 10
```

[ConstraintKinds]: http://blog.omega-prime.co.uk/?p=127
[ghc]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#declaring-data-types-with-explicit-constructor-signatures
