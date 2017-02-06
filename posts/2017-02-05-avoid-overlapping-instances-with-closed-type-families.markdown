---
title: Avoid overlapping instances with closed type families
tags: overlapping instances, closed type families
author: Kwang Yul Seo
---
*Overlapping instances* are one of the most controversial features in Haskell. Fortunately, there are many tricks that let us avoid overlapping instances. In this post, I will introduce one such trick which uses closed type families.

<!--more-->

# Why overlapping instances are bad

In Haskell, we expect adding an extra instance in one module does not cause any other modules that depend on the given module to fail to compile or have different behaviors as long as the dependent modules use explicit import lists.

Unfortunately, *OverlappingInstances* breaks this expectation.

* Module A

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module A where

class C a b c | a b -> c where
  f :: a -> b -> c

instance C String a String where
  f s _ = s
```

* Module B

```haskell
module B where

import A(C(..))

func :: String -> Int -> String
func = f
```

`func "foo" 3` evaluates to `"foo"`.

Let's add a new instance declaration in `A`.

```haskell
instance {-# OVERLAPPING #-} C String Int String where
  f s i = concat $ replicate i s
```

Module `B` still compiles, but `func "foo" 3` now evaluates to `"foofoofoo"` because `C String Int String` is more specific than `C String a String`.

Wen can see that adding an extra instance silently broke the backward compatibility. To make the matters worse, there is no way to go back to the old behavior. GHC automatically chooses a more specific instance. In this case, `C String Int String` is chosen because it is more specific than `C String a String`.

# Use cases of overlapping instances

Overlapping instances are controversial because they are too useful to remove. Overlapping instances are appealing because they express the common pattern of adding a special case to an existing set of overloaded functions.

Let's check how `show` method from Prelude handles a list.

```
λ> show [1,2,3]
"[1,2,3]"
λ> show [False, True, False]
"[False,True,False]"
```

It converts a given list to a string by putting a comma between elements. According to the rule, it must show `"foo"` as `['f', 'o', 'o']`. But `show` handles a string (a list of characters) in a different manner.

```
λ> show "abc"
"\"abc\""
```

This requires overlapping instances because `[a]` overlaps with `[Char]`.

```haskell
instance Show a => Show [a] where
  ...

instance {-# OVERLAPPING #-} Show [Char] where
  ...
```

# Haskell 98 solution

Haskell Prelude avoided overlapping instances by using the extra-method trick. The trick does not require any GHC extensions, but class definitions become more complicated. Interested readers are referred to Brandon Simmons's [How the Haskell Prelude Avoids Overlapping Instances in Show][brandon] for the details.

# Another solution with closed type families

This solution is a variation of the solution introduced in Overcoming Overlapping section of Oleg Kiselyov's [Type equality predicates: from OverlappingInstances to overcoming them][typeEQ].

Here's the list of GHC extensions and imports we need.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Proxy
```

`F` is a type-level function which returns `'True` for `Char` and `'False` for any other types. This does not require overlapping instances because the set of special cases are closed.

```haskell
type family (F a) :: Bool where
  F Char  = 'True
  F a     = 'False
```

`ShowList` class defines `showl` method.

```haskell
class ShowList a where
  showl :: [a] -> String
```

The type checker computes the type of `flag` by evaluating `F a` and dispatches the method based on the type of `flag`. If it is `'True`, it searches the special case instances. Otherwise, it searches the generic case instance.

```haskell
instance (F a ~ flag, ShowList' flag a) => ShowList a where
  showl = showl' (Proxy :: Proxy flag)

class ShowList' (flag :: Bool) a where
  showl' :: Proxy flag -> [a] -> String

instance ShowList' 'True Char where
  showl' _ x = x

instance (Show a) => ShowList' 'False a where
  showl' _ x = show x
```

We can add another special case for `Bool` as follows:

```haskell
type family (F a) :: Bool where
  F Char  = 'True
  F Bool  = 'True
  F a     = 'False

instance ShowList' 'True Bool where
  showl' _ x = map toBinaryDigit x
    where toBinaryDigit False = '0'
          toBinaryDigit True  = '1'
```

Now `showList [True,False,True]` evaluates to `101` instead of `[True,False,True]`.

# Other solutions

Oleg Grenrus's [gist][overlap.hs] contains other workarounds for *OverlappingInstances*.

[brandon]: http://brandon.si/code/how-the-haskell-prelude-avoids-overlapping-types-in-show/
[typeEQ]: http://okmij.org/ftp/Haskell/typeEQ.html#without-over
[overlap.hs]: https://gist.github.com/phadej/cae76444548b9cffa10d9675e013847b
