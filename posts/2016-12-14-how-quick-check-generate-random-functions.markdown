---
title: How QuickCheck generates random functions
tags: QuickCheck, test
author: Kwang Yul Seo
---
In [QuickCheck][quickcheck], test data is produced by *test generators* whose types are of the form `Gen a`. `Gen a` is a generator for values of type `a`. A type can define a default test data generator by defining an instance of the class `Arbitrary`:

```haskell
class Arbitrary a where
  arbitrary   :: Gen a
  ...
```

We can define instances of `Arbitrary` using the combinators provided by *QuickCheck*. For example, the instances of `Bool` and `Ordering` are defined using `choose` and `elements` respectively. `choose` generates a random element in the given inclusive range and `elements xs` generates an arbitrary element of the list `xs`.

```haskell
instance Arbitrary Bool where
  arbitrary = choose (False,True)
  shrink True = [False]
  shrink False = []

instance Arbitrary Ordering where
  arbitrary = elements [LT, EQ, GT]
  shrink GT = [EQ, LT]
  shrink LT = [EQ]
  shrink EQ = []
```

Simple and easy! But how about a function? Can we generate a function in the same way? The answer is surprisingly yes, but you need another type class named `CoArbitrary`.

Before explaining how `CoArbitrary` works, we need to check how `Gen` is defined:

```haskell
-- | A generator for values of type @a@.
newtype Gen a = MkGen{
  unGen :: QCGen -> Int -> a -- ^ Run the generator on a particular seed.
                             -- If you just want to get a random value out, consider using 'generate'.
  }
```

Internally, `Gen a` is a function which takes 2 arguments (`QCGen` and `Int`) and returns `a`. Here `QCGen` is a newtype wrapper around either `StdGen` or `TFGen`.

So `Gen (a -> b)` expands to `QCGen -> Int -> a -> b`. By reordering parameters, this is equivalent to `a -> Int -> QCGen -> b`, which represents `a -> Gen b`. Thus by defining

```haskell
promote :: (a -> Gen b) -> Gen (a->b)
```

we can produce a generator for a *function type*, provided that we can construct a generator for the *result type* which somehow depends on the argument value.

So we need `coarbitrary` which modifies a generator in a way depending on its first parameter.

```haskell
class CoArbitrary a where
  coarbitrary :: a -> Gen b -> Gen b
```  

 To actually define an instance of `CoArbitrary`, we need a helper function `variant`, which perturbs the generator. It creates a new generator that produces different pseudo-random results than the original.

```haskell
-- | Modifies a generator using an integer seed.
variant :: Int -> Gen a -> Gen a
```

Now we can define instances of `CoArbitrary`.

```haskell
instance CoArbitrary Bool where
  coarbitrary False = variant 0
  coarbitrary True = variant 1

instance CoArbitrary a => CoArbitrary (Maybe a) where
  coarbitrary Nothing  = variant 0
  coarbitrary (Just x) = variant 1 . coarbitrary x
```

With all the pieces in place, we can finally define an `Arbitrary` for the function type `a -> b`:

```haskell
instance (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = promote (\a -> coarbitrary a arbitrary)
```

To see how this works:

1. `\a -> coarbitrary a arbitrary` has type `a -> Gen b`
2. `promote` has type `(a -> Gen b) -> Gen (a->b)`
3. So, the entire expression has type `Gen (a->b)`

The current implementation of *QuickCheck* is a bit different as it is generalized to `Monad`, but When `m` is the function instance of `Monad`, `promote` is the same as we derived here.

```
promote :: Monad m => m (Gen a) -> Gen (m a)
```

# References

* [QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs][paper] by Koen Claessen and John Hughes
* [StackOverflow: How to generate random, typed functions][so]

[quickcheck]: https://hackage.haskell.org/package/QuickCheck
[paper]: http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
[so]: http://stackoverflow.com/a/16220336/2028189
