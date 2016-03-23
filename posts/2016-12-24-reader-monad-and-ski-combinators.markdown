---
title: Reader monad and SKI combinators
tags: reader monad, ski combinator
author: Kwang Yul Seo
---
In this post, I am going to show you the relationship between the reader monad and the SKI combinators.

# Reader monad

The reader monad encapsulates computations which read values from a shared environment. Here's the definition of our beloved `Reader` monad.

```haskell
newtype Reader r a = Reader {
  runReader :: r -> a
}

instance Monad (Reader r ) where
  return a = Reader $ \_ -> a
  m >>= k = Reader $ \r -> runReader (k (runReader m r )) r
```

There are actually two instances of the reader monad in Haskell. The function monad `(->) r` is also a reader monad. The definition of the function monad is just like the `Reader` monad, but without `newtype` wrapping and unwrapping.

```
instance Monad ((->) r ) where
  return a = \_ -> a
  m >>= k = \r -> k (m r ) r
```

From now on, I will use the function monad definition because it makes our presentation more clear. But remember these two monad definitions are isomorphic.

# SKI combinators

[Combinatory logic][cl] is a formal system without the need for variables. It was introduced by Moses Schönfinkel and Haskell Curry.

The SKI calculus is a combinatory logic with three combinators S, K and I. It is a universal system as all operations in lambda calculus can be encoded via *abstraction elimination* into the SKI calculus.

The Haskell definition of `s`, `k` and `i` are as follows:

```
k :: a -> b -> a
k x y = x

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = (x z) (y z)

i :: a -> a
i x = x
```

From the types of these combinators, we can see that `k` is Haskell's `const` function and `i` is Haskell's `id` function.

Technically speaking, we don't need the I combinator to encode lambda calculus into the SKI combinators because we can encode I in terms of S and K. I is just for convenience.

# Relationship

So what's the relationship between the reader monad and the SKI combinators? Let's look at the types of the reader monad more closely.

```haskell
return :: a -> r -> a
(>>=) :: (r -> a) -> (a -> r -> b) -> r -> b
```

Can you see the similarity to the types of the combinator `k` and `s`? The function `return` is the same as `k`, but the function `(>>=)` is somewhat similar but not exactly the same as `s`.

Let's see the *applicative functor* definition of the reader monad instead.

```haskell
pure :: a -> r -> a
(<*>) :: (r -> a -> b) -> (r -> a) -> r -> b
```

Now we can see that the type of `(<*>)` is the same to the type of `s`. Because these functions are fully polymorphic, we don't need to check the definition of these functions to see if they are really equivalent.

[Conor McBride and Ross Paterson's paper][applicative] also mentions this relationship briefly.

# Conclusion

We've just discovered that `k` and `s` combinators of SKI calculus correspond to `pure` and `(<*>)` functions of the reader monad. Because we can encode any lambda calculus term into SKI calculus, we can encode any lambda calculus term into the reader monad too! Amazing, isn't it?

# References

1. [The Monad.Reader Issue 17: The Reader Monad and Abstraction Elimination][issue17]
2. [Do Applicative Functors Generalize the S & K Combinators?][Brandon.Si]

[cl]: https://en.wikipedia.org/wiki/Combinatory_logic
[applicative]: http://www.staff.city.ac.uk/~ross/papers/Applicative.html
[issue17]: https://themonadreader.wordpress.com/2011/01/09/issue-17/
[Brandon.Si]: http://brandon.si/code/do-applicative-functors-generalize-the-s-k-combinators/
