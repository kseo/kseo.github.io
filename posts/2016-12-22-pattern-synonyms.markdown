---
title: Pattern Synonyms
tags: GHC extension, PatternSynonyms
author: Kwang Yul Seo
---
([Reddit discussion][reddit])

[Pattern synonyms][PatternSynonyms] allow us give names to pattern matches. It is a relatively new GHC extension which was first introduced in GHC 7.8 and further enhanced in GHC 8.0. In this post, I will show you an usage of pattern synonyms with a practical example.

Our assignment is to create a compiler for a toy functional language. We've already finished writing the parser and the type checker. It uses a small IR based on lambda calculus. Our next plan is to transform this IR into SK combinators for execution on the graph reduction machine.

Here's the datatype of our IR:

```haskell
data Exp = Ap Exp Exp
         | Lambda VarId Exp
         | Fun FunId
         | Var VarId
         | Int Int
```

The SK compilation scheme is taken from Simon Peyton Jones's [The Implementation of Functional Programming Languages][slpj-book-1987]. (You don't need to understand how this actually works. Interested readers are referred to SPJ's book.)

```
C[e] compiles e to SK combinators

C[e1 e2] = C[e1] C[e2]
C[\x.e] = A x [C[e]]
C[cv] = cv

A x [f] abstracts x from f

A x [f1 f2] = S (A x [f1]) (A x [f2])
A x [x] = I
A x [cv] = K cv
```

This compilation scheme can be succinctly implemented in Haskell using pattern matches on `Exp` because the scheme uses the data constructors of `Exp`.

```haskell
compile :: Exp -> Exp
compile (Ap e1 e2) = Ap (compile e1) (compile e2)
compile (Lambda v e) = abstract v (compile e)
compile cv = cv

abstract :: String -> Exp -> Exp
abstract x (Ap f1 f2) = (abstract x f1) (abstract x f2)
abstract x (Var v) | x == v = i
abstract x (Fun v) | x == v = i
abstract _ cv = k cv

s :: Exp -> Exp -> Exp
s f g = Ap (Ap (Fun "S") f ) g

k :: Exp -> Exp
k c = Ap (Fun "K") c

i :: Exp
i = Fun "I"
```

So far so good, but we realized that this basic compilation algorithm tends to produce large combinator expressions. SPJ suggests that we can improve the scheme by introducing additional combinators `B` and `C`.

Reduction rules:
```
B f g x = f (g x)
C f g x = f x g
```

Optimization rules:
```
S (K p) (K q) = K (p q)
S (K p) I = p
S (K p) q = B p q
S p (K q) = C p q
```

The optimization algorithm can be implemented as pattern matches on `Exp`.

```haskell
optimize :: Exp -> Exp
optimize (Ap (Ap (Fun "S") (Ap (Fun "K") p)) (Ap (Fun "K") q)) = k (Ap p q)
optimize (Ap (Ap (Fun "S") (Ap (Fun "K") p)) (Fun "I")) = p
optimize (Ap (Ap (Fun "S") (Ap (Fun "K") p)) q) = b p q
optimize (Ap (Ap (Fun "S") p) (Ap (Fun "K") q)) = c p q
optimize x = x
```

Unfortunately, this time the code is very complicated because `S`, `K` and `I` are not the data constructors of `Exp`. We have to sprinkle `Ap` and `Fun` all over to represent `S`, `K` and `I` combinators.

Can we improve it? As you might have expected, GHC's `PatternSynonyms` extension is the rescue! It lets us make synonyms for complicated patterns like these. We can define `S`, `K` and `I` as pattern synonyms and use them as if they were data constructors.

```haskell
pattern S p q = Ap (Ap (Fun "S") p) q
pattern K p = Ap (Fun "K") p
pattern I = Fun "I"
```

With the help of these pattern synonyms, we can rewrite `optimize` function compactly.

```haskell
optimize :: Exp -> Exp
optimize (S (K p) (K q)) = k (Ap p q)
optimize (S (K p) I) = p
optimize (S (K p) q) = b p q
optimize (S p (K q)) = c p q
optimize x = x
```

Compare the code with the optimization rules. Now they look almost the same!

[reddit]: https://www.reddit.com/r/haskell/comments/5jq9gi/kwangs_haskell_blog_pattern_synonyms/
[PatternSynonyms]: https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms
[slpj-book-1987]: https://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/
