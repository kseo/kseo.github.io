---
title: Two encodings of substitution operation
tags: substitution
author: Kwang Yul Seo
---
Substitution operations are heavily used in implementing type checkers. A substitution is a finite mappings from type variables to types. Thus it is natural to define `Subst` as a list of `(Name, Type)` pairs as in the following example.

```haskell
import Data.Bifunctor

type Name = String

data Type = TVar Name
          | TCon Name [Type]

type Subst = [(Name, Type)]
```

Composition of substitutions can be encoded as operations over the underlying association list.

```haskell
idSubst :: Subst
idSubst = []

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = map (second (applySubst s1)) s2 ++ s1
```

`applySubst` applies the substitution given over the structure of the type replacing type variables as specified.

```haskell
applySubst :: Subst -> Type -> Type
applySubst s (TVar n) = case lookup n s of
                          Nothing -> TVar n
                          Just t -> t
applySubst s (TCon n ts) = TCon n (map (applySubst s) ts)
```

Martin Grabm¨uller's [Algorithm W Step by Step][algorithmw] uses a slight variation of this encoding to implement Hindley-Milner type system. His version uses `Map Name Type` instead of `[(Name, String)]` for efficient lookup.

Alternatively, we can encode substitution operations directly as a function from type variables to types.

```haskell
type Subst = Name -> Type

idSubst :: Subst
idSubst n = TVar n

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = applySubst s1 . s2

applySubst :: Subst -> Type -> Type
applySubst s (TVar n) = s n
applySubst s (TCon n ts) = TCon n (map (applySubst s) ts)
```

Simon Peyton Jones's [The Implementation of Functional Programming Languages][slpj-book-1987] uses this encoding of substitution to implement the type checker.

These two encodings are equivalent.

[algorithmw]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf
[slpj-book-1987]: https://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/
