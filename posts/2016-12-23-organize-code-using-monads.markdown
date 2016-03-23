---
title: Organize code using monads
tags: monad, monad transformer, lambda lifting
author: Kwang Yul Seo
---
Novice Haskell programmers think that monads are only for IO and stateful computations. But experienced Haskell programmers use monads to better structure their programs.

In this blog post, I am going to show you how we can better organize our code using monads.

# Motivating example: Lambda lifting

*Lambda lifting* is a compiler transformation which eliminates all free variables from function definitions. It is an important step in a lazy functional language because it greatly simplifies evaluation on the graph reduction machine.

In his [paper][paper], Simon Peyton Jones describes how to perform lambda lifting in a modular fashion. The lambda lifter works in three steps:

```haskell
-- | freeVars: Annotate every node in the expression with its free variables.
freeVars :: Expression -> AnnExpr Name (Set Name)

-- | Abstract the free variables from each lambda abstraction, replacing the lambda abstraction with the application of the new abstraction.
abstract :: AnnExpr Name (Set Name) -> Expression

-- | Give a unique name to each supercombinator and collect all the supercombinator definitions.
collectSCs :: Expression -> [SCDefn]
```

`lambdaLift` is the composition of these three functions.

```haskell
lambdaLift :: Expression -> [SCDefn]
lambdaLift = collectSCs . abstract . freeVars
```

I am not going to explain the details of these steps in this post. Interested readers are referred to SPJ's [paper][paper] and [book][book].

Instead, let's dive into the last step and see how `collectSCs` is actually implemented.

# Collecting supercombinators

`collectSCs` is defined in terms of a helper function named `collecSC'` which returns both the collection of supercombinators it has found and the transformed expression. It also carries around a *name supply* as an argument and returns the depleted supply as a result because it needs to generate fresh names for supercombinators.

```haskell
-- | Gives a unique name to each supercombinator, collects all the
-- supercombinator definitions into a single list, and introduce the
-- $main supercombinator definition.
collectSCs :: Expression -> [SCDefn]
collectSCs e = ("$main", [], e') : scs
  where
  (_, scs, e') = collectSCs' initialNameSupply e

collectSCs' :: NameSupply -> Expression -> (NameSupply, [SCDefn], Expression)
collectSCs' ns (EConst k) = (ns, [], EConst k)
collectSCs' ns (EVar v) = (ns, [], EVar v)
collectSCs' ns (EAp e1 e2) =
  (ns2, scs1 ++ scs2, EAp e1' e2')
  where
  (ns1, scs1, e1') = collectSCs' ns e1
  (ns2, scs2, e2') = collectSCs' ns1 e2
collectSCs' ns (ELam args body) =
  (ns2, (name, args, body') : bodySCs, EConst (CFun name))
  where
  (ns1, bodySCs, body') = collectSCs' ns body
  (ns2, name) = newName ns1 "SC"
collectSCs' ns (ELet isRec defns body) =
  (ns2, scs, ELet isRec defns' body')
  where
  (ns1, bodySCs, body') = collectSCs' ns body
  ((ns2, scs), defns') = mapAccumL collectSCs'' (ns1, bodySCs) defns

  collectSCs'' (ns, scs) (name, rhs) =
    ((ns1, scs ++ scs'), (name, rhs'))
    where
    (ns1, scs', rhs') = collectSCs' ns rhs
```

The code is rather complex compared to what it actually does. The only place where interest things happen is lambda abstractions. It replaces lambda abstractions by names and return supercombinators.

The code is complex because it violates the most important software engineering principle: *separation of concerns*. `collectSCs` contains at least three orthogonal concerns:

1. Generation of fresh names
2. Accumulation of supercombinatros
3. Transformation of expressions

# Organize code using monads

Monads are great tools to separate concerns. For example, `Reader` monad helps us get rid of an extra argument used to pass a context. `Writer` monad frees us from the agony of returning the accumulated results in every function.

So let's separate our concerns in `collectSCs`.

1. `Supply` monad for fresh name generation (it's a `State` monad in disguise)
2. `Writer` monad for accumulation of supercombinators

Because we need to compose two different monads, we use the monad transformer `SupplyT`.

```haskell
type Collector a = SupplyT (Writer [SCDefn]) a

collectSCs :: Expression -> [SCDefn]
collectSCs e = ("$main", [], e') : scs
  where
  (e', scs) = runWriter $ evalSupplyT 0 (collectSCs' e)

collectSCs' :: Expression -> Collector Expression
collectSCs' (EConst k) = return $ EConst k
collectSCs' (EVar v) = return $ EVar v
collectSCs' (EAp e1 e2) = do
  e1' <- collectSCs' e1
  e2' <- collectSCs' e2
  return $ EAp e1' e2'
collectSCs' (ELam args body) = do
  name <- freshName
  body' <- collectSCs' body
  collect (name, args, body')
  return $ EConst (CFun name)
collectSCs' (ELet isRec defns body) = do
  body' <- collectSCs' body
  defns' <- traverse (\(name, defn) -> (name,) <$> collectSCs' defn) defns
  return $ ELet isRec defns' body'

collect :: SCDefn -> Collector ()
collect defn = tell [defn]
```

We can see that the resulting code is much more readable by removing all the clutters that not not essential to the core logic.

[paper]: https://www.microsoft.com/en-us/research/publication/a-modular-fully-lazy-lambda-lifter-in-haskell/
[book]: https://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/
