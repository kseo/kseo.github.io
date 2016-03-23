---
title: Implementing a call-by-value interpreter in Haskell
tags: call-by-value, interpreter, Strict
author: Kwang Yul Seo
---
Call-by-value is the most commonly used evaluation strategy in which all arguments to a function are reduced to normal form before they are bound inside lambda. Languages such as Java, C++, Scala and F# all use this evaluation model. A notable exception is Haskell, which uses call-by-need evaluation in which expressions are represented as *thunks* which are passed into a function unevaluated and only evaluated when needed.

This difference in evaluation model poses some challenges in writing a call-by-value interpreter in Haskell. In this post, I am going to explain how we can implement a call-by-value interpreter using various methods.

Let's start the discussion by writing a lambda calculus interpreter.

```haskell
data Expr
  = Var Int
  | Lam Expr
  | App Expr Expr
  | Lit Int
  | Prim PrimOp Expr Expr
  | Bot
  deriving Show

data Value
  = VInt Int
  | VClosure Expr Env

instance Show Value where
  show (VInt i) = show i
  show VClosure{} = "<<closure>>"

data PrimOp = Add | Mul
  deriving Show

type Env = [Value]

eval :: Env -> Expr -> Value
eval env term = case term of
  Var n -> env !! n
  Lam a -> VClosure a env
  App a b ->
    let VClosure c env' = eval env a in
    let v = eval env b in
    eval (v : env') c

  Lit n -> VInt n
  Prim p a b -> (evalPrim p) (eval env a) (eval env b)
  Bot -> error "Evaluation would not terminate"

evalPrim :: PrimOp -> Value -> Value -> Value
evalPrim Add (VInt a) (VInt b) = VInt (a + b)
evalPrim Mul (VInt a) (VInt b) = VInt (a * b)

emptyEnv :: Env
emptyEnv = []

-- (\x y -> x) 10 bot
test :: Value
test = eval emptyEnv $ App (App (Lam (Lam (Var 1))) (Lit 10)) Bot
```

Can you guess the evaluation order implemented by this interpreter? Because `test` is equivalent to `(\x y -> x) 10 undefined`, it would be `undefined` in a call-by-value language.

Let's evaluate `test` on GHCi.

```
λ> test
10
```

The evaluation order implemented by our interpreter is call-by-need because the defining language, Haskell, uses the call-by-need evaluation order and our interpreter depends on this. Transforming our interpreter into a call-by-value interpreter is not trivial because we need to find and fix every place where lazy evaluation is used in our interpreter.

In his seminar paper [Definitional interpreters for higher-order programming languages][definitional], John C. Reynolds showed how to remove this order dependence by CPS transformation. ~~But in Haskell, we can use monads to enforce the evaluation order. This is not a coincidence because there is a [close relationship][monad] between computational monads and generalized CPS.~~

**UPDATE: There is a technical mistake in the original article. The Identity monad does not make any difference here. I should have used either a strict variant of Identity monad or the Cont monad to force strict evaluation.**

Here's a monadic version of our interpreter.

```haskell
import Control.Monad.Identity

data Expr
  = Var Int
  | Lam Expr
  | App Expr Expr
  | Lit Int
  | Prim PrimOp Expr Expr
  | Bot
  deriving Show

data Value
  = VInt Int
  | VClosure Expr Env

instance Show Value where
  show (VInt i) = show i
  show VClosure{} = "<<closure>>"

data PrimOp = Add | Mul
  deriving Show

type Env = [Value]

eval :: (Monad m) => Env -> Expr -> m Value
eval env term = case term of
  Var n -> return $ env !! n
  Lam a -> return $ VClosure a env
  App a b -> do
    VClosure c env' <- eval env a
    v <- eval env b
    eval (v : env') c

  Lit n -> return $ VInt n
  Prim p a b -> evalPrim p <$> eval env a <*> eval env b
  Bot -> error "Evaluation would not terminate"

evalPrim :: PrimOp -> Value -> Value -> Value
evalPrim Add (VInt a) (VInt b) = VInt (a + b)
evalPrim Mul (VInt a) (VInt b) = VInt (a * b)

emptyEnv :: Env
emptyEnv = []

-- (\x y -> x) 10 bot
test :: Value
test = runIdentity $ eval emptyEnv $ App (App (Lam (Lam (Var 1))) (Lit 10)) Bot
```

Let's evaluate `test` again.

```
λ> test
10
```

Oops. What went wrong? The problem is that our interpreter does not enforce the evaluation of the argument in `App a b` case of `eval`. `v <- eval env b` just binds a thunk to `v` and it won't be evaluated until it is actually needed. To fix the problem, we need to force the evaluation of the argument using *bang patterns*.

**UPDATE: This bang pattern is not necessary if we used a strict monad.**

```haskell
{-# LANGUAGE BangPatterns #-}

...

eval :: (Monad m) => Env -> Expr -> m Value
eval env term = case term of
  Var n -> return $ env !! n
  Lam a -> return $ VClosure a env
  App a b -> do
    VClosure c env' <- eval env a
    !v <- eval env b
    eval (v : env') c

  Lit n -> return $ VInt n
  Prim p a b -> evalPrim p <$> eval env a <*> eval env b
  Bot -> error "Evaluation would not terminate"

...
```

Finally, we can see that evaluating `test` throws an error.

```
λ> test
*** Exception: Evaluation would not terminate
```

The moral of this story is that it is really hard to correctly implement a call-by-value interpreter in Haskell. There is high chance of making a mistake. For example, let's add a division operator to our interpreter.

```haskell
data PrimOp = Add | Mul | Div
  deriving Show

evalPrim :: PrimOp -> Value -> Value -> Value
evalPrim Add (VInt a) (VInt b) = VInt (a + b)
evalPrim Mul (VInt a) (VInt b) = VInt (a * b)
evalPrim Div (VInt a) (VInt b) = VInt (a `div` b)

-- (\x y -> x) 10 (20 / 0)
test :: Value
test = runIdentity $ eval emptyEnv $ App (App (Lam (Lam (Var 1))) (Lit 10)) (Prim Div (Lit 20) (Lit 0))
```

Evaluating `test` must throw an divide-by-zero error because its second argument is `20 / 0`. But GHCi shows that we reverted back to cal-by-need.

```
λ> test
10
```

This happens because the data constructor `VInt` is not strict. `20 / 0` is evaluated to `VInt undefined` instead of `undefined`. To make it call-by-value again, we need to add another bang pattern to `VInt` data constructor as follows:

```haskell
data Value
  = VInt !Int
  | VClosure Expr Env
```

Fortunately, we can avoid this tricky business and make our first interpreter call-by-value by just adding [Strict][strict] language extension introduced in GHC 8. `Strict` pragma allows us to switch the default evaluation strategy to call-by-value on a per module basis. This saves us huge efforts because writing a call-by-value interpreter in a call-by-value language is an easy task!

```haskell
{-# LANGUAGE Strict #-}
```

[definitional]: http://surface.syr.edu/cgi/viewcontent.cgi?article=1012&context=lcsmith_other
[strict]: https://ghc.haskell.org/trac/ghc/wiki/StrictPragma
[monad]: https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/the-essence-of-functional-programming.pdf
