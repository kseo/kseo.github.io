---
title: Continuation Passing Style Interpreter
tags: CPS, interpreter
author: Kwang Yul Seo
---
Lisp programmers learn Lisp by writing various flavors of Lisp interpreters. Two famous Lisp books, [Lisp in Small Pieces][lisp] and [Essentials of Programming Languages][eopl3], teach us how to write Lisp interpreters in Lisp. Both books start with a direct style interpreter which is easy to implement. But they soon rewrite the interpreter in a continuation passing style because advanced control structures such as *abort* and *call/cc* can be implemented more easily in this style.

In this post, we will follow the tradition of Lisp and will write a continuation passing style interpreter for a small functional language in Haskell. Then we will see how easily we can add *escape expression* to the language by extending the interpreter.

# Direct-style Interpreter

Our first interpreter is a straightforward implementation of the enriched lambda calculus `Expr`. It extends the lambda calculus with integer literals and primitive operators.

```haskell
data Expr
  = Var Int
  | Lam Expr
  | App Expr Expr
  | Lit Int
  | Prim PrimOp Expr Expr
  deriving Show

data PrimOp = Add | Mul
  deriving Show
```

The central component of our interpreter is a function `eval` that produces the value of an expression `term` in an environment `env`. `n` in `Var n` is the [De Bruijn index][debruijn]. The [Evaluation][eval] chapter of Stephen Diehl's [Write You a Haskell][wyah] explains the details of this direct-style interpreter. There is one difference here. Our version uses a higher-order function to represent lambda expression (`VClosure`).

```haskell
data Value
  = VInt Int
  | VClosure (Value -> Value)

instance Show Value where
  show (VInt i) = show i
  show VClosure{} = "<<closure>>"

type Env = [Value]

eval :: Env -> Expr -> Value
eval env term = case term of
  Var n -> env !! n
  Lam a -> VClosure (\v -> eval (v : env) a)
  App a b ->
    let VClosure c = eval env a in
    let v = eval env b in
    c v

  Lit n -> VInt n
  Prim p a b -> (evalPrim p) (eval env a) (eval env b)

evalPrim :: PrimOp -> Value -> Value -> Value
evalPrim Add (VInt a) (VInt b) = VInt (a + b)
evalPrim Mul (VInt a) (VInt b) = VInt (a * b)

emptyEnv :: Env
emptyEnv = []

evalExpr :: Expr -> Value
evalExpr = eval emptyEnv
```

# Continuation-passing-style Interpreter

We can think of a continuation as what to do next in a program. In direct-style, a callee returns a value to the caller. Thus the caller of the function determines what to do next and the continuation is implicitly present in the caller. In continuation-passing-style, the continuation is passed as an argument of a function and the callee determines what to do next by invoking the continuation. A function in continuation-passing-style never returns.

We can transform our interpreter into a continuation-passing-style by adding a continuation argument `Cont` to `eval` and `VClosure`.

```haskell
type Cont = Value -> Value

data Value
  = ...
  | VClosure (Value -> Cont -> Value)

eval :: Env -> Expr -> Cont -> Value
eval env term k = case term of
  Var n -> k $ env !! n
  Lam a -> k $ VClosure (\v k' -> eval (v : env) a k')
  App a b ->
    eval env a $ \(VClosure c) ->
    eval env b $ \v ->
    c v k

  Lit n -> k $ VInt n
  Prim p a b -> eval env a $ \v1 ->
                eval env b $ \v2 ->
                k $ evalPrim p v1 v2

evalExpr :: Expr -> Value
evalExpr e = eval emptyEnv e id
```

In `Var`, `Lit` and `Lam` cases, `eval` simply applies the value to the continuation. In `App` case, `eval` evaluates the function first and then subsequently evaluates the argument. The evaluation order is enforced as only one value can be passed to the continuation. `c v` applies the argument to the function and its result is passed to the original continuation `k`. `Prim` case similarly enforces the left-to-right evaluation order.

`evalExpr` passes `id` as the initial continuation which merely returns the value back.

# Escape Expression

Because all the control paths are explicit in continuation-passing-style, we can easily add control operators to our interpreter. Let's extend our interpreter with *escape expressions* that was first introduced in [Definitional interpreters for higher-order programming languages][definitional].

```
escape x in r
```

is an escape expression, whose *escape variable* is `x` and whose *body* is `r`. If `x` is applied to `a` in `r`, the body is aborted and `a` is returned. Otherwise, the evaluation of `r` proceeds normally.

```
escape x in (1 + 3) * (4 + x 10)
```

evaluates to `10` because `x` is applied to `10` inside the escape expression.

The implementation of the escape expression is one-liner. `eval` of `Escape a` adds a closure to the environment and then evaluates the expression. This closure is a reified continuation which ignores the current continuation and passes the argument as a value to the saved continuation of the escape expression.

```haskell
data Expr
  = ...
  | Escape Expr
  ...

eval :: Env -> Expr -> Cont -> Value
eval env term k = case term of
  ...
  Escape a -> eval (VClosure (\v _ -> k v) : env) a k
```

```
λ> evalExpr $ Escape (Prim Mul (Prim Add (Lit 1) (Lit 3)) (Prim Add (Lit 4) (App (Var 0) (Lit 10))))
10
```

Yay, it works!

[lisp]: https://www.amazon.com/Lisp-Small-Pieces-Christian-Queinnec/dp/0521545668
[eopl3]: http://www.eopl3.com/
[debruijn]: https://en.wikipedia.org/wiki/De_Bruijn_index
[eval]: http://dev.stephendiehl.com/fun/005_evaluation.html
[wyah]: http://dev.stephendiehl.com/fun/index.html
[definitional]: http://surface.syr.edu/cgi/viewcontent.cgi?article=1012&context=lcsmith_other
