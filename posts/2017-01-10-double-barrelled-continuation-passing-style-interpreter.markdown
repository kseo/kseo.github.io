---
title: Double-barrelled Continuation Passing Style Interpreter
tags: double-barrelled, CPS, interpreter
author: Kwang Yul Seo
---
In the [Continuation Passing Style Interpreter][cps-interp], we wrote a continuation-passing style interpreter for a small functional language and implemented the *escape expression* which is the binder form of Scheme's *call/cc*.

Though *call/cc* is a powerful control operator, it is generally considered as a [bad abstraction][against-callcc] as a core language feature. So, in this post, we will drop escape expressions and add ML-style exceptions.

Exceptions can be used to effect non-local transfers of control. By using an *exception handler* we may "catch" a raised exception and continue evaluation. For example,

```
1 + (raise 2)
handle \x -> x + 3
```

evaluates to `5` because `2` raised by `raise 2` is passed to the exception handler `\x -> x + 3`.

To support exceptions in our interpreter, `eval` function is modified to take two continuations: an exception-handler continuation, and a return continuation. This is the so-called *double-barrelled continuation-passing style* introduced in [Comparing Control Constructs by Double-barrelled CPS][HOSC-double-barrel].

```haskell
eval :: Env -> Expr -> Cont -> Cont -> Value
eval env term h k = case term of
  Var n -> k $ env !! n
  Lam a -> k $ VClosure (\v k' -> eval (v : env) a h k')
  App a b ->
    eval env a h $ \(VClosure c) ->
    eval env b h $ \v ->
    c v k

  Lit n -> k $ VInt n
  Prim p a b -> eval env a h $ \v1 ->
                eval env b h $ \v2 ->
                k $ evalPrim p v1 v2

evalExpr :: Expr -> Value
evalExpr e = eval emptyEnv e (\x -> error "uncaught exception") id
```

`h` is the exception-handler continuation and it is simply passed along the application of `eval`. `evalExpr` is also modified to handle an uncaught exception.

Once our interpreter is transformed into a double-barrelled continuation-passing style, it is easy to add handle and raise expressions. First, let's extend `Expr` with `Handle` and `Raise` nodes.

```haskell
data Expr
  = ...
  | Handle Expr Expr
  | Raise Expr
  ...
```

Then extend `eval` function with two additional AST nodes.

```haskell
eval :: Env -> Expr -> Cont -> Cont -> Value
eval env term h k = case term of
  ...
  Raise a -> eval env a h h
  Handle a b ->
    let h' x = eval (x : env) b h k
    in eval env a h' k
```

`Raise` evaluates `a` with both continuations set to the error-handler continuation `h`. So the value is passed to the current error-handler.

`Handle` sets up a new error-handler `h'` which evaluates `b` with the environment extended with the raised value `x`. Note that `a` is evaluated with the error-handler set to `h'` so that any exception raised while evaluating `a` is passed to `h'`.

Let's run the example above!

```
λ> evalExpr $ (Prim Add (Lit 1) (Raise (Lit 2))) `Handle` (Prim Add (Var 0) (Lit 3))
5
```

Yay, it works again!

If you would like to know why we can't implement exceptions using *call/cc* alone, please read Oleg Kiselyov's article [Vast difference between delimited and undelimited continuations][undelimited].

[cps-interp]: https://kseo.github.io/posts/2017-01-09-continuation-passing-style-interpreter.html
[against-callcc]: http://okmij.org/ftp/continuations/against-callcc.html
[HOSC-double-barrel]: http://www.cs.bham.ac.uk/~hxt/research/HOSC-double-barrel.pdf
[undelimited]: http://okmij.org/ftp/continuations/undelimited.html#delim-vs-undelim
