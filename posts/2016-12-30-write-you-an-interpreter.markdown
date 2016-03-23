---
title: Write you an interpreter
tags: interpreter
author: Kwang Yul Seo
---
Writing an interpreter for a functional language is a good exercise in Haskell. There are several tutorials on this topic.

* [Write Yourself a Scheme in 48 Hours][scheme]
* [Write You a Haskell][haskell]

Implementation techniques used in these tutorials are similar even though their source languages are distinct. They all compile the source language into a small core language based on lambda calculus, and evaluate the program with a context (or an environment).

In this post, I am not going to revisit this common technique. Instead, I will show you how to compile a program to a finite, fixed set of combinators (SKI), and then evaluate these combinators as normal Haskell function. This technique was introduced in Matthew Naylor's [Evaluating Haskell in Haskell][TMR-Issue10].

The source code is available [here][source].

# Poly

We are going to borrow the parser and type checker from Stephen Diehls's [Poly][poly], a simple ML dialect with definitions, let polymorphism and a fixpoint operator.

An example of Poly:

```ocaml
let rec factorial n = if (n == 0) then 1 else (n * (factorial (n-1)));
```

The core language of Poly is a variant of lambda calculus. `Let`, `If`, `Fix` and `Op` are added as additional constructs.

```haskell
type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)
```

# Desugar

Our first task is to desugar `Let`, `If`, `Fix` and `Op` to simplify the later stage of compilation.

```haskell
desugar :: Expr -> Expr
desugar (App fun arg) = App (desugar fun) (desugar arg)
desugar (Lam x body) = Lam x (desugar body)
desugar (Let x e body) = App (Lam x (desugar body)) (desugar e)
desugar (If cond tr fl) = foldl App (Var "$IF") args
   where args = map desugar [cond, tr, fl]
desugar (Fix e) = App (Var "$FIX") (desugar e)
desugar (Op op a b) = foldl App (Var n) args
  where
    args = map desugar [a, b]
    n = case op of
      Add -> "$ADD"
      Sub -> "$SUB"
      Mul -> "$MUL"
      Eql -> "$EQL"
desugar e = e
```

`desugar` function converts `let x = e in body` into `(\x -> body) e`. `If`, `Fix` are `Op` are desugared into function applications. `$IF`, `$FIX`, `$ADD`, `$SUB`, `$MUL`, `$EQL` will be provided as primitive functions. (Note that `$IF` can be a function because we piggy back on the lazy evaluation of the host language, Haskell.)

# Compilation to SKI combinators

The next step is to compile expressions into a fixed, finite combinators. The key idea is to replace `Lam` and `Ap` constructors with Haskell's built-in lambda and application constructs. The original interpreter of Poly is slow because it emulates beta reduction on top of Haskell, but our implementation avoids this overhead by utilizing the host system's support for beta-reduction.

For example,

```haskell
Lam "f" (Lam "a" (Lam "b" (App (App (Var "f") (Var "b") (Var "a")))
```

is compiled to

```haskell
CLam (\f -> CLam (\a -> CLam (\b -> ap (ap f b) a)))
```

Here's the definition of `CExpr`. You can see that `CLam` contains a Haskell function `CExpr -> CExpr`. No variable in the lambda abstraction is necessary.

```haskell
data CExpr
  = CVar Name
  | CApp CExpr CExpr
  | CLam (CExpr -> CExpr)
  | CBool Bool
  | CInt Integer
```

`compile` transforms a lambda calculus expression into an expression involving only `S`, `K`, `I` and constants. The SK compilation algorithm is well described in Simon Peyton Jones's [The Implementation of Functional Programming Languages][slpj-book-1987].

```haskell
compile :: Expr -> CExpr
compile (Var n) = CVar n
compile (App fun arg) = CApp (compile fun) (compile arg)
compile (Lam x body) = abstract x (compile body)
compile (Lit (LInt k)) = CInt k
compile (Lit (LBool k)) = CBool k

abstract :: Name -> CExpr -> CExpr
abstract x (CApp fun arg) = combS (abstract x fun) (abstract x arg)
abstract x (CVar n) | x == n = combI
abstract _ k = combK k

combS :: CExpr -> CExpr -> CExpr
combS f = CApp (CApp (CVar "$S") f)

combK :: CExpr -> CExpr
combK = CApp (CVar "$K")

combI :: CExpr
combI = CVar "$I"
```

For example, `(\x -> + x x) 5` is transformed as follows:

```
S --> S (\x -> + x) (\x -> x) 5
S --> S (S (\x -> +) (\x -> x)) (\x -> x) 5
I --> S (S (\x -> +) I) (\x -> x) 5
I --> S (S (\x -> +) I) I 5
K --> S (S (K +) I) I 5
```

# Primitives

Here's the definition of our primitive functions:

```haskell
infixl 0 !
(!) :: CExpr -> CExpr -> CExpr
(CLam f) ! x = f x

primitives :: [(String, CExpr)]
primitives =
  [ ("$I", CLam $ \x -> x)
  , ("$K", CLam $ \x -> CLam $ \_ -> x)
  , ("$S", CLam $ \f -> CLam $ \g -> CLam $ \x -> f!x!(g!x))
  , ("$IF", CLam $ \(CBool cond) -> CLam $ \tr -> CLam $ \fl -> if cond then tr else fl)
  , ("$FIX", CLam $ \(CLam f) -> fix f)
  , ("$ADD", arith (+))
  , ("$SUB", arith (-))
  , ("$MUL", arith (*))
  , ("$EQL", logical (==))
  ]

arith :: (Integer -> Integer -> Integer) -> CExpr
arith op = CLam $ \(CInt a) -> CLam $ \(CInt b) -> CInt (op a b)

logical :: (Integer -> Integer -> Bool) -> CExpr
logical op = CLam $ \(CInt a) -> CLam $ \(CInt b) -> if op a b then true else false

true, false :: CExpr
true = CBool True
false = CBool False
```

# Link

The final step is link our compiled program with other functions and primitives in the environment. `link` traverses the structure of `CExpr` and replaces `CVar` node with the actual function definition.

```haskell
type TermEnv = Map.Map String CExpr

emptyTmenv :: TermEnv
emptyTmenv = Map.fromList primitives

link :: TermEnv -> CExpr -> CExpr
link bs (CApp fun arg) = link bs fun ! link bs arg
link bs (CVar n) = fromJust (Map.lookup n bs)
link _ e = e
```

# Eval

Finally, `eval` is just a composition of `desugar`, `compile` and `link env`.

```haskell
eval :: TermEnv -> Expr -> CExpr
eval env = link env . compile . desugar

runEval :: TermEnv -> String -> Expr -> (CExpr, TermEnv)
runEval env nm ex =
  let res = eval env ex in
  (res, Map.insert nm res env)
```

# Optimization

The basic compilation algorithm shown above tends to produce large combinator expressions. New combinators such as `B`, `C`, `S'`, `B'` and `C'` can optimize both execution speed and program size.

[scheme]: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
[haskell]: http://dev.stephendiehl.com/fun/
[baastad]: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
[TMR-Issue10]: https://wiki.haskell.org/wikiupload/0/0a/TMR-Issue10.pdf
[poly]: https://github.com/sdiehl/write-you-a-haskell/tree/master/chapter7/poly_constraints
[slpj-book-1987]: https://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/
[source]: https://github.com/kseo/poly
