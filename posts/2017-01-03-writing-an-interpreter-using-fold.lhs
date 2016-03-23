---
title: Writing an interpreter using fold
tags: fold, interpreter, catamorphism
author: Kwang Yul Seo
---
*fold* is a Swiss Army knife in functional programming. It is [expressive][fold] enough to write an interpreter for a simple functional programming language.

Let's start with a simple arithmetic language.

> data Expr = Const Int
>           | Add Expr Expr
>           | Mul Expr Expr

Writing an interpreter for this language is trivial.

> interp :: Expr -> Int
> interp (Const x) = x
> interp (Add e1 e2) = interp e1 + interp e2
> interp (Mul e1 e2) = interp e1 * interp e2

Writing a pretty printer is also easy.

> pretty :: Expr -> String
> pretty (Const x) = show x
> pretty (Add e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
> pretty (Mul e1 e2) = "(" ++ pretty e1 ++ " * " ++ pretty e2 ++ ")"

Sensitive readers might have noticed the duplication of code in `interp` and `pretty`. Yes, recursion on the structure of `Expr` is repeated.

We can extract recursion as `foldExpr` and algorithms as `ExprA`. `foldExpr` does recursion on the structure of `Expr` regardless of the algorithm being used.

> data ExprA a = ExprA
>   { val :: Int -> a
>   , add :: a -> a -> a
>   , mul :: a -> a -> a
>   }
>
> foldExpr :: ExprA a -> Expr -> a
> foldExpr alg (Const i)   = val alg i
> foldExpr alg (Add e1 e2) = add alg (foldExpr alg e1) (foldExpr alg e2)
> foldExpr alg (Mul e1 e2) = mul alg (foldExpr alg e1) (foldExpr alg e2)

Now it is possible to define the interpreter just by giving `val`, `add` and `mul` functions.

> interpA :: ExprA Int
> interpA = ExprA
>   { val = id
>   , add = (+)
>   , mul = (*)
>   }

The same goes for pretty printer.

> prettyA :: ExprA String
> prettyA = ExprA
>   { val = show
>   , add = \a b -> "(" ++ a ++ " + " ++ b ++ ")"
>   , mul = \a b -> "(" ++ a ++ " * " ++ b ++ ")"
>   }

Here is our `interp'` function defined in terms of `foldExpr` and `interpA`.

> interp' :: Expr -> Int
> interp' = foldExpr interpA

We successfully isolated algorithms from recursion, but we are still not satisfied. `ExprA` is mostly duplication of `Expr` and defining `foldExpr` is boilerplate.

We can fix this by introducing *F-algebras* and *catamorphisms*. Interested readers might want to take a look at   Bartosz Milewski's [Understanding F-Algebras][falgebra] article.

[fold]: http://www.cs.nott.ac.uk/~pszgmh/fold.pdf
[falgebra]: https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/
