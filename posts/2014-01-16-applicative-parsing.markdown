---
title: Applicative parsing
tags: Haskell, applicative functor, applicative parsing
author: Kwang Yul Seo
---

While implementing a parser for a toy imperative language, I used a parser
combinator library [Parsec][parsec], which supports both applicative parsing and
monadic parsing. I first wrote the parser in monadic style and then changed it
to applicative style because it gave me a more succinct parser.

This article explains the difference between these two styles and discusses the
benefits of applicative parsing over monadic parsing.

Here a simple C like program fragment:

```haskell
int max(int a, b)
{
    if (a > b)
        return a;
    else
        return b;
}
```

The following is a snippet of the AST.

```haskell
data Def = DFun Type Id [Arg] [Stm]

data Stm =
    ...
    | SIfElse Exp Stm Stm
```

The parser for function definitions is compact. `DFun` is a pure function which
takes 4 arguments. For each argument, we need to perform an effectful
computation, which is parsing. So I used `($)` to lift a pure value to the effectful world
and applied effectful arguments using `(*)`.

```haskell
def :: Parser Def
def = DFun <$> typ <*> ident <*> parens (commaSep arg) <*> braces (many stm)
```

The same parser can be written in monadic style as in the following. You can
see that we need to create more bindings to name intermediate results.

```haskell
def :: Parser Def
def = do
    t <- typ
    id <- ident
    args <- parens (commaSep arg)
    stms <- braces (many stm)
    return $ Dfun t id args stms
```

Let’s look at another example. `ifElseStm` is a parser which parses an if-else
statement into a `STM` instance with `SIfElse` data constructor. Here `(*>)` is
used to sequencing actions, discarding the result of the first operand because
we don’t need to store "if" and "else" keywords in the AST.

```haskell
ifElseStm :: Parser Stm
ifElseStm =  SIfElse <$> (reserved "if" *> parens exp) <*> stm <*> (reserved "else" *> stm)
```

The same parser also can be written in monadic style.

```haskell
ifElseStm :: Parser Stm
ifElseStm =  do
    reserved "if"
    condExp <- exp
    thenStm <- stm
    reserved "else"
    elseStm <- stm
    return $ SIfElse condExp thenStm elseStm
```

In monadic parsing, `(>>)` is used to discard the result of an effectful
computation. This parser also creates a lot of bindings to name intermediate
results.

From these two examples, we can see the stylistic difference between
applicative parsing and monadic parsing. Applicative parsing is more succinct
in this particular example. This is not always true, so I usually try both
options to find the better one.

However, the difference is not just on the style. The real difference is in how
sequential composition is handled. Monad is more powerful than applicative
functor, but that’s the exact reason why we can reason about monad less than
applicative functor. This difference in turn gives applicative parsing better
performance optimization chances. [What are the benefits of applicative parsing
over monadic parsing?][7861903] explains this point well.

For more in-depth understanding of applicative programming, refer to [FUNCTIONAL
PEARL: Applicative programming with eﬀects][IdiomLite].

[parsec]: https://hackage.haskell.org/package/parsec
[7861903]: http://stackoverflow.com/questions/7861903/what-are-the-benefits-of-applicative-parsing-over-monadic-parsing
[IdiomLite]: http://strictlypositive.org/IdiomLite.pdf
