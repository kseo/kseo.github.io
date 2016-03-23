---
title: Parsing arithmetic expressions with Parsec
tags: Haskell, Parsec
author: Kwang Yul Seo
---

[Parsec][parsec] provides [Text.Parsec.Expr][expr] module to parse
*expressions*. `buildExpressionParser` creates an expression parser from the
given operator table.

For example, let’s assume that we want to parse a simple arithmetic expression
such as `1+2*3-4/2`. Here `*` and `/` has higher precedence than `+`, `-` and
all operators are left-associative. We can represent the grammar in [BNF
Converter][bnf] notation.

```
EAdd. Exp ::= Exp "+" Exp1 ;
ESub. Exp ::= Exp "-" Exp1 ;
EMul. Exp1 ::= Exp1 "*" Exp2 ;
EDiv. Exp1 ::= Exp1 "/" Exp2 ;
EInt. Exp2 ::= Integer ;

coercions Exp 2 ;
```

Here’s our AST for the arithmetic expression.

```haskell
data Exp = EAdd Exp Exp
         | ESub Exp Exp
         | EMul Exp Exp
         | EDiv Exp Exp
         | EInt Integer
```
We can build an expression parser, `expr` as in the following. We specify
operators with their precedences and associativities in the operator table.
Operators with higher precedence come first in the operator table. So `*` and
`/` has higher precedence than `+` and `-` in this example.

```haskell
term =  parens expr
    <|> EInt <$> natural

table = [ [binary "*" EMul AssocLeft, binary "/" EDiv AssocLeft ]
        , [binary "+" EAdd AssocLeft, binary "-" ESub AssocLeft ]
        ]

binary  name fun assoc = Infix   (do { reservedOp name; return fun }) assoc

expr :: Parser Exp
expr = buildExpressionParser table term
```

`Text.Parsec.Expr` can handle prefix and postfix operators too. We can use the
following helper functions to specify these operators.

```haskell
prefix  name fun       = Prefix (do { reservedOp name; return fun })
postfix name fun       = Postfix (do { reservedOp name; return fun })
```

See [the calculator example][calc] for the complete code.

[parsec]: http://hackage.haskell.org/package/parsec
[expr]: http://hackage.haskell.org/package/parsec-3.1.4/docs/Text-Parsec-Expr.html
[bnf]: http://bnfc.digitalgrammars.com/
[calc]: https://github.com/kseo/calc
