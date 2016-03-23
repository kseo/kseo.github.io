---
title: Type Checking with Phantom Type
tags: Haskell, phantom type, empty type
author: Kwang Yul Seo
---

In a toy programming language compiler I've written recently, the AST for
`Program` has a type variable `a` which is not used on the right-hand side of
its definition.

```haskell
data Program a =
    PDefs [Def]
    deriving (Eq, Show)
```

Here `Program` is a [phantom type][phantom] because the type parameter `a` does
not appear after the `=` sign.

But why do we need a type variable if it won't be used anyway? You can see the
hint from the type signature of the type checker:

```haskell
check :: (Program NoTypeAnnotation) -> Either TypeError (Program TypeAnnotation)
```

Once a program passes type checking, its type changes from `Program
NoTypeAnnotation` to `Program TypeAnnotation`. A type variable `a` is used to
differentiate a program with and without type annotations.

Later compiler phases explicitly require its argument type to be
`Program TypeAnnotation`, so I can’t accidentally pass a program which hasn't
been type-checked yet. For example, `desugarProgram` requires its argument to be
of type `Program TypeAnnotation`.

```haskell
desugarProgram :: Program TypeAnnotation -> ProgramC
```

`NoTypeAnnotation` and `TypeAnnotation` are declared as [empty type][empty]s
because their values are never needed. [EmptyDataDecls][EmptyDataDecls] language
extension allows us to not specify any constructors as in the following:

```
data NoTypeAnnotation
data TypeAnnotation
```

[phantom]: http://www.haskell.org/haskellwiki/Phantom_type
[empty]: https://wiki.haskell.org/Empty_type
[EmptyDataDecls]: https://prime.haskell.org/wiki/EmptyDataDecls
