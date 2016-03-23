---
title: Fun with hint
tags: hint, dynamic evaluation, interpreter
author: Kwang Yul Seo
---
If you are a Haskell convert from Lisp, JavaScript or any other dynamic programming language, you might miss [eval][eval] function of those languages. `eval` lets us load code dynamically and execute it on the fly. It is commonly used to provide user-defined plugins and is a very handy tool for software extension.

Dynamic evaluation is not limited to dynamic languages. Even Java supports dynamic class loading through class loaders. It seems Haskell does not support dynamic evaluation as it is a strictly defined language. But GHC allows us to compile and execute Haskell code dynamically through GHC API.

[hint][hint] library provides a Haskell interpreter built on top of GHC API. It allows to load and execute Haskell expressions and even coerce them into values.

*hint* provides a bunch of monadic actions based on `InterpreterT` monad transformer. `runInterpreter` is used to execute the action.

```haskell
runInterpreter :: (MonadIO m, MonadMask m) => InterpreterT m a -> m (Either InterpreterError a)
```

# Type check

We can check the type of a Haskell expression using `typeOf`.

```
λ> import Language.Haskell.Interpreter
λ> runInterpreter $ typeOf "\"foo\""
Right "[GHC.Types.Char]"
λ> runInterpreter $ typeOf "3.14"
Right "GHC.Real.Fractional t => t"
```

# Import modules

*hint* does not import prelude implicitly. We need import modules explicitly using `setImport`. For qualified imports, use `setImportQ` instead.

```
λ> runInterpreter $ do { setImports ["Prelude"]; typeOf "head [True, False]" }
Right "Bool"
λ> runInterpreter $ do { setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M") ]; typeOf "M.empty" }
Right "M.Map k a"
```

# Evaluate expressions

`eval` function lets us evaluate Haskell expressions dynamically.

```
λ> runInterpreter $ do { setImports ["Prelude"]; eval "head [True, False]" }
Right "True"
λ> runInterpreter $ do { setImports ["Prelude"]; eval "1 + 2 * 3" }
Right "7"
```

The result type of evaluation is `String`. To convert the result into the type we want, use `interpret` with `as`. Here `as` provides a witness for its monomorphic type.

```
λ> runInterpreter $ do { setImports ["Prelude"]; interpret "head [True, False]" (as :: Bool) }
Right True
λ> runInterpreter $ do { setImports ["Prelude"]; interpret "1 + 2 * 3" (as :: Int) }
Right 7
```

# Load modules

It is also possible to load modules dynamically.

Here's a small module `Foo` stored in `Foo.hs` file.

```haskell
module Foo where

f = head
g = tail
```

We can load `Foo` using `loadModules` function. `setTopLevelModules` ensures that all bindings of the module are in scope.

```haskell
import Control.Monad
import Language.Haskell.Interpreter

ex :: Interpreter ()
ex = do
  loadModules ["Foo.hs"]
  setTopLevelModules ["Foo"]
  setImportsQ [("Prelude", Nothing)]

  let expr1 = "f [1, 2, 3]"
  a <- eval expr1
  liftIO $ print a

  let expr2 = "g [1, 2, 3]"
  a <- eval expr2
  liftIO $ print a

main :: IO ()
main = do
  r <- runInterpreter ex
  case r of
    Left err -> print err
    Right () -> return ()
```

Executing this program prints

```
"1"
"[2,3]"
```

because `f` is `head` and `g` is `tail`.

[eval]: https://en.wikipedia.org/wiki/Eval
[hint]: https://hackage.haskell.org/package/hint
