---
title: Golden tests are tasty
tags: tasty, golden, test
author: Kwang Yul Seo
---
Haskell programmers love property based testing thanks to glorious [QuickCheck][quickcheck] library. Once we specify the program behavior in terms of properties, *QuickCheck* generates random test cases and checks if the given properties hold for these test cases. Once it finds a failing test case, it automatically shrinks the test case to the minimal value which still violates the property.

However, we, Haskell programmers, often forget that there are other test methodologies. In this post, I am going to introduce *gold testing* provided by [tasty-golden][tasty-golden] package.

The idea is simple. A golden test is just an `IO` action that writes its result to a file. To pass the test, this output file should be identical to the corresponding *golden* file, which contains the correct result for the test.

# A cast study: untyped lambda calculus

We have an [untyped lambda calculus implementation][untyped]. It provides `run` function which evaluates the given lambda calculus script to a `String`.

```haskell
run :: String -> Either String String
```

For testing, we would like to check if this function works correctly for various input scripts. To enumerate a few test scenarios:

* `\x.x` evaluates to itself because there is no redex in the term.
* `(\x.x)(\x.x)` evaluates to `(\x.x)` by substitution.
* `\x.y` throws an error because `y` is an unbound variable.

Of course, we can manually create a test case for each scenario, but this is boring and repetitive. Instead of writing test cases in Haskell, let's create a script file and its expected output file for each input:

* t1.lc
```
\x.x
```

* t1.golden
```
\x.x
```

* t2.lc
```
(\x.x)(\x.x)
```

* t2.golden
```
(\x.x)
```

* t3.lc
```
(\x.y)
```

* t3.golden
```
"untyped lambda-calculus" (line 2, column 1):
unexpected end of input
The variable y has not been bound
```

Once we have these files ready, what golden tests do is to read each script file and evaluates the script, and compares the output with the expected output contained in the expected file. If the actual output and the expected output are different, the test fails. We call the expected output file as the *golden file*.

For convenience, tasty-golden generates the golden files if they are not available. So you don't need to create golden files initially. You also can regenerate the golden files using the `--accept` flag. This is useful when you know that your change is valid and want to rebase all the golden files accordingly.

# Implementation

Here's the actual code which performs golden tests as described above. `listTestFiles` enumerates all the script files under `test/tests` directory and `mkGoldenTest` creates a golden test from the script file using `goldenVsString` function provided by tasty-golden.

```haskell
import Language.LambdaCalculus

import qualified Data.ByteString.Lazy.Char8 as BS
import System.FilePath
import System.FilePath.Glob

import Test.Tasty
import Test.Tasty.Golden as G

main :: IO ()
main = do
  paths <- listTestFiles
  goldens <- mapM mkGoldenTest paths
  defaultMain (testGroup "Tests" goldens)

listTestFiles :: IO [FilePath]
listTestFiles = globDir1 pat "test/tests"
  where pat = compile "*.lc"

mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  let testName = takeBaseName path
  let goldenPath = replaceExtension path ".golden"
  return (goldenVsString testName goldenPath action)
  where
    action :: IO BS.ByteString
    action = do
      script <- readFile path
      let actual = either id id (run script)
      return (BS.pack actual)
```

[quickcheck]: https://hackage.haskell.org/package/QuickCheck
[tasty-golden]: https://hackage.haskell.org/package/tasty-golden
[untyped]: https://github.com/kseo/untyped
