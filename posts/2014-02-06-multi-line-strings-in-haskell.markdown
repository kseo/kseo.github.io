---
title: Multi-line strings in Haskell
tags: Haskell, multi-line string, quasiquotation
author: Kwang Yul Seo
---

Haskell supports multi-line string literals in several ways.

## unlines
[unlines][unlines] joins lines, after appending a terminating newline to each.

```haskell
multi = unlines ["line1", "line2", "line3"]
```

## Multi-line string literal

We should escape it using `\` and then another `\` where the string starts again.

```haskell
multi = "line1\
\line2\
\line3"
```

## Quasiquotation

The [raw-strings-qq][raw-strings-qq] package provides a quasiquoter for raw
string literals. In addition to supporting multi-line string, it does not
recognize escape sequences. So we don’t need to add `\` as in multi-line string
literals. `{-# LANGUAGE QuasiQuotes #-}` is required to use this feature.

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
multi = [r|line1
line2
line3|]
```

I prefer quasiquotation because I use multi-line string literals for HTML/XML
fragments and it is laborious to escape all special characters such as quotes.

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
 
multiline :: String
multiline = [r|<HTML>
<HEAD>
<TITLE>Auto-generated html formated source</TITLE>
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1252">
</HEAD>
<BODY LINK="800080" BGCOLOR="#ffffff">
<P> </P>
<PRE>|]
```

There are other quasi-quote packages such as [string-qq][string-qq],
[string-quote][string-quote] and [interpolatedstring-qq][interpolatedstring-qq].

[unlines]: http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#v:unlines
[raw-strings-qq]: http://hackage.haskell.org/package/raw-strings-qq
[string-qq]: http://hackage.haskell.org/package/interpolatedstring-qq
[string-quote]: http://hackage.haskell.org/package/string-quote
[interpolatedstring-qq]: http://hackage.haskell.org/package/interpolatedstring-qq
