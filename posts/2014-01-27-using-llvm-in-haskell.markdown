---
title: Using LLVM in Haskell
tags: Haskell, LLVM
author: Kwang Yul Seo
---

Stephen Diehl’s [Implementing a JIT Compiled Language with Haskell and
LLVM][llvm] is a good introductory material on how to use LLVM in Haskell. It
explains how to build a compiler for a toy language, Kaleidoscope in Haskell.

Because LLVM is a compiler infrastructure written in C++, we need a Haskell
binding for LLVM. The tutorial uses [llvm-general][llvm-general]. llvm-general
depends on another package named [llvm-general-pure][llvm-general-pure] which
provides a pure Haskell AST (no FFI needed) for [LLVM IR][llvm-ir]. BTW, Bryan
O’Sullivan’s [LLVM binding][llvm-binding] is deprecated in favor of
llvm-general.

[llvm]: http://www.stephendiehl.com/llvm/
[llvm-general]: http://bscarlet.github.io/llvm-general/3.3.8.2/doc/html/llvm-general/index.html
[llvm-general-pure]: http://bscarlet.github.io/llvm-general/3.3.8.2/doc/html/llvm-general-pure/index.html
[llvm-ir]: http://llvm.org/docs/LangRef.html
[llvm-binding]: https://github.com/bos/llvm
