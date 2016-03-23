---
title: Learning Prolog to be a better Haskell programmer
tags: Haskell, prolog, type-level programming, functional dependencies, type families
author: Kwang Yul Seo
---

While learning some advanced topics of the Haskell type system, I found type
level programming is reminiscent of logic programming.

For example, [Fun with Functional Dependencies][wm01] shows how to implement
insertion sort using functional dependencies in a programming style similar to
Prolog. [Fun with Type Functions][FunWithTypeFuns] also shows a similar example
using type families.

This similarity is not a coincidence because of the correspondence between a
logic system and a type system, which is known as [Curry-Howard
isomorphism][Curry–Howard_correspondence].

Lesson: Learn Prolog to be a better Haskell programmer!

## References
There is [a discussion][079412] on the Haskell-cafe.

[wm01]: http://www.cse.chalmers.se/~hallgren/Papers/wm01.html
[FunWithTypeFuns]: https://wiki.haskell.org/Simonpj/Talk:FunWithTypeFuns
[Curry–Howard_correspondence]: https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence
[079412]: https://mail.haskell.org/pipermail/haskell-cafe/2010-June/079412.html

