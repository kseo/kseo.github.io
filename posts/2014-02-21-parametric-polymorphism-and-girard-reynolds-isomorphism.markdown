---
title: Parametric Polymorphism and Girard-Reynolds Isomorphism
tags: Combinatory logic, Curry-Howard Isomorphism, Djinn, Girard-Reynolds, Hindley-Milner, Intuitonistic logic, lambda calculus, Lennart Augustsson, Phil Gossett
author: Kwang Yul Seo
---

[A talk][talk] by Phil Gossett given in Google’s Advanced Programming Language
series is the best explanation of Curry-Howard isomorphism I’ve seen.

[![IMAGE ALT TEXT](http://img.youtube.com/vi/h0OkptwfX4g/0.jpg)](http://www.youtube.com/watch?v=h0OkptwfX4g "Advanced Topics in Programming Languages Series: Parametric Polymorphism and the Girard-Reynolds Isomorphism")

He briefly mentioned the origin of type system at the beginning and explained
Hindley-Milner, Type Classes (Wadler-Blott), Curry-Howard and Girard-Reynolds.
Especially, he showed the relationship between parametricity and Girard-Reynolds
isomorphism.

He finished his talk by describing Lennart Augustsson’s [Djinn][djinn], which is
small program that takes a (Haskell) type and gives you back a function of that
type if one exists.

In his talk, he gave some examples of Curry-Howard isomorphism.

* Intuitonistic logic <-> Combinatory logic (-> Lambda Calculus)
* Girard representation theorem <-> Reynolds abstraction theorem
* F2 <-> P2

BTW, there are a few technical errors in the talk. Please refer to [Wadler’s
comment][comment] on the talk.

[talk]: https://www.youtube.com/watch?v=h0OkptwfX4g
[djinn]: https://hackage.haskell.org/package/djinn
[comment]: http://wadler.blogspot.kr/2007/04/google-tech-talk-parametric.html
