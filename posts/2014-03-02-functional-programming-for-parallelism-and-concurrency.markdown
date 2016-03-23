---
title: Functional Programming for Parallelism and Concurrency
tags: Martin Odersky, concurrency, parallelism, Scala
author: Kwang Yul Seo
---
One reason why functional programming is gaining more attentions from main stream developers is that it is a promising technology which might solve so called "PPP (popular parallel programming) grand challenge".

Our current situation looks doomed. Moore's law is being achieved by increasing # of cores not clock cycles and huge volume workloads requires horizontal scaling. We are hopeless because mutable states in imperative programming combined with parallel processing result in non-determinism and it is not easy to cope with non-determinism in programming.

One possible way to solve this problem is to avoid mutable states. There is no non-determinism without mutable states. There is no race condition or dead lock without mutable states. Mutable states are the root of the problem. Martin Odersky explained how functional programming can solve this problem in his OSCON 2011 Java keynote presentation.

# Oscon keynote: Working hard to keep it simple

<iframe width="560" height="315" src="https://www.youtube.com/embed/3jg1AheF4n0" frameborder="0" allowfullscreen></iframe>
