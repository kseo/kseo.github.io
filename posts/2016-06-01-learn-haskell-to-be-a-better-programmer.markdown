---
title: Learn Haskell to be a better programmer
tags: Haskell, category theory, composition
author: Kwang Yul Seo
---

Haskell is notorious for being hard to learn. Pure functions, lazy evaluation,
Haskell type system are just start.

To use Haskell effectively, it is necessary to learn the abstract concepts
borrowed from the category theory such as Functor, Applicative Functor and
Monad. You need to understand these concepts throughly because most of Haskell
code is written using these abstract non-sense.

Of course, it is still possible to write IO code and use `Maybe`, `Either` and
`List` types without understanding Monad. But then why do you want to learn
Haskell anyway? If you avoid learning these concpets, you can't learn much from
Haskell. It would be much beneficial to learn other more practical languages.

Before explaining why you should learn Haskell, let's ask a question. What's the
essence of programming?

Programming is basically instructing the computer to some labor. For example,
"Load the value at memory x into the register, add 1 to it, and store the value
back to the memory" is a program.

But a program is not just a sequence of instructions. It is a solution to our
real problem. What makes programming interesting is that the problems we solve
as a programmer is much bigger in size than simply loading a value from the
memory and doing some arithmetic.

So programming is to divide a big problem that can't be solved at once into many
small problems, solve them independently, and compose the resulting small
programs into a program that solves the original problem. In other words, the
essence of programming is recomposition after decomposition. See Bartosz
Milewski's [Category: The Essence of Composition][composition].

Here comes the most important property of programming, which is called
*composability*. We need to solve many complex problems which are similar but
not exactly same. If we can compose small reusable programs into a new program
which solves the new problem, the productivity of a programmer will be
dramatically increased.

The changes of programming paradigm in the history can be explained as our
continous endeavor to enhance the composability. For example, the shift from
assembly programming with goto to structure programming emphasizing subroutine
and loop was necessary as the problem size increases. We desparately needed a
better way to compose programs.

But as the complexity of problem drastically increased again in 80-90s, we
needed a new programming paradigm called object-oriented programming. Classes
and objects, encapsulation and information hiding were another endeavor to
imporve the composability of programs.

Now in 2010s, functional programming is gaining attention. The complexity of
problems we have today is enormous and we need new tools and concepts to cope
with ever increasing complexity. Classes are not enough. We need more
composability.

Haskell provides new tools and concepts which can help organize code better.
Concepts like Functor, Applicative Functor, Monad, Arrow and Lense all provide
new means to compose programs. See Gabriel Gonzalez's [The category design
pattern][category].

In fact, you already know some of these concepts. For example, ES6's Promise,
C#'s null propagation operator, Python's list comprehension all share the same
monadic structure. But you probably never noticed the common structure lying
behind these different language features. After you learn Haskell, you will
begin to see the common structure you've never imagined before.

In summary, the essence of programming is *composition*. Haskell provides new
tools and concepts to compose programs. Learning Haskell improves your code
organizational skills and make you prepared to handle more complex problems.
[Learn you a Haskell for great good!][lyhgg]

[composition]: https://bartoszmilewski.com/2014/11/04/category-the-essence-of-composition/
[category]: http://www.haskellforall.com/2012/08/the-category-design-pattern.html
[lyhgg]: http://learnyouahaskell.com/chapters
