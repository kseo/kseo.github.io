---
title: How I learned Haskell
tags: Haskell
author: Kwang Yul Seo
---
Happy lunar new year! Today I would like to share my experience of learning Haskell. It's been a really long journey but a really wonderful one.

<!--more-->

# First Encounter

Back in 2000, I was an undergraduate student majoring in computer science. At that time, I was into system programming and enjoyed learning low-level implementation details of operating systems and system applications. My primary language was C and programmed all programming assignments in C. I was proud that I understood how my code was compiled and executed on the machine.

One day my friend told me about *Haskell*. I wasn't interested in functional programming at that time, but I became curious because he enthusiastically persuaded me to learn Haskell. I ended up reading two books on Haskell.

* [Haskell: the Craft of Functional Programming][craft]
* [The Haskell School of Expression][school]

Both books were really nice and I learned I could program in an entirely different way. However, I wasn't sure if Haskell could solve the real-world problems I wanted to solve. So my interest in Haskell stopped there.

# Dark age

My first job was mainly about porting and optimizing Java Virtual Machine for embedded systems. My company licensed Sun's CDC JVM and I was responsible for maintaining it.

It was 2002 and Linux was still a luxury for embedded systems. RTOSes such as pSOS and VxWorks were popular on STBs and I ported JVM to these OSes. These RTOSes didn't have distinctions between kernel and user space and an application was linked statically with the kernel and ran as a single (kernel) process application on the device.

The implication was profound. I had no safety guarantee provided by modern operating systems. A bug in an application could corrupt the kernel data and crash the entire system. Moreover, because there were dozens of threads competing for shared resources, race conditions and dead locks were a common place. It took hours or even days to find and fix a trivial bug.

The situation was much better when debugging an application written in Java. Thanks to the safety guarantee of Java, certain types of bugs were impossible. A Java program can't corrupt memory and crash the system. Dead locks are reported systematically by the JVM. It was relatively fun to fix a bug in Java applications.

These experiences motivated me to find systematic ways of preventing bugs and led me to read Benjamin C. Pierce's [Types and Programming Languages][tapl]. It was the best computer science text book I've ever read. I understood why types were important in statically typed functional languages like Haskell! If universities had used this book as a undergraduate PL textbook, many of the confusions and misunderstandings about dynamic vs static typing would have disappeared.

# Stuck again

By fully understanding the merits of type systems, I started to learn Haskell in a different perspective. I read [A Gentle Introduction To Haskell][tutorial] and many tutorials on monads. It wasn't very hard to understand specific instances of monads such as *Reader*, *Writer*, *State*, *List* and *Maybe*. But I couldn't figure out how they were related. I managed to write simple applications in Haskell, but wasn't confident that I could use Haskell productively because I couldn't fully understand one of the core ideas of Haskell.

# The Challenges of Multi-core Programming

In the meantime, I changed my career and founded a tech start-up in 2008. I built mobile web browsers for the embedded systems. I created a port of [WebKit][WebKit] and hacked various components of WebKit to speed up the performance. The primary means for optimization was to leverage the multi-core CPU and GPU.

WebKit performs lots of tasks concurrently but it is mostly single-threaded. Loading a page does not benefit much from having a multi-core CPU. So I offloaded some tasks to separate threads but I only gained marginal performance benefits in exchange for largely increased complexity. I learned a lesson that I must pay very high costs of complexity to get small benefits of performance boost. Considering the already complex nature of WebKit, I ended up abandoning most of performance optimizations to keep the complexity under control.

While struggling to squeeze performance out of WebKit, I learned Haskell again to get some insights on parallel programming because Haskell was the only programming language which natively supported *STM(Software Transaction Memory)*. Simon Marlow's [Parallel and Concurrent Programming in Haskell][Parallel] helped me understand how Haskell supported parallel and concurrent programming. Though I learned many valuable lessons from the book, I also felt that the lazy nature of Haskell didn't go well with parallel programming.

# Reunion

I have spent more than 10 years of my career on embedded systems and increasingly got frustrated with the tools available. So I decided to teach myself Haskell again and use it at work. This time I started to read classic papers on functional programming and Haskell.

Philip Wadler's [Monads for functional programming][baastad] clicked my mind and I finally became enlightened. The paper is really well written, but I don't think I could understand *Monad* just because I read the paper. Years of trials and errors were necessary to understand abstract concepts like monad. It was the most exciting moment of my long journey to Haskell.

Once I understood how I could learn abstractions, the rest was easy. Now I don't get discouraged just because I don't understand abstractions at first glance. It takes time and practice to understand abstract things. I also realized that monad was just the beginning. There exist many Haskell idioms that require other abstract concepts such as *applicative functor*, *arrow*, *profunctor* and so on.

Here is the list of papers I found most enlightening when learning Haskell. I also recommend you read any paper with "Functional Pearl" attached to it.

* [A tutorial on the universality and expressiveness of fold][fold]
* [Monadic Parsing in Haskell][parsing]
* [Monad Transformers and Modular Interpreters][interpreters]
* [Data types a la carte][DataTypesALaCarte]
* [The Essence of the Iterator Pattern][iterator]
* [The Zipper][huet-zipper]

# Back to Real-World

I was confident that Haskell was a good programming language and I was looking for opportunities to use Haskell in production. Bryan O'Sullivan, Don Stewart, and John Goerzen's [Real World Haskell][realworldhaskell] was a good reference in this direction. It showed how I could use Haskell to do my daily work such as networking, system programming, databases and web programming.

Finally, I started to read real-world Haskell code available on the hackage and realized that the *Haskell I know* was different from the *Haskell that is actually used*. Real world Haskell uses lots of GHC extensions which makes me feel it is an entirely different language. A typical Haskell module starts with:

```haskell
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveDataTypeable         #-}
```

It seems sticking to Haskell 98 or 2000 does not have much practical sense because many Haskell packages already use many GHC extensions. So I learned them too. [24 Days of GHC Extensions][ghc-extensions] was a really great way of learning this topic.

I like the approach of [Yesod Web Framework Book][yesod-book] which explains the GHC extensions used in the library before explaining how to use the library. This is often the first step to learn a new library for many Haskell programmers. For example, you can't use [Servant][servant] unless you understand *DataKinds* and *TypeOperators*. So I encourage Haskell library authors to write more about the GHC extensions they use.

I also found that some packages are essential to use Haskell in practice.

* `String` type has problems. You need either [text][text] or [bytestring][bytestring] for efficient string data processing.
* Lazy IO looks nice, but does not work well in practice. To process streaming data properly you need either [pipes][pipes] or [conduit][conduit].
* You will need a custom monad or monad transformer for your application sooner or later. Either [mtl][mtl] or [transformers][transformers] is required.
* JSON is a really universal data exchange format these days. [aeson][aeson] will help you here.
* [QuickCheck][QuickCheck] is a bonus you get from using Haskell!

# Haskell in production

I founded a small Haskell shop this year and started to use Haskell in production. I realized that using Haskell in production was, surprisingly, easier than learning Haskell. It took me more than 10 years to learn Haskell, but I felt confident that I could use Haskell in production only after a few months of experiments.

There is one thing I would like to emphasize. Using Haskell does not mean that you must understand all the dependencies you use. Haskell programmers tend to care much about the implementation details of their dependencies because Haskell makes it so easy to understand the meaning of programs with types and equational reasoning. But in my opinion, this is a blessed curse.

That's not how civilization works. You can drive a car without understanding how engines work. Python or JavaScript programmers do not care about the implementation details of their dependencies because it is simply impossible. Haskell is no exception. Time and money are limited. Please don't spend too much time on understanding things. Spend more time on building things. Be practical.

Fortunately, some library authors provide a high-level overview of their library. [Type-level Web APIs with Servant][servant-paper] is a great example. It explains the core concepts and the implementation techniques of the library without involving accidental complexities of implementation details. I would love to see more papers like this.

# Tools and Libraries

[Stackage][Stackage] and the [Stack][stack] are indispensable tools for using Haskell in production. All the hard work of [FP Complete][fpcomplete] gave me confidence that Haskell was production-ready. The Haskell ecosystem is not small anymore. There are multiple competing web frameworks such as [Yesod][yesod], [Scotty][scotty], [Snap][snap], [Happstack][HappStack] and [Servant][servant]. Qualities of these packages are all good.

If you write web servers in Haskell, all the packages you need such as web servers, web frameworks, logging packages, database drivers are already available. I use *Servant*, [persistent][persistent] and [esqueleto][esqueleto] for my server. So far, everything works fine.

# Haskell Community

Haskell community is relatively small compared to other major languages, but I am often surprised by the quality of feedback I get from the community. Haskell is a great language, but the community is even greater. That's the biggest reason why I love programming in Haskell.

My journey to Haskell is still going on.

[craft]: http://www.haskellcraft.com/craft3e/Home.html
[school]: http://www.cs.yale.edu/homes/hudak/SOE/
[tapl]: https://www.cis.upenn.edu/~bcpierce/tapl/
[tutorial]: https://www.haskell.org/tutorial/index.html
[WebKit]: https://webkit.org/
[Multiprocessor]: https://www.amazon.com/gp/product/0123705916?ie=UTF8&tag=nirshavitshom-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0123705916
[Parallel]: http://chimera.labs.oreilly.com/books/1230000000929/index.html
[baastad]: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
[iterator]: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf
[huet-zipper]: https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
[fold]: http://www.cs.nott.ac.uk/~pszgmh/fold.pdf
[DataTypesALaCarte]: http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
[interpreters]: http://haskell.cs.yale.edu/wp-content/uploads/2011/02/POPL96-Modular-interpreters.pdf
[parsing]: http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
[realworldhaskell]: http://book.realworldhaskell.org/
[ghc-extensions]: https://ocharles.org.uk/blog/posts/2014-12-01-24-days-of-ghc-extensions.html
[yesod-book]: http://www.yesodweb.com/book/haskell
[servant]: http://haskell-servant.readthedocs.io/en/stable/
[pipes]: https://hackage.haskell.org/package/pipes-4.3.2/docs/Pipes-Tutorial.html
[conduit]: https://github.com/snoyberg/conduit#readme
[text]: https://hackage.haskell.org/package/text
[bytestring]: https://hackage.haskell.org/package/bytestring
[mtl]: https://hackage.haskell.org/package/mtl
[transformers]: https://hackage.haskell.org/package/transformers
[aeson]: https://hackage.haskell.org/package/aeson-1.1.0.0/docs/Data-Aeson.html
[QuickCheck]: https://hackage.haskell.org/package/QuickCheck
[Stackage]: https://www.stackage.org/
[stack]: https://docs.haskellstack.org/en/stable/README/
[fpcomplete]: https://www.fpcomplete.com/
[yesod]: http://www.yesodweb.com/
[snap]: http://snapframework.com/
[scotty]: https://hackage.haskell.org/package/scotty
[happstack]: http://www.happstack.com/page/view-page-slug/1/happstack
[persistent]: https://hackage.haskell.org/package/persistent
[esqueleto]: https://hackage.haskell.org/package/esqueleto
[servant-paper]: https://www.andres-loeh.de/Servant/servant-wgp.pdf
