---
title: Summary of Philip Walder’s "Proofs are Programs"
tags: Curry-Howard Isomorphism, lambda calculus, logic, natural deduction, program, proof, type system
author: Kwang Yul Seo
---
This is my short summary of Philip Wadler’s [Proofs are Programs: 19th Century Logic and 21st Century Computing][wadler].

Proofs and programs are the same thing. But it took a century of efforts to recognize the relationship.

* Modern logic began with Gottlob Frege’s Begriffschrift in 1879.
* Gerhard Gentzen introduced natural deduction in 1934.
* Alonzo Church introduced lambda calculus in 1932. He also introduced a typed version of lambda calculus in 1940.
* Dag Prawitz showed how to simplify natural deduction proofs directly in 1956.
* Haskell Curry noted a correspondence between the types of the combinators and the laws of logic as formulated by Hilbert in 1956.
* W. A. Howard put together the results of Curry and Prawitz, and wrote down the correspondence between natural deduction and lambda calculus in 1969.
* Howard’s work was published in 1980.

So it took more than 30 years to recognize the correspondence between Gentzen’s natural deduction and Church’s typed lambda calculus. They are the same thing! Proofs and programs. This correspondence is called Curry-Howard Isomorphism.

Logicians and computer scientists have discovered exactly the same type systems!

* Hindley-Milner type system: Hindley (1969), Milner (1978).
* Girard-Reynolds system: Girard (1972), Reynolds (1974).
* .. still being discovered

[wadler]: http://homepages.inf.ed.ac.uk/wadler/papers/frege/frege.pdf
