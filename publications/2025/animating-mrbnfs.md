---
title: "Animating MRBNFs: Truly Modular Binding-Aware Datatypes in Isabelle/HOL"
date: 2025-09-21
authors:
    - "[Jan van Brügge](https://orcid.org/0000-0003-1560-7326)"
    - "[Andrei Popescu](https://orcid.org/0000-0001-8747-0619)"
    - "[Dmitriy Traytel](https://orcid.org/0000-0001-7982-2768)"
conference: ITP
doi: https://doi.org/10.4230/LIPIcs.ITP.2025.11
draft: https://traytel.bitbucket.io/papers/itp25-mrbnf/mrbnf.pdf
toAppear: true
openAccess: false
---
Nominal Isabelle provides powerful tools for meta-theoretic reasoning about syntax of logics or
programming languages, in which variables are bound. It has been instrumental to major verification successes, such as Gödel’s incompleteness theorems. However, the existing tooling is not compositional. In particular, it does not support nested recursion, linear binding patterns, or infinitely branching syntax. These limitations are fundamental in the way nominal datatypes and functions on them are constructed within Nominal Isabelle. Taking advantage of recent theoretical advancements that overcome these limitations through a modular approach using the concept of map-restricted bounded natural functor (MRBNF), we develop and implement a new definitional package for binding-aware datatypes in Isabelle/HOL, called MrBNF. We describe the journey from the user specification to the end-product types, constants and theorems the tool generates. We validate MrBNF in two formalization case studies that so far were out of reach of nominal approaches: (1) Mazza’s isomorphism between the finitary and the infinitary affine λ-calculus, and (2) the POPLmark 2B challenge, which involves non-free binders for linear pattern matching.
