---
title: "Liquid Amortization: Proving Amortized Complexity with LiquidHaskell (Functional Pearl)"
date: 2024-08-28
authors:
    - "[Jan van Brügge](https://orcid.org/0000-0003-1560-7326)"
conference: "Haskell Symposium"
doi: https://doi.org/10.1145/3677999.3678282
pdf: https://dl.acm.org/doi/pdf/10.1145/3677999.3678282
openAccess: true
---

Formal reasoning about the time complexity of algorithms and data structures is usually done in interactive theorem provers like Isabelle/HOL. This includes reasoning about _amortized_ time complexity which looks at the worst case performance over a _series_ of operations. However, most programs are not written within a theorem prover and thus use the data structures of the production language. To verify the correctness it is necessary to translate the data structures from the production language into the language of the prover. Such a translation step could introduce errors, for example due to a mismatch in features between the two languages.

We show how to prove amortized complexity of data structures directly in Haskell using LiquidHaskell. Besides skipping the translation step, our approach can also provide a didactic advantage. Learners do not have to learn an additional language for proofs and can focus on the new concepts only. For this paper, we do not assume prior knowledge of amortized complexity as we explain the concepts and apply them in our first case study, a simple stack with multipop. Moving to more complicated (and useful) data structures, we show that the same technique works for binomial heaps which can be used to implement a priority queue. We also prove amortized complexity bounds for Claessen’s version of the finger tree, a sequence-like data structure with constant-time cons/uncons on either end. Finally we discuss the current limitations of LiquidHaskell that made certain versions of the data structures not feasible.
