---
title: "Notes on STARK Arithmetization"
date: 2022-09-22T08:51:31+00:00
draft: false
katex: true
tags:
  - "Cryptography"
  - "ZK Proofs"
---

Recently.

# What is Arithmetization?

# Execution Traces and Constraints

The starting point for STARKs is an *execution trace*.
Conceptually, this represents the evolution
of some computation through time.

More concretely, an exection trace is a matrix
of field elements, with $w$ columns and $T$ rows.
You can think of each column as a register,
and each row as a point in time.
The execution trace thus represents the value of
a set of registers at different points in time.

It's common to think of each register as
a function $f_i : [T] \to \mathbb{F}$, mapping
each time step to a field element.
We thus have $w$ column functions, $f_1, \ldots, f_w$.

When doing any kind of ZK proof, we care about
proving that some kind of statement holds.
This means that we want to prove that the execution
trace satisfies certain properties, which will
then convince us that a certain computation
was performed correctly.

These constraints come in two basic forms:
- Boundary Constraints.
- Transition Constraints.
Later in this post we'll see how to generalize
this to a broader kind of constraint, but for
now let's just focus on these two.

## Boundary Constraints

## Transition Constraints

# Arithmetized Traces

# Generalized Constraints

# What Costs You?

# A Comparison with (Turbo)-Plonk

# Conclusion
