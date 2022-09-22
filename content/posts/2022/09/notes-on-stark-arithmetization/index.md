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

This kind of constraint asserts that a register
takes on a certain value at some point in time.
This is useful for asserting that the correct
input was used, or that the computation produced
a certain output, among other things.

Concretely, a boundary constraint is a tuple
$(i, j, \alpha)$, which represents the assertion:
$$
f_i[j] = \alpha
$$
In other words, the $i$-th register is equal
to $\alpha$ at time $j$.

## Transition Constraints

This kind of constraint is about the *evolution*
of the trace.
It asserts that two subsequent time steps of the
computation satisfy a certain relation.
One way this can be used is to check that a certain
program is being used to move the state forward.

Concretely, a transition constraint is
a polynomial $P$ over $2w$ variables.
This represents the assertion:
$$
\forall j \in [T - 1].\ P(f_1[j], \ldots, f_w[j], f_1[j + 1], \ldots, f_w[j + 1]) = 0
$$
In other words, at each time step, a certain relation
holds between the current state, and the next state.

In the common case that each state is a function
of the previous state, we can use a simple relation,
which checks that $s_{j + 1} = f(s_j)$.

That said, the transition constraints are actually
more general. For example, we could instead
check that $s_{j} = f(s_{j + 1})$, in effect
running the computation "backwards".

This bidirectionality is very useful.
For example, if we want to calculate the square root
of some number, we can do that outside of the trace,
and then assert that $s_{i + 1}^2 = s_i$,
which is simpler to check than $\sqrt{s_i} = s_{i + 1}$.

## Why both are necessary

# Arithmetized Traces

# Generalized Constraints

# What Costs You?

# A Comparison with (Turbo)-Plonk

# Conclusion
