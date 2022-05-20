---
title: "State-Separable Proofs for the Curious Cryptographer"
date: 2022-05-20T18:02:59+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Foundations"
---

<!--more-->

# Introduction

## Why We Care

## Overview

# State-Separable What?

## Security Games

## Packages

With state-separable proofs, we decompose security games into a smaller
unit, called a *package*. A package consists of program code, which
should be similar to what you'd write when describing schemes in pseudo-code.
How we interpret this code isn't very important, we just need to know
that it has a well defined semantics.

Part of this code is dedicated to the *state* of the package. This state
also has code to initialize it. The rest of the package is dedicated
to *functions*. As an example, consider this package:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\mathcal{L}_F$
}\cr
\cr
&k \xleftarrow{R} \\{0, 1\\}^\lambda\cr
\cr
&\underline{\mathtt{F}(m):}\cr
&\ \texttt{return } m \oplus k\cr
\end{aligned}
}
$$

The state of this package is the variable $k$. The functions exposed
by this package are just $\\{\texttt{F}\\}$. We
say that the *exports* of $\mathcal{L}_F$ are $\text{out}(\mathcal{L}_F) = \\{\texttt{F}\\}$. The semantics here are
that before any of the functions can be called the initialization
code is executed. This will set $k$ to a (uniform) random value in $\\{0, 1\\}^\lambda$.

Packages are not limited to just *exporting* functions. They can
also *import* functions. For example:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\mathcal{L}_G$
}\cr
\cr
&k \xleftarrow{R} \\{0, 1\\}^\lambda\cr
\cr
&\underline{\mathtt{G}(m):}\cr
&\ \texttt{return } \texttt{F}(m) \oplus k\cr
\end{aligned}
}
$$

This package is similar to the previous, except now it relies
on a function $\texttt{G}$, which isn't defined inside of the package.
Instead, this package depends on some external function. Think of
this like a computer program which depends on another library.
We say that the *imports* of $\mathcal{L}_G$ are ${\text{in}(\mathcal{L}_G) =
\\{\texttt{G}\\}}$.

## Composition

Since $\mathcal{L}_G$ imports the function $\texttt{F}$, and
$\mathcal{L}_F$ exports that same function, a natural question is if we can
create a larger package by linking the two packages together. Whenever
$\mathcal{L}_G$ would make a call to $\texttt{F}$, the code inside
of $\mathcal{L}_F$ would be executed. This is exactly what we define
as *sequential composition*.

Whenever we have two packages $A$ and $B$, such that $\text{in}(A) \subseteq \text{out}(B)$, we can define the sequential composition:

$$
A \circ B
$$

This package has the same exports as $A$, with $\text{out}(A \circ B) = \text{out}(A)$,
and the same imports as $B$, with $\text{in}(A \circ B) = \text{in}(B)$.

This composition defines a new package, whose state is the combination
of the states in $A$ and $B$. We replace a call to a function in $B$
by inlining the code of that function directly inside of $A$.

Let's take the example of $\mathcal{L}_G$ and $\mathcal{L}_F$. Now,
one slight issue is that their internal state $k$ shares a name,
so simply inlining the code in $\mathcal{L}_F$ wouldn't work. To get
around this, one convention I like to use is that everything inside
of a package is implicitly namespaced by that package. So the $k$
inside of $\mathcal{L}_F$ is really $\mathcal{L}_F.k$, and the
$k$ inside of $\mathcal{L}_G$ is also shorthand for $\mathcal{L}_G.k$.


### Parallel Composition

## Adversaries

### Advantages

## Summary

# Reductions

## Example: Encryption with a PRF

# Hybrid Arguments

## The General Hybrid Argument

## CDH

# Random Oracles

# Conclusion

## Resources
