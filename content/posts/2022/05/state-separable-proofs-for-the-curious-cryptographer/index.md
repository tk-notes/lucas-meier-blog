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

Let's take the example of $\mathcal{L}_G$ and $\mathcal{L}_F$.
One slight issue is that their internal state $k$ shares a name,
so simply inlining the code in $\mathcal{L}_F$ wouldn't work. To get
around this, one convention I like to use is that everything inside
of a package is implicitly namespaced by that package. So the $k$
inside of $\mathcal{L}_F$ is really $\mathcal{L}_F.k$, and the
$k$ inside of $\mathcal{L}_G$ is also shorthand for $\mathcal{L}_G.k$.
The same goes for function names.
With this convention, we can explicitly describe the package as follows:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\mathcal{L}_G \circ \mathcal{L}_F$
}\cr
\cr
&\mathcal{L}_F.k \xleftarrow{R} \\{0, 1\\}^\lambda\cr
&\mathcal{L}_G.k \xleftarrow{R} \\{0, 1\\}^\lambda\cr
\cr
&\underline{\mathtt{G}(m):}\cr
&\ \texttt{return } m \oplus \mathcal{L}_F.k \oplus \mathcal{L}_G.k\cr
\end{aligned}
}
$$

Instead of the call to $\texttt{F}$ we had before, we've now inlined
that code directly in the package. To resolve ambiguities in the variable
names, we've also explicitly included their namespace.

### Associativity

One interesting aspect of composition is that it's *associative*. In other
words, $(A \circ B) \circ C$ is the exact same package as $A \circ (B \circ C)$. The order in which you inline function definitions doesn't matter.
When you first inline the definitions in $B$, and then those in $C$,
this yields the result as first inlining those in $C$, and then inlining
all of that inside of $A$.

Now, I've said that these packages are "the same", but I mean this
in a precise way. In this case I mean that $(A \circ B) \circ C$
and $A \circ (B \circ C)$ are the exact same package, the state and
code are all exactly the same, up to a potential renaming of variable
and function names. We'll refer to this kind of relation
between packages as *definitional equality*, and we'll denote it
by $\equiv$. Continuing with this example, we have:

$$
(A \circ B) \circ C \equiv A \circ (B \circ C)
$$

Another silly example is that if we add the word $\text{foo}$ to every
name inside of the package $A$, in order to get the package $A_{\text{foo}}$,
then we'd have:

$$
A \equiv A_{\text{foo}}
$$

because definitional equality doesn't care about variable or function names.

## Parallel Composition

So far we've seen how to link packages together, using the exports
of one package to satisfy the imports of another. We called this *sequential composition*.
There's another form of composition which isn't as useful, but is still
interesting to look at. This is *parallel composition*. The idea is that
if the functions exported by two packages are distinct, then we can
form a new package by combining the state of both packages, and
exporting the functions provided by both packages. We denote
this composition by $\begin{matrix}A\cr \hline B \end{matrix}$ or $A \otimes B$.

As an example, we have:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $A$
}\cr
\cr
&k \xleftarrow{R} \mathbb{Z}/(q)\cr
\cr
&\underline{\mathtt{F}(m):}\cr
&\ \texttt{return } m + k\cr
\end{aligned}
}
\otimes
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $B$
}\cr
\cr
&k \xleftarrow{R} \mathbb{Z}/(q)\cr
\cr
&\underline{\mathtt{G}(m):}\cr
&\ \texttt{return } m \cdot k\cr
\end{aligned}
}
\equiv
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $A \otimes B$
}\cr
\cr
&A.k \xleftarrow{R} \mathbb{Z}/(q)\cr
&B.k \xleftarrow{R} \mathbb{Z}/(q)\cr
\cr
&\underline{\mathtt{F}(m):}\cr
&\ \texttt{return } m + A.k\cr
&\underline{\mathtt{G}(m):}\cr
&\ \texttt{return } m \cdot B.k\cr
\end{aligned}
}
$$

The parallel composition combines both states, and exposes both of the functions.

More formally, this composition is well defined when
$\text{out}(A) \cup \text{out}(B) = \emptyset$, and we have:

$$
\begin{aligned}
\text{out}(A \otimes B) &= \text{out}(A) \cup \text{out}(B)\cr
\text{in}(A \otimes B) &= \text{in}(A) \cup \text{in}(B)\cr
\end{aligned}
$$

One property of parallel composition which is apparent from the definition
is that it's associative and commutative. You have:

$$
(A \otimes B) \otimes C \equiv A \otimes (B \otimes C)
$$

and

$$
A \otimes B \equiv B \otimes A
$$

### Combining Sequential and Parallel Composition

These two notions of composition actually work quite well together.
If you have 4 packages $A, A', B, B'$, then you have:

$$
\frac{A}{A'} \circ \frac{B}{B'} \equiv \frac{A \circ B}{A' \circ B'}
$$

provided that the individual compositions are well defined, based on the imported and exported functions.

This equality holds because inlining one function doesn't affect
any of the other functions. This equality is very useful
when linking packages defined via parallel composition.

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
