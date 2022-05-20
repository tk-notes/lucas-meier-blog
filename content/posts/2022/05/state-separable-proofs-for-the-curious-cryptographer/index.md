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

So far, we've defined packages which import and export functions,
and can be composed together in various ways. While this is very neat,
it's not yet "praktisch", as they'd say in German; we're still
pretty far away from capturing the notion of security, as we
did with the more traditional game-based definition. To do this, we
need to introduce a few more notions based on packages.

The first notion is that of a *game*. A game is simply a package $G$
with no imports, i.e. $\text{in}(G) = \emptyset$. This captures
the idea that a game is something you can interact with directly.
If the game had any imports, there wouldn't be a well defined notion
of what it does, because the imported functions could significantly
change the behavior.

The second notion is that of an *adversary*. An adversary is a package $\mathcal{A}$
with many potential imports, but only a single exported function. This
function, commonly named $\texttt{run}(): \\{0, 1\\}$ is used to execute the adversary.

The idea here is that each adversary plays against some interface of functions
it can interact with. When we link an adversary $\mathcal{A}$ with
a game $G$ implementing the right interface, we end up with a package
$\mathcal{A} \circ G$, exposing a single function $\texttt{run}$.
Executing this function makes the adversary and the game interact,
and ends up with us having a single bit $0$ or $1$. In fact, because
of the random choices taken by these packages, we have a probability
distribution over these outputs.

### Advantages

This leads us to the natural notion of *advantage*. The idea is that
we have a pair of games $G_0$ and $G_1$, with $\text{out}(G_0) = \text{out}(G_1)$. The advantage of an adversary $\mathcal{A}$ interacting with $G$
tells us how well that adversary can distinguish between $G_0$ and $G_1$.

We define the *advantage* of such an adversary as:

$$
\epsilon(\mathcal{A} \circ G_b) := |P[1 \gets \mathcal{A} \circ G_0] - P[1 \gets \mathcal{A} \circ G_1]|
$$

We look at the probability that $\texttt{run}$ returns $1$ in either situation,
and the advantage measures how well the adversary is able to separate
the two cases.

One useful game I like to define is $?_b(A, B)$, which behaves
like $A$ when $b = 0$, and $B$ otherwise. In other words:

$$
\begin{aligned}
?_0(A, B) &\equiv A\cr
?_1(A, B) &\equiv B
\end{aligned}
$$

### Equality

Sometimes there are multiple ways of defining a game, where despite
the differences in the code, the behavior of the games is identical.

For example, consider these two games:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $G_0$
}\cr
\cr
&k \xleftarrow{R} \\{0, 1\\}^\lambda\cr
\cr
&\underline{\mathtt{F}(m):}\cr
&\ \texttt{return } m \oplus k\cr
\end{aligned}
}
\quad \quad
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $G_1$
}\cr
\cr
&k_0, k_1 \xleftarrow{R} \\{0, 1\\}^\lambda\cr
\cr
&\underline{\mathtt{F}(m):}\cr
&\ \texttt{return } m \oplus k_0 \oplus k_1 \cr
\end{aligned}
}
$$

It's not true that $G_0 \equiv G_1$, because the code is cleary
different, even after potential renaming. On the other hand,
the result is the same, because $k_0 \oplus k_1$ has the same distribution
as $k$.

Capturing what it means to have "the same behavior" is tricky. One way
to do this is to lean on the notion of advantage we've just defined.

We say that two games $A, B$ are *equal*, denoted $A = B$, when
for any unbounded adversary, we have:

$$
\epsilon(\mathcal{A} \circ ?_b(A, B)) = 0
$$

In other words, no adversary, even with an unbounded amount of computation,
can distinguish between the two games. This captures the intuitive idea
that their behavior is the same.

In our previous example, we had $G_0 = G_1$, because the distributions
were indeed the same.

In most situations, $=$ is much more useful than $\equiv$, since it's
more robust to unimportant changes, like moving variables around,
or replacing a complicated expression with something having the same
distribution. In fact, outside of the basic definitions, $\equiv$
is rarely needed.

### Indistinguishability

Now, this notion of equality is much too strong. It requires games
to look exactly the same, even under the scrutiny of an adversary
with no limits on their computation time. This isn't enough to
do very much Cryptography at all.

We relax this notion by only considering
"efficient" adversaries. These adversaries have a runtime polynomial
in some ambient security parameter $\lambda$. In fact, when
we refer to "adversaries" we usually mean "efficient adversaries".

This leads us to the notion of $\epsilon$-indistinguishability.
We say that two games $A$ and $B$ are $\epsilon$-indistinguishable,
or $A \stackrel{\epsilon}{\approx} B$ when for all efficient
adversaries $\mathcal{A}$, we have

$$
\epsilon(\mathcal{A} \circ ?_b(A, B)) \leq \epsilon
$$

In other words, for adversaries bounded in their computation,
they can only distinguish the two games with advantage at most $\epsilon$.

One useful property of this definition comes from the triangle inequality:

$$
A \stackrel{\epsilon_1}{\approx} B, \ B \stackrel{\epsilon_2}{\approx} C
\implies A \stackrel{\epsilon_1 + \epsilon_2}{\approx} C
$$

This property allows us to do "game-hopping" where we chain small
differences together to examine the indistinguishability of a larger
game.

Finally, we say that two games are *indistinguishable* when they
are $\epsilon$-indistinguishable, with $\epsilon$ being a *negligeable*
function of $\lambda$, defined in the same way as for traditional
game-based security.

## Summary

# Defining Security

# Reductions

## Example: Encryption with a PRF

As an example, let's consider the case of using a Pseudo-Random Function (PRF)
in order to build an encryption scheme secure under chosen plaintext attack.

### PRFs

A PRF $F : \mathcal{K} \times \mathcal{X} \to \mathcal{Y}$ is function
from $\mathcal{X} \to \mathcal{Y}$ which also takes in a key. The idea
is that if you don't know the key, you shouldn't be able to tell this
function apart from a random function. We capture this notion of
security through a pair of games:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{PRF}_0$
}\cr
\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{QueryF}(m : \mathcal{X}): \mathcal{Y}}\cr
&\ \texttt{return } F(k, m)\cr
\end{aligned}
}
\quad \quad
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{PRF}_1$
}\cr
\cr
&\text{out}[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{QueryF}(m : \mathcal{X}): \mathcal{Y}}\cr
&\ \texttt{if } m \notin \text{out}:\cr
&\ \quad \text{out}[m] \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } \text{out}[m] \cr
\end{aligned}
}
$$

In one game, we actually query the PRF, and in the other, we're querying
a random function. We say that the PRF $F$ is secure if
$\text{PRF}_0$ and $\text{PRF}_1$ are indistinguishable.

### Encryption

A symmetric encryption scheme consists of two functions:

$$
\begin{aligned}
E &: \mathcal{K} \times \mathcal{M} \xrightarrow{R} \mathcal{C}\cr
D &: \mathcal{K} \times \mathcal{C} \to \mathcal{M}
\end{aligned}
$$

Both of them take a key. One encrypts a message with a key,
producing a ciphertext, and the
other decrypts a ciphertext with a key, producing a message.
Encryption can be randomized, while decryption should be deterministic.

For the scheme to be considered correct, encrypting a message and then
decrypting the ciphertext should return the same message.

In terms of security, the intuitive idea is that an adversary shouldn't
be able to distinguish between an encryption of an actual message and
an encryption of a random message.

Using a real vs a random message is often referred to as $\text{IND}\text{\textdollar}$, with $\text{IND}$ being reserved for a variant of the game
where you submit two messages, and receive the encryption of one of the messages.

With state-separable proofs, you really want to use "real vs ideal" as the
defining principle for all of your games, since that makes composition
a lot easier.

Because of this, I'll refer to this notion as $\text{IND}$. 
We'll also also let the adversary encrypt messages of their choice.
This addition is usually referred to as chosen plaintext attack (CPA).

Thus, $\text{IND-CPA}$ security is defined by a pair of games:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{IND-CPA}_b$
}\cr
\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \texttt{return } E(k, m_b)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } E(k, m)\cr
\end{aligned}
}
$$

An encryption scheme is $\text{IND-CPA}$ secure if these two games
are indistinguishable.

### Encryption with a PRF

# Hybrid Arguments

## The General Hybrid Argument

## CDH

# Random Oracles

# Conclusion

## Resources
