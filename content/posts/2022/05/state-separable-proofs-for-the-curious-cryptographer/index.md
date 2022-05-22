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
We say that two games $A$ and $B$ are $\epsilon$-indistinguishable
with respect to $\mathcal{A}$,
or $A \stackrel{\epsilon}{\approx} B$ when we have

$$
\epsilon(\mathcal{A} \circ ?_b(A, B)) \leq \epsilon
$$

(the adversary is left implicit very often)

In other words, this adversary can only
distinguish the two games with advantage at most $\epsilon$.

One useful property of this definition comes from the triangle inequality:

$$
A \stackrel{\epsilon_1}{\approx} B, \ B \stackrel{\epsilon_2}{\approx} C
\implies A \stackrel{\epsilon_1 + \epsilon_2}{\approx} C
$$

This property allows us to do "game-hopping" where we chain small
differences together to examine the indistinguishability of a larger
game.

Finally, we say that two games are *indistinguishable* when
for every efficient adversary $\mathcal{A}$, there exists an $\epsilon$,
which is a negligeable function of $\lambda$,
such that these games
are $\epsilon$-indistinguishable.

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
&\ \texttt{return } \texttt{Encrypt}(m_b)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } E(k, m)\cr
\end{aligned}
}
$$

An encryption scheme is $\text{IND-CPA}$ secure if these two games
are indistinguishable.

### Encryption with a PRF

The simplest encryption scheme is the one-time pad. One problem with this
scheme is that we can only encrypt one message. A PRF lets us get
around this limitation, by letting us generate new pads on demand.

Given a PRF over $\mathcal{X}$ and $\mathcal{Y}$,
with $\mathcal{Y}$ some type where $\oplus$ makes sense, we can define
an encryption scheme with $\mathcal{M} = \mathcal{Y}$  and $\mathcal{C} = \mathcal{X} \times \mathcal{Y}$ as follows:

$$
\begin{aligned}
&\underline{E(k, m):}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ \texttt{return } (x, m \oplus F(k, x))\cr
\cr
&\underline{D(k, (x, c)):}\cr
&\ \texttt{return } c \oplus F(k, x) \cr
\end{aligned}
$$

What we want to show is that the security of this scheme reduces
to the security of the underlying PRF.

In particular, we want to show that $\text{IND-CPA}_b$ reduces to
$\text{PRF}_b$. The precise statement is that:

$$
\text{IND-CPA}_b \leq \frac{Q^2}{|\mathcal{X}|} + 2\text{PRF}_b
$$

using our short-hand notation for reductions from earlier, and with $Q$
denoting the number of queries made to the $\texttt{Encrypt}$ function
in the $\text{IND-CPA}_b$ game.

{{<note>}}
We'll use this $Q$ in a somewhat informal way, but you could also explicitly
modify the $\text{IND-CPA}_b$ game to take in this $Q$ as a parameter
to the game, and have $\texttt{Encrypt}$ return $\bot$ after $Q$ queries
have been made.
{{</note>}}

Our goal is to start with $\text{IND-CPA}_0$, and then hop our way
over to $\text{IND-CPA}_1$, quantifying the distinguishing advantage
along the way, based on the security of the underlying PRF.

First, note that:

$$
\text{IND-CPA}_1 =
\boxed{
\begin{aligned}
&\underline{\mathtt{Challenge}(m_0 : \mathcal{Y}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ y \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, y)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{Y}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ \texttt{return } (x, m \oplus \texttt{QueryF}(x))\cr
\end{aligned}
}
$$

This is because xoring with a random message is equivalent to just sampling
a random output.

Second, note that we can abstract out the use of the PRF inside
of these games:

$$
\text{IND-CPA}_b =
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^0_b$
}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \texttt{return } \texttt{Encrypt}(m_b)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ \texttt{return } (x, m \oplus \texttt{QueryF}(x))\cr
\end{aligned}
}
\circ \text{PRF}_0
$$

Then we have $\Gamma^0_b \circ \text{PRF}_0 \stackrel{\epsilon_0}{\approx} \Gamma^0_b \circ \text{PRF}_1$, with:

$$
\epsilon_0 = \epsilon(\mathcal{B} \circ \text{PRF}_b)
$$

for some adversary $\mathcal{B}$.

{{<note>}}
In this case, $\mathcal{B} \equiv \mathcal{A} \circ \Gamma^0_b$, but the actual
adversary doesn't matter, we just need to show that our adversary
$\mathcal{A}$ against $\text{IND-CPA}_b$ yields such an adversary $\mathcal{B}$, in order to reason about the advantage of $\mathcal{A}$.
{{</note>}}

Now, if we inline the random function in $\text{PRF}_1$, we have:

$$
\Gamma^0_0 \circ \text{PRF}_1 =
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^1$
}\cr
\cr
&\text{out}[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Challenge}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } \texttt{Encrypt}(m)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ \texttt{if } x \notin \text{out}:\cr
&\ \quad \text{out}[x] \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, m \oplus \text{out}[x])\cr
\end{aligned}
}
$$

Now, if we were to remove the $x \notin \text{out}$ check, this would
only change the behavior if the same $x$ were generated twice.
This happens with probability at most $\frac{Q^2}{2 |\mathcal{X}|}$, by a standard birthday paradox argument. Thus, we have
$\Gamma^1 \stackrel{Q^2 / 2 |\mathcal{X}|}{\approx} \Gamma^2$,
where $\Gamma^2$ is defined as:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^2$
}\cr
\cr
&\underline{\mathtt{Challenge}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } \texttt{Encrypt}(m)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ y \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, m \oplus y)\cr
\end{aligned}
}
$$

An equivalent way to write this would be:

$$
\Gamma^2 = 
\boxed{
\begin{aligned}
&\underline{\mathtt{Challenge}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ y \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, y)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ y \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, m \oplus y)\cr
\end{aligned}
}
$$

We can then add in a random table again, to get:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^3$
}\cr
\cr
&\text{out}[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Challenge}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ y \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, y)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ \texttt{if } x \notin \text{out}:\cr
&\ \quad \text{out}[x] \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, m \oplus \text{out}[x])\cr
\end{aligned}
}
$$

By the same difference lemma argument as before, we have
$\Gamma^3 \stackrel{Q^2 / 2 |\mathcal{X}|}{\approx} \Gamma^2$.

Notice that we have:

$$
\Gamma^3 = \Gamma^0_1 \circ \text{PRF}_1
$$

Since in the case where we use a random message, the call to $\texttt{Encrypt}$
doesn't matter, as we saw earlier. From this point, we can reach
$\text{IND-CPA}_1$, so we've done all the hops we need. Putting everything together, we have:

$$
\begin{aligned}
\text{IND-CPA}_0 &= \Gamma^0_0 \circ \text{PRF}_0\cr
&\stackrel{\epsilon_0}{\approx} \Gamma^0_0 \circ \text{PRF}_1\cr
&= \Gamma^1\cr
&\stackrel{Q^2 / 2 |\mathcal{X}|}{\approx} \Gamma^2\cr
&\stackrel{Q^2 / 2 |\mathcal{X}|}{\approx} \Gamma^3\cr
&= \Gamma^0_1 \circ \text{PRF}_1\cr
&\stackrel{\epsilon_0}{\approx} \Gamma^0_1 \circ \text{PRF}_0\cr
&= \text{IND-CPA}_1
\end{aligned}
$$

So all we have to do is apply the triangle lemma, to get:

$$
\text{IND-CPA}_0 \stackrel{\epsilon}{\approx} \text{IND-CPA}_1
$$

with:

$$
\epsilon \leq \frac{Q^2}{|\mathcal{X}|} + 2 \epsilon(\mathcal{B} \circ \text{PRF}_b)
$$

In more words, we've shown that given an adversary $\mathcal{A}$
for $\text{IND-CPA}_b$, there exists an adversary $\mathcal{B}$
for $\text{PRF}_b$ such that:

$$
\epsilon(\mathcal{A} \circ \text{IND-CPA}_b) \leq
\frac{Q^2}{\mathcal{|X|}} + 2\epsilon(\mathcal{B} \circ \text{PRF}_b)
$$

with $Q$ the number of queries made to the encryption oracle.

$\square$


# Hybrid Arguments

In our definition of the $\text{IND-CPA}$ game, we allowed adversaries
to make multiple queries to $\texttt{Challenge}$. A different
variant of the game only allows one query to be made:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{IND-CPA-1}_b$
}\cr
\cr
&\text{count} \gets 0\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{assert } \text{count} = 0\cr
&\ \text{count} \gets \text{count} + 1\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \texttt{return } E(k, m_b)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}) : \mathcal{C}}\cr
&\ \texttt{return } E(k, m)
\end{aligned}
}
$$

The $\texttt{assert}$ will make sure that only one query can be made,
returning $\bot$ if further queries are attempted.

A natural question is how much being able to make multiple queries helps.
An adversary for $\text{IND-CPA-1}$ can obviously break $\text{IND-CPA}$,
since the latter allows them to make the one query they need.
In the other direction, the question is more subtle. It turns out
that if we make $Q$ queries in the $\text{IND-CPA}$ game, then our advantage
is only larger by a factor of $Q$, compared to the $\text{IND-CPA-1}$ game.

The intuition behind the proof is that we bridge that gap
between $\text{IND-CPA}_0$ and $\text{IND-CPA}_1$, we create a series of
hybrid games $H_0, \ldots, H_Q$. The first hybrid starts
at $\text{IND-CPA}_0$, and the last ends at $\text{IND-CPA}_1$. At each step,
instead of moving from $m_0$ to $m_1$ in all of the queries, we only
change to $m_1$ in a single query:

{{<todo>}}
illustration
{{</todo>}}

Then the idea is that distinguishing between two successive games is like
distinguishing between $\text{IND-CPA-1}_0$ and $\text{IND-CPA-1}_1$:

{{<todo>}}
illustration
{{</todo>}}

Since there are $Q$ hops, that's how we get the factor of $Q$ in our
advantage.

## The General Hybrid Argument

We can formalize this argument in a general way, which will be useable
for a wide variety of games.

The basic setup is that we have two different game pairs $G_0$ and $G_1$,
with $\text{out}(G_0) = \text{out}(G_1)$,
which represent our "single query" games, and $M_0$ and $M_1$,
with $\text{out}(M_0) = \text{out}(M_1)$, which
represent our "multi query" games. We then have a series of hybrid games
$H_0, \ldots, H_Q$, satisfying $\text{out}(H_i) = \text{out}(M_b)$.
Finally, we have a series of "shims" $R_0, \ldots, R_{Q - 1}$,
with $\text{in}(R_i) = \text{out}(G_b)$ and $\text{out}(R_i) = \text{out}(M_b)$.

If it holds that:

$$
\begin{aligned}
H_0 &= M_0\cr
H_Q &= M_1
\end{aligned}
$$

and that for all $i \in \\{0,\ldots, Q - 1\\}$, we have:

$$
\begin{aligned}
R_i \circ G_0 &= H_i\cr
R_i \circ G_1 &= H_{i + 1}\cr
\end{aligned}
$$

Then for any adversary $\mathcal{A}$ against $M$, there exists an adversary
$\mathcal{B}$ against $G$, satisfying:

$$
\epsilon(\mathcal{A} \circ M_b) = Q \cdot \epsilon(\mathcal{B} \circ G_b)
$$

**Proof:**

First, we have:

$$
\epsilon(\mathcal{A} \circ M_b)
= |P[1 \gets \mathcal{A} \circ M_0] - P[1 \gets \mathcal{A} \circ M_1]|
$$

Because of the properties of $H_0$ and $H_Q$, we can write this as:

$$
|P[1 \gets \mathcal{A} \circ H_0] - P[1 \gets \mathcal{A} \circ H_1]|
$$

We can then write this as a telescoping sum:

$$
\left|\sum_{i = 0}^{Q - 1} P[1 \gets \mathcal{A} \circ H_i] -
P[1 \gets \mathcal{A} \circ H_{i + 1}]\right|
$$

This works because the intermediate terms cancel each other out,
giving us the final result.

Next, we apply the properties of $R_i$, giving us:

$$
\left|\sum_{i = 0}^{Q - 1} P[1 \gets \mathcal{A} \circ R_i \circ G_0] -
P[1 \gets \mathcal{A} \circ R_i \circ G_1]\right|
$$

At this point, we need to introduce a little gadget. We define
a new package $R$, which samples a random $j \in \\{0, \ldots, Q - 1\\}$,
and then behaves like the package $R_j$.

Conditioning on $j$, we can write our current value as:

$$
\left|\sum_{i = 0}^{Q - 1} P[1 \gets \mathcal{A} \circ R \circ G_0 \ |\ j = i] -
P[1 \gets \mathcal{A} \circ R_i \circ G_1 \ |\ j = i]\right|
$$

Now, by basic probability, we have that:

$$
\frac{1}{Q} \sum_{i = 0}^{Q - 1} P[1 \gets \mathcal{A} \circ R \circ G_b \ |\ j = i] = P[1 \gets \mathcal{A} \circ R \circ G_b]
$$

since $P[j = i] = \frac{1}{Q}$.

Multiplying our current telescoping sum by $\frac{1}{Q}$ inside,
and $Q$ on the outside, we get:

$$
Q \cdot \left|P[1 \gets \mathcal{A} \circ R \circ G_0] -
P[1 \gets \mathcal{A} \circ R \circ G_1]\right|
$$

Denoting the package $\mathcal{A} \circ R$ as an adversary $\mathcal{B}$,
This is just the value:

$$
Q \cdot \epsilon(\mathcal{B} \circ G_b)
$$

Tying everything together, we conclude that:

$$
\epsilon(\mathcal{A} \circ M_b) = Q \cdot \epsilon(\mathcal{B} \circ G_b)
$$


$\square$

## Our Specific Case

Now, let's recall our specific case. We have the following game:

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

As well as a variant $\text{IND-CPA-1}_b$, which only allows a single
query to challenge. $\text{IND-CPA-1}_b$ will play the role of $G_b$,
and $\text{IND-CPA}_b$ the role of $M_b$. We'll implicitly limit
$\text{IND-CPA}_b$ to $Q$ queries, for the sake of the argument.

The next step is to define $H_0, \ldots H_Q$:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{H}_i$
}\cr
\cr
&\text{count} \gets Q\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ b \gets \text{count} \geq i\cr
&\ \text{count} \gets \text{count} - 1\cr
&\ \texttt{return } E(k, m_b)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } E(k, m)\cr
\end{aligned}
}
$$

For $H_0$, we always use $m_0$, for $H_1$ we start using $m_1$ only
for the last query, when count is $0$, for $H_2$ we use $m_1$
for the last two, etc., until we reach $H_Q$, which always uses
$m_1$. Thus, we clearly have $H_0 = \text{IND}_0$ and $H_1 = \text{IND}_1$.

The final step is to construct our shims $R_0, \ldots, R_{Q - 1}$.
The idea is that we use the $\text{IND-CPA-1}$ game to make the transition
between choosing $m_0$ and choosing $m_1$.

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{R}_i$
}\cr
\cr
&\text{count} \gets Q\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{if } \text{count} > i:\cr
&\ \quad \texttt{return } \text{IND-CPA-1}.\texttt{Encrypt}(m_0)\cr
&\ \texttt{elif } \text{count} = i:\cr
&\ \quad \texttt{return } \text{IND-CPA-1}.\texttt{Challenge}(m_0)\cr
&\ \texttt{else } \text{count} < i:\cr
&\ \quad m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \quad \texttt{return } \text{IND-CPA-1}.\texttt{Encrypt}(m_1)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } \text{IND-CPA-1}.\texttt{Encrypt}(m)\cr
\end{aligned}
}
$$

For example, in the game $R_0$, we always encrypt $m_0$ until
we reach the $Q$th query. At this point, if we're against
$\text{IND-CPA-1}_0$, then we'll encrypt $m_0$, otherwise, we
end up encrypting $m_1$. And the linking works fine, since we
only make a single call to $\texttt{Challenge}$.

Thus, we have that:

$$
\begin{aligned}
R_i \circ \text{IND-CPA-1}_0 &= H_i\cr
R_i \circ \text{IND-CPA-1}_0 &= H\_{i+1}\cr
\end{aligned}
$$

At this point, all of the conditions are satisfied for us to apply
the hybrid argument lemma we proved earlier, to conclude that
for every adversary $\mathcal{A}$ against $\text{IND-CPA}$,
making $Q$ challenge queries,
there exists an adversary $\mathcal{B}$ against $\text{IND-CPA-1}$
satisfying:

$$
\epsilon(\mathcal{A} \circ \text{IND-CPA}_b) \leq
Q \cdot \epsilon(\mathcal{B} \circ \text{IND-CPA-1}_b)
$$

This means that whether or not you allow multiple challenge
queries doesn't fundamentally change the security of the scheme.
You just get a linear increase in advantage by being able to make
more queries. Thus, you can safely allow multiple challenge queries
if that's more convenient.

{{<note>}}
It's important to note that this hybrid argument wouldn't work
without having the $\texttt{Encrypt}$ queries available to us.
This is an essential difference, in fact. If the encryption
scheme were deterministic, then $\text{IND-1}$ might be secure,
but $\text{IND}$ would fail to be, because we could call
$\texttt{Challenge}$ multiple times with the same message, and figure
out whether the game is encrypting real messages or not.
{{</note>}}

# Hybrid Encryption

As a final example, I'd like to explore hybrid encryption,
in the form of ElGamal encryption.
This will let us explore a few interesting techniques in proofs,
including random oracles, and modelling games where the adversary
is expected to return a complex answer, like a group element.

First, let's recall how a the ElGamal encryption scheme works.
The basic idea is to combine a Diffie-Hellman key exchange with
an encryption scheme. First, we need some Cryptographic
group $\mathbb{G}$, generated by $G$, and with order $q$.
We also need an underlying symmetric encryption scheme $\Sigma$,
defined by:

$$
\begin{aligned}
\Sigma.E &: \mathcal{K} \times \mathcal{M} \to \mathcal{C}\cr
\Sigma.D &: \mathcal{K} \times \mathcal{C} \to \mathcal{M}\cr
\end{aligned}
$$

Finally, we need a hash function $H : \mathbb{G} \to \mathcal{K}$.

With this, we can define the ElGamal encryption scheme:

$$
\begin{aligned}
&\underline{\text{Gen}():}\cr
&\ a \xleftarrow{R} \mathbb{Z}/(q)\cr
&\ A \gets a \cdot G\cr
&\ \texttt{return } (a, A)\cr
\cr
&\underline{E(A, m):}\cr
&\ b \xleftarrow{R} \mathbb{Z}/(q)\cr
&\ k \gets H(b \cdot A)\cr
&\ \texttt{return } (b \cdot G, \Sigma.E(k, m))\cr
\cr
&\underline{D(a, (B, c)):}\cr
&\ k \gets H(a \cdot B)\cr
&\ \texttt{return } \Sigma.D(k, m)\cr
\end{aligned}
$$

Encryption works by generating a new key pair, and then performing
a key exchange with the receiver. This ephemeral key is included
with the ciphertext, to allow the receiver to perform their end
of the key exchange, deriving the symmetric key needed to decrypt
the ciphertext.

## The CDH Problem

When trying to break this scheme, you see the public key $A = a \cdot G$, the ephemeral key $B = b \cdot G$. If you could derive $ab \cdot G$
from these values, you would be able to decrypt ciphertexts.
This problem is referred to as the Computational Diffie-Hellman (CDH)
problem.

In traditional game-based security


## The $\text{IND-CPA}$ Game, with Random Oracles

## Reducing to CDH

# Conclusion

## Resources
