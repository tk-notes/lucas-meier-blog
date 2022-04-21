---
title: "Canetti's Paradoxical Signature Scheme"
date: 2022-04-15T23:58:37+02:00
draft: true
katex: true
tags:
  - "Cryptography"
---

Hook

<!--more-->

Introduction.

# The Ideal of the Random Oracle

A *random oracle* is not really something that exists. Rather,
it is a technique we use when modelling security. We
model some component of a Cryptographic scheme *as* a random
oracle, in order to study the security of that scheme.

## Example: Hybrid Encryption

As an example, let's consider a scheme using *hybrid encryption*.
We'll combine a Diffie-Hellman key-exchange, with a symmetric
encryption scheme, in order to create an *asymmetric* encryption
scheme.

The private key will be a scalar $x \in \mathbb{F}_q$,
and the associated public key will be the point
${X := x \cdot G \in \mathbb{G}}$.

Encrypting a message can be describe with three steps

1. Generate a random key-pair $(e, E := e \cdot G)$, and obtain the shared secret ${S := e \cdot X}$.
2. Derive a symmetric key $k$ from this secret.
3. Encrypt the message $m$ using $k$, to obtain $c$. Join $c$ with $E$ to produce the ciphertext $(E, c)$.

Step 2 is where we bridge points in the group with
keys for our symmetric encryption. In practice, this is usually done
with a hash function $H : \mathbb{G} \to \mathcal{K}$.
A random oracle is used to *model* how this hash function behaves.
The idea is that this function should act like a randomly
chosen function. There shouldn't be any way to predict
what the output of the function will be, without actually
calling the function.

The analogy we often use is that this random oracle
is like a gnome in a box. When we give an input to the gnome,
it will check if it's already decided on an output, and
generate a random output otherwise.

In practice, we don't actually need to talk about gnomes
when proving the security of schemes. Instead, this book-keeping
is managed by the challenger in the security game.

To illustrate, let's look at the $\text{IND-CPA}$ security
of this encryption scheme.

{{<note>}}
I've been getting into the somewhat unorthodox libary based presentation of security games, as featured in
[The Joy of Cryptography](https://joyofcryptography.com/),
so I'm going to present things in this style. Feel free
to skim over the security game aspects: they're secondary
to the point I'm trying to get at.
{{</note>}}

We model the security of this encryption scheme by
having an adversary interact with it:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\mathcal{L}_{b}$
}\cr
\cr
&x \xleftarrow{R} \mathbb{F}_q\cr
&X := x \cdot G\cr
\cr
&\underline{\mathtt{Pk()}: \mathbb{G}}\cr
&\ \texttt{return } X\cr
\cr
&\underline{\mathtt{Challenge}(m_0, m_1 : \mathcal{M}): \mathbb{G} \times \mathcal{C}}\cr
&\ e \xleftarrow{R} \mathbb{F}_q\cr
&\ E := e \cdot G\cr
&\ k := H(e \cdot X)\cr
&\ c := \text{Enc}(k, m_b)\cr
&\ \texttt{return } (E, c)\cr
\end{aligned}
}
$$

This represents two libraries, $\mathcal{L}_0$, and $\mathcal{L}_1$.
If the encryption scheme is secure, then an adversary shouldn't
be able to figure out which of the two libraries it's interacting with.

This library makes calls to the hash function $H$. In the random
oracle model, we model this function by generating random
outputs on demand:

$$
\boxed{
\begin{aligned}
&\text{outputs}[\cdot] := \bot\cr
\cr
&\underline{H(P : \mathbb{G}): \mathcal{K}}\cr
&\ \texttt{if } \text{outputs}[P] = \bot:\cr
&\ \quad \text{outputs}[P] \xleftarrow{R} \mathcal{K}\cr
&\ \texttt{return } \text{outputs}[P]\cr
\end{aligned}
}
$$

Every time we need to query the function, we generate a random
output, making sure to reuse the output if we've already
generated it before.

And this is really all there is to random oracles. They're
much easier to work with, because their output has no structure
whatsoever, unlike a real hash function. This makes proofs
in the random oracle model quite a lot simpler. In this
particular case, the idea of the security proof is essentially
that the only way to break the scheme would be to somehow
gain information about the key $k$, but because we model
$H$ as a random oracle, the only way to do that would be
to learn $e \cdot X$, since the output
is completely unrelated. And being able to learn $e \cdot X$
from $E$ and $X$ is reducable to the Computation-Diffie-Hellman
(CDH) problem.

# Hash Functions

# Canetti's Paradox

# Lessons?

# Conclusion
