---
title: "On Monero's Ring Signatures"
date: 2022-02-27T17:45:42+01:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Elliptic Curve"
  - "Signature"
---

One of Monero's main tools to increase the privacy of its transactions
is the ring signature. Ring signatures allow you to sign on behalf of
a group, without revealing which member of the group you are. They
can be constructed as
an elegant extension of Schnorr signatures, and are not that hard to understand
either.

<!--more-->

The idea behind ring signatures is that you can construct a group of
public keys, called a *ring*, and have any member of the ring able to sign
on its behalf, without revealing which member they are.

This lets us obscure our identity to a certain extent: instead of signing
a transaction with our identity, we add in additional identities in
order to obscure our own.

{{<img "1.png">}}

This is different from a *group* signature, wherein a group of parties
split a private key amongst themselves, and cooperate together
to sign on behalf of that group. With ring signatures, we have no
affiliation with the other members of the ring. In practice, they're simply
chosen to obscure our own identity.

There are different ring signature schemes, of course, but I'll be focusing
on the variant used in Monero, since those are the ones that piqued
my interest recently, and that introduced me the concept.

# Preliminaries

Let $\mathbb{G}$ be a group of prime order $q$, generated
by $G$, and let $\mathbb{F}_q$
be its field of scalars.

# Schnorr Signatures

The starting point for this variant of ring signature is the ubiquitous
*Schnorr* signature. Since this scheme has been explained many times,
including [in this blog](/posts/2021/07/signatures_from_identification/),
I'll limit myself to a brief summary.

**Key Generation**

$$
\begin{aligned}
x &\xleftarrow{R} \mathbb{F}_q\cr
X &\longleftarrow x \cdot G
\end{aligned}
$$

**Signing**

$$
\begin{aligned}
\text{sign}(x, m):\cr
k &\xleftarrow{R} \mathbb{F}_q\cr
K &\longleftarrow k \cdot G\cr
e &\longleftarrow H(K, X, m)\cr
s &\longleftarrow k + e \cdot x\cr
(K, s)&
\end{aligned}
$$

**Verifying:**

$$
\begin{aligned}
\text{verify}(X, m, (K, s)):\cr
s\cdot G &\stackrel{?}{=} K + H(K, X, m) \cdot X
\end{aligned}
$$

This is "merely" the [Fiat-Shamir-ification](https://www.wikiwand.com/en/Fiat%E2%80%93Shamir_heuristic)
of a sigma protocol for proving knowledge of
the discrete logarithm of $X$.

It's clear that our verification check is correct as well, given
the definition of $s$.

# Schnorr Signatures with Hash

# SAG

# bLSAG

# MLSAG

# CLSAG

# Notes on Thresholdization

# Conclusion

# References
