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
public keys, called a _ring_, and have any member of the ring able to sign
on its behalf, without revealing which member they are.

This lets us obscure our identity to a certain extent: instead of signing
a transaction with our identity, we add in additional identities in
order to obscure our own.

{{<img "1.png">}}

This is different from a _group_ signature, wherein a group of parties
split a private key amongst themselves, and cooperate together
to sign on behalf of that group. With ring signatures, we have no
affiliation with the other members of the ring. In practice, they're simply
chosen to obscure our own identity.

There are different ring signature schemes, of course, but I'll be focusing
on the variant used in Monero, since those are the ones that piqued
my interest recently, and that introduced me the concept.

These signatures are described a bit more completely in
Zero to Monero {{<ref-link "2">}}, so I'd also recommend
readers have a look at that resource as well.

# Preliminaries

Let $\mathbb{G}$ be a group of prime order $q$, generated
by $G$, and let $\mathbb{F}_q$
be its field of scalars.

# Schnorr Signatures

The starting point for this variant of ring signature is the ubiquitous
_Schnorr_ signature. Since this scheme has been explained many times,
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

With Schnorr signatures, we used the nonce commitment $K$, along with the
response $s$ as our signature $(K, s)$. An alternative version of the
scheme uses the response $e = H(K, X, m)$ instead of the commitment
$K$, making the signature $(e, s)$ instead:

**Signing**

$$
\begin{aligned}
\text{sign}(x, m):\cr
k &\xleftarrow{R} \mathbb{F}_q\cr
K &\longleftarrow k \cdot G\cr
e &\longleftarrow H(K, X, m)\cr
s &\longleftarrow k - e \cdot x\cr
(e, s)&
\end{aligned}
$$

{{<note>}}
We use $s = k - e x$ instead of $s = k + ex$ as before. This doesn't make any difference,
but allows us to only use subtraction in the signing process.
{{</note>}}

**Verifying:**

$$
\begin{aligned}
\text{verify}(X, m, (e, s)):\cr
e &\stackrel{?}{=} H(s\cdot G + e \cdot X, X, m)
\end{aligned}
$$

This scheme is correct, because:

$$
s \cdot G + e \cdot X = (s + ex) \cdot G = k \cdot G = K
$$

As for security, notice that we have these mappings:

$$
\begin{aligned}
(K, s) &\longrightarrow (H(K, X, m), s)\cr
(s \cdot G + e \cdot X, s) &\longleftarrow (e, s)\cr
\end{aligned}
$$

This naturally implies security reductions between these two signature schemes.
A full proof is left as an exercise to the reader 😉. 

# SAG

The next scheme is a simplification of the one proposed in {{<ref-link "1">}}.
This is the first signature scheme that qualifies as a *ring* signature.
Instead of having a single public key used to verify, we instead have
an entire ring of public keys. The signer's key is hidden somewhere in that
ring, but the signature does not reveal where the signer's key is. We don't
need to know anything about the public keys in the ring to create a signature
for it, we just need a private key for one of the elements of the ring.

Formally, we have a ring $\mathcal{R} = \\{X_1, \ldots, X_n\\}$ of public keys.
We also have a secret index $\pi \in \\{1, \ldots, n\\}$, where our
public key $X_\pi$ is located. We also have a secret key $x_\pi$, satisfying:

$$
x_\pi \cdot G = X_\pi
$$

as with previous schemes.

**Signing:**

Generate:

$$
\begin{aligned}
&k \xleftarrow{R} \mathbb{F}_q\cr
&r_i \xleftarrow{R} \mathbb{F}_q \quad (i \neq \pi)
\end{aligned}
$$

Then calculate:

$$
e_{\pi + 1} \longleftarrow H(\mathcal{R}, m, k \cdot G)
$$

and then calculate all the other challenges for $i \in \\{1, \ldots, N\\}$,
starting from $\pi + 1$, and wrapping around:

$$
e_{i + 1} \longleftarrow H(\mathcal{R}, m, r_i \cdot G + e_i \cdot X_i)
$$

And finally, we close the ring by setting:

$$
r_\pi \longleftarrow k - e_\pi x_\pi
$$

(Notice that this makes $k = r_\pi + e_\pi x_\pi$.)

Our signature is:

$$
(e_1, r_1, \ldots, r_n)
$$

**Verifying:**

Given a signature $(e_1, r_1, \ldots, r_n)$, we re-calculate the challenges:
$$
e'_{i + 1} \longleftarrow H(\mathcal{R}, m, r_i \cdot G + e_i \cdot X_i)
$$
wrapping around from $n \to 1$.

We then check:
$$
e'_1 \stackrel{?}{=} e_1
$$

## Why this works

Intuitively, this works because a valid signature satisfies:

$$
e_{i + 1} = H(\mathcal{R}, m, r_i \cdot G + e_i \cdot X_i)
$$

The difficulty in forging this signature comes from the recursive, or
wraparound nature of this chain of values:

{{<img "2.png">}}

Verifiers of the signature observe this circle as a whole. The signer,
on the other hand, creates the ring starting from their secret index $\pi$,
in a linear fashion. Then, they use their knowledge of $x_\pi$ in order
to "tie" the ring together, creating the recursive link:

{{<img "3.png">}}

Since the ring is treated as a whole, there's no way to figure out
which index $\pi$ the signer had.

# Linkability

In the context of Monero, ring signatures are used to sign transactions.
More specifically, you can think of coins with different amounts having
public keys, which can be used to authorize spending them. With a ring signature,
you can hide which coin you're actually spending, by dissimulating it among
many others.

One problem with this hiding is that you could potentially spend the same
coin twice, since you wouldn't be able to tell that the same coin was used
twice as the hidden part of a ring. This is a classic "double-spend" attack.

We want to be able to add some kind of tag to our signatures to detect
this duplication. This property is called *linkability*. It allows us
to link different ring signatures together, so that we can tell if the same
output gets spent twice. Naturally, we don't want to reveal which output
of the ring this is either.

# bLSAG

# MLSAG

# CLSAG

# Notes on Thresholdization

# Conclusion

# References

{{<ref
  "1"
  "https://eprint.iacr.org/2004/027.pdf"
  "[1] Joseph K. Liu, Victor K. Wei, and Duncan S. Wong, Linkable Spontaneous Anonymous Group Signature for Ad Hoc Groups">}}

{{<ref
  "2"
  "https://www.getmonero.org/library/Zero-to-Monero-2-0-0.pdf"
  "[2] Zero to Monero koe, Kurt M. Alonso, Sarang Noether">}}
