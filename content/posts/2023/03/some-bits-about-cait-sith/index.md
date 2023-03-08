---
title: "Some Bits about Cait-Sith"
date: 2023-03-08T10:48:23+01:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "MPC"
---

Recently, I've been working on a threshold ECDSA protocol---and its implementation---called [Cait-Sith](https://github.com/cronokirby/cait-sith).
I thought I might write a little blog post to talk about it.

<!--more-->

# Threshold ECDSA

We should probably start this post by talking about *ECDSA* to begin with.
This is a signature algorithm for elliptic curves.
To abstract a bit, we work with a so-called "cryptographic group"
$\mathbb{G}$, with an associated field of scalars $\mathbb{F}_q$,
and a generator $G$.

A key pair consists of a private scalar $x$,
along with a public point $X = x \cdot G$.

To produce a signature, we run the following algorithm:
$$
\begin{aligned}
&\underline{\text{Sign}(m)}\cr
&\quad m \gets H(m)\cr
&\quad k \xleftarrow{R} \mathbb{F}_q\cr
&\quad K \gets \frac{1}{k} \cdot G\cr
&\quad s \gets k \cdot (H(m) + h(K) \cdot x)\cr
&\quad \texttt{return } (h(K), s)
\end{aligned}
$$
Here $H$ is a hash function producing a scalar from a message,
and $h$ is a function returning the "x coordinate"
of a point $K$.
You can sort of think of this as a hash, but not quite.

And to verify $\sigma = (r, s)$, we check that:
$$
r \overset{?}{=} h \left(\frac{1}{s} \cdot (H(m) \cdot G + r \cdot X) \right)
$$

This works because:
$$
\frac{s}{k} \cdot G = (H(m) + rx) \cdot G = H(m) \cdot G + r \cdot X
$$
Then, dividing both sides by $s$, we get:
$$
K = \frac{1}{k} \cdot G = \frac{1}{s} (H(m) \cdot G + r \cdot X)
$$
Then, taking $h$ on both sides gives us the verification equation.

## The Threshold Setting.

Now, so far we've just described a standard signature scheme.
One person has a private key, and this lets them create signatures.
The public key is, well, public, and can be used by anybody
else to verify these signatures.

In the threshold setting, we want to make it so that no
single person has the private key.
Instead, the key should be shared amongst $n$ parties,
such that any $t$ of them are able to reconstruct the key.
We then want to design a protocol allowing $t$ parties
to sign, *without* them ever learning with the private key is.

One advantage of doing this is that we gain additional security,
since no single party ever learns the key,
assuming that a sufficient number of parties aren't compromised.

## Abstracting Many Details

Many details of this setting can be abstracted.

Basically, we assume that $n$ parties have shares $x_1, \ldots, x_n$
such that $\sum_i x_i = x$.
One way to denote this situation is as $[x]$.
One convenient property of this sharing is that we can add
and multiply by a public scalar.
If we have $[x]$, and $[y]$, we can obtain $[x + y]$,
by setting $(x + y)_i \gets x_i + y_i$.
Similarly, from a public $a$ and $[x]$, we can obtain $[a \cdot x]$,
by setting $(a \cdot x)_i \gets a \cdot x_i$.

The end goal of the parties is to generate a signature.
Here's a sketch of how to do this:

- First, generate a secret shared
$[k]$.

This can be done by having each party generate
$k_i$ randomly by themselves.

- Multiply by $G$ to get $[k \cdot G]$, and then have the parties
open this value to learn $K$.

- Next, *somehow* obtain a secret sharing $[k \cdot x]$.

- Finally, calculate shares of $[s] = H(m)[k] + h(K)[kx]$,
which can be opened to yield the last part of the signature.

Now, the hardest part in all of these steps is calculating
$[k \cdot x]$.
While secret shared values can easily be *added*, multiplying
them cannot be done as easily.
In fact, most of the complexity in threshold ECDSA protocols lies precisely
in performing this multiplication.

# Cait-Sith's Approach

The approach cait-sith takes is to try and abstract away this complexity.
The basic idea 

Using triples.

Committed triples help a lot.

# Some Benchmarks

Showing benchmarks.

# Networked Benchmarks

Wrote a tool.

Here are the results.

# Simplified API

What the API is like.

# Potential Improvements

Some things to add:
- Support for more hashes
- Supporting generic curves
