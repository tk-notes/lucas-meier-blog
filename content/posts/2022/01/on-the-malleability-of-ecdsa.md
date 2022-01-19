---
title: "On The Malleability of ECDSA Signatures"
date: 2022-01-19T17:49:33+01:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "ECDSA"
  - "Signature"
---

<!--more-->


If I have a public-private keypair $(x, X)$, then I can sign a message
$m$ with private key, producing a signature $\sigma = S(x, m)$. Anyone can verify
this signature using my public key, with an algorithm $V(X, m, \sigma)$.
If the signature algorithm is secure, it's not possible to create
a signature $\sigma'$ such that $V(X, m, \sigma')$ returns true,
unless you know the private key $x$.

But, if you just have a message $m$, and a signature $\sigma$, can
you check that the signature was produced by the private
key associated with $X$?

It turns out that for the ECDSA signature scheme (as used in Bitcoin,
Ethereum, and many other places), the signature does not actually
bind to a public key.

In particular, if you have a public key, a message, and a signature,
which are valid together: $(X, m, \sigma)$, you can construct
two different kinds of triples that will also verify, albeit
with a different public key.

The first method is to modify the message, giving you a valid:

$$
(X', m', \sigma)
$$

And the second method is to modify the signature, giving you a valid:

$$
(X', m', \sigma)
$$

In this second method, you don't need to know the private key associated
with $X'$ either. This might violate quite a few assumptions about
what signatures guarantee, especially in contexts like cryptocurrencies.

