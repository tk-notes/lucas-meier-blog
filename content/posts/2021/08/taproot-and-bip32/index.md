---
title: "Taproot Signatures and BIP-32"
date: 2021-08-03T06:50:15+00:00
draft: true
katex: true
tags:
  - "Cryptocurrency"
  - "Cryptography"
  - "Security"
---

How do Bitcoin's new Taproot signatures interact with the good old key derivation
methods from BIP-32? It turns out that the answer isn't all that straightforward.

<!--more-->

# BIP-32, Briefly

A Bitcoin key-pair is composed of a secret scalar $x \in \mathbb{Z} / (q)$, and a public
point $X = x \cdot G$ on the secp256k1 curve. This key-pair can be used for
the ECDSA signature scheme, and is used to sign transactions in the Bitcoin protocol.

[BIP-32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
is a standard for deriving a new Bitcoin key-pair from a pre-existing one.
Given a key-pair $m$, this standard specifies a way to derive a child key-pair $m/i$
for some index $0 \leq i < 2^{32}$.

The essential idea is to hash in the index, along with information about the key,
in order to adjust the current key-pair in a deterministic way.

In more detail,
we first augment key-pairs with a public "chaining key" $c \in \\{0, 1\\}^{256}$,
which is just an extra bit of public randomness. Then, to derive the child key
at index $i$, we first create a 512 bit hash:

$$
H(c, X, i)
$$

The first 256 bits are interpreted as a scalar $k \mod q$, and the last 256 bits
are interpreted as a new chaining key $c'$.

We then adjust our key-pair by adding in $k$:

$$
\begin{aligned}
x' &= x + k\cr
X' &= X + k \cdot G
\end{aligned}
$$

The new key-pair $m/i$ is then $(x', X', c')$.

{{<note>}}
We can also include the secret key $x$ in the Hash, instead of the public key $X$.
This mode is called "hardened child" and is used when $i \geq 2^{31}$.
{{</note>}}

What we need to remember is that you hash in some information, including
the public key, to get a new scalar, which you add to the existing secret key,
adjusting the public key accordingly.

# Taproot Signatures, Briefly

[BIP-340](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki) is one
of the specifications going into Bitcoin's major "Taproot" changes. This
standard replaces the older ECDSA signature scheme with a new Schnorr signature scheme.

What matters for this post is not how signatures are produced, but rather
how keys are represented. Once again, key-pairs are represented as a scalar
$x \in \mathbb{Z}/(q)$, and point $X = x \cdot G$ on the secp256k1 curve.

The crucial difference is in how the point $X$ is encoded.

A point on the curve (in affine coordinates) is represented by two field elements
$x, y \in \mathbb{F}_p$. Each of these elements, alone, occupies a full 32 bytes of space.
But, because of the structure of the curve, if you know the $x$ coordinate of a point,
you only need one bit of information about $y$ to recover the full point.

Bitcoin stores public keys in compressed form. We use 32 bytes for $x$, and
then an extra byte for $y$: we store $2$ if $y$ is even, and $3$ if $y$ is odd.
So, 33 bytes in total.

Taproot, on the other hand, only uses 32 bytes. It does this by only storing
the $x$ coordinate, and assuming that $y$ is even. You can think of a Taproot
public key as a Bitcoin key with an implicit $2$ in front.

This means that there can potentially be a mismatch between our public key
and our private key. If our public key $X = x \cdot G$ has an odd $y$ coordinate,
this means that $-X = -x \cdot G$ will have an even $y$ coordinate. Because
of this, when we generate our private key, we need to potentially negate it,
so that the corresponding public key has an even $y$ coordinate. Otherwise we'd
lose this information, and our secret key wouldn't match our public key.

{{<note>}}
In practice, Taproot does this adjustment *when signing*, and not when generating
keys, but what needs to happen is the same.

Also, the reason this adjustment works is that given a point $P = (x, y)$,
negating $P$ yields $-P = (x, -y)$. Since the order of our field is odd,
negating an even element yields an odd element, and vice-versa.
{{</note>}}

# Ambiguity

Because Taproot stores public keys differently, you have two new sources of
ambiguity when trying to use BIP-32 key derivation:

- How do you hash in a Taproot public key?
- What do you do if the derived public point has an odd $y$ coordinate?

It's also [likely a bad idea](https://lists.linuxfoundation.org/pipermail/bitcoin-dev/2021-February/018381.html)
to use the same key for both Schnorr and ECDSA signatures, so you'd like a way to organize
wallets to avoid mixing up the two key types.

## Workarounds

There are relatively obvious ways to clear up these ambiguities.

For hashing in public keys, you can do things in a "backwards-compatible" way by recovering
the full point from just the $x$ coordinate, implicitly choosing the even $y$ coordinate.
Then you just hash in this full point according to BIP-32. The specification needs
a point on the curve, and you're giving it exactly what it wants.

After deriving a new key, you have:

$$
\begin{aligned}
x' &= x + k\cr
X' &= x' \cdot G = X + k \cdot G
\end{aligned}
$$

If $X'$ has an odd y coordinate, we can use the same trick as for key generation (or signing),
and negate both $x'$ and $X'$ so that the resulting point has an even y coordinate.

For wallet organization, this [mailing list post](https://lists.linuxfoundation.org/pipermail/bitcoin-dev/2019-October/017378.html) proposes a simple adjustment.
Basically, you first organize your wallet into an ECDSA half, and a Schnorr half, and
then do whatever wallet organization you'd normally use from there.
This makes it difficult to accidentally use one type of key for the other type
of signature, because the two domains are separated so early.

# Conclusion

It's certainly possible to use BIP-32 key derivation with Taproot keys, there's just
a few hurdles to clear. Thankfully, the workaround in each case is fairly obvious.
That being said, it would be nice to have a short specification explicitly detailing
these adjustments, so that different wallets can interoperate correctly.
