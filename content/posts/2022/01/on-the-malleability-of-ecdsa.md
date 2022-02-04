---
title: "On the Malleability of ECDSA Signatures"
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
this signature using my public key. They do this
by running an algorithm $V(X, m, \sigma)$, which checks if the signature
is valid.
If the signature scheme is secure, it shouldn't be possible for someone to forge
a signature $\sigma'$ such that $V(X, m, \sigma')$ returns true,
unless they know the private key $x$.

That's the usual way these schemes are used.
But, if you just have a message $m$, and a signature $\sigma$, can
you check that the signature was produced by the private
key associated with $X$?

It turns out that for the ECDSA signature scheme (as used in Bitcoin,
Ethereum, and many other places), the signature does not actually
bind to a public key.

In particular, if you have a public key, a message, and a signature,
which are valid together: $(X, m, \sigma)$, you can construct
two different kinds of triples that will also verify, if you swap
out the public key for a different one. 

The first method is to modify the message, giving you a valid:

$$
(X', m', \sigma)
$$

And the second method is to modify the signature, giving you a valid:

$$
(X', m, \sigma')
$$

In both cases, you don't need to know the private key associated with $X'$
to produce these signatures. This can violate many assumptions around
how digital signatures are supposed to work, especially in the context
of cryptocurrencies.

# Recalling the ECDSA Scheme

Before we investigate how to create these faulty signatures,
we need to look at how the ECDSA signature scheme actually works. We'll
be exploiting the inner mechanisms of the scheme, so understanding it is key.

We start with an Elliptic Curve group $\mathbb{G}$, having an associated generator
$G$, and a scalar field $\mathbb{F}_q$.

We also have a hash function $h : \mathbb{G} \to \mathbb{F}_q$, hashing 
points into scalars, as well as a hash function $H : \\{0, 1\\}^* \to \mathbb{F}_q$
hashing messages into scalars.

{{<note>}}
In practice, the hash function $h$ is actually just taking the $x$ coordinate
of an Elliptic Curve point, interpreted as an integer in $\mathbb{Z}$,
and then reducing that value modulo $q$.

"This has made a lot of people very angry and has been widely regarded as a bad move."
{{</note>}}

### Key Generation

A private key is a random scalar $x \in \mathbb{F}_q$, and the public key
is the associated point $X := x \cdot G$.

### Signing

To sign, you first choose a random nonce:

$$
k \xleftarrow{R} \mathbb{F}_q
$$

We then define a "commitment" to this nonce as:

$$
R = \frac{1}{k} \cdot G
$$

At this point, if $h(R) = 0$, we restart the signature process with
a different nonce. This point $R$ makes up the first half of our signature.

The second half of our signature is a scalar, and is computed as follows:

$$
s = k(H(m) + h(R)x)
$$

With $x$ being our private key.

Finally, our complete signature is the pair:

$$
\sigma = (R, s)
$$

### Verification

To verify a signature $\sigma = (R, s)$, we first make sure
that $h(R) \neq 0$ and $s \neq 0$, and then we check the following formula:

$$
s \cdot R \stackrel{?}{=} H(m) \cdot G + h(R) \cdot X
$$

This works out, because the $\frac{1}{k}$ inside $R$ cancels with
the $k$ inside of $s$, and because the public key $X$ is equal to
$x \cdot G$.

{{<note>}}
Often ECDSA is presented in a slightly different, but equivalent way.

Sometimes you'll see $\frac{1}{k}$ used as the nonce instead, with
the commitment being $k \cdot G$. This makes no difference whatsoever,
it's just a matter of notation.

Secondly, schemes will often only transfer the $x$ coordinate of $R$,
because of the specific hash function used, and then rearrange the verification
as:

$$
R \stackrel{?}{=} \frac{1}{s}(H(m) \cdot G + h(R) \cdot X)
$$

This alternate formulation has the advantage of not needing to have a full
point $R$, since you can instead check:

$$
h(R) \stackrel{?}{=} h(\frac{1}{s}(H(m) \cdot G + h(R) \cdot X))
$$

comparing only the $x$ coordinates.

But this makes no difference as to how the scheme really works; it's
just a bit of obfuscation, for our purposes.
{{</note>}}

# Same Signature, Different Message

Now, let's tackle the first kind of malleability. Keeping the same signature,
we'd like to find a new message $m'$ and public key $X'$ that will still
verify with that signature.

Looking at our previous equation, we need to have:

$$
s \cdot R = H(m) \cdot G + h(R) \cdot X
$$

We can rearrange things so that $X$ is on one side of the equation:

$$
X = \frac{1}{h(R)}(s \cdot R - H(m) \cdot G)
$$

Now, everything on that right side is public information. What we've now
done, essentially, is written the public key $X$ as a function $X(\sigma, m)$,
of the signature and the message. I want to insist on the importance of this.
With this function in place, we can choose any signature $\sigma'$ and message
$m'$, and find a public key $X'$ which will validate this message and signature.
At first glance, you might think that this would allow us to recover the public
key that produced a given signature over some message. The problem is that
we don't know *which* message the signature was made over.

In particular, if we plug in a different message $m'$
into this formula, we get a different public key $X'$:

$$
X' = \frac{1}{h(R)}(s \cdot R - H(m') \cdot G)
$$

Note that since we don't know what the discrete logarithm of $R$ is,
we don't know what the discrete logarithm of $X'$ is either. Yet,
we've managed to produce a signature under that public key,
which is the issue here.

# Same Message, Different Signature

We can also reuse this same formulation, but swap out the signature $\sigma$
instead, while
keeping the message fixed:

$$
X' = \frac{1}{h(R')}(s \cdot R' - H(m) \cdot G)
$$

Now, another way to do this is to create your own key-pair, and then sign
the message yourself, producing a new triple $(X', m, \sigma')$. Now,
the difference with our method here is that we don't know the discrete
logarithm of $X'$. If you generated a new key-pair, you would naturally
know the discrete logarithm.

# Some Potential Issues

# How Schnorr Signatures Fix This

# Summary

