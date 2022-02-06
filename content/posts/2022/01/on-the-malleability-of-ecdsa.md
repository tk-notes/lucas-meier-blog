---
title: "On the Malleability of ECDSA Signatures"
date: 2022-02-06T12:28:42+01:00
draft: false
katex: true
tags:
  - "Cryptography"
  - "ECDSA"
  - "Signature"
---

The ECDSA signature scheme is quite ubiquitous, used everywhere from TLS
to various cryptocurrencies like Bitcoin. Funnily enough, it turns out that
it suffers from a few *malleability* issues, although I doubt these pose
a serious issue in practice.

<!--more-->


The way signature schemes work is relatively straightforward.
Using my private-public key-pair $(x, X)$, then I can sign a message
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

In both cases, you don't need to know the private key, i.e. the discrete logarithm, associated with $X'$
to produce these signatures. This can violate many assumptions around
how digital signatures are supposed to work, especially in the context
of cryptocurrencies.

# Recalling the ECDSA Scheme

Before we investigate how to forge these new signatures,
we need to look at how the ECDSA signature scheme actually works. We'll
be exploiting the inner mechanisms of the scheme, so understanding it is
important.

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
a different nonce. The point $R$ makes up the first half of our signature.

The second half of our signature is a scalar, computed as:

$$
s = k(H(m) + h(R)x)
$$

with $x$ being our private key.

Finally, our complete signature is the pair:

$$
\sigma = (R, s)
$$

### Verification

To verify a signature $\sigma = (R, s)$, we first make sure
that $h(R) \neq 0$ and $s \neq 0$, and then we check that:

$$
s \cdot R \stackrel{?}{=} H(m) \cdot G + h(R) \cdot X
$$

This works out, because the $\frac{1}{k}$ inside $R$ cancels with
the $k$ inside of $s$, and because the public key $X$ is equal to
$x \cdot G$.

{{<note>}}
Often ECDSA is presented in a slightly different, but equivalent way.

Sometimes you'll see $\frac{1}{k}$ used as the nonce instead, with
the commitment being $k \cdot G$. This makes no difference whatsoever:
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

Now, since everything on that right side is public information, we can actually
calculate $X$. What we've now
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
the message yourself, producing a new triple $(X', m, \sigma')$.
The difference with our method here is that we don't know the discrete
logarithm of $X'$. If you generated a new key-pair, you would naturally
know the discrete logarithm.

# Some Potential Issues

In all honesty, I don't know of any practical examples where this malleability
has caused issues. In fact, I can only think of a few theoretical situations
where this would matter.

Transactions in cryptocurrencies are usually accompanied with a *signature*,
which attests that the author of the transaction. Naturally, to verify a transaction,
you also need to know the author. One potential issue is if the author weren't
included with the transaction, but rather derived using the transaction and the signature.
This would have the problem of not being unique, as we've seen earlier.

In general, you can't uniquely identify transactions by their signatures,
because of all the forms of malleability we've seen so far. That being said,
I doubt that any system would actually end up trying to do this, but you
never know what kind of shortcuts people will end up trying to take.

The second kind of malleability creates a different source of issues:
we can produce a signature for a public key $X'$ without knowing the private
key associated with $X'$. Now, in more intricate protocols, like threshold
signatures, this can create a bit of an issue, but I'm not directly aware
of how the kind of malleability we saw would lead to issues of this kind.
But, once again, you never know what kind of ad-hoc protocols people will
try and create on top of signatures.

# How Schnorr Signatures Fix This (In Theory)

A popular alternative to ECDSA signatures are Schnorr Signatures,
most commonly in the form of [Ed25519 signatures](https://datatracker.ietf.org/doc/html/rfc8032).
You can actually set up this signature scheme so that the signatures are
tied to a specific public key, avoiding this kind of malleability.

I wrote [a post](/posts/2021/07/signatures_from_identification) on Schnorr Signatures
previously, and I'd recommend taking a look at it for more
information, but I'll recap the essence of the scheme here.

To sign a message $m$, you first generate a nonce:

$$
k \xleftarrow{R} \mathbb{F}_q
$$

And then a commitment to that nonce:

$$
K = k \cdot G
$$

Then, you generate a challenge:

$$
e = H(K, m)
$$

and then your response:

$$
s = k + ex
$$

The final signature is $\sigma = (K, s)$.

This signature is verified by checking:

$$
s \cdot G \stackrel{?}{=} K + H(K, m) \cdot X
$$

(Omitting some $\neq 0$ checks which are also needed)

Now, what's interesting with Schnorr signatures is that we can actually
add more elements to the challenge generation. Right now, we just
hash the commitment to the nonce, and we also include the message.
Including the message means that this signature is tied to this
particular message, which is something we definitely want.
If we include our public key:

$$
e = H(K, X, m)
$$

then this ties our signature to our public key as well, preventing the kinds
of malleability we saw earlier.

We can go even further, and add in an additional piece of context:

$$
e = H(K, X, \text{ctx},  m)
$$

For example, we could add in a string identifying our kind of cryptocurrency,
to prevent transactions being used in a different one. This might
actually end up being relevant if different systems end up using
the same transaction format, since you could end up sniffing a transaction
on one system and submitting it to the other. By binding our signatures
to a specific context, we disallow reusing signatures from one context
in a different one.

{{<note>}}
In practice, Schnorr Signatures are by no
means perfect. The popular Ed25519, in particular, requires good attention
to the details of encoding and verification, as elaborated in
{{<ref-link "1">}}.
{{</note>}}

# Summary

To summarize, given a message $m$, and an ECDSA signature $\sigma$, it's possible
to possible to find a public key $X(m, \sigma)$ which allows the signature
for that message to be verified. This means that two kinds of malleability exist.
Given a triple $(X, m, \sigma)$, you can change the public key and message,
keeping the signature fixed:
$$
(X', m', \sigma)
$$
Or, you can change the public key and the signature, keeping the message fixed:
$$
(X', m, \sigma')
$$
In both cases, you do not need to know the discrete logarithm of $X'$,
which can pose some issues, in theory.

This interesting aspect of ECDSA was brought to my attention
by [@dystopiabreaker](https://twitter.com/dystopiabreaker/status/1471418186499117062).
This kind of malleability is also discussed further in
{{<ref-link "2">}}.

{{<ref
  "1"
  "http://www.bolet.org/~pornin/2005-acns-pornin+stern.pdf"
  "[1] Jacqueline Brendel, Cas Cremers, Dennis Jackson, and Mang Zhao. “The Provable Security of Ed25519: Theory and Practice”">}}

{{<ref
  "2"
  "http://www.bolet.org/~pornin/2005-acns-pornin+stern.pdf"
  "[2] Thomas Pornin and Julien P. Starn. “Digital Signatures do Not Guarantee Exclusive Ownership”">}}
