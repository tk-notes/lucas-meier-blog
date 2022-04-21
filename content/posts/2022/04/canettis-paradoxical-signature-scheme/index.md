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

This approach also extends to the case of protocols,
where multiple players are interacting, instead of
just an adversary interacting with a challenger. In this case,
the random oracle is modelled as a common function that all
the players have access to. Internally, it still works
with the lazy random outputs technique we used here; or
with something equivalent.

# Hash Functions

In practice, we don't have random oracles, nor magic gnomes.
Instead, we have actual hash functions, like BLAKE3, SHA256, etc.
So, the question is: do these functions behave like random oracles?

## Trivial Differences

There's one trivial way in which all of these fail to be random
oracles, which is that we can *pre-compute* their outputs.
For example, the SHA256 of the empty string is:

```txt
e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
```

I can tell that SHA256 is being used, and not a random oracle,
simply by querying the random oracle on the empty string.
Since the random oracle is initialized at the start of the
security game, it won't already have this output baked in. In
all likelihood, I'll see a different outupt for the empty string.

This stems from a more fundamental issue with modelling the security
of hash functions. Because a hash function doesn't have any
dependency on secret inputs, it's possible for an adversary
trying to break that hash function to have a lot of knowledge
already "baked in".

For example, SHA256 necessarily has collisions,
two inputs with the same hash, because
the output space is smaller than the input space; more pigeons
than holes. But, no one knows a collision, nor does anyone
know procedure for efficiently finding one. Nonetheless,
there does *exist* some program which immediately outputs
a collision. And, if you tried to model collision resistance
as a security game, you'd run into this issue that this preminiscent
adversary would win that game, because there's no secret
information involved.

On the other hand, if the hash function were calculated using
a secret key, such that calculating the function without the key were difficult, then the adversary would be forced to actually
interact with the challenger, and find a collision, rather
than being born knowing that collision.

Another related approach is to use a keyed hash function,
but also provide the adversary with the key. We could even
integrate this into our hybrid encryption scheme from the
previous section. The public key would also include
a hashing key to be used with the hash function, when deriving
the encryption key from the share Diffie-Hellman secret.

You can abstract this a bit further, by modelling
a hash function such as SHA256 as a *family* of hash functions.
SHA256 relies on the following block of constant values:

```txt
428a2f98 71374491 b5c0fbcf e9b5dba5
3956c25b 59f111f1 923f82a4 ab1c5ed5
d807aa98 12835b01 243185be 550c7dc3
72be5d74 80deb1fe 9bdc06a7 c19bf174
e49b69c1 efbe4786 0fc19dc6 240ca1cc
2de92c6f 4a7484aa 5cb0a9dc 76f988da
983e5152 a831c66d b00327c8 bf597fc7
c6e00bf3 d5a79147 06ca6351 14292967
27b70a85 2e1b2138 4d2c6dfc 53380d13
650a7354 766a0abb 81c2c92e 92722c85
a2bfe8a1 a81a664b c24b8b70 c76c51a3
d192e819 d6990624 f40e3585 106aa070
19a4c116 1e376c08 2748774c 34b0bcb5
391c0cb3 4ed8aa4a 5b9cca4f 682e6ff3
748f82ee 78a5636f 84c87814 8cc70208
90befffa a4506ceb bef9a3f7 c67178f2
```

"These words represent the first 32
   bits of the fractional parts of the cube roots of the first sixty-
   four prime numbers.",
as per [RFC 6234](https://datatracker.ietf.org/doc/html/rfc6234#section-5.1).

But you could have chosen *different* values for these constants.
So, in some sense, you can think of SHA256 as a family
of hash functions, one for each choice of constants. Then
you'd model security games involving SHA256 as first involving
a public, but random, choice for these parameters, and then
using the version of SHA256 with those parameters.
This would avoid the trivial issue of adversaries magically
knowing collisions, because their knowledge would
depend on this choice of parameters.

## Some Non-Trivial Differences

# Canetti's Paradox

# Lessons?

# Conclusion
