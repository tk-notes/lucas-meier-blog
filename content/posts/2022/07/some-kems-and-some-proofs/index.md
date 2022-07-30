---
title: "Some KEMs and Some Proofs"
date: 2022-07-29T18:01:33+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Foundations"
  - "KEMs"
---

# Defining KEMs

A key encapsulation mechanism (KEM), is a scheme similar to public key encryption.
This scheme consists of three algorithms:

$$
\begin{aligned}
\text{Gen} &: () \to (\bold{SK}, \bold{PK})\cr
\text{Encap} &: \bold{PK} \to (\bold{K}, \bold{C})\cr
\text{Decap} &: (\bold{SK}, \bold{C}) \to \bold{K}
\end{aligned}
$$

The first algorithm, $\text{Gen}$ creates new keypair, consisting
of a private key, and its corresponding public key.

The second algorithm, $\text{Encap}$, takes in a public key,
for the recipient, and results in a symmetric key, and an encapsulation,
or ciphertext.

With $\text{Decap}$, a recipient can use their private key to extract
this symmetric key from the ciphertext.

This scheme also needs to satisfy some notion of correctness.
Intuitively, decapsulating should yield the same key that was encapsulated.

More formally, the following procedure must always succeed:

$$
\begin{aligned}
&(\text{sk}, \text{pk}) \gets \text{Gen}()\cr
&(k_S, c) \gets \text{Encap}(\text{pk})\cr
&k_R \gets \text{Decap}(\text{sk}, c)\cr
&k_S \stackrel{?}{=} k_R
\end{aligned}
$$

## $\text{IND}$ Security

Naturally, having a scheme that's correct is easy if you don't care about
security.
The usual notion of security is similar to that of public key encryption.
The basic idea is that you shouldn't be able to tell whether or not
a specific key is hidden inside of the ciphertext.
In particular, you shouldn't be able to distinguish between receiving
a random key, and receiving the key inside of a ciphertext.

We formalize this as a pair of games, in the state separable style:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{IND}_b$
}\cr
\cr
&(\text{sk}, \text{pk}) \xleftarrow{R} \text{Setup}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } \text{pk} \cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (k_0, c) \gets \text{Encap}(\text{pk}) \cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } (k_b, c) \cr
\end{aligned}
}
$$

In one version of the game, we get the key related to the encapsulation,
and in the other version of the game, we get a completely random key.
An adversary should not be able to distinguish between the two games.

{{<note>}}
In this variant, you can query the challenge multiple times.
This is equivalent to only being able to query once, although the security
gets worse as a linear function of the number of queries you do.
For simplicity, I'll stick with the multi query variants for the rest
of this post.
{{</note>}}

This variant of security is also sometimes referred to as $\text{IND-CPA}$.
This is because the adversary is able to create encapsulations themselves,
by virtue of having the public case.
This kind of query is a "chosen plaintext", hence the "chosen plaintext attack (CPA) in the name.
With symmetric encryption, you need to have a secret key to even
_encrypt_ data, so the $\text{CPA}$ capability is a meaningful distinction.

## $\text{IND-CCA}$ Security

An even stronger variant of security also allows the adversary to make
_decapsulation_ queries on ciphertexts of their choice.
We call these _chosen ciphertext attacks_ (CCA).
This models situations where we might know the keys corresponding
with certain ciphertexts.
While in practice these leakages could be quite limited, having a more
expansive model of security covers many more situations,
and we can construct schemes which satisfy this general model.

We model this with a pair of games, like before:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{IND-CCA}_b$
}\cr
\cr
&\text{seen} \gets \emptyset\cr
&(\text{sk}, \text{pk}) \xleftarrow{R} \text{Setup}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } \text{pk} \cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (k_0, c) \gets \text{Encap}(\text{pk}) \cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \text{seen} \gets \text{seen} \cup \\{c\\}\cr
&\ \texttt{return } (k_b, c) \cr
\cr
&\underline{\mathtt{Decap}(c):}\cr
&\ \texttt{assert } c \notin \text{seen}\cr
&\ \texttt{return } \text{Decap}(\text{sk}, c) \cr
\end{aligned}
}
$$

This is the same as the previous $\text{IND}$ game, except that we now
have the ability to make decapsulation queries.
In order to make the game not trivially easy to win, we keep track
of which challenge ciphertexts have been produced, and refuse decapsulation
queries for that set.

## Equivalence with Other Definitions

{{<note>}}
You can skip this section.
It's mainly concerned with resolving a small discrepancy in definitions
between this post and the broader literature.
{{</note>}}

In the games we've seen so far, the adversary only sees one of the keys.
We can also model a situation where the adversary sees both of the keys:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{IND-Both-CCA}_b$
}\cr
\cr
&\text{seen} \gets \emptyset\cr
&(\text{sk}, \text{pk}) \xleftarrow{R} \text{Setup}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } \text{pk} \cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (k_b, c) \gets \text{Encap}(\text{pk}) \cr
&\ k\_{(1 - b)}  \xleftarrow{R} \bold{K} \cr
&\ \text{seen} \gets \text{seen} \cup \\{c\\}\cr
&\ \texttt{return } (k_0, k_1, c) \cr
\cr
&\underline{\mathtt{Decap}(c):}\cr
&\ \texttt{assert } c \notin \text{seen}\cr
&\ \texttt{return } \text{Decap}(\text{sk}, c) \cr
\end{aligned}
}
$$

One natural question is whether or not this new game is equivalent
to our previous definitions.

It is.

As a bit of a warmup, let's prove this equivalence.

### $\text{IND-Both-CCA} \leq 2 \cdot \text{IND-CCA}$

The idea of this reduction is that we can replace $k_0$ from the first
encapsulation with a random key, because of $\text{IND-CCA}$ security,
thus then allows us to swap $k_0$ with $k_1$, and then walk our way backwards,
giving us a bound for $\epsilon(\text{IND-Both-CCA}_b)$.

We start by extracting out the encapsulation in $\text{IND-Both-CCA}_b$,
using $\text{IND-CCA}$:

$$
\text{IND-Both-CCA}_b =
\begin{aligned}
&\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{W}_b$
}\cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (k_b, c) \gets \texttt{super.Challenge}() \cr
&\ k\_{(1 - b)}  \xleftarrow{R} \bold{K} \cr
&\ \text{seen} \gets \text{seen} \cup \\{c\\}\cr
&\ \texttt{return } (k_0, k_1, c) \cr
\end{aligned}
}\cr
&\otimes 1\\{\texttt{GetPk}, \texttt{Decap}\\}
\end{aligned}
\circ
\text{IND-CCA}_0
$$

Using this, we can already march some of the way forward, giving us:

$$
\text{IND-Both-CCA}_b = W_b \circ \text{IND-CCA}_0
\stackrel{\epsilon_1}{\approx} W_b \circ \text{IND-CCA}_1
$$

Now, one thing we can note is that:
$$
W_0 \circ \text{IND-CCA}_1 = W_1 \circ \text{IND-CCA}_1
$$

To notice this, first expand out the game:

$$
W_b \circ \text{IND-CCA}_1 = 
\begin{aligned}
\boxed{
\begin{aligned}
&\text{seen} \gets \emptyset\cr
&(\text{sk}, \text{pk}) \xleftarrow{R} \text{Setup}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } \text{pk} \cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (\bullet, c) \gets \text{Encap}(\text{pk}) \cr
&\ k_b \xleftarrow{R} \bold{K}\cr
&\ k\_{(1 - b)} \xleftarrow{R} \bold{K}\cr
&\ \text{seen} \gets \text{seen} \cup \\{c\\}\cr
&\ \texttt{return } (k_0, k_1, c) \cr
\cr
&\underline{\mathtt{Decap}(c):}\cr
&\ \texttt{assert } c \notin \text{seen}\cr
&\ \texttt{return } \text{Decap}(\text{sk}, c) \cr
\end{aligned}
}
\end{aligned}
$$

Because both $k_0$ and $k_1$ get sampled at random, it doesn't matter which
order they have.

We can now align these togethers to get:
$$
\begin{aligned}
\text{IND-Both-CCA}_0 &= W_0 \circ \text{IND-CCA}_0\cr
 &\stackrel{\epsilon_1}{\approx} W_0 \circ \text{IND-CCA}_1\cr
 &= W_1 \circ \text{IND-CCA}_1\cr
 &\stackrel{\epsilon_1}{\approx} W_1 \circ \text{IND-CCA}_0\cr
 &= \text{IND-Both-CCA}_1\cr
\cr
\end{aligned}
$$

which gives us our result.

$\square$

Now, for the other direction.

### $\text{IND-Both-CCA} \leq \text{IND-CCA}$

The idea of this proof is that we can emulate $\text{IND-CCA}$
using $\text{IND-Both-CCA}$ by dropping one of the keys.
The question is: which key do we drop?
Because we can't distinguish between $k_0$ and $k_1$, it doesn't matter
which one we choose, essentially.

First, let's define the following wrapper package:

$$
W_b :=
\boxed{
\begin{aligned}
&\underline{\mathtt{Challenge}():}\cr
&\ (k_0, k_1, c) \gets \texttt{super.Challenge}() \cr
&\ \texttt{return } (k_b, c) \cr
\end{aligned}
} \otimes 1\\{\texttt{GetPk}, \texttt{Decap}\\}
$$

This perfectly emulates $\text{IND-CCA}_b$ when composed with
$\text{IND-Both-CCA}_0$:

$$
W_b \circ \text{IND-Both-CCA}_0 = \text{IND-CCA}_b
$$

With $\text{IND-Both-CCA}_1$ on the other hand, the bit is flipped:

$$
W_b \circ \text{IND-Both-CCA}_0 = \text{IND-CCA}\_{(1 - b)}
$$

This is enough for us to write:

$$
\begin{aligned}
\text{IND-CCA}_0 &= W_0 \circ \text{IND-Both-CCA}_0\cr
&\stackrel{\epsilon_1}{\approx} W_0 \circ \text{IND-Both-CCA}_1\cr
&= \text{IND-CCA}_1\cr
\end{aligned}
$$

Which gives us the reduction we sought.

$\square$


### Real-or-Random is Essential


# $\text{IND-CCA}$ via the Fujisaki-Okamoto Transform

# Constructing KEMs

## From RSA

## From Groups

## From Lattices

# Authenticated KEMs

## Generic Construction via Signatures

# Deniable Authenticated KEMs

## Construction from Groups

# Multi-User KEMs

## Generic Constructions

## More Efficient Constructions
