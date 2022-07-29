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
&(\text{sk}, \text{pk}) \xleftarrow{R} \\{0, 1\\}^\lambda\cr
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

## $\text{IND-CPA}$ Security

## $\text{IND-CCA}$ Security

## Equivalence with Left or Right Security

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
