---
title: "Basic Cryptography Without Fluff"
date: 2022-07-11T07:59:16+02:00
draft: true
katex: true
tags:
  - "Cryptography"
---

Many topics in cryptography on this blog so far, but not many basic topics.
This post is a crack at providing such an approach.
With luck, it should bring utility to unfamiliar folk, but also
grins for folk familiar with this art.

<!--more-->

What is cryptography?
A way of hiding information?
Magic moon math?

I think cryptography is an art of privacy, rigidity, and trust.
It allows hiding information, by folks who want to own this information
without sharing it.
It also allows controlling that information, so that bad actors cannot
do any manipulations of it.
Cryptography also has a capacity for functionality not known prior.
Blockchains, for illustration, saw unfamiliar cryptography, such as unknowing proofs, and thus brought
bright and cool applications to light.
ZCash is a good illustration of this.
But, for now, I'll start with a basic illustration.

# Data Hiding

Data hiding was among many first applications of cryptography;
many trials in our past, but not of much utility today.

A basic illustration of how data hiding works: you hold a dispatch
that you want to forward to a pal, but bad actors might scan
any dispatch you forward; you want to try to fashion a hiding of that dispatch
as it transits.

First, you and your pal fashion a *sigil*, which allows you to
accomplish data hiding.
A *hiding transformation* works with a sigil, and a dispatch,
and forms a *dark dispatch*.
I also call a normal dispatch a *light dispatch*, in contrast
with this dark dispatch.
You can also do a contrary transformation, an *unhiding transformation*,
which works with that sigil, and a dark dispatch, undoing that hiding,
and giving you an original light dispatch.

A hiding sigil and an unhiding sigil must match: no variation at all.
Both participants must know this matching sigil to forward communication.
Without a matching sigil, you cannot obtain any information about a light dispatch from a dark dispatch.
In addition, you cannot obtain information about a sigil from any dark dispatch
it forms.

For sigils must match, I call this form of data hiding:
*similar sigil data hiding* (SSDH).
Most HTTP traffic is in fact HTTPS,
which has this kind of data hiding (that 'S' stands for "sigil").
This form of hiding is ubiquitous, and most common.

An old approach is to hold a sigil as big as your dispatch,
and doing data hiding by xoring that sigil with your dispatch.
This approach is known as **individual pad data hiding**.
A main flaw with this approach is that you must hold a sigil
as big as your dispatch:
$$
(s \oplus d_1) \oplus (s \oplus d_2) = d_1 \oplus d_2
$$
This quantity, $d_1 \oplus d_2$ might hold information you don't want public.
This is why it's known as an *individual* pad: a sigil can only do hiding
for just an individual dispatch.

Nowadays, you might work with algorithms without this flaw.
An algorithm in this class is **ChaCha20**, which works with fast
instructions such as addition, bit shifting, and rotation.
Data hiding with ChaCha20 works bit by bit, which is why you might
call it a **trickling data hiding algorithm**.

# Dissimilar Sigil Data Hiding

With SSDH, you must fashion a matching sigil with all your pals you want
to contact.
For $N$ pals, you must hold $N^2$ sigils.
Mainting this mass of sigils is painful.
What if you could only hold $N$ "sigils", with which you could
contact a pal holding that "sigil"?

This vision is what **dissimilar sigil data hiding (DSDH)** is for.
All participants hold a **public sigil**, and a **privy sigil**.
Your public sigil is known by all, but only you should know
your own privy sigil, and you should hold it in hiding.

You can do data hiding to forward a dispatch to any party, using
only a public sigil for that party.
This forms a dark dispatch, as in similar sigil data hiding.
That party can do a contrary transformation, using a privy sigil,
which must match its public sigil.
Without that privy sigil, you cannot do any unhiding of that
dark dispatch to know its original light dispatch.

In summary, with DSDH, anybody can forward a dispatch
to any participant, using that participant's public sigil, which forms
a dark dispatch.
But, without a privy sigil to do an unhiding of this dispatch, you cannot
know anything about is original dispatch.
No information about privy sigils can flow out of a dark dispatch too,
as in SSDH.

## Accomplishing This

This is hard to do, actually.
An [important work](https://ee.stanford.edu/~hellman/publications/24.pdf) to do this is from 1976, using big multiplication groups in a ring.

An additional approach, which is known as **RSA**, from its original authors' last initials, is to work with a product of two primary factors.
It is not known how to factor this product fast, and this allows you to form
a data hiding function which has a trapdoor, with which you can undo
that hiding.
In this situation, our trapdoor is knowing how to factor this product.
This allows you to fashion a dissimilar sigil data hiding algorithm
in a straightforward way.

Finally, I am fond of **cubic arcs** in particular.
Cubic arcs know an origin in a fascinating location in math, but surprisingly
cryptography has found utility in this location.
Simplifying a bit, a cubic arc is a formula of this kind:

$$
y^2 = x^3 + a x + b
$$

An arc contains all points in a ring which satisfy its formula.
What is fascinating is that you can *add* two points on this arc,
forming a third point on it.
A cubic arc on a ring with no nil divisors, (and a bound on its count),
forms a group!
This group is similar to our multiplication groups from that first approach,
but, in contrast, is tiny, and thus fast to work with too.

# Digital Stamps

# Unknowing Proofs

You might say that *unknowing proofs* form a broad kind of
digital stamps.
