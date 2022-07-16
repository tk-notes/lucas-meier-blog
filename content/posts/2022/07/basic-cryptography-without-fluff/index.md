---
title: "Basic Cryptography Without Fluff"
date: 2022-07-16T11:24:37+02:00
draft: false
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
But, for now, I'll start with basic topics.

# Data Hiding

Data hiding was among many first applications of cryptography;
many trials in our past, but not of much utility today.

A basic illustration of how data hiding works: you hold a dispatch
that you want to forward to a pal, but bad actors might scan
any dispatch you forward; you want to try to fashion a hiding of that dispatch
as it transits.

First, you and your pal fashion a **sigil**, which allows you to
accomplish data hiding.
A **hiding transformation** works with a sigil, and a dispatch,
and forms a **dark dispatch**.
I also call a normal dispatch a **light dispatch**, in contrast
with this dark dispatch.
You can also do a contrary transformation, an **unhiding transformation**,
which works with that sigil, and a dark dispatch, undoing that hiding,
and giving you an original light dispatch.

A hiding sigil and an unhiding sigil must match: no variation at all.
Both participants must know this matching sigil to forward communication.
Without a matching sigil, you cannot obtain any information about a light dispatch from a dark dispatch.
In addition, you cannot obtain information about a sigil from any dark dispatch
it forms.

For sigils must match, I call this form of data hiding:
**similar sigil data hiding** (SSDH).
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
This is why it's known as an _individual_ pad: a sigil can only do hiding
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
What is fascinating is that you can _add_ two points on this arc,
forming a third point on it.
A cubic arc on a ring with no nil divisors, (and a bound on its count),
forms a group!
This group is similar to our multiplication groups from that first approach,
but, in contrast, is tiny, and thus fast to work with too.

# Digital Stamps

DSDH is similar to SSDH.
Both allow you to accomplish a kind of data hiding,
although SSDH has both participants holding matching privy sigils,
but DSDH allows you to do hiding with just a public sigil.

A digital stamp is akin to a mirror of DSDH, and work
with a functionality that's dissimilar from data hiding.
A **digital stamp** allows you to form a stamp on a dispatch,
using a privy sigil.
Anybody can audit this stamp, using your public sigil, and confirm that this stamp
was of your making.
Without your privy sigil, making a stamp which can pass this
confirmation should stay difficult.

As an illustration, look at an approval for a financial transaction.
You don't want bad actors to obtain bogus financial transactions
without your approval.
A digital stamp could allow your bank to confirm that
any transaction found its origin with your approval.

Blockchains such as Bitcoin can with digital stamps
to allow this approval of transactions without having
a root authority such as a bank approving all transactions.

To accomplish digital stamps, you work with similar math
tools as with dissimilar sigil data hiding.
Our RSA approach has an adapation for digital stamps.
Multiplication groups also find an analogous algorithm,
known as **DSA** (digital stamp algorithm).
With an instantiation using _cubic args_, this is known
as **CADSA** (cubic arc digital stamp algorithm).
This is what Bitcoin, and many similar blockchains draw on.

I'm also fond of [Schnorr stamps](https://www.wikiwand.com/en/Schnorr_signature), which work, most commonly, with
cubic arcs too.

# Unknowing Proofs

You might say that **unknowing proofs** form a broad kind of
digital stamp.
Many digital stamp algorithms, such as Schnorr stamps, work
by proving that you know a privy sigil in association
with with a public sigil.
This proof is also bound to a particular dispatch,
allowing it to act as a stamp.
But, this proof holds our privy data, such as our privy sigil,
in hiding.
This ability to hold information in hiding, but still form
proofs with that information, is known as an _unknowing_ ability.

Broadly, an unknowning proof algorithm allows you to form
proofs of arbitrary formulas, involving both public and privy
data.
Any privy data is still in hiding, no privy information
can flow from our proof.

As an illustration, you might show a tax authority
that your salary fits in a particular tax band,
without showing a particular amount.
You would form an unknowing proof using this band,
and your salary, with this salary in hiding.

Unknowing proofs bring a lot of utility for having privacy.
You can do many things without having to drop privacy at all.

Blockchains such as ZCash work with unknowing proofs
to form privy transactions, which can hold information
about transaction amounts and participants in hiding.
Thus, you hold a lot of privacy with
blockchains such as ZCash, particulary in comparison
with blockchains such as Bitcoin, which only allow
public transactions, which show particular amounts and participants.

Any formula is a possibility with unknowing proofs,
so your imagination is your limitation.

# Conclusion

This was just a short post sampling a handful of important
topics in cryptography.
With luck, you found this information of utility,
although I could go into a lot of additional minutia
about any topic in this post.
A month or so ago, my blog had [a book list](/posts/2022/05/some-cryptography-books-i-like/) on cryptography, which you
might also find intriguing.
