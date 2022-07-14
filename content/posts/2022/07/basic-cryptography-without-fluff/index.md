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
any dispatch you forward; you want to try to fashion a hiding of dispatch
as it transits.

First, you and your pal fashion a *sigil*, which allows you to
accomplish data hiding.
A *hiding transformation* works with a sigil, and a dispatch,
and results in a *dark dispatch*.
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

# Dissimilar Sigil Data Hiding

# Digital Stamps

# Unknowing Proofs

You might say that *unknowing proofs* are a broad form of
digital stamps.
