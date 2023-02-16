---
title: "Cryptography without Security"
date: 2023-02-16T10:45:16+01:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Foundations"
---

The typical presentation of theoretical cryptography has one central goal:
defining what it means for cryptographic objects to be "secure".
I think this goal is misguided.

<!--more-->

In theoretical cryptography, you usually start by defining what
"security" should mean, then you go about trying to prove that
various constructions are secure.
Sometimes, you can succeed perfectly, like with the one-time pad,
and other times, you need to rely on assumptions, like with every
other encryption scheme.
Most often you then argue that more complicated schemes
are *secure*, by reducing their security to that of other
schemes, or basic assumptions.

I don't think this focus on *security* is very useful.

My perspective is that theoretical cryptography should instead
focus on *reductions*, in fact, we should even embrace
the use of many cryptographic models, in order to prove
both positive and negative results about reductions.

This reduction-centric perspective defers thinking about
"security" as much as necessary.
In any given model of cryptography, you'll have assumptions
about what things are secure, and then your web of reductions
will let you draw implications from that.
In that way, the reduction perspective subsumes the security
perspective, since we have more information,
and can even consider competing models of what security should mean.

The above paragraphs basically summarize the point I'm trying to make,
but I doubt you'll be convinced by just the few things I've said so far,
so in the rest of this post I'll be elaborating and explaining this
perspective in more detail.

# Cryptography is about Security

The most common perspective.

Cryptography is about security.

You define what it means to be secure from first principles.

Negligible.

This allows for some problems to be hard.

You can argue via reduction.

The issue is that most problems can only be assumed to be hard.

In fact, people may disagree about what assumptions are safe.

# Cryptography is about Reductions

Because security is most often relative to assumptions, although not always,
maybe focus on reductions.

Since reductions are so common, make their syntax easier.

You can even take the perspective that equality matters first and foremost.

I think the reduction perspective is liberating in two ways.

# Some Like Precise Advantages

Reductions allow for precise counting.

You can take a chain of reduction, then count backwards to a desired security level.

This concrete notion of security is what's used more often in practice.

You can also extend the theory with a notion of resource usage,
for a better accounting of effort.

# Some Like Meta-Cryptography

My perspective is more that of meta-cryptography, which is about considering
different cryptographic models at the same time.

There's no one set of correct assumptions.

The focus on reductions allows abstracting away the cryptographic model
you happen to be using.

Think random oracle, generic group, cryptographic models of different varieties
are useful.

I'll have more to write about the advantages of not
sticking to a concrete model, but for now it makes it easier to
prove impossibility results, and other meta-theorems.

# Conclusion

To summarize:
- Theoretical cryptography cares about security
- Security depends mainly on assumptions
- Reductions are thus more important
- Reductions are worth studying even under shifting assumptions
- For concrete security
- But also for meta-cryptography
