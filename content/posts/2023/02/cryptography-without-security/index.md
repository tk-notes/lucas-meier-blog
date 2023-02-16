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

# Cryptography is about Security?

Let's start by examining the most common perspective that presentations
of theoretical cryptography share.
I don't actually have any statistics for this, but it is common
across various theoretical tomes like Goldreich's [series of books](https://www.wisdom.weizmann.ac.il/~oded/foc.html),
[Boneh & Shoup](https://toc.cryptobook.us/), etc.

This modern perspective is about trying to define what
it means for various cryptographic schemes to be secure.
You want to have encryption schemes producing ciphertexts
that are hard to decrypt, signature schemes
producing signatures that are hard to forge, and so on.

The goal of theoretical cryptography, is then about:
- how to characterize the desirable properties of cryptographic schemes,
- how to define what it means for those properties to hold.

I think we've developed good tools for the former,
namely the notion of security games, in particular,
in the form of state-separable proofs.

It's the latter that this blog post is about.
Given a security game describing the properties
a scheme should have, you can then define what it means
for that scheme to be *secure*.
This property tries to guarantee that the properties
of the scheme will hold.

This definition arises in a very natural way.

First, you characterize what properties a scheme should have by defining
a *game*, that an *adversary* (some arbitrary algorithm / computer / whatever)
can interact with.
The scheme's properties should be such that the game should hopefully
not be winnable.

For example, a game for signatures could involve an adversary
trying to forge a signature on some message,
winning if they succeed:
a good signature scheme should not allow an adversary to win!

The first definition of secure that arises from this is something like:
"a game is secure if no adversary can win".
You define what it means to be secure from first principles.

Negligible.

This allows for some problems to be hard.

You can argue via reduction.

The issue is that most problems can only be assumed to be hard.

In fact, people may disagree about what assumptions are safe.

# Cryptography is about Reductions!

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
