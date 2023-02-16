---
title: "Cryptography with Security"
date: 2023-02-16T10:45:16+01:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Foundations"
---

Theoretical cryptography has as goal to define what security is.

I don't think security matters.

Rather, security should be deferred as much as possible.

I'll try to explain why.

Let's look at some perspectives about what theoretical cryptography is for.

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
