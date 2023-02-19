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
"a game is secure if no adversary can *ever* win".
One issue with this definition is that it's much too restrictive.
The two main ways it's restrictive are that:
- The adversary cannot win, even with a small probability. For example, you could have a game where an adversary has to guess a value in $\\{0, 1\\}^{2000}$, which happens with probability $2^{-2000}$, an absurdly low number, but nonetheless enough to count as winning sometimes.
- The adversary has unbounded computational power, which will inherently break cryptographic assumptions.

The latter basically precludes most cryptography, restricting
us to schemes like the one-time pad, where the key used for encrypting
a message has to be at least as long as the message.

{{<note>}}
Or, more precisely, that the key has at least as much **entropy**
as the message.
{{</note>}}

So, usually, we instead say that a game is secure if
"no *efficient* adversary can win, except with some *small* probability".

The notion of *efficient* computation is not very controversial,
nor hard to define.
What a "small" probability should mean is something we do need
to think about and define.

The most common notion of smallness is that of being *negligible*.
We measure the amount of time computation some algorithm
takes relative to some (security) parameter $\lambda$.
Efficient algorithms should only use $\mathcal{O}(\text{poly}(\lambda))$
worth of computation: i.e. only a polynomial amount.
As $\lambda$ grows, so can the amount of computation.
Some tasks are prohibited by this bound, like trying all values
in the set $\\{0, 1\\}^\lambda$, which would take $2^\lambda$ steps.
Since this is exponential, and not polynomial in $\lambda$.
A negligible amount is sort of like the opposite of this logic:
some function $f(\lambda)$ is negligible if $1 / f(\lambda)$
grows faster than any polynomial function.

The reason this definition is useful is that it behaves well
under composition.
If you some two negligible values together, you'll get a negligible
value, as long as you only do this operation a reasonable
(i.e. $\text{poly}(\lambda)$) number of times.
This is why we use this as our notion of what amount of success
probability can be allowed while still having security:
we can compose a bunch of little schemes together, knowing
that if they're all secure, the end result will be, because
summing up all the negligible amounts of success probability
we might have will still give us a negligible amount.

One common example of a negligible value that shows up
is when trying to guess a value sampled from $\\{0, 1\\}^\lambda$,
which has success probability $2^-\lambda$,
and is thus negligible, since $2^\lambda$ grows faster
than any polynomial in $\lambda$.

Thus, the usual definition of security we end up with is:
"A game is secure if no efficient adversary can win the game except
with negligible probability".

Some games can be shown directly to be secure: 
for example, a game which requires the adversary to
guess some value in a large set.
But, most games require some kind of hardness assumption for their security.

For example, the security of public key encryption will rely
on a hardness assumption about things like factoring, or elliptic curves,
and encryption will need to assume that some kind of block cipher
or PRF is secure.

This kind of "conditional security" is known as a *reduction*.
This is a proof that if some set of games are assumed to be secure,
then some other game is also secure.
For example, you might prove that if a block cipher is secure,
then a larger mode of encryption using that cipher is secure as well.

In fact, most security results are of this second kind.
We can't prove most things to be secure in the abstract,
but only secure relative to some assumptions.

In the classical view we've been talking about so far, cryptography
is mainly about:
- Proving various large cryptographic schemes conditionally secure, ideally with the simplest assumptions possible.
- Developing and analyzing the best attacks against certain assumptions, until reasonable *confidence* is attained that they're not insecure.

There's usually a good consensus about what assumptions are reasonable.
Many assumptions have "stood the test of time", in that while attacks
have improved, their success has satured at a comfortable level.
Sometimes there is disagreement though, and
novel attacks do of course get developed.

Now, here comes my *opinion*, which is that I am personally
less interested in studying and analyzing assumptions than
I am in studying reductions.
Cryptanalysis is a fun and vibrant field of cryptography which---at the moment--I am happy to leave to other people.
You don't need that many assumptions to do a *lot* of cryptography,
it turns out.

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
