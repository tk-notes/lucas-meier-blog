---
title: "On Adding Chaining to SAFE"
date: 2022-09-15T21:48:33+02:00
type: note
note-tags:
  - "Cryptography"
  - "ZK Proofs"
katex: true
---

The Fiat-Shamir transform is pretty ubiquitous in cryptography,
and there are many choices to make when implementing it in practice.
One very useful toolbox for implementing the FS transform
is [SAFE](https://hackmd.io/bHgsH6mMStCVibM_wYvb2w) (Sponge API for Field Elements), which defines a structured way to use sponge functions
for situations requiring a hash function.

The difference with SAFE is that it's defined using sponge functions
over fields, rather than over bytes.

{{<note>}}
As I write this, it occurrs to me that you could actually reconcile
both field sponges and traditional sponges, like Keccak, by relaxing
the field sponge a bit.

If you instead allow *rings*, rather than only fields, then you can support
traditional sponges defined over bytes, by instead considering
them as ring sponges over the ring $\mathbb{F}_2^8$.
Addition in this ring is the XOR operation, giving you the traditional
way to absorb elements.

(In fact, Keccak 1600 is kind of a ring sponge over $\mathbb{F}_2^{64}$.)
{{</note>}}

The essence of SAFE is that you initialize the state of the sponge
by hashing an "IO pattern", representing how you're going to use
the sponge, as a sequence of absorb and squeeze operations, as well
as a domain string, used for separating different protocols making
use of the same sponge algorithm.

Hashing in the pattern can be used to commit to framing information
about the hash, which is quite useful.

{{<note>}}
I'll note another point of disagreement about SAFE does things here.

SAFE will coalesce adjacent absorb calls together. So `Absorb(1)`
followed by `Absorb(1)` is equivalent to `Absorb(2)`.

In my view, not doing this is better, because it lets you avoid the need
to hash in framing data in addition to data itself.
By treating `Absorb(2)` as distinct, you're preventing issues
like `A(1), A(2), A(1)` colliding with `A(1)` 4 times.
{{</note>}}

More technically, the spec says:

>  Set the permutation state to all zeros and add T to the first 128 bits of the state (with respect to the field’s addition). If field elements are 128-bit or more, T is converted to a field element. Otherwise T
is parsed as two or more field elements.

(T being the result of hashing the IO pattern and domain separation string).

And then:

> Set both absorb and squeeze positions to zero: $\text{absorb\\_pos} = \text{squeeze\\_pos} = 0$.

## Why chaining is useful

Other approaches like [Merlin](https://docs.rs/merlin/latest/merlin/)
have this interesting property of being chainable.
Basically, you write your scheme to take in a mutable "transcript",
and your scheme adds data to the transcript,
and then uses it to generate challenges, which also mutate
the state of the transcript.

You can get a standard fiat shamir by initializing the transcript
with your protocol, and maybe a context, and then passing it to
the scheme.

You can also do fancier things, like making a kind of signature scheme,
by having the transcript include a message before being forwarded along.

You can also chain multiple schemes together.
For example, you could sequentially compose two proof protocols.
You'd do this by using the same running transcript for both.
You'd initialize the transcript, pass it into the first protocol,
and then use the same transcript object for the second protocol.
The second protocol's challenges will be bound to what happened
in the first protocol, because it uses the same transcript.

This chainability is very nice, since it lets you write a scheme
once, and then have it be usable both in a standalone context,
but also be easily bindable to arbitrary contexts, like with
a signature scheme, or even sequentially composable with other schemes.

## Adding chaining to SAFE

One thing you could do would be to add a chaining key or something
to the domain string which gets hashed to initialize the sponge.
One thing I dislike about this is that it makes the initialization
data of a scheme not constant.
What's nice with SAFE in a standalone context is that the hashing
of the IO pattern and domain can be done ahead of time,
and then the initial state of the sponge is then hardcoded.
This is very useful when doing things in the context of an arithmetic
circuit, which is why field sponges are useful.

If you're calculating hashes in a circuit, you want to avoid doing
any extra work, so being able to hardcode the initialization tag T
is great.

My proposal is actually pretty simple.
Rather than completely overwrite the state, simply write the
tag instead.

First, rather than setting the permutation state to all zeros,
a scheme would instead accept an incoming permutation state,
including an absorb position.

Then, it would perform $\text{WRITE}(|T|, T)$, which
is like $\text{ABSORB}$, except that the state is *overwritten*
rather than added to.

Notice that if the initial permutation is all zeroes, with the absorb
position set to $0$, this is equivalent to the initialization scheme
defined in the SAFE spec.

Second, you would always set $\text{squeeze\\_pos} = n - c$,
so that you need to ratchet before reading any output.
IMO, this should be done in the standard spec, and it ends up
happening as soon as you do your first absorb call, which almost
always proceeds your first squeeze call in protocols.

The way chaining would work in this situation is then quite simple.

Each scheme would accept an existing SAFE object rather than creating
its own, but would otherwise not have to know anything else about what
happened previously.
It would know its hardcoded tag T based on its individual IO pattern
and domain separation.
Also, by overwriting the state and forcing a ratchet before being
able to squeeze, you avoid potential collision issues.

This would combine the advantages of SAFE with the chainability of the transcript
approach brought forward by Merlin, which would be very very neat, in
my opinion.