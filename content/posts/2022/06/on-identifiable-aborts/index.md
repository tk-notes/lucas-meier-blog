---
title: "On Identifiable Aborts"
date: 2022-06-04T23:08:22+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Protocols"
---

Many Cryptographic protocols attempt to satisfy a notion of "identifiable
abort", where if a malicious party causes the protocol to prematurely halt,
then they can be detected.
In practice, I think that this notion isn't all that useful.
<!--more-->

The motivation behind identifiable aborts is to strongly disincentivize
causing protocols to abort, because doing so means that you'll be found
out, at least if the protocol actually satisfies identifiable abort.
Even if a protocol can't prevent aborts entirely, the theory is that
identifiable aborts prevents them from happening in practice, because
the consequences are so prohibitive.

I have two major gripes with identifiable aborts:
1. The "identifiable abort" portion of protocols is very often underspecified,
   sometimes requiring a complicated "detective" protocol to attribute aborts
   after they've happened.
2. In practice there are faults, like network partitions, or invalid signatures,
   which are fundamentally not attributable.

Because of these two gripes, I think that achieving identifiable aborts
*in practice* is much more difficult than people think, and cannot be done
in a completely repeatable and automatic way.
A practical identifiable abort scheme requires, in my view, tight integration
with the application making use of the protocol.

In the rest of this post, I'll explain this notion of identifiable aborts
in more detail, and hopefully convince you a bit of why I think that
notion falls short in practice.

# Background

Let's start with some background on Cryptographic protocols.
The general situation here is that a group of parties need to cooperate
to form some task.
For example, they might want to run an auction using their bids, or they
might want to use there shares of a private key in order to create a signature.
To do this, they need to use some kind of *protocol* which specifies how
they should interact and what they need to compute, in order to complete
this task.

There are two categories of properties you want from this kind of protocol:

- **Liveness**: Making something good eventually happen.
- **Safety**: Preventing bad things from happening.

At a basic level, one liveness property we want is that the task gets
completed.
We also want to know that the task has been computed correctly,
which could be considered a safety property.
In *Cryptographic* protocols,
we also often care about *privacy*, making sure that secret values
can be used without being revealed.

When thinking about these properties, you also have to consider
what kind of failures can happen. In fact, might you have to consider
how *malicious* participants might be able to affect the result.

The basic threat-models for failures in these kinds of protocols are

- **Semi-Honest**: The participants will follow the rules of the protocol,
but may still want to learn the private results, and may also crash.
- **Malicious** (Often call Byzantine, in distributed systems jargon): The participants can arbitrarily deviate from the protocol.

# Why You Might Want Identifiable Aborts

One property you might care about is *guaranteed output delivery*.
This means that malicious participants can't prevent others from
learning the result. Preventing others from learning the result by
stopping execution is what we call an **abort**. Now, aborts are
related to *liveness*. An abort doesn't make a protocol return the wrong
result, it just halts the execution of the protocol.

One problem is that if a majority of participants are malicious,
then it's **not possible** to prevent them from causing aborts.
This is [a theorem](https://dl.acm.org/doi/10.1145/12130.12168)
in distributed systems theory, and there's no direct way around it.

Now, while we can't prevent malicious parties from causing aborts,
what we can try do is to make it so that aborts allow us to identify
these parties. As an example, if you have a simple protocol which
consists of everyone announcing their favorite food, and then waiting
until everyone has spoken, then it's easy to detect malicious participants,
since if they try to stall the protocol, it's also evident that they're
not speaking.

This bring us to the notion of **identifiable aborts**, which is a
property that a protocol can satisfy.
If a protocol has identifiable aborts, it means that while it cannot
prevent malicious parties from causing the protocol to halt, it
can make sure that *at least one* of those parties is identified
upon such a malicious abort.

Now, if this property is actually achieved, then it's almost as good
as preventing aborts. This is because causing an abort can now
have a penalty associated with it. Since a malicious participant
causing aborts can be detected, they can also be penalized, by being
removed from the set of participants. You could also require
participants to put up some kind of "security deposit", which would
be taken from them if they were ever caught, thus providing a direct
economic penalty for causing aborts.

On the other hand, with "unidentifiable" aborts, you can cause
harm to the execution of the protocol without any penalty.
This might allow a malicious participant to severely degrade the performance
of any application making use of that protocol; a denial-of-service attack,
essentially.

# Identifiable Aborts are Complicated

{{<todo>}}
Explain the complications of having IA, as well as the example of
echo broadcast.
{{</todo>}}

# The Network is not Perfect

# Identification Requires Authentication

{{<todo>}}
Explain why authentication necessarily requires signatures.
{{</todo>}}

# Handling Invalid Authentication

{{<todo>}}
Explain the dilemma of invalid signatures.
{{</todo>}}

# Context matters

{{<todo>}}
Explain why you need to consider the needs of the application.
{{</todo>}}

# Some Caveats

# Conclusion
