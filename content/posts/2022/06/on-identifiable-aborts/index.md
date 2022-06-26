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

Now, if this property is *actually* achieved, then it's almost as good
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

When analyzing protocols, you need to use some kind of model of how
participants can interact:
trying to model individual TCP packets isn't going to make proving
things about your protocol very easy. In practice, you abstract
away this communication using a vastly simplified model. The basic
model usually assumes that the communication channels between
peers are *authenticated*, in the sense that you can verify
that messages were actually sent by the peer who claims to have sent it.
Often you'll also implicitly assume that this authentication
is "shareable". If you receive a message from some peer $A$,
then you can convince other peers that $A$ sent you that message.
This can be accomplished in practice by having a public key associated
with each peer, and having each peer sign their messages.

One issue is that the implementation details of this model can actually
matter, and yet they're often swept under the rug. The idea of
signing messages is relatively obvious, to the point that it isn't
explicitly part of the descriptions for protocols. One thorny issue
is how to deal with invalid signatures, which is rarely explicitly
considered when analyzing the abort properties of a protocol. We'll
get to this specific issue later.

One bigger problem is that identifiable aborts are often a tacked
on concern to protocols, and are sometimes poorly specified.
One common pattern is for identifiable aborts to be *possible*
but to require substantial detective work to actually implement.

As an example, let's consider *authenticated broadcast*.
With this primitive, you want one participant to send a message
to the other participants, while guaranteeing that they can't cheat
by sending different messages to this people.
This is an important primitive for many protocols.
Often you need to commit to the random choices you make in a protocol,
and it's important that you're everyone agrees on what you've committed to.
Using an authenticated broadcast makes it so that you can't send different
values to different people.

Often this primitive is used in protocols without actually specifying
an implementation.
A common method to implement it is with an *echo broadcast*.
The idea is that after receiving a value from the broadcaster, each
participant then re-transmits this value to the other participants.
Each participant can then compare the value they initially received
with these new values.
If they see the same values, they know that the initial broadcast
was consistent.

This protocol is also often used in contexts where identifiable
aborts are required.
In principle, this protocol satisfies identifiable abort.
The idea is that if each message is signed, and the broadcaster
sends two different messages, then the parties will eventually
be able to come to a consensus about this fact.
Doing this involves a lot of implicit work that's not really
specified.
For example, each party needs to verify the signatures on all
the messages they've received, otherwise they could be deceived
into believing that the broadcaster cheated, when they in fact didn't
Another important detail is that the messages need to be precisely
bound to that execution of the protocol.
Otherwise, you could use previously signed messages in order to attempt
to make others believe that the broadcaster cheated in this round.
This kind of session binding is a detail which is often omitted
from protocol descriptions, but actually starts to matter if you need
identifiable aborts.

Anyhow, all of this is just to say that in practice providing
identifiable aborts is often more complicated than hinted at in
protocol descriptions.

So, even with an abstracted model, identifiable aborts are complicated.
Nonetheless, identifiable aborts might be worth it, if they can effectively
prevent spurious aborts from happening.

Unfortunately, I don't think this is the case in practice.

# The Network is not Perfect

One fundamental problem is that some aborts can't be attributed
to any particular person.
Aborts which involve network failures can happen spuriously,
and it's not possible to blame any participant in particular
when they happen.
If a participant stops communicating, it might be that their
machine has crashed, or that a network partition has formed
between them and the other participants, or a variety
of other reasons.

One aspect of network failures which stops them from being attributable
is that participants might disagree about the nature of a failure.
For example, if two nodes are separated from the other nodes
because of a network partition, the two nodes might believe the
other nodes have crashed, and conversely, the other nodes think
those two nodes are the ones that have failed.
In general, it's difficult to have immediate consensus over
the state of the network after a failure, and establishing consensus
over what happened isn't trivial either.

Another pernicious aspect of network failures is that they
can be induced.
For example, it's possible to conduct a denial of service (DoS)
attack against a node, which will appear as if that node
has crashed.
Effectively, the resources of that node will be tied
up with the DoS attack, and the node won't be able to respond
to the normal messages of the protocol.
Another possibility would be to attempt a bottleneck
in the network between the nodes.
While it can be possible to do a root cause analysis of
these kinds of attacks, the attributability you get is
much weaker than the Cryptographic identity that identifiable
aborts try to achieve.

Now, these kinds of failures are usually below the
level of abstraction that we use to model communication.
In some sense, they're "out of scope" when considering
identifiable aborts.
Nonetheless, for practical deployment of protocols, network failures
must be considered.

While artificial network failures may not be attributable, they are
a lot *noisier* than the kind of aborts we might cause inside
the protocol.
A DoS attack is a not a silent way to cause an abort, it's
relatively easy to detect that it's happening, even if
the culprit can't be identified.

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
