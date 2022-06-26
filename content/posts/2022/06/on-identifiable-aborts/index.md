---
title: "On Identifiable Aborts"
date: 2022-06-26T20:36:18+02:00
draft: false
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

In the rest of this post, I'll explain the notion of identifiable aborts
in more detail, and hopefully convince you, somewhat, of why I think this
notion falls short in practice.

# Background

Let's start with some background on Cryptographic protocols.
The general situation here is that a group of parties need to cooperate
to perform some task.
For example, they might want to run an auction using their bids, or they
might want to use their shares of a private key in order to create a signature.
To do this, they need to use some kind of *protocol* which specifies how
they should interact and what they need to compute in order to complete
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
what kind of failures can happen. In fact, you might have to consider
how *malicious* participants might be able to affect the result.

The basic threat-models for failures in these kinds of protocols are

- **Semi-Honest**: The participants will follow the rules of the protocol,
but may still want to learn the private results, and may also crash.
- **Malicious**: (Often called Byzantine, in distributed systems jargon): The participants can arbitrarily deviate from the protocol.

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
until everyone has spoken, then it's easy to detect malicious participants:
if they try to stall the protocol, it's also evident that they're
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
This kind of penalty is often referred to as **slashing**.

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
and it's important that everyone agrees on what you've committed to.
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
into believing that the broadcaster cheated, when they in fact didn't.
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
Nonetheless, they might be worth it, if they can effectively
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
other nodes have crashed, and conversely, the other nodes might think
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

If you want to achieve identifiable aborts, you need
the messages to be authenticated in some way.
This authentication needs to be Cryptographic,
using some kind of signature scheme which can't be forged,
or something of similar strength.

If messages are not authenticated in this way, than anybody
can send messages on behalf of another participant.
This would allow them to maliciously trigger
an abort which gets blamed on that participant.

In practice, you might try and identify the author of messages
using their network address.
In this case, spoofing a message would require also spoofing
packets.
While this is possible, it's not necessarily trivial either.

Sometimes communication is done via a third party, like a bulletin
board.
I've seen this setup before in the context of threshold signatures,
especially for cryptocurrency bridges.
In this case, you make use of an existing blockchain as
a broadcast mechanism, which also adds a historic
record of the protocol's execution.
In this case, pretending to be someone else is trivial
without signatures; there's no need to even spoof packets.

## Handling Invalid Authentication

Because signatures are necessary for identifiable aborts,
this brings me to my favorite "trick question" when it comes
to thinking about them:

*What do you do with an invalid signature?*

If you continue executing the protocol after seeing an invalid
signature, then you'll be susceptible to all of the issues
we talked about in the previous section.
If signatures aren't checked, it's trivial to pretend to
be someone else.

On the other hand, if you abort the protocol after seeing an invalid
signature, then a malicious participant can cause spurious
aborts by simply sending unsigned messages.
If you try to punish that participant, you run into the fundamental issue
that unsigned messages cannot be attributed.
This is because a message with an invalid signature
can be generated by any participant.
We run into all of the issues from the previous section,
once again.

This issue of how to deal with signature failures is also
usually swept under the rug. 
The model we use for communication assumes authenticated
channels.
This is a fair assumption: we know how to build these.
The issue is that identifiable aborts are built on top of this
assumption, without considering whether identifiability
can be implemented at this lower level of abstraction.

# Conclusion

I've been pretty harsh about identifiable aborts so far,
so perhaps I should caveat some of my statements a bit more.

First, the rationale behind identifiable aborts is sound.
Spurious aborts are a real concern for protocols,
and being able to identify aborts can help apply
incentives to heavily discourage participants from attempting
to cause them.

My criticism is more so that identifiable aborts are poorly
implemented at the theoretical level, and depend on assumptions
about the underlying communication layer which are not necessarily
implementable in an identifiable way.

Nonetheless, spurious aborts at the protocol level are often
much easier to trigger, and much more silent than other kinds
of failures.
Because of this, it can still be desirable to implement "identifable
aborts".

I'd just caution people to not consider them as a panacea.
I'm skeptical of attaching automatic slashing mechanisms
to identifiable aborts, because you might end up with systems
where it's possible to cause honest participants to be slashed,
which is even worse.

In practice, I think that if a Cryptographic protocol starts suffering
from spurious aborts, some kind of human investigation needs
to happen.
Identifiable aborts are useful here, because without some
form of identifiability, it's difficult to attribute fault
to any set of participants.
However, trying to automatically attribute fault might potentially
allow for exploitation of this attribution mechanism,
or might fail to capture the aborts stemming from lower
layers, like authentication or networking.

In summary, identifiable aborts require more work to implement
than their paper specifications, and don't entirely
prevent spurious aborts because of practical violations of
their communication model.
If you're trying to make use of identifiable aborts in practice,
I'd recommend proceeding with great caution.
They're not a panacea, and I don't think it's yet
possible to use them safely automate post-mortem analysis
of protocol failures.
Nonetheless, having identifiable aborts can be necessary
to do any kind of analysis of protocol failures
at all.
