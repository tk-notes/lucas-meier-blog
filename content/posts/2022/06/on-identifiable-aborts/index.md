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

{{<todo>}}
Explain what Cryptographic protocols are, the threat model, as well
as what identifiable aborts try to achieve.
{{</todo>}}

# Why You Might Want Identifiable Aborts

{{<todo>}}
Explain the consequences of not having aborts, and the economic reason.
{{</todo>}}

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

# Conclusion
