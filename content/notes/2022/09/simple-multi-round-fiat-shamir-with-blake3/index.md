---
title: "Simple Multi-Round Fiat-Shamir with BLAKE3"
date: 2022-09-11T22:59:48+02:00
type: note
note-tags:
  - "Cryptography"
  - "ZK Proofs"
katex: true
---

BLAKE3 is a hash function which I like using, because it's simple, fast, and provides convenient modes for key derivation and keyed hashing out of the box.

These properties also make it relatively amenable to use for doing a Fiat-Shamir transformation for multiple round public coin protocols.

For such a transformation, you want to make sure that the transformation is bound to the particular context of a proof, including the proof statement, and other information.

One way to implement the transformation would be to hash the entire transcript so far, including the latest message, and use that to generate your response.

Another way would be to have some stateful object which you could incrementally feed input to, and pull output from.

This is what I'll try to describe in this note.

# A Stateful Hasher

The state will simply be a key $k \in \{0, 1\}^{2\lambda}$, or whatever the size of keys for your keyed hash function is. For $\lambda$ bits of security, you do need a key of size $2 \lambda$ because of collisions.

The state will be initialized from the context $\text{ctx}$, by setting $k \gets \text{KDF}(\text{ctx})$.

Then, at each round, when receiving message $m_i$, we use our keyed XOF $H_k$ to hash $(i, m_i)$, first deriving a new key $k'$, and then whatever randomness we need for that round. We set $k \gets k'$, for the next rounds.

While it may not strictly be necessary to include the round counter $i$, including it certainly doesn't hurt. Also, it's important that the encoding of $m_i$ be unambiguous. In particular, if any of the objects inside of $m_i$ are of variable length, it becomes imperative to separate them in some way, for example by prepending a length before each of them. It can also be a good idea to do domain separation, by including a string (also length prefixed) before each object, indicating what kind of object it is.

So, in essence, the API is that you initialize this object, and then you can incrementally throw more bytes at it in a given round (length prefixed, of course). Then, you can finalize that round, ratcheting the state, and giving you a PRNG you can use to extract out any randomness for that round.

Naturally, since BLAKE3 provides both a KDF, and a keyed XOF, it fits naturally
in this paradigm.

# Alternate Approaches

I should mention [merlin](https://docs.rs/merlin/latest/merlin/), which is instead based on a sponge function, and provides a bit of a different API.
