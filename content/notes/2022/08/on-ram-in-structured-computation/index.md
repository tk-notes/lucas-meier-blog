---
title: "On RAM In Structured Computation"
date: 2022-08-24T19:24:12+02:00
type: note
note-tags:
  - "Cryptography"
  - "SNARKs"
  - "ZK Proofs"
katex: true
---

An idea for encoding random access memory in structured computation as in
STARK arithmetization, or more general algebraic automata.

<!--more-->

If you look at the [memory access design in MidenVM](https://maticnetwork.github.io/miden/design/chiplets/memory.html), then you notice
that they have this interesting constraint of needing memory accesses
to have increasing addresses.

This is thus more like "linear access memory" than "random access memory".

There are two interesting aspects of their approach though:

1. Computation is encoded with a VM, which hashes the instructions it processes.
2. For memory accesses, they keep a log of each memory operation.

The log approach for memory allows relatively straight forward compilation
to circuits:

1. With a time-sorted memory log, replace each read to memory with an access in
the private log, and each write with a constraint check to the private log.
2. With an access-sorted memory log, prove that the log is consistent,
in that when the values read come from the latest write, etc.
3. Prove that you know a permutation such that the access-sorted log permutes
to the time-sorted log.

For 3., you can efficiently encode a permutation of a log of size $N$ with $O(N lg N)$ gates,
using a routing network, so this can be done efficiently.

But this yields a circuit, which may not have the structured format
we need for, e.g. STARKs.

To get around this, we encode the computations in step 2. and 3. with
instructions for a VM, like with the MidenVM approach. 

Linking this with step 1. is tricky though.
Basically, we need to make sure that the time-sorted log we work with
in the checking step is the same as was referenced in the "normal" computation.

One way to do this is for the normal computation to *hash* each entry
in the log as it uses them.

Then you also compute this hash when checking log integrity after sorting.

### Summary

To summarize, here's an approach for supporting random access memory in
STARK-like automaton encodings of computation:

0. You work with a VM architecture, like with Miden, or other ZK VMs.
1. In your bytecode, compute the hash of the memory operations you do as
they get processed, creating a hash $H$.
2. Using bytecode without memory operations, compute the following steps:
3. Check that a private log, sorted by address, is consistent (reads use the most recent value written, etc.).
4. Check that a private permutation maps this log to another one whose
hash is $H$.

Checking that permutation in a small amount of space is tricky, but
most likely doable given the ability to reference a private input
by index in your bytecode.