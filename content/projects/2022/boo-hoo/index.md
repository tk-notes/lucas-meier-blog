---
title: "Boo-Hoo"
date: 2022-05-10
type: project
tech:
  - "Cryptography"
  - "Rust"
withpost: false
description: ""
link: "https://github.com/cronokirby/boo-hoo"
---

An implementation of [ZKBoo](https://eprint.iacr.org/2016/163).

This is a library for creating Non-Interactive Zero-Knowledge Proofs
of Knowledge (NIZKPoKs) for arbitary boolean functions.

In other words, given some arbitrary function $f$, and some claimed
output $y$, you can prove that you know an input $x$ such that $f(x) = y$,
without revealing what the secret $x$ is.
