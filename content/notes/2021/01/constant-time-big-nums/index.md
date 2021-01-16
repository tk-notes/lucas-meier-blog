---
title: "Constant-Time Big Numbers"
date: 2021-01-16T18:04:31+01:00
type: note
note-tags:
  - "Math"
  - "Cryptography"
  - "Programming"
katex: true
---

# Resources

### Go standard library issue

https://github.com/golang/go/issues/20654

I came across this issue before, but should probably read it again.

### Simple High-Level Code for Cryptographic Arithmetic
 
http://adam.chlipala.net/papers/FiatCryptoSP19/FiatCryptoSP19.pdf

Having skimmed this, they use high-level descriptions of code
to generate correct but performant C. The advantages are obvious when
working over different curves, which lead to similar high-level code,
but different low level implementations.
