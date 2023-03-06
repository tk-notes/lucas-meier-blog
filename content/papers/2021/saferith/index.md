---
title: "Constant-Time Arithmetic for Safer Cryptography"
date: 2021-09-03
type: paper
author:
  - "Lúcás C. Meier"
  - "Simone Colombo"
  - "Marin Thiercelin"
  - "Bryan Ford"
withpost: false
description: ""
link: "https://eprint.iacr.org/2021/1121"
katex: true
---

The humble integers,  are the backbone of many
cryptosystems.
When bridging the gap from theoretical systems to real-world
implementations, programmers
often look towards general purpose libraries
to implement the arbitrary-precision arithmetic required.
Alas, these libraries are often conceived without cryptography in mind,
leaving applications potentially vulnerable to timing attacks.

To address this, we present saferith, a library providing
safer arbitrary-precision arithmetic for cryptography, through
constant-time operations.
