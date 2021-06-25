---
title: "Nuntius"
date: 2021-06-25
type: project
tech:
  - "Cryptography"
  - "Go"
withpost: false
description: ""
link: "https://github.com/cronokirby/nuntius"
---

A little CLI tool for E2E encrypted messaging. I implemented
Signal's [X3DH](https://signal.org/docs/specifications/x3dh/)
and [Double Ratchet](https://signal.org/docs/specifications/doubleratchet/)
in a pretty straightforward way. Unlike Signal, this application
is session based instead of asynchronous, out of simplicity.
