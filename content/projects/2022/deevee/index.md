---
title: "Deevee"
date: 2022-06-16
type: project
tech:
  - "Cryptography"
  - "Rust"
withpost: false
description: ""
link: "https://github.com/cronokirby/deevee"
---

An implementation of [Designated Verifier Signatures](https://www.wikiwand.com/en/Designated_verifier_signature).

This is like a normal signature scheme, except that the signer
designates a verifier for each signature.
Only this verifier can validate the signature.
Furthermore, the verifier can forge signatures which designate them.
