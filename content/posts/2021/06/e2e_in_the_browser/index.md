---
title: "End-to-End Encryption in The Browser"
date: 2021-06-13T16:27:38+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Security"
  - "Web"
---

# The Promise of End-to-End Encryption

# The Problem with the Browser

Why the browser can fundamentally not provide E2E encryption without
server trust. Cryptography can be done in the browser just fine.
You need to trust the server is giving you the right code.

## Targeted Attacks

How attacks can be uniquely targeted.

# Native Apps are Different

Why native apps don't suffer from the same issues as in browser encryption.

## Signing

How signing is used to testify the origin of an app, and
in app stores.

Acknowledge that this is used in the browser as well.

## Releases

How slow releases help verify security.

## No Targeted Attacks

The app store acts as an intermediate verifier.

# Why Provide an E2E Web-App?

Why would you still want to provide an E2E web-app,
knowing these defaults

## Accepting Trust

If you are willing to trust the service more, then this
can work out.

## Reducing Liability

No need to store plaintext data at all.

Maybe mention Australia's encryption bill?

# Some Solutions

Go over some solutions to have e2e encryption

## Use Native Apps

Using native apps can regain these properties

## Local Bundles

Using a local web app.

## Pinned Versions

Some kind of browser based pinning.

# Conclusion
