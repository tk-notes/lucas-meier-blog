---
title: "Cait-Sith Security"
date: 2023-04-16T21:50:00+02:00
type: note
note-tags:
  - "Cait-Sith"
  - "Cryptography"
  - "Protocols"
  - "TSS"
katex: true
---

This serves as an organizing document for the
security analysis of [Cait-Sith](github.com/cronokirby/cait-sith).
This document isn't intended to be a specification
for the protocol itself, rather,
it's intended to analyze the protocol *as specified*
in the Github repo.
The code in the repo implements the specification in the repo,
and this document investigates the security of that specification.

There are 3 main protocols to look at:
- Key Generation (or, key sharing, in its general form)
- Signing (with presignatures)
- Triple generation

Each of these also uses intermediate protocols in their analysis,
which helps simplify the proofs, reusing hard work across the proofs.

Because of this, it's somewhat important to read the documents in order,
since some intermediate protocols will then get used in later documents.

To that effect:

# [Part 0: Preliminaries](/notes/2023/04/cait-sith-security-0-preliminaries)

This document presents some preliminary information on the security
framework being used ([MPS](https://eprint.iacr.org/2023/187),
which you can think of as being like UC security, in essence),
as well as some notational conventions, and how we structure round communication
and aborts.

# [Part 1: Broadcast](/notes/2023/04/cait-sith-security-1-echo-broadcast)

One key intermediate protocol is broadcast, which ensures that a party
sends the same message to all other parties.

# [Part 2: Key Sharing](/notes/2023/04/cait-sith-security-2-key-sharing)

This document looks at the key sharing protocol.
You can think of this as a generalization of key generation to also handle
the cases of refreshing keys, and sharing keys with new parties.

# [Part 3: Triples](/notes/2023/04/cait-sith-security-3-triples)

This document looks at the triple generation protocol.

# [Part 4: Signing](/notes/2023/04/cait-sith-security-4-signing)

This document looks at signing, using presignatures.
