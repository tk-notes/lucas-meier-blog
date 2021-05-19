---
title: "Introducing Nimotsu"
date: 2021-05-11T20:52:56+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Math"
  - "Security"
---

Recently, I've been working on a little encryption tool called
[Nimotsu](https://github.com/cronokirby/nimotsu). I had a lot of fun
implementing the cryptography involved, and thought it would make
for an interesting blog post.

<!--more-->

Please note that I'm not advocating the use of this application over
other alternatives. I made the app to learn how to implement various
primitives, and to have fun. Use a battle-tested application
instead.

# The Application

Nimotsu allows you to encrypt data to someone else, given their public key,
such that only they can decrypt that data. (Of course, since you encrypted
the data in the first place, you would also know what it decrypts to,
but that's beside the point).

To use Nimotsu, you'd first generate a key-pair:

```sh
→ nimotsu generate --out key.txt                                
Public Key:
荷物の公開鍵089F9CFDF80C7EFCCDE2BFAE73677C2567AD3C398AF93B1D505D9D788E4DB078
```

Now, the file `key.txt` contains your private key, and looks like this:

```txt
# Public Key: 荷物の公開鍵089F9CFDF80C7EFCCDE2BFAE73677C2567AD3C398AF93B1D505D9D788E4DB078
荷物の秘密鍵4036684A2C4DC2FC195E0FE53F17A90FA3CFAAF70708F5373E263582200D4027
```

If someone wanted to send you a cute cat picture, they can do
that using your public key:

```sh
→ nimotsu encrypt cat.png --out encrypted.bin
  --recipient 荷物の公開鍵089F9CFDF80C7EFCCDE2BFAE73677C2567AD3C398AF93B1D505D9D788E4DB078
```

They would then send you the file `encrypted.bin`. Since you have your
private key, you (and only you) can decrypt the file:

```sh
→ nimotsu decrypt encrypted.bin --out cat.png --key key.txt
```

And that's about it really! This is a super simple application. The concept
is pretty old-school, and surpassed by various more practical and more
secure programs, but I wanted a simple project to let me implement
some of the primitives involved.

# The Protocol

So, what primitives are needed to make this program work?

# Implementation

# Curve25519

## Arithmetic Modulo $2^{255} - 19$

## x25519

# Blake3

# ChaCha20-Poly1305

## ChaCha20

## Poly1305

# Further Work

# Conclusion
