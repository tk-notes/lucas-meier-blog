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

So, what primitives are needed to make this program work? Let's have a closer
look at how the protocol for encrypting data is designed.

A nimotsu keypair is just an
[x25519](https://datatracker.ietf.org/doc/html/rfc7748) keypair.
You generate a public key, that you share with others, and a secret key,
that you keep to yourself:

{{<img "1.png">}}

When someone wants to send you a file, they first generate a new
keypair, and send you the public part. We call this keypair "ephemeral",
because it only gets used this one time. Using your public key,
and the secret key they've just generated, they can use the
x25519 function to derived a shared secret. On your side, you can combine
your secret, and the public key they've just sent you, and derive
the same secret!

{{<img "2.png">}}

The beauty of public key cryptography is that you've managed to derive
a shared secret, while only communicating public data to each other!

From this shared secret, we derive a symmetric key using
[Blake3](https://github.com/BLAKE3-team/BLAKE3). We can then
encrypt and authenticate data using
[ChaCha20-Poly1305](https://tools.ietf.org/html/rfc7539):

{{<img "3.png">}}

On your end, you can derive the same key, and use that to decrypt the
data, and ensure that it wasn't tampered with. And that's about it!

This protocol is pretty simple. Essentially, it's just
[ECIES](https://www.wikiwand.com/en/Integrated_Encryption_Scheme).
The fun part was implementing it, of course.

# Implementation

I decided to implement the application in Rust; I like Rust.
There are plenty of crates I could use for each of these primitives,
but my whole reason to make this application was to have an excuse
to implement all of them from scratch!

I learned quite a bit by implementing these primitives, and hopefully
I can recap some of the interesting aspects of these process for you.

# Curve25519

I initially started this project after getting interested in
Elliptic Curve Cryptography. I learn new concepts best by implementing them,
so I wanted an excuse to implement some ECC myself. Curve25519
is a very popular curve, design to be easy to implement in a constant-time
fashion. Timing attacks being another interest of mine, and already
being a Daniel J. Bernstein fanboy, I had no choice but to use
x25519 (the diffie hellman variant of Curve25519, the actual Elliptic Curve)
for the key exchange component!


{{<note>}}
Unfortunately, I won't be going into the details of how Elliptic Curve
Cryptography works. Hopefully I'll have the time to write an introductory
post about that sometime soon!
{{</note>}}

Curve25519 is an Elliptic Curve (Montgomery, specifically) defined
by the following equation:

$$
y^2 = x^3 + 48862 x^2 + x
$$

over the prime field of numbers modulo
$p = 2^{255} - 19$. Because of the Montgomery shape, there's a neat
constant-time way of doing scalar multiplication of points on the curve,
which is the key operation needed to implement the x25519 function,
as we'll see later.


## Arithmetic Modulo $2^{255} - 19$

## x25519

# Blake3

# ChaCha20-Poly1305

## ChaCha20

## Poly1305

# Further Work

# Conclusion
