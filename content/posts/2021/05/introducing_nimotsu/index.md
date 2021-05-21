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

Because of the specific structure of this prime number, there
are a few interesting tricks to optimize implementation of modular
arithmetic. I'd recommend having a look at the
[arithmetic.rs](https://github.com/cronokirby/nimotsu/blob/main/src/curve25519/arithmetic.rs)
file if you want all of the details of how the implementation works.

That basic strategy I went with was to represent numbers in
$\mathbb{F}_p$ over 4 limbs of 64 bits:

```rust
pub struct Z25519 {
    pub limbs: [u64; 4],
}
```

This gives us 256 bits in total, which is actually enough to hold
the addition of two elements, since that's at most:

$$
2 \cdot (p - 1) = 2^{256} - 40 < 2^{256}
$$

Another strategy is to use *unsaturated limbs*, of only 51 bits. 5 limbs
of 51 bits each aligns exactly with 255, which is convenient.
There are several reasons to use unsaturated limbs. One of which is not
having to rely on intrinsics like `adc`. Another is making montgomery
multiplication more efficient, by requiring fewer registers.
Ultimately, I went with saturated limbs out of familiarity, and simplicity.
Conversion to 64 bit limbs from bytes is a lot easier, for example.

### Intrinsics

When adding multiple limbs together, we need to add them limb-by-limb,
making sure to propagate the carry produced at each step:

{{<img "4.png">}}

Fortunately, ISAs usually come with a convenient `adc` instruction,
which allows adding two 64 bit numbers together, along with a carry
from a previous step. We can chain multiple `adc`s together to
implement our addition.

To use this intrisc on the `x86_64` isa, we can use feature gating:

```rust
pub fn adc(carry: u8, a: u64, b: u64, out: &mut u64) -> u8 {
    #[cfg(target_arch = "x86_64")]
    {
        unsafe { arch::_addcarry_u64(carry, a, b, out) }
    }
    #[cfg(not(target_arch = "x86_64"))]
    {
        let full_res = u128::from(a) + u128::from(b) + u128::from(carry);
        *out = full_res as u64;
        (full_res >> 64) as u8
    }
```

If this intrinsic isn't available, we fall back on using `u128`.
Using `u128` is slightly slower than using the `adc` instruction directly,
unfortunately.

For subtraction, we need a similar operation, `sbb`. This subtracts
one 64 bit number from another, along with a borrow bit.
We can use feature gating once more to use the intrinsic when available:

```rust
pub fn sbb(borrow: u8, a: u64, b: u64, out: &mut u64) -> u8 {
    #[cfg(target_arch = "x86_64")]
    {
        unsafe { arch::_subborrow_u64(borrow, a, b, out) }
    }
    #[cfg(not(target_arch = "x86_64"))]
    {
        let full_res = i128::from(a) - i128::from(b) - i128::from(borrow);
        *out = full_res as u64;
        u8::from(full_res < 0)
    }
}
```

The availability of `u128` in Rust is also convenient for multiplying
two 64 bit numbers together, to produce a 128 bit number.

### Modular arithmetic and Constant Time Operations

## x25519

# Blake3

# ChaCha20-Poly1305

## ChaCha20

## Poly1305

# Further Work

# Conclusion
