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

Addition and subtraction are the fundamental building blocks
for arithmetic in $\mathbb{F}_p$, but we still need to do reduction
modulo $p$ after calling these primitives.

After an addition, our value is at most:

$$
2p - 2
$$

After a single subtraction of $p$, we get $p - 2$, which is in range.
So, to do *modular* addition, we do normal addition, and then
subtract $p$ if necessary. We need to subtract $p$ if our number is
$\geq p$. We can check this by performing the subtraction, and
checking if an underflow happens, by looking at the last borrow.
We can then keep this result if there's no borrow.

One problem is that you can't actually use an if statement to check
this condition and select the right result. This is because
of timing side-channels. Essentially, not only will writing back
the subtraction if necessary take more time, but the branch predictor
itself can be oberved to see which branch was taken. Because of this,
you instead always write down a result, but use bit-masking
to make the selection process completely opaque.

Thankfully, there's a nice crate called
[subtle](https://doc.dalek.rs/subtle/)
which provides basic primitives for constant-time operations,
and I've made heavy use of this crate for implementing arithmetic.

One nice primitive provided by this library is
a `conditional_select` function for vairous types. This allows us
to choose between two alternatives, based on a condition, without
leaking the value of that condition.

This function is already implemented for `u64`, and we can build
off of that implementation for our `Z25519` type as well:

```rust
impl ConditionallySelectable for Z25519 {
    fn conditional_select(a: &Self, b: &Self, choice: Choice) -> Self {
        Z25519 {
            limbs: [
                u64::conditional_select(&a.limbs[0], &b.limbs[0], choice),
                u64::conditional_select(&a.limbs[1], &b.limbs[1], choice),
                u64::conditional_select(&a.limbs[2], &b.limbs[2], choice),
                u64::conditional_select(&a.limbs[3], &b.limbs[3], choice),
            ],
        }
    }
}
```

We can then implement modular addition between $x$ and $y$
by adding them, to get $z = x + y$. Then we calculate $z - m$, and select
between $z$ and $z - m$ based on whether our first addition produced
a carry, and whether our subtraction produced a borrow. The subtraction
is always calculated, and the selection is performed without leaking
the value of this condition. Our modular addition routine is thus
constant-time.

### Folding Large Results

For modular addition and subtraction, our result is small enough
that a conditional addition our subtraction of $p$ is enough to reduce
it.

For larger results, like after a scaling, or a multiplication, this
doesn't suffice. Thankfully, we can use the special structure
of $p$ to reduce this values faster than for a generic modulus.

**Scaling**

One useful operation is multiplying a multi-limb number $x$
by a single-limb factor $\alpha$. Since $x < 2^{255}$, we can write
the result as:

$$
\alpha \cdot x = q \cdot 2^{255} + r
$$

Now, note that since $p = 2^{255} - 19$, we see that
$2^{255} \equiv 19 \mod p$. We can then rewrite our result as:

$$
\alpha \cdot x \equiv 19q + r \mod p
$$

The value $19q$ fits over two limbs, so we can use our standard modular
addition routine.

**Multiplication**

After multiplying two field elements $x$ and $y$, we end up
with a number that fits over 8 limbs, which we can write as:

$$
xy = a \cdot 2^{256} + b
$$

with $a, b < 2^{256}$. Using the structure of $p$, we note that
$2^{256} \equiv 38 \mod p$. This means that our multiplication
is nothing more than:

$$
xy \equiv 38a + b \mod p
$$

We can calculate this result by combining the scaling operation
we defined earlier, and a modular addition.

There are probably faster ways to do multiplication, but I settled
on these simple optimizations.

## x25519

The define the x25519 function, we use our elliptic curve:
$$
C: y^2 = x^3 + 48862x^2 + x
$$
defined over the prime field $\mathbb{F}_p$, with $p = 2^{255} - 19$.
We've already seen a bit of how $\mathbb{F}_p$ is implemented, so
now we can look at how we use this curve.

The x25519 function uses scalar multiplication. This takes
a scalar $s \in \mathbb{Z}$, and a point $P \in C(\mathbb{F}_p)$,
and then computes:

$$
s \cdot P = \sum_{i = 1}^n P
$$

using the point addition formula defined on this curve.

We can use this to create a key exchange system, using ECDH.
We choose a basepoint $G$, and then work in the group $\langle G \rangle$
generated by this point. For x25519, this basepoint is:

$$
(9, \sqrt{39420360})
$$

A private key is a scalar $s \in \mathbb{Z}/q \mathbb{Z}$, where 
$q$ is the order of $G$ (this is $\approx 2^{252}$).
The corresponding public key is the point
$P = s \cdot G$. It is widely believed that recovering $s$ from $P$
is exceedingly difficult. (This problem is the infamous
ECDLP (Elliptic-Curve Discrete Logarithm Problem)).

Two keypair holders with $(s_1, P_1 = s_1 \cdot G)$ and
$(s_2, P_2 = s_2 \cdot G)$ can calculate a shared secret, only
using each other's public key. The first person calculates
$$
s_1 \cdot P_2
$$
and the second calculates
$$
s_2 \cdot P_1
$$

they then both end up with the shared secret:

$$
s_1s_2 \cdot G = s_2s_1 \cdot G
$$

### Implementation

In theory, all we need to implement x25519 is a routine for
scalar multiplication.

The first divergence from this theory is that we only actually need
the $x$ coordinate of our points.

Another interesting quirk is that our scalars are not uniformly
in $[0, q - 1]$. Instead, we generate a random number
in $[0, 2^{256}]$, fitting over 32 bytes, and then use a special clamping
procedure:

```rust
scalar[0] &= 248;
scalar[31] &= 127;
scalar[31] |= 64;
```

This clamping clears the lowest 3 bits, and sets the highest 2 bits
to $01$.

Clearing the lowest 3 bits ensures that our scalar $s$ is a multiple
of $8$. This is because the number of points on the curve
is actually $8 \cdot q$. We say that $8$ is the *cofactor*
for this curve. When we use scalar multiplication with a point
outside of $\langle G \rangle$, having a scalar that's a multiple of
$8$ ensures that the result is $0$, which prevents leaking
information about our scalar.

Setting the top bits ensures that we have a high order point,
and was also designed to mitigate certain implementations varying
in time based on the number of leading zero bits, by making this number fixed.

To implement scalar multiplication, the rough idea is to use
binary exponentiation. Esentially, we do:

```txt
R = Infinity
for b in s.msb..s.lsb:
    R = 2 * R
    if b == 1:
        R = R + P
```

One way of seeing this is correct is by writing
$R = v \cdot P$ at each step, and then seeing how the exponent $v$ is
modified throughout this routine. At the end of the routine, we want
$v = e$. We accomplish this by shifting each bit of $e$ into $v$, from
to bottom. At each iteration, we need to shift $v$ left by one bit,
which means doubling it, and thus doubling $R$ as well.
If the next bit of $e$ is set, then we need to add $1$ to $v$,
and thus add $P$ to $R$. By the end, we have $R = e \cdot P$.

Of course, checking this bit is actually not constant time. It would
also seem like we need a routine for adding any two points, but we
can streamline this quite a bit.

The technique for streamling this scalar multiplication, making
it both fast, and constant-time, is called the Montgomery Ladder,
and I've written some [detailed notes](/notes/2021/04/montgomery-ladder/)
about how it works. I don't think the details are all that interesting
for this post.

# Blake3

After deriving a shared secret, wehn can use this secret to create
a symmetric encryption key. The usual way to do this is to
use some kind of
[KDF](https://www.wikiwand.com/en/Key_derivation_function).
This takes in some random data, and then generates a secret key
using that data. This isn't strictly necessary, but is a good practice.

For this KDF, I went with
[Blake3](https://github.com/BLAKE3-team/BLAKE3), mainly because it was
shiny and new, but also because I liked how it had a unified structure
for different hashing modes, including key derivation.

The neat thing about Blake3 is that it splits the data it needs
to hash into many chunks, and then organizes those chunks into
a tree. This tree structure is great for parallelization,
and is essentially a [Merkle Tree](https://www.wikiwand.com/en/Merkle_tree).

Unfortunately, I didn't actually need to use this neat functionality,
because I only ever use Blake3 as a KDF over a tiny amount of data.
This means that I'll have to actually implement all of Blake3 some
other time, in order to learn how it works!

The basic idea behind Blake3 is pretty simple. You first split your
data into individual blocks of 64 bytes. For each block, you
initialize a state using different values based on the context.
One of these values is used to ensure that this state depends
on the hash of previous blocks, another assigns a different
counter value for different blocks, another distinguishes
between different uses of the hash, etc.
Then, this state is mixed around, guided by the message data inside
of the block. You do several rounds of mixing, each time permuting
the message data as well.

This makes it so that a small change in the initial state,
or a small change in the message, leads to a large difference
in the output state. This ouptut state can then be used as a hash
value.

Key derivation is essentially calculating:

$$
H(\text{ctx} || m)
$$

so that different contexts can derive separate keys, and so that
the key depends on the material we use.

# ChaCha20-Poly1305

## ChaCha20

## Poly1305

# Further Work

# Conclusion
