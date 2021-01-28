---
title: "Thoughts on Big Number APIs"
date: 2021-01-28T11:13:25+01:00
type: note
note-tags:
  - "Math"
  - "Cryptography"
katex: true
---

Big numbers are useful for general purpose applications,
but also necessary for certain cryptographic protocols,
notably **RSA**.

# General Purpose Numbers

For general purpose numbers, you want to be able to represent arbitrary
integers in $\mathbb{Z}$. This is usually done by storing a collection
of unsigned integers, usually $64$ or $32$ bits (called *limbs*)
, along with a sign.

The API allows you to convert normal numbers into this general integer type,
and to perform arithmetic operations without restrictions.

A usual convention is the the representation of integers is *normalized*,
so that there aren't any redundant zeros stored above the most significant
limb of our numbers. Just how the number $0034$ is normalized to $34$ instead.

Many programming languages have such integers, notably
Python, Go {{<ref-link "3">}}, and Haskell {{<ref-link "4">}}

# Constant-Time Numbers

The problem with this implementation is that operations can take
a variable amount of time, depending on the value of some integer.
Since the number of limbs taken up by an integer depends roughly
on its value, and operations can take time varying in the number of limbs,
this can potentially leak information about the value of an integer.

This might leak information about a private key, for example,
which is something you want to avoid.

To address this, the execution time of operations can only depend
on *publicly available information*. In most situations, this is possible
in theory, since the bounds of some secret value are known in advance.
For example, an RSA private key would have a size bounded by
the modulus, which is publicly known.

This has lead to actual problems in practice, see {{<ref-link "5">}},
{{<ref-link "6">}}, and {{<ref-link "7">}}.

## Blinding

Some sensitive operations work by
first generating some random mask, combining it with
a the sensitive integer, performing the operation, and the extracting
out the random mask, somehow. This mask needs to be applied in a way
that's compatible with the operation, and such that the inclusion
of the mask correctly prevents leaking information about the sensitive value.

This is an approach used by BoringSSL {{<ref-link "8">}},
for example, in the `inverse_blinded` function.

### Pros:

- Simple way to patch specific easier timing attacks

### Cons:

- In practice, this will leave blindspots, since many operations are variable time
- This technique is not applicable to all operations
- Requires a source of entropy for many operations, especially if applied more generally
- The masking can cause some operations to randomly fail (see `inverse_blinded`)

## Mixed Time Integers

This approach amends an existing big integer type to support
constant time operation. This can be done by flagging down
the type, enabling constant time algorithms on certain code types,
or by using some kind of capping method.

It's still possible to use this type as a "normal" big integer type,
with variable time operation.

This is one of the main propositions to amend Go's
big numbers to support constant time operation. See
{{<ref-link "1">}}.

### Pros:

- It's possible to amend an existing library to support constant time operation in this way

### Cons:

- It's possible to forget to enable the more secure mode of operation
- The API is easier to misuse given that both modes of operation are possible
- Having branching logic around the mode of operation is prone to errors

## Rigidly Capped Integers

The idea here is that each integer has a "cap" or "announced length",
which should be large enough to contain that integer. This length is usually
in bits, and essentially translates to a number of limbs.

Operations will now be variable based on the announced length, which
should not be sensitive information.

This mode is "rigid", because whenever an operation exceeds the announced
length, some kind of error is issued. This is usually an implementation
error for that cryptographic protocol, since the bounds of operation should
be known in advance.

This is the mode of operation proposed for Go's standard library {{<ref-link "1">}}
(as a concrete mode for mixed time integers).

### Pros:

- Correctness is straightforward
- There's usually an obvious cap to use, such as a modulus

### Cons:

- Care needs to be taken to make sure that overflowing over the cap never happens
- Can't work as a general purpose integer type

## Flexibly Capped Integers

This is the same idea as the last version, except in terms of cap overflow.
The idea is that we can have operations that create a new cap based on the
maximum possible size allowable for that operation. For example,
multiplication of two integers with caps $c_1$ and $c_2$ can yield
an integer of cap $c_1 + c_2$. Taking modular operations with a cap
of $m$ can yield integers with a cap of $m$.

Since all of these caps are public information, having variable
time operation, even creating new caps, so long as they depend only
on the cap size, and not the *value* of any given integer, should
not leak information about any values, only about caps.

This is done in BearSSL {{<ref-link "2">}}.

### Pros:

- Operation is more flexible wrt overflow
- Can work as a general purpose type

### Cons:

- Correctness is more complex compared to the rigid approach
- Either a lot of space is wasted, or specialized operations for things like multiplication are required

## Modular Integers

Integers as proposed thus far can be arbitrarily sized, and negative. In practice,
one often needs big integers not for general purpose computation,
but for arithmetic modulo some $M$.

Instead of having a capacity stored with the integer, you instead
store the big integer $M$ (via a pointer, or some other means).
Then, all operations on these integers are done modulo $M$.

The time for different operations can be variable in the *size*
of the modulus, but not the exact values.

There exists a package providing this
in Dedis's kyber library {{<ref-link "9">}}

### Pros:

- Eliminate complexity required for general purpose operation (negatives, uncapped, etc.)
- Reduce boilerplate for the case of modular arithmetic, which is common

### Cons:

- Can't be used as a general purpose type

# References

{{<ref
  "1"
  "https://github.com/golang/go/issues/20654"
  "[1] math/big: support for constant-time arithmetic">}}

{{<ref
  "2"
  "https://www.bearssl.org/bigint.html"
  "[2] BearSSL - Big Integer Design">}}

{{<ref
  "3"
  "https://golang.org/pkg/math/big/"
  "[3] big - The Go Programming Language">}}

{{<ref
  "4"
  "https://hackage.haskell.org/package/integer-gmp-0.5.1.0/docs/GHC-Integer.html"
  "[4] GHC.Integer">}}

{{<ref
  "5"
  "https://eprint.iacr.org/2016/224.pdf"
  "[5] Yarom, Yuval, Daniel Genkin, and Nadia Heninger. “CacheBleed: A Timing Attack on OpenSSL Constant Time RSA”">}}

{{<ref
  "6"
  "https://www.usenix.org/system/files/sec20-weiser.pdf"
  "[6] Weiser, Samuel, Lukas Bodner, David Schrammel, and Raphael Spreitzer. “Big Numbers – Big Troubles: Systematically Analyzing Nonce Leakage in (EC)DSA Implementations”">}}

{{<ref
  "7"
  "https://github.com/openssl/openssl/issues/6640"
  "[7] OpenSSL - BIGNUM code is not constant-time due to bn_correct_top">}}

{{<ref
  "8"
  "https://boringssl.googlesource.com/boringssl/+/HEAD/include/openssl/bn.h"
  "[8] BoringSSL - include/openssl/bn.h">}}

{{<ref
  "9"
  "https://github.com/dedis/kyber/blob/master/group/mod/int.go"
  "[9] Dedis - Kyber - kyber/int.go">}}
