---
title: "What I've Been Working On: 2022-W13"
date: 2022-04-03T19:26:11+02:00
draft: false
katex: true
tags:
  - "WIBWO"
---

Secret sharing seed phrases, studying MPC, and Yao's Garbled Circuits.
<!--more-->

# Secret Sharing Seed Phrases

Cryptocurrency wallets can contain many private keys, for all the different
accounts or coins you control (depending on the model that particular
currency uses). Instead of having to store or remember all of these private
keys, it's often more convenient to store a secret *seed*. From the seed,
an entire tree of private keys can be derived.

Remembering 128 random bits is quite difficult though, so a standard
was devised, in order to encode these bits as a *seed phrase*, which looks
like a sequence of random words:

```txt
pool pink tunnel bachelor hundred tackle ribbon leopard lyrics banner rebuild cart
```

Memorizing this is much easier than a long string of random characters.
The advantage of memorizing a seed phrase instead of storing it, is that
it makes it harder for someone to steal it, since they'd need to
get inside your head.

But, this means that you're reliant on your own memory, or have a really
important secret stored without redundancy in a single spot, which
isn't ideal either.

The idea behind *secret sharing* is to split this precious secret
into multiple *shares*. This split is done such that a certain number
of these shares are enough to reconstruct the original secret.
For example, you can split the secret into 3 shares, such that any
2 of them are enough to recover the secret.

Continuing with this example, you could give one share to a friend of yours,
store another share in a secure place, and then memorize the final share.
Your friend can't recover the secret by themselves, since they don't
have access to 2 shares. Someone stealing the share you've stashed away
can't recover it either, for the same reason. Yet, if you were to forget
the share you've memorized, you could use the share your friend has,
and the share you've stored away, in order to recover the secret.

The mathematical tool we use to accomplish this feat is
[Shamir's Secret Sharing](https://www.wikiwand.com/en/Shamir%27s_Secret_Sharing) (or at least, that's the one I ended up going with).

The essence of the scheme is that you work over a finite field $\mathbb{F}$,
and encode the secret as a random polynomial $p$ such that $p(0) = \text{secret}$. The shares are then evaluations $p(1), p(2), \ldots$.

Given enough evaluations of the polynomial, you can reconstruct it, and thus
learn the secret. If you pick a lower degree polynomial, you need fewer
shares in order to reconstruct the polynomial.

Now, there are tools to do this kind of secret sharing already,
for example [ssss](https://linux.die.net/man/1/ssss). The problem
I had with these tools is that their output for shares would be hexadecimal
strings. What I wanted was a tool which would take in a seed phrase,
and spit out shares, which would themselves be seed phrases as well.

This meant that I kind of had to implement the secret sharing myself,
from scratch. And that's how I ended up writing the [seed-split](https://github.com/cronokirby/seed-split) tool, last weekend.

A seed phrase encodes exactly 128 bits, so to avoid any extra words in
the output of the shares, the field I needed to use was
$GF(2^{128})$, a binary field. I also wanted to support long seed phrases,
which have 256 bits, so I need arithmetic in $GF(2^{256})$.

The arithmetic in both of these fields uses binary polynomials, and I
actually managed to abstract a lot of the common functionality between
the fields, to avoid repeating myself too much.

As always when I have to remember how to do binary field arithmetic,
I referred to my copy of "Guide to Elliptic Curve Cryptography", by
Menezes, Hankerson, and Vanstone.

# Studying MPC

I started reading [Pragmatic MPC](https://securecomputation.org/)
this week. I've been enjoying it so far, although I don't have any
particularly profound observations yet. I'm at the part where they
describe various protocols for semi-honest secure general MPC.

Well, maybe I should briefly explain what MPC is. Basically, the idea
is that you have multiple parties with some secret inputs $x_i$, and
they'd like to jointly compute a function on those inputs:

$$
f(x_1, \ldots, x_n)
$$

They'd also like to do this without revealing their secrets $x_i$,
because that would just be no fun.

The classic example is a group of billionaires trying to figure out
which one of them is richest, without revealing their exact amount
of wealth.

My eventual goal is to implement one of the maliciously secure MPC protocols,
along with a high level language for specifying functionalities. Hopefully
the system should be able to run the MPC protocol with decent performance
(compared to other implementations).

# Yao's Garbled Circuits

The first semi-honest MPC protocol presented in this book is Yao's
Garbled Circuits, which was one of the first general purpose MPC protocols
as well.

I found this scheme so clever that I felt that I had to implement
it myself, which [I started doing this weekend](https://github.com/cronokirby/yao-gc). For now, I've gotten a basic language for boolean circuits
up and running, which is very lispy:

```txt
(& (! a0) b0)
```

I use lisp syntax, mainly because it's easier to parse.

I intend to make a nice post explaining how this scheme works once I've
actually finished this project, in a couple of weeks or so.

For now I'm just at the point where I've implementing parsing
the language into a representation of circuits, and I've started
implementing oblivious transfer, using
the [Simplest OT paper](https://eprint.iacr.org/2015/267.pdf).

I actually implemented a variant of that oblivious transfer protocol
last summer, when I implemented
[Doerner's two party ECDSA](https://eprint.iacr.org/2018/499).
They use that oblivious transfer scheme to multiply secret shares.
Funnily enough, this same technique is used, essentially, in some of
the MPC schemes I've been reading about.
