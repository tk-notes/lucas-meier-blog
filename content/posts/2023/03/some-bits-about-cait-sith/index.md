---
title: "Some Bits about Cait-Sith"
date: 2023-03-09T10:41:25+01:00
draft: false
katex: true
tags:
  - "Cryptography"
  - "MPC"
---

Recently, I've been working on a threshold ECDSA protocol---and its implementation---called [Cait-Sith](https://github.com/cronokirby/cait-sith).
I thought I might write a little blog post to talk about it.

<!--more-->

# Threshold ECDSA

We should probably start this post by talking about _ECDSA_ to begin with.
This is a signature algorithm for elliptic curves.
To abstract a bit, we work with a so-called "cryptographic group"
$\mathbb{G}$, with an associated field of scalars $\mathbb{F}_q$,
and a generator $G$.

A key pair consists of a private scalar $x$,
along with a public point $X = x \cdot G$.

To produce a signature, we run the following algorithm:

$$
\begin{aligned}
&\underline{\text{Sign}(m)}\cr
&\quad m \gets H(m)\cr
&\quad k \xleftarrow{R} \mathbb{F}_q\cr
&\quad K \gets \frac{1}{k} \cdot G\cr
&\quad s \gets k \cdot (H(m) + h(K) \cdot x)\cr
&\quad \texttt{return } (h(K), s)
\end{aligned}
$$

Here $H$ is a hash function producing a scalar from a message,
and $h$ is a function returning the "x coordinate"
of a point $K$.
You can sort of think of this as a hash, but not quite.

And to verify $\sigma = (r, s)$, we check that:

$$
r \overset{?}{=} h \left(\frac{1}{s} \cdot (H(m) \cdot G + r \cdot X) \right)
$$

This works because:

$$
\frac{s}{k} \cdot G = (H(m) + rx) \cdot G = H(m) \cdot G + r \cdot X
$$

Then, dividing both sides by $s$, we get:

$$
K = \frac{1}{k} \cdot G = \frac{1}{s} (H(m) \cdot G + r \cdot X)
$$

Then, taking $h$ on both sides gives us the verification equation.

## The Threshold Setting.

Now, so far we've just described a standard signature scheme.
One person has a private key, and this lets them create signatures.
The public key is, well, public, and can be used by anybody
else to verify these signatures.

In the threshold setting, we want to make it so that no
single person has the private key.
Instead, the key should be shared amongst $n$ parties,
such that any $t$ of them are able to reconstruct the key.
We then want to design a protocol allowing $t$ parties
to sign, _without_ them ever learning with the private key is.

One advantage of doing this is that we gain additional security,
since no single party ever learns the key,
assuming that a sufficient number of parties aren't compromised.

## Abstracting Many Details

Many details of this setting can be abstracted.

Basically, we assume that $n$ parties have shares $x_1, \ldots, x_n$
such that $\sum_i x_i = x$.
One way to denote this situation is as $[x]$.
One convenient property of this sharing is that we can add
and multiply by a public scalar.
If we have $[x]$, and $[y]$, we can obtain $[x + y]$,
by setting $(x + y)_i \gets x_i + y_i$.
Similarly, from a public $a$ and $[x]$, we can obtain $[a \cdot x]$,
by setting $(a \cdot x)_i \gets a \cdot x_i$.

The end goal of the parties is to generate a signature.
Here's a sketch of how to do this:

- First, generate a secret shared
  $[k]$.

This can be done by having each party generate
$k_i$ randomly by themselves.

- Multiply by $G$ to get $[k \cdot G]$, and then have the parties
  open this value to learn $K$.

- Next, _somehow_ obtain a secret sharing $[k \cdot x]$.

- Finally, calculate shares of $[s] = H(m)[k] + h(K)[kx]$,
  which can be opened to yield the last part of the signature.

Now, the hardest part in all of these steps is calculating
$[k \cdot x]$.
While secret shared values can easily be _added_, multiplying
them cannot be done as easily.
In fact, most of the complexity in threshold ECDSA protocols lies precisely
in performing this multiplication.

# Cait-Sith's Approach

The approach cait-sith takes is to try and abstract away this complexity.
The basic idea is that if you perform this hard multiplication setp on a random
pair of values, you can then use that result to more easily
perform a multiplication on values you actually care about.

In more detail, say you've managed to setup sharings
$[a]$, $[b]$, and $[c = ab]$ (we call this a _triple_).

Then, if you want to multiply two real values, $x$, and $y$, you can do the following trick:

1. Open $[x + a]$ and $[y + b]$, making these values public.
2. Then, $(x + a)[y] - (y + b)[a] - [ab] = [xy + ay - ay + ab - ab] = [xy]$

Now, as long as you don't use the same triple more then once,
this process also doesn't leak any information about $x$ or $y$,
since $x + a$ is a uniformly random value, if $a$ isn't known,
and ditto for $b$.

So, the high level ideal is that by generating these triples in advance,
you can move the hard work of the protocol into a _key-independent_
preprocessing phase: i.e., generating a triple doesn't require the private key at all.

Having this preprocessing phase not depend on the key is very useful,
because in many situations you might have multiple keys you want to sign with,
and being able to generate triples in advance, and then consume them
based on which keys are actually used is very interesting.

The high level overview of the protocol is then:

1. (once) Run a distributed key generation protocol (3 rounds)
2. (twice per signature) Generate a pair of triples in advance (relatively expensive)
3. Generate a presignature, with the key, but before knowing the message (1 round)
4. Generate a signature with a presignature and a message (1 round)

Now, the triple generation is the relatively expensive part, but the advantage
is that it's very well isolated from the rest of the protocol.
Right now, I'm using oblivious transfer, which is computationally inexpensive,
but requires a lot of bandwidth (relatively speaking).
One could also consider using *homomorphic encryption* to generate triples
instead, and this wouldn't affect the rest of the protocol, which is nice.

# Some Benchmarks

(Run on an Intel Core i5-4690K CPU)

I set up some benchmarks, and they don't look too shabby.
These just look at the computational cost, on a single thread:

```txt
setup 3
time:   [94.689 ms 95.057 ms 95.449 ms]

triple generation (3, 3)
time:   [36.610 ms 36.682 ms 36.757 ms]

keygen (3,3)
time:   [3.0901 ms 3.1095 ms 3.1297 ms]

presign (3,3)
time:   [2.5531 ms 2.5640 ms 2.5761 ms]

sign (3,3)
time:   [446.79 µs 447.89 µs 449.02 µs]
```

As you can see, the main cost is in triple generation.
(The `setup` protocol is an implementation detail of triple generation,
and needs to be performed only once, before allowing arbitrary triples to be generated)

# Networked Benchmarks

Of course, just looking at the computational cost can be pretty misleading.
For example, the choice of oblivious transfer is about trading off communication
cost for computational cost, and this tradeoff is simply not reflected in
plain benchmarks.

To aid with this, I wrote [a little library](https://github.com/cronokirby/haisou-chan) to simulate network delays, and then applied
that towards benchmarking Cait-Sith.

Here's an example with 3 parties, with 100ms latency between them, and a 10 MB/s outgoing link each.

```txt
> cargo run --release --example network-benches -- 3 100 10000000  

Triple Setup 3 [100 ms, 10000000 B/S]
time:   304.884093ms
up:      10322 B
down:    10322 B

Triple Gen 3 [100 ms, 10000000 B/S]
time:   740.041888ms
up:      106202 B
down:    106202 B

Keygen (3, 3) [100 ms, 10000000 B/S]
time:   207.137969ms
up:      1068 B
down:    1068 B

Presign (3, 3) [100 ms, 10000000 B/S]
time:   104.090877ms
up:      961 B
down:    961 B

Sign (3, 3) [100 ms, 10000000 B/S]
time:   100.606562ms
up:      151 B
down:    151 B
```

The tool reports the time the protocol takes to run, as well as the average
number of bytes sent and received.

You can even try much more strained examples, for example with an even
slower network, and 100 parties:

```txt
> cargo run --release --example network-benches -- 100 300 1000000

Triple Setup 100 [300 ms, 1000000 B/S]
time:   51.269278194s
up:      510843 B
down:    510843 B

Triple Gen 100 [300 ms, 1000000 B/S]
time:   32.959644915s
up:      6765025 B
down:    6765025 B

Keygen (100, 100) [300 ms, 1000000 B/S]
time:   5.871460998s
up:      551527 B
down:    551527 B

Presign (100, 100) [300 ms, 1000000 B/S]
time:   2.891458487s
up:      546835 B
down:    546835 B

Sign (100, 100) [300 ms, 1000000 B/S]
time:   359.795393ms
up:      7859 B
down:    7859 B
```

The main cost is triple generation, at 30s.
But, with 100 parties, and such a bad network, this doesn't seem too bad.
Note that if you're bottlenecked by communication, you can generate
triples in *parallel*, so the throughput a system with many parties
could get is potentially much higher than this benchmark suggests.

# Simplified API

Another goal of the library is to try and provide a simplified API.
One approach to providing an MPC protocol as a library is to have
have various types representing the state of computation at each round,
and then to have explicit transition functions between rounds.
This requires the user of the library to handle the logic for buffering
messages between rounds themselves, as well as manually orchestrating
the protocol's state machines.
Some losses in efficiency also happen with this kind of API, since it becomes
difficult to allow some parts of the computation to "run ahead".

With cait-sith, I tried to simplify the API a lot, and also allowed
for greater interleaving of computation and communication.
One philosophy in designing the protocol was that every message should
be sent *as early* as possible.
As soon as the computation is done for a message, it should be sent out,
so that the cost of waiting for other people to send the message is overlaid
with the cost of computing other values.
This interleaving should help with performance, in theory.

The API can be summed up with the following trait:
```rust
pub trait Protocol {
    type Output;

    fn poke(&mut self) -> Result<Action<Self::Output>, ProtocolError>;
    fn message(&mut self, from: Participant, data: MessageData);
}
```

Given an instance of this trait, which represents a single party participating in a protocol, you can do two things:

- You can provide a new message received from some other party.
- You can "poke" the protocol to see if it has some kind of action it wants you to perform, or if an error happened.

This action is either:

- The protocol telling you it has finished, with a return value of type Output.
- The protocol asking you to send a message to all other parties.
- The protocol asking you to privately send a message to one party.
- The protocol informing you that no more progress can be made until it receives new messages.

In particular, all of the state machine details are internal to the protocol.
All you need to do is provide it with messages you received, without worrying
about what that message contains: `MessageData` is just `Vec<u8>`; raw bytes.
All of the buffering logic for advanced messages is handled internally.
I think this greatly simplifies the use of the library.

# Potential Improvements

While the library is usable, there are still some things I'd like to add:

- First of all, the protocol has not been formally analyzed, so take caution there.

I am working on a security proof at the moment, so stay tuned for news on that front.

In terms of feature-set, the main missing things are:

- Support for more curves.

Right now it's just Secp256k1.

- Support for more hashes.

Right now it's hardcoded to be SHA256, for hashing the message to sign.

{{<note>}}
It's very important that consensus is taken over the message to be signed,
and not the hash.
To that end, the API requires signers to provide the *message*, and not its
hash.
This requires the API to be aware of how to hash messages, naturally.
{{</note>}}

Anyhow, I'm pretty happy with the results of the project so far,
and hope that other people will find it useful.

