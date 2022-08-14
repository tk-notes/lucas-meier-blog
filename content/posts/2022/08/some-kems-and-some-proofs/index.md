---
title: "Some KEMs and Some Proofs"
date: 2022-08-14T08:00:00+02:00
draft: false
katex: true
tags:
  - "Cryptography"
  - "Foundations"
  - "KEMs"
---

In this post, I'd like to provide a technical introduction to
key encapsulation mechanisms (KEMs), with a focus on proving the
security of various constructions.

<!--more-->

A key encapsulation mechanism is like a public key encryption
scheme, tailored to the specific use case of sending a key to the other
party.
This can be used to establish shared key between two parties.
One of them runs the KEM, getting a shared key and a ciphertext,
and then sends that ciphertext to the other person.
That person can unwrap the ciphertext to recover the shared key.
From that point on, the parties can use that shared key to communicate
privately.

In this way, you can think of KEMs as a generalization of key exchanges,
which don't have to be symmetric.
In fact, you can use a key exchange to construct a KEM, as we'll
see later.

In this post, we'll go over:

- How to define KEMs.
- How to capture the security of KEMs with games.
- How these notions of security compare with other variations.
- How to construct a secure KEM from RSA.
- How to construct a secure KEM using Elliptic Curves.

There are other important topics around KEMs that I'd also like to touch
upon, but this sample is already quite a hefty serving
if you include all of the proofs; those will have to wait for a future post.

# Background

For a high level overview of KEMs, I'd recommend
[Neil Madden's post on the subject](https://neilmadden.blog/2021/01/22/hybrid-encryption-and-the-kem-dem-paradigm/).

This post is more focused on the technical side of things, and on provable
security.
I'll be using [state separable proofs](https://eprint.iacr.org/2018/306)
pervasively throughout this post, so I'd recommend
reading [my post on the subject](/posts/2022/05/state-separable-proofs-for-the-curious-cryptographer/) if you'd
like to get up to speed.

# Defining KEMs

To begin, let's formally define what a KEM is.

A key encapsulation mechanism (KEM), is a scheme similar to public key encryption.
This scheme consists of three algorithms:

$$
\begin{aligned}
\text{Gen} &: () \to (\bold{SK}, \bold{PK})\cr
\text{Encap} &: \bold{PK} \to (\bold{K}, \bold{C})\cr
\text{Decap} &: (\bold{SK}, \bold{C}) \to \bold{K}
\end{aligned}
$$

The first algorithm, $\text{Gen}$, creates a new key pair, consisting
of a private key, and its corresponding public key.

The second algorithm, $\text{Encap}$, takes in a public key,
for the recipient, and returns a symmetric key and an encapsulation
(also called a ciphertext).

With $\text{Decap}$, a recipient can use their private key to extract
a symmetric key from the ciphertext.

This scheme also needs to satisfy some notion of correctness.
Intuitively, decapsulating should return the same key that was encapsulated.

More formally, the following procedure must always succeed:

$$
\begin{aligned}
&(\text{sk}, \text{pk}) \gets \text{Gen}()\cr
&(k_S, c) \gets \text{Encap}(\text{pk})\cr
&k_R \gets \text{Decap}(\text{sk}, c)\cr
&k_S \stackrel{?}{=} k_R
\end{aligned}
$$

No matter what key pair comes out of $\text{Gen}$, encapsulating
and then decapsulating must return the same key.
If this didn't hold, your KEM wouldn't be very useful for establishing
a shared key between two parties, or for public key encryption,
which are two of the main use cases for KEMs.

## $\text{IND}$ Security

Naturally, having a correct scheme is easy if you don't care about
security.
The usual notion of security is similar to that of public key encryption.
The basic idea is that you shouldn't be able to tell whether or not
a specific key is hidden inside of the ciphertext.
In particular, you shouldn't be able to distinguish between receiving
a random key, and receiving the key inside of a ciphertext.

We formalize this as a pair of games, in the state separable style:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{IND}_b$
}\cr
\cr
&(\text{sk}, \text{pk}) \xleftarrow{R} \text{Gen}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } \text{pk} \cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (k_0, c) \gets \text{Encap}(\text{pk}) \cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } (k_b, c) \cr
\end{aligned}
}
$$

In one version of the game, we get the key inside the encapsulation,
and in the other version of the game, we get a completely random key.
An adversary should not be able to distinguish between the two games.

This means that the adversary can't learn any information about the
key from the ciphertext.

{{<note>}}
In this variant, you can query the challenge multiple times.
This is equivalent to only being able to query once, although the security
gets worse as a linear function of the number of queries you do.
For simplicity, I'll stick with the multi-query variants for the rest
of this post.
{{</note>}}

This variant of security can also be referred to as $\text{IND-CPA}$.
This is because the adversary is able to create encapsulations themselves,
because they know the public key.
This kind of query is a _chosen plaintext_, hence the _chosen plaintext attack_ (CPA) in the name.

With symmetric encryption, on the other hand, you need to have a secret key to even
_encrypt_ data, so the $\text{CPA}$ capability is a meaningful distinction.

## $\text{IND-CCA}$ Security

An even stronger variant of security also allows the adversary to make
_decapsulation_ queries on ciphertexts of their choice.
We call these _chosen ciphertext attacks_ (CCA).
This models situations where we might know the keys inside certain ciphertexts.
While in practice these leakages could be quite limited, having a more
expansive model of security covers many more situations,
and we can construct schemes which satisfy this general model.

We model this with a pair of games, like before:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{IND-CCA}_b$
}\cr
\cr
&\text{seen} \gets \emptyset\cr
&(\text{sk}, \text{pk}) \xleftarrow{R} \text{Gen}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } \text{pk} \cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (k_0, c) \gets \text{Encap}(\text{pk}) \cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \text{seen} \gets \text{seen} \cup \\{c\\}\cr
&\ \texttt{return } (k_b, c) \cr
\cr
&\underline{\mathtt{Decap}(c):}\cr
&\ \texttt{assert } c \notin \text{seen}\cr
&\ \texttt{return } \text{Decap}(\text{sk}, c) \cr
\end{aligned}
}
$$

This is the same as the previous $\text{IND}$ game, except that we now
have the ability to make decapsulation queries.
In order to make the game not trivially easy to win, we keep track
of which challenge ciphertexts have been produced, and refuse decapsulation
queries for that set.
Otherwise, you could ask for the decapsulation for a ciphertext you saw
earlier, and compare the result with the key you received before.

## Equivalence with Other Definitions

{{<note>}}
You can skip this section.
It's mainly concerned with resolving a small discrepancy in definitions
between this post and the broader literature.
{{</note>}}

In the games we've seen so far, the adversary only sees one of the keys.
We can also model a situation where the adversary sees both of the keys:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{IND-Both-CCA}_b$
}\cr
\cr
&\text{seen} \gets \emptyset\cr
&(\text{sk}, \text{pk}) \xleftarrow{R} \text{Gen}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } \text{pk} \cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (k_b, c) \gets \text{Encap}(\text{pk}) \cr
&\ k\_{(1 - b)}  \xleftarrow{R} \bold{K} \cr
&\ \text{seen} \gets \text{seen} \cup \\{c\\}\cr
&\ \texttt{return } (k_0, k_1, c) \cr
\cr
&\underline{\mathtt{Decap}(c):}\cr
&\ \texttt{assert } c \notin \text{seen}\cr
&\ \texttt{return } \text{Decap}(\text{sk}, c) \cr
\end{aligned}
}
$$

Instead of giving them one of the keys like before, we give them both,
but we swap their order.
The adversary has to tell which key is which.

One natural question is whether or not this new game is equivalent
to our previous definitions.

It is.

As a bit of a warmup, let's prove this equivalence.

### $\text{IND-Both-CCA} \leq 2 \cdot \text{IND-CCA}$

The idea of this reduction is that we can replace $k_0$ from the first
encapsulation with a random key, because of $\text{IND-CCA}$ security.
This then allows us to swap $k_0$ with $k_1$, and then walk our way backwards,
giving us a bound for $\epsilon(\text{IND-Both-CCA}_b)$.

We start by extracting out the encapsulation in $\text{IND-Both-CCA}_b$,
using $\text{IND-CCA}$:

$$
\text{IND-Both-CCA}_b =
\begin{aligned}
&\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{W}_b$
}\cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (k_b, c) \gets \texttt{super.Challenge}() \cr
&\ k\_{(1 - b)}  \xleftarrow{R} \bold{K} \cr
&\ \text{seen} \gets \text{seen} \cup \\{c\\}\cr
&\ \texttt{return } (k_0, k_1, c) \cr
\end{aligned}
}\cr
&\otimes 1\\{\texttt{GetPk}, \texttt{Decap}\\}
\end{aligned}
\circ
\text{IND-CCA}_0
$$

Using this, we can already march some of the way forward, giving us:

$$
\text{IND-Both-CCA}_b = W_b \circ \text{IND-CCA}_0
\stackrel{\epsilon_1}{\approx} W_b \circ \text{IND-CCA}_1
$$

Now, one thing we can note is that:

$$
W_0 \circ \text{IND-CCA}_1 = W_1 \circ \text{IND-CCA}_1
$$

To notice this, first expand out the game:

$$
W_b \circ \text{IND-CCA}_1 =
\begin{aligned}
\boxed{
\begin{aligned}
&\text{seen} \gets \emptyset\cr
&(\text{sk}, \text{pk}) \xleftarrow{R} \text{Gen}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } \text{pk} \cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (\bullet, c) \gets \text{Encap}(\text{pk}) \cr
&\ k_b \xleftarrow{R} \bold{K}\cr
&\ k\_{(1 - b)} \xleftarrow{R} \bold{K}\cr
&\ \text{seen} \gets \text{seen} \cup \\{c\\}\cr
&\ \texttt{return } (k_0, k_1, c) \cr
\cr
&\underline{\mathtt{Decap}(c):}\cr
&\ \texttt{assert } c \notin \text{seen}\cr
&\ \texttt{return } \text{Decap}(\text{sk}, c) \cr
\end{aligned}
}
\end{aligned}
$$

Because both $k_0$ and $k_1$ get sampled at random, it doesn't matter which
order they have.

We can now align these togethers to get:

$$
\begin{aligned}
\text{IND-Both-CCA}_0 &= W_0 \circ \text{IND-CCA}_0\cr
 &\stackrel{\epsilon_1}{\approx} W_0 \circ \text{IND-CCA}_1\cr
 &= W_1 \circ \text{IND-CCA}_1\cr
 &\stackrel{\epsilon_1}{\approx} W_1 \circ \text{IND-CCA}_0\cr
 &= \text{IND-Both-CCA}_1\cr
\cr
\end{aligned}
$$

which gives us our result.

$\square$

Now, for the other direction.

### $\text{IND-Both-CCA} \leq \text{IND-CCA}$

The idea of this proof is that we can emulate $\text{IND-CCA}$
using $\text{IND-Both-CCA}$ by dropping one of the keys.
The question is: which key do we drop?
Because we can't distinguish between $k_0$ and $k_1$, it doesn't matter
which one we choose, essentially.

First, let's define the following wrapper package:

$$
W_b :=
\boxed{
\begin{aligned}
&\underline{\mathtt{Challenge}():}\cr
&\ (k_0, k_1, c) \gets \texttt{super.Challenge}() \cr
&\ \texttt{return } (k_b, c) \cr
\end{aligned}
} \otimes 1\\{\texttt{GetPk}, \texttt{Decap}\\}
$$

This perfectly emulates $\text{IND-CCA}_b$ when composed with
$\text{IND-Both-CCA}_0$:

$$
W_b \circ \text{IND-Both-CCA}_0 = \text{IND-CCA}_b
$$

With $\text{IND-Both-CCA}_1$ on the other hand, the bit is flipped:

$$
W_b \circ \text{IND-Both-CCA}_1 = \text{IND-CCA}\_{(1 - b)}
$$

This is enough for us to write:

$$
\begin{aligned}
\text{IND-CCA}_0 &= W_0 \circ \text{IND-Both-CCA}_0\cr
&\stackrel{\epsilon_1}{\approx} W_0 \circ \text{IND-Both-CCA}_1\cr
&= \text{IND-CCA}_1\cr
\end{aligned}
$$

Which gives us the reduction we sought.

$\square$

### Real-or-Random is Essential

These definitions turned out to be equivalent, but both
fit into a kind of "real-or-random" paradigm: one of the keys
is randomly selected, while the other is related to the ciphertext.
This is a general kind of paradigm, which can be used to define the security of other schemes as well.
It's useful to stay within this paradigm, since phrasing all of
our security definitions in terms of real-or-random makes it easier
to do reductions.

One alternative paradigm, that gets used for public key encryption,
is "left-or-right".
With this approach, instead of having a random output and a real output,
you instead have two outputs, and you try and distinguish between them.

For the KEM case, this would give the following game:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{IND-LR}_b$
}\cr
\cr
&(\text{sk}, \text{pk}) \xleftarrow{R} \text{Gen}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } \text{pk} \cr
\cr
&\underline{\mathtt{Challenge}():}\cr
&\ (k_0, c_0) \gets \text{Encap}(\text{pk}) \cr
&\ (k_1, c_1) \gets \text{Encap}(\text{pk}) \cr
&\ \texttt{return } (k_0, k_1, c_b) \cr
\end{aligned}
}
$$

Unfortunately, this definition fails to capture a useful notion.
The issue is that we have no guarantee that the key is sufficiently
random, which makes it not usable for encryption later.

As an example, consider the following KEM:

$$
\begin{aligned}
&\underline{\text{Encap}(\text{pk}):}\cr
&\ c \xleftarrow{R} \bold{C} \cr
&\ \texttt{return } (0, c)\cr
\cr
&\underline{\text{Decap}(\text{sk}, c):}\cr
&\ \texttt{return } 0
\end{aligned}
$$

This KEM always returns the same key for encapsulation and decapsulation,
which makes it completely useless to construct a secure public key
encryption scheme.
It does, however, satisfy the $\text{IND-LR}$ definition of security above.
This is because $(0, 0, c_b)$ contains no information about $b$.

# Constructing KEMs

We've now seen a bit of how KEMs work in theory, but we've yet to actually
see an example of making one.
In this section, we go over a few potential constructions of KEMs,
and show that they're secure, under appropriate assumptions.

## From RSA

Let's start with [RSA](<https://www.wikiwand.com/en/RSA_(cryptosystem)>).
There are thousands of good explanations of RSA, so I'll settle for
a bad, but short, explanation.

In RSA, your public key consists of a modulus $N$, and a number $e$.
Your secret key consists of a factorization $p, q$ such that $N = p \cdot q$,
along with a secret exponent $d$, such that

$$
d \cdot e \equiv 1 \mod \varphi(N)
$$

These parameters give us a trapdoor permutation for the set $\mathbb{Z}/(N)$:

$$
\begin{aligned}
F(x) &:= x^e \mod N\cr
F^{-1}(y) &:= y^d \mod N\cr
\end{aligned}
$$

The function $F$ is a permutation, with $F^{-1}$ its inverse.
Computing the inverse should be hard without knowing the secret exponent
$d$, or being able to factor $N$.

We make this security notion precise with the following game:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{RSA}_b$
}\cr
\cr
&(d, (N, e)) \xleftarrow{R} \text{Gen}()\cr
&x \xleftarrow{R} \mathbb{Z}/(N)\cr
&y \gets x^e \mod N\cr
\cr
&\underline{\mathtt{Instance}():}\cr
&\ \texttt{return } (N, e, y) \cr
\cr
&\underline{\mathtt{Guess}(\hat{x}):}\cr
&\ \texttt{return } b = 0 \land \hat{x} = x\cr
\end{aligned}
}
$$

The adversary makes guesses for the pre-image of the permutation,
and it should be difficult for them to succeed in their guess.
If the adversary has a good strategy for guessing, then they'll be able
to figure out what $b$ is.

We can use RSA to create a KEM.
The pre-image $x$ will be our key, and then
$y := x^e \mod N$ is our ciphertext, hiding the key.
The recipient can use their secret to unwrap $x$ from this ciphertext.
Now, because $\mathbb{Z}/(N)$ may not be a very useful key by itself,
we also make use of a hash function $H : \mathbb{Z}/(N) \to \bold{K}$,
so that we can derive a more useful key from $x$.

For example, if we wanted to use our key for encryption with AES,
or ChaCha20, we'd want to use a hash function from integers to 256 bit keys.

More formally, we define the following KEM:

$$
\begin{aligned}
&\underline{\text{Gen}():}\cr
&\ (d, (N, e)) \gets \text{RSA}.\text{Gen}()\cr
&\ \texttt{return } ((N, d), (N, e)) \cr
\cr
&\underline{\text{Encap}((N, e)):}\cr
&\ x \xleftarrow{R} \mathbb{Z}/(N)\cr
&\ y \gets x^e \mod N\cr
&\ \texttt{return } (H(x), y)\cr
\cr
&\underline{\text{Decap}((N, d), y):}\cr
&\ x \gets y^d \mod N\cr
&\ \texttt{return } H(x)\cr
\end{aligned}
$$

Because the RSA function is a permutation, correctness is satisfied.

As for security, this isn't a trivial matter.
First, we'll model our hash function $H$ as a _random oracle_, where
the outputs will be generated perfectly at random, on demand.
Second, rather than considering normal $\text{IND-CCA}$ security,
instead we'll consider $\text{IND-CCA-1}$ security, where the adversary
can only make a single challenge query.
Making $Q$ challenge queries instead of just a single one only decreases
security by a factor of $Q$, as can be shown via a hybrid argument.
([My post on state-separable proofs](/posts/2022/05/state-separable-proofs-for-the-curious-cryptographer/) contains a proof of this in the general case).

### $\text{IND-CCA-1} \leq 2 \cdot \text{RSA}$

First, let's write down the $\text{IND-CCA-1}$ game in this context.

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{IND-CCA-1}_b$
}\cr
\cr
&\text{seen} \gets \emptyset\cr
&((N, d), (N, e)) \xleftarrow{R} \text{Gen}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } (N, e) \cr
\cr
&\underline{(1)\ \mathtt{Challenge}():}\cr
&\ x \xleftarrow{R} \mathbb{Z}/(N)\cr
&\ y \gets x^e \mod N\cr
&\ k_0 \gets \texttt{H}(x)\cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \text{seen} \gets \text{seen} \cup \\{y\\}\cr
&\ \texttt{return } (k_b, y) \cr
\cr
&\underline{\mathtt{Decap}(y):}\cr
&\ \texttt{assert } y \notin \text{seen}\cr
&\ \texttt{return } \mathtt{H}(y^d \mod N) \cr
\cr
&h[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{H}(x)}:\cr
&\ \texttt{if } x \notin h\cr
&\quad\ h[x] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h[x]\cr
\end{aligned}
}
$$

The $(1)$ notation means that $\texttt{Challenge}$ can only be queried
a single time.
We've also replaced the hash function with a lazily initialized table
of random values.
This is why we're doing our proof in the "random oracle model", since
the hash function is being modelled this way.

Next, we follow the same strategy as in Theorem 12.2 of
[Boneh and Shoup](https://toc.cryptobook.us/).

Rather than having our table $h$ be used for the pre-images $x$, instead
we'll setup a table $h'$ for the images $y$.
This will allow us to minimize our use of the secret key $d$ in decapsulation
queries, which makes it easier to extract out the RSA functionality for our
package.

Since we only have a single challenge query, we can generate the instance
before $\texttt{Challenge}$ is called.
We'll also be able to do a lot of tricks with the decapsulation
queries, but it's easier to explain those after seeing the new game:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^0_b$
}\cr
\cr
&((N, d), (N, e)) \xleftarrow{R} \text{Gen}()\cr
&\ \hat{x} \xleftarrow{R} \mathbb{Z}/(N)\cr
&\ \hat{y} \gets \hat{x}^e \mod N\cr
&\ k_0 \xleftarrow{R} \bold{K}\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } (N, e) \cr
\cr
&\underline{(1)\ \mathtt{Challenge}():}\cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } (k_b, \hat{y}) \cr
\cr
&h[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Decap}(y):}\cr
&\ \texttt{assert } y \neq \hat{y}\cr
&\ \texttt{if } y \notin h\cr
&\quad\ h[y] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h[y] \cr
\cr
&\underline{\mathtt{H}(x)}:\cr
&\ \texttt{if } x = \hat{x}\cr
&\quad\ \texttt{return } k_0\cr
&\ y \gets x^e \mod N\cr
&\ \texttt{if } y \notin h\cr
&\quad\ h[y] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h[y]\cr
\end{aligned}
}
$$

The big trick we've pulled in this game is in our decapsulation function.
By having a hash table for the images $y$, we avoid the need to make use
of the secret key $d$ when decapsulating.
Whereas we'd normally compute $x \gets y^d \mod N$, and then query
$\texttt{H}(x)$, because our hash function then computes
$x^e \mod N$ to index into our table, we can avoid all of that,
and use the table ourselves.

Naturally, we have $\text{IND-CCA-1}_b = \Gamma^0_b$.

From this point on, it's relatively straight sailing.

We start by extracting out the RSA functionality:

$$
\Gamma^0_b =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^1_b$
}\cr
\cr
&(N, e, \hat{y}) \gets \texttt{Instance}()\cr
&\ k_0 \xleftarrow{R} \bold{K}\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } (N, e) \cr
\cr
&\underline{(1)\ \mathtt{Challenge}():}\cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } (k_b, \hat{y}) \cr
\cr
&h[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Decap}(y):}\cr
&\ \texttt{assert } y \neq \hat{y}\cr
&\ \texttt{if } y \notin h\cr
&\quad\ h[y] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h[y] \cr
\cr
&\underline{\mathtt{H}(x)}:\cr
&\ \texttt{if } \texttt{Guess}(x)\cr
&\quad\ \texttt{return } k_0\cr
&\ y \gets x^e \mod N\cr
&\ \texttt{if } y \notin h\cr
&\quad\ h[y] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h[y]\cr
\end{aligned}
}
\circ \text{RSA}_0
$$

Now we use the $\text{RSA}_0$ game to determine if we need to return $k_0$
in our hash function.
Next, note that if we switch to using $\text{RSA}_1$, we get the following game:

$$
\Gamma^1_b \circ \text{RSA}_1 =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^2_b$
}\cr
\cr
&(N, e, \hat{y}) \gets \texttt{Instance}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } (N, e) \cr
\cr
&\underline{(1)\ \mathtt{Challenge}():}\cr
&\ k_0 \xleftarrow{R} \bold{K}\cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } (k_b, \hat{y}) \cr
\cr
&h[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Decap}(y):}\cr
&\ \texttt{assert } y \neq \hat{y}\cr
&\ \texttt{if } y \notin h\cr
&\quad\ h[y] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h[y] \cr
\cr
&\underline{\mathtt{H}(x)}:\cr
&\ y \gets x^e \mod N\cr
&\ \texttt{if } y \notin h\cr
&\quad\ h[y] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h[y]\cr
\end{aligned}
}
$$

And in this game, the difference between $k_0$ and $k_1$ is just a matter
of naming.
Because of this, $\Gamma^2_0 = \Gamma^2_1$.

We can now tie everything together, to get:

$$
\begin{aligned}
\text{IND-CCA-1}_0 &= \Gamma^0_0\cr
&= \Gamma^1_0 \circ \text{RSA}_0\cr
&\stackrel{\epsilon_1}{\approx} \Gamma^1_0 \circ \text{RSA}_1\cr
&= \Gamma^2_0\cr
&= \Gamma^2_1\cr
&= \Gamma^1_1 \circ \text{RSA}_1\cr
&\stackrel{\epsilon_1}{\approx} \Gamma^1_1 \circ \text{RSA}_0\cr
&= \Gamma^0_1\cr
&= \text{IND-CCA-1}_1\cr
\end{aligned}
$$

And this yields our result.

$\square$

## From Groups

Another way to construct a KEM is with a cryptographic group, like
a suitable elliptic curve.

This is a group $\mathbb{G}$ of prime order $q$, generated by $G$,
and with an associated field of scalars $\mathbb{F}_q$.

A key pair for the KEM will be a scalar $a$, for the private key,
and a point ${A := a \cdot G}$, for the public key.
In order to encapsulate a key, we generate a random scalar $b$,
set ${B := b \cdot G}$ as our encapsulation, and $H(b \cdot A)$ as the key.
The receiver can set $H(a \cdot B)$ as their key, arriving at the same
result because of commutativity:

$$
a \cdot B = a \cdot b \cdot G = b \cdot a \cdot G = b \cdot A
$$

More formally, given a cryptographic group $\mathbb{G}$,
and a hash function $H : \mathbb{G}^2 \to \bold{K}$, we define the KEM as follows:

$$
\begin{aligned}
&\underline{\text{Gen}():}\cr
&\ a \xleftarrow{R} \mathbb{F}_q\cr
&\ A \gets a \cdot G\cr
&\ \texttt{return } (a, A) \cr
\cr
&\underline{\text{Encap}(A):}\cr
&\ b \xleftarrow{R} \mathbb{F}_q\cr
&\ B \gets b \cdot G\cr
&\ C \gets b \cdot A\cr
&\ \texttt{return } (H(B, C), B)\cr
\cr
&\underline{\text{Decap}(a, B):}\cr
&\ C \gets a \cdot B\cr
&\ \texttt{return } H(B, C)\cr
\end{aligned}
$$

One slight difference from the brief presentation above is that we hash
both $B$ and $C$, rather than just $C$.
This makes the security proof easier.

To characterize the security of the KEM, we need to have some kind notion
of security for the group itself.
The notion we'll be relying on (at least, in our proof), is the
"interactive computational Diffie-Hellman" problem.
This states, essentially, that given two points $A = a \cdot G$ and
$B = b \cdot G$, it should be hard to find $C = ab \cdot G$, even given
access to an oracle which tells you whether or not two points $\hat{B}$
and $\hat{C}$ satisfy the relation $a \cdot \hat{B} = \hat{C}$.

We capture this notion through a guessing game:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{ICDH}_b$
}\cr
\cr
&a, b \xleftarrow{R} \text{Gen}()\cr
&A \gets a \cdot G\cr
&B \gets b \cdot G\cr
&C \gets a \cdot B\cr
\cr
&\underline{\mathtt{Instance}():}\cr
&\ \texttt{return } (A, B) \cr
\cr
&\underline{\mathtt{Query}(\hat{B}, \hat{C}):}\cr
&\ \texttt{return } a \cdot \hat{B} = \hat{C}\cr
\cr
&\underline{\mathtt{Guess}(\hat{C}):}\cr
&\ \texttt{return } b = 0 \land \hat{C} = C\cr
\end{aligned}
}
$$

This notion suffices to prove $\text{IND-CCA}$ security of the KEM:

### $\text{IND-CCA-1} \leq 2 \cdot \text{ICDH}_b$

We'll model our hash function as a random oracle, like with RSA.

Let's start by explicitly writing down the $\text{IND-CCA-1}$ game
in this context:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{IND-CCA-1}_b$
}\cr
\cr
&\text{seen} \gets \emptyset\cr
&a \xleftarrow{R} \mathbb{F}_q\cr
&A \gets a \cdot G\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } A\cr
\cr
&\underline{(1) \mathtt{Challenge}():}\cr
&\ b \xleftarrow{R} \mathbb{F}_q\cr
&\ B \gets b \cdot G\cr
&\ C \gets c \cdot C\cr
&\ k_0 \gets \texttt{H}(B, C)\cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \text{seen} \gets \text{seen} \cup \\{B\\}\cr
&\ \texttt{return } (k_b, B) \cr
\cr
&\underline{\mathtt{Decap}(B):}\cr
&\ \texttt{assert } B \notin \text{seen}\cr
&\ \texttt{return } \texttt{H}(B, a \cdot B) \cr
\cr
&h[\cdot] \gets \bot\cr
&\underline{\mathtt{H}(B, C):}\cr
&\ \texttt{if } (B, C) \notin h\cr
&\quad\ h[B, C] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h[B, C]\cr
\end{aligned}
}
$$

Next, like with the RSA kem proof, we need to minimize our use
of the secret key when decapsulating.
We follow the same strategy as in Theorem 12.4 of
[Boneh and Shoup](https://toc.cryptobook.us/).

We replace our hash table for pairs $(B, C)$ with a table
for $B$, when $C = a \cdot B$, and a table for $(B, C)$, when $B$ and $C$
are not related.
Since we only have a single challenge query, we can also lift generation
of those values outside the challenge.

This gives us the following game:

$$
\text{IND-CCA-1}_b =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^0_b$
}\cr
\cr
&a, b \xleftarrow{R} \mathbb{F}_q\cr
&A \gets a \cdot G\cr
&B \gets b \cdot G\cr
&C \gets a \cdot B\cr
&\ k_0 \xleftarrow{R} \bold{K}\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } A\cr
\cr
&\underline{(1) \mathtt{Challenge}():}\cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } (k_b, B) \cr
\cr
&h[\cdot], h'[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Decap}(\hat{B}):}\cr
&\ \texttt{assert } \hat{B} \neq B\cr
&\ \texttt{if } \hat{B} \notin h'\cr
&\quad\ h'[\hat{B}] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h'[\hat{B}] \cr
\cr
&\underline{\mathtt{H}(\hat{B}, \hat{C}):}\cr
&\ \texttt{if } (\hat{B}, \hat{C}) \notin h \cr
&\quad\ \texttt{if } a \cdot \hat{B} = \hat{C}\cr
&\quad\quad\ \texttt{if } \hat{C} = C \cr
&\quad\quad\quad\ h'[\hat{B}] \gets k_0\cr
&\quad\quad\ \texttt{if } \hat{B} \notin h' \cr
&\quad\quad\quad\ h'[\hat{B}] \xleftarrow{R} \bold{K}\cr
&\quad\quad\ h[\hat{B}, \hat{C}] \gets h'[\hat{B}]\cr
&\quad\ \texttt{else }\cr
&\quad\quad\ h[\hat{B}, \hat{C}] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h[\hat{B}, \hat{C}]\cr
\end{aligned}
}
$$

Basically, we maintain the invariant that $a \cdot \hat{B} = \hat{C} \implies h[\hat{B}, \hat{C}] = h'[\hat{B}]$.
this allows us to make decapsulation queries without making use of the secret
key.
This will make extracting out the $\text{ICDH}$ game much easier.
This extraction is also made easier in that the query $a \cdot \hat{B} = \hat{C}$ is precisely the kind of query we can do in this game.

The next step is to extract out $\text{ICDH}$:

$$
\Gamma^0_b =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^1_b$
}\cr
\cr
&(A, B) \gets \texttt{Instance()}\cr
&\ k_0 \xleftarrow{R} \bold{K}\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } A\cr
\cr
&\underline{(1) \mathtt{Challenge}():}\cr
&\ k_1 \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } (k_b, B) \cr
\cr
&h[\cdot], h'[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Decap}(\hat{B}):}\cr
&\ \texttt{assert } \hat{B} \neq B\cr
&\ \texttt{if } \hat{B} \notin h'\cr
&\quad\ h'[\hat{B}] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h'[\hat{B}] \cr
\cr
&\underline{\mathtt{H}(\hat{B}, \hat{C}):}\cr
&\ \texttt{if } (\hat{B}, \hat{C}) \notin h \cr
&\quad\ \texttt{if } \texttt{Query}(\hat{B}, \hat{C})\cr
&\quad\quad\ \texttt{if } \texttt{Guess}(\hat{C}) \cr
&\quad\quad\quad\ h'[\hat{B}] \gets k_0\cr
&\quad\quad\ \texttt{if } \hat{B} \notin h' \cr
&\quad\quad\quad\ h'[\hat{B}] \xleftarrow{R} \bold{K}\cr
&\quad\quad\ h[\hat{B}, \hat{C}] \gets h'[\hat{B}]\cr
&\quad\ \texttt{else }\cr
&\quad\quad\ h[\hat{B}, \hat{C}] \xleftarrow{R} \bold{K}\cr
&\ \texttt{return } h[\hat{B}, \hat{C}]\cr
\end{aligned}
}
\circ \text{ICDH}_0
$$

Now, notice that in the game $\Gamma^1_b \circ \text{ICDH}_1$, $k_0$ isn't
used anywhere but inside of $\texttt{Challenge}$, because $\texttt{Guess}$
always returns $0$.
This means that the difference between $k_0$ and $k_1$ becomes a matter
of naming, so:

$$
\Gamma^1\_0 \circ \text{ICDH}\_1 = \Gamma^1\_1 \circ \text{ICDH}\_1
$$

This is enough to make our full walk, and tie everything together:

$$
\begin{aligned}
\text{IND-CCA-1}_0 &= \Gamma^0_0\cr
&= \Gamma^1_0 \circ \text{ICDH}_0\cr
&\stackrel{\epsilon_1}{\approx} \Gamma^1_0 \circ \text{ICDH}_1\cr
&= \Gamma^1_1 \circ \text{ICDH}_1\cr
&\stackrel{\epsilon_1}{\approx} \Gamma^1_1 \circ \text{ICDH}_0\cr
&= \Gamma^1_0\cr
&= \text{IND-CCA-1}_1\cr
\end{aligned}
$$

$\square$

## And Other Methods

Another method that's going to be increasingly important is using
_lattices_ to construct KEMs.

Both of the schemes I've mentioned in this post, using RSA,
and using elliptic curves, will be broken by future quantum computers.
This is why NIST started [a competition](https://csrc.nist.gov/projects/post-quantum-cryptography) in order to standardize
KEMs secure against quantum computers.

Many of the candidates were based on lattices, including the finalist,
[Kyber](https://pq-crystals.org/kyber/index.shtml).

Other candidates used different primitives, like isogenies, or
random codes.

# Composing KEMs

Talking about the post-quantum KEMs segues nicely into this next topic.
Right now, we have very well tested and trusted KEMs, which aren't
secure against quantum computers.
We also have a new KEM which should be secure against quantum computers,
but given its novelty, you may not trust it that much yet.

Because of this, many people are trying to deploy both schemes together
in a _hybrid_ fashion.
This means that you can get post-quantum security if the new KEM stands
the test of time, but you also don't sacrifice classical security
if the new KEM you include happens to be flawed.

To achieve this, what you want is a way to _combine_ KEMs,
mixing together KEMs $A$ and $B$ to create a new KEM, which should
be secure as long as at least one of the two ingredients is.
If $A$ is broken, that's fine as long as $B$ is secure, and vice versa.

This construction is called a _KEM combiner_, and it can combine two
KEMs in this way, without having to inspect how the KEMs work internally
at all.
The paper {{<ref-link "[GHP18]">}} goes over this notion of combiners,
and presents a very elegant construction.

To encapsulate, you call each of the individual
KEMs first, giving you $(k_A, c_A)$ and $(k_B, c_B)$.
Next, $(c_A, c_B)$ becomes your ciphertext.
Now, you need some way to combine all of this information to derive
a single key $k$.
One idea would be to simply xor the two keys, giving you $k \gets k_A \oplus k_B$.
Unfortunately, [this is not IND-CCA secure](https://twitter.com/cronokirby/status/1554092874538668032), because the resulting key is malleable.

The idea in this paper is instead to use a kind of pseudo-random function (PRF) to derive the result:

$$
F(k_A, k_B, (c_A, c_B))
$$

This is a special kind of PRF called a _split-key_ PRF.

## Split-Key PRFs in Theory

More formally, a split-key PRF is a function:

$$
F : \bold{K}_0 \times \bold{K}_1 \times \bold{X} \to \bold{Y}
$$

We have two types for keys, $\bold{K}_0$ and $\bold{K}_1$, as well
as an input type $\bold{X}$, and an output type $\bold{Y}$.
The intuition for this function is that it should behave like
a random function $\bold{X} \to \bold{Y}$ as long as the adversary
doesn't know _either_ of the keys.
Even if the adversary controls one of the keys, and can query
the function $F$ on different values for this key, they still shouldn't
be able to distinguish this function from a random one.

We formalize this with a game:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{SPLIT-PRF}_b(\sigma)$
}\cr
&\ k\_{\sigma} \xleftarrow{R} \bold{K}_0\cr
&\ \text{seen} \gets \emptyset\cr
\cr
&\underline{\texttt{QueryF}(k\_{(1 - \sigma)}, x):}\cr
&\ \texttt{assert } x \notin \text{seen}\cr
&\ y_0 \gets F(k_0, k_1, x)\cr
&\ y_1 \xleftarrow{R} \bold{Y}\cr
&\ \texttt{return } y_b
\end{aligned}
}
$$

One subtlety is that we don't allow the adversary to query the
same input point twice, even with different keys.
A stronger security notion would be to allow queries to the same input
with a different key, but we won't need that for proving the security
of our KEM combination scheme.

### Split-Key PRFs are Secure PRFs

{{<note>}}
If you're convinced of this just from reading the section header,
feel free to skip this section.
{{</note>}}

With a split-key PRF, we model security in a situation where the adversary
can control one of the keys.
When looking at the security of a PRF, the adversary doesn't control
the key at all.
One interesting fact is that a split-key PRF is necessarily a secure
PRF, considering $(k_0, k_1)$ as a single "key".

$$
\text{PRF} \leq \text{SPLIT-PRF}(\sigma)
$$

Let's recall the PRF game (in this context), briefly:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{PRF}_b$
}\cr
\cr
&\ k_0 \xleftarrow{R} \bold{K}_0\cr
&\ k_1 \xleftarrow{R} \bold{K}_1\cr
&\ \text{seen} \gets \emptyset\cr
\cr
&\underline{\texttt{QueryF}(x):}\cr
&\ \texttt{assert } x \notin \text{seen}\cr
&\ y_0 \gets F(k_0, k_1, x)\cr
&\ y_1 \xleftarrow{R} \bold{Y}\cr
&\ \texttt{return } y_b
\end{aligned}
}
$$

From here you just need to extract out the split-key game:

$$
\text{PRF}_b =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma$
}\cr
\cr
&\ k\_{(1 - \sigma)} \xleftarrow{R} \bold{K}\_{(1 - \sigma)}\cr
\cr
&\underline{\texttt{QueryF}(x):}\cr
&\ \texttt{return } \texttt{super.QueryF}(k\_{(1 - \sigma)}, x)
\end{aligned}
}
\circ
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{SPLIT-PRF}_b(\sigma)$
}\cr
\cr
&\ k\_{\sigma} \xleftarrow{R} \bold{K}_0\cr
&\ \text{seen} \gets \emptyset\cr
\cr
&\underline{\texttt{QueryF}(k\_{(1 - \sigma)}, x):}\cr
&\ \texttt{assert } x \notin \text{seen}\cr
&\ y_0 \gets F(k_0, k_1, x)\cr
&\ y_1 \xleftarrow{R} \bold{Y}\cr
&\ \texttt{return } y_b
\end{aligned}
}
$$

This gives us:

$$
\text{PRF}_0 = \Gamma \circ \text{SPLIT-PRF}_0(\sigma) \stackrel{\epsilon_1}{\approx} \Gamma \circ \text{SPLIT-PRF}_1(\sigma) = \text{PRF}_1
$$

concluding our proof.

$\square$

## KEM Combination with PRF

It turns out that split-key PRFs precisely capture what we need
to compose KEMs together.
In this section, we formally define the composition of two KEMs using
such a PRF, and prove that it's $\text{IND-CCA}$ secure, assuming the PRF is split-key secure,
and one of the underlying KEMs is $\text{IND-CCA}$ secure.

We start with two KEMs, $A$, and $B$.
The composition will be like the informal idea we saw earlier:
in order to encapsulate, we use the encapsulation from each KEM,
and then combine them together as a pair $(c_A, c_B)$.
When decapsulating, we end up with two keys $k_A$ and $k_B$.
We'll use a split-key PRF to combine them, including the ciphertexts, to give us:

$$
k \gets F(k_A, k_B, (c_A, c_B))
$$

A bit more formally, given two KEMs $A$ and $B$, and a function

$$
F : A.\bold{K} \times B.\bold{K} \times (A.\bold{C} \times B.\bold{C}) \to \bold{K}
$$

we can define the combined KEM $A \times_F B$ as follows:

$$
\begin{aligned}
&\underline{\text{Gen}():}\cr
&\ (\text{sk}_A, \text{pk}_A) \gets \text{A.Gen}()\cr
&\ (\text{sk}_B, \text{pk}_B) \gets \text{B.Gen}()\cr
&\ \texttt{return } ((\text{sk}_A, \text{sk}_B), (\text{pk}_A, \text{pk}_B)) \cr
\cr
&\underline{\text{Encap}((\text{pk}_A, \text{pk}_B):}\cr
&\ (k_A, c_A) \gets \text{A.Encap}(\text{pk}_A)\cr
&\ (k_B, c_B) \gets \text{B.Encap}(\text{pk}_B)\cr
&\ k \gets F(k_A, k_B, (c_A, c_B))\cr
&\ \texttt{return } (k, (c_A, c_B))\cr
\cr
&\underline{\text{Decap}((\text{sk}_A, \text{sk}_B), (c_A, c_B)):}\cr
&\ k_A \gets \text{A.Decap}(\text{sk}_A, c_A)\cr
&\ k_B \gets \text{B.Decap}(\text{sk}_B, c_B)\cr
&\ k \gets F(k_A, k_B, (c_A, c_B))\cr
&\ \texttt{return } k\cr
\end{aligned}
$$

In terms of encapsulation, this works in the way you'd expect for a product
construction.
The only real trick comes from decapsulation, where we use our function
$F$ to combine both keys and the ciphertexts to produce a final key.

### Security Proof

It turns out that if $F$ is a split-key PRF, then the security of this
scheme reduces to that of either one of the KEMs.

Because our combined KEM definition is completely symmetric with respect
to $A$ or $B$, we prove this, without loss of generality, just by treating
the case where $A$ is $\text{IND-CCA}$ secure, and show that:

$$
\text{IND-CCA-1}(A \times_F B) \leq 2 \cdot \text{IND-CCA-1}(A) + 2 \cdot \text{SPLIT-PRF}
$$

Let's start, as we've done a few times in this post, by explicitly writing
out the $\text{IND-CCA-1}$ game using our KEM:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{IND-CCA-1}_b$
}\cr
\cr
&\text{seen} \gets \emptyset\cr
&(\text{sk}_A, \text{pk}_A) \xleftarrow{R} \text{A.Gen}()\cr
&(\text{sk}_B, \text{pk}_B) \xleftarrow{R} \text{B.Gen}()\cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } (\text{pk}_A, \text{pk}_B) \cr
\cr
&\underline{(1)\mathtt{Challenge}():}\cr
&\ (k_A, c_A) \gets \text{A.Encap}(\text{pk}_A) \cr
&\ (k_B, c_B) \gets \text{B.Encap}(\text{pk}_B) \cr
&\ k_0 \gets F(k_A, k_B, (c_A, c_B))\cr
&\ k_1 \xleftarrow{R} \bold{K} \cr
&\ \text{seen} \gets \text{seen} \cup \\{(c_A, c_B)\\}\cr
&\ \texttt{return } (k_b, (c_A, c_B)) \cr
\cr
&\underline{\mathtt{Decap}((\hat{c}_A, \hat{c}_B)):}\cr
&\ \texttt{assert } (\hat{c}_A, \hat{c}_B) \notin \text{seen}\cr
&\ \hat{k}_A \gets \text{A.Decap}(\text{sk}_A, \hat{c}_A)\cr
&\ \hat{k}_B \gets \text{B.Decap}(\text{sk}_B, \hat{c}_B)\cr
&\ \texttt{if } \hat{k}_A = \bot \lor \hat{k}_B = \bot\cr
&\ \quad \texttt{return } \bot\cr
&\ \texttt{return } F(\hat{k}_A, \hat{k}_B, (\hat{c}_A, \hat{c}_B)) \cr
\end{aligned}
}
$$

Next, we pull the usual trick of pulling out code from $\texttt{Challenge}$,
since it only gets called a single time.
We also want to change the decapsulation function to minimize the use
of $\text{sk}_A$, since we'll be replacing explicit calls to $\text{Decap}$
with oracle calls to $\text{IND-CCA-1}(A)$.

$$
\text{IND-CCA-1}_b =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^0_b$
}\cr
\cr
&(\text{sk}_A, \text{pk}_A) \xleftarrow{R} \text{A.Gen}()\cr
&(\text{sk}_B, \text{pk}_B) \xleftarrow{R} \text{B.Gen}()\cr
&\ (k_A, c_A) \gets \text{A.Encap}(\text{pk}_A) \cr
&\ (k_B, c_B) \gets \text{B.Encap}(\text{pk}_B) \cr
&\ k_0 \gets F(k_A, k_B, (c_A, c_B))\cr
&\ k_1 \xleftarrow{R} \bold{K} \cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \texttt{return } (\text{pk}_A, \text{pk}_B) \cr
\cr
&\underline{(1)\mathtt{Challenge}():}\cr
&\ \texttt{return } (k_b, (c_A, c_B)) \cr
\cr
&d[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Decap}((\hat{c}_A, \hat{c}_B)):}\cr
&\ \texttt{assert } (\hat{c}_A, \hat{c}_B) \neq (c_A, c_B)\cr
&\ \hat{k}_B \gets \text{B.Decap}(\text{sk}_B, \hat{c}_B)\cr
&\ \texttt{if } \hat{k}_B = \bot\cr
&\ \quad \texttt{return } \bot\cr
&\ \texttt{if } \hat{c}_A = c_A\cr
&\ \quad \hat{k}_A \gets k_A\cr
&\ \texttt{else }\cr
&\ \quad \hat{k}_A \gets \text{A.Decap}(\text{sk}_A, \hat{c}_A)\cr
&\ \quad \texttt{if } \hat{k}_A = \bot\cr
&\ \quad\quad \texttt{return } \bot\cr
&\ \texttt{if } (\hat{c}_A, \hat{c}_B) \notin d\cr
&\ \quad d[(\hat{c}_A, \hat{c}_B)] \gets F(\hat{k}_A, \hat{k}_B, (\hat{c}_A, \hat{c}_B))\cr
&\ \texttt{return } d[(\hat{c}_A, \hat{c}_B)] \cr
\end{aligned}
}
$$

The behavior of decapsulation is the same, we just make a few preparations
for later.

First, we cache the output of the decapsulation in $d$.
This is because the PRF oracle we'll use later can't be queried twice
on the same input, so we'll need to use this table to avoid doing that.

Second, we check whether $\hat{c}_A = c_A$ in order to explictly
use the hardcoded $k_A$.
This will make extracting out the $A$ KEM much easier.

Our next step is to extract out this KEM.

$$
\Gamma^0_b =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^1_b$
}\cr
\cr
&(\text{sk}_B, \text{pk}_B) \xleftarrow{R} \text{B.Gen}()\cr
&\ (k_A, c_A) \gets \texttt{super.Challenge}() \cr
&\ (k_B, c_B) \gets \text{B.Encap}(\text{pk}_B) \cr
&\ k_0 \gets F(k_A, k_B, (c_A, c_B))\cr
&\ k_1 \xleftarrow{R} \bold{K} \cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \text{pk}_A \gets \texttt{super.GetPk}()\cr
&\ \texttt{return } (\text{pk}_A, \text{pk}_B) \cr
\cr
&\underline{(1)\mathtt{Challenge}():}\cr
&\ \texttt{return } (k_b, (c_A, c_B)) \cr
\cr
&d[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Decap}((\hat{c}_A, \hat{c}_B)):}\cr
&\ \texttt{assert } (\hat{c}_A, \hat{c}_B) \neq (c_A, c_B)\cr
&\ \hat{k}_B \gets \text{B.Decap}(\text{sk}_B, \hat{c}_B)\cr
&\ \texttt{if } \hat{k}_B = \bot\cr
&\ \quad \texttt{return } \bot\cr
&\ \texttt{if } \hat{c}_A = c_A\cr
&\ \quad \hat{k}_A \gets k_A\cr
&\ \texttt{else }\cr
&\ \quad \hat{k}_A \gets \texttt{super.Decap}(\hat{c}_A)\cr
&\ \quad \texttt{if } \hat{k}_A = \bot\cr
&\ \quad\quad \texttt{return } \bot\cr
&\ \texttt{if } (\hat{c}_A, \hat{c}_B) \notin d\cr
&\ \quad d[(\hat{c}_A, \hat{c}_B)] \gets F(\hat{k}_A, \hat{k}_B, (\hat{c}_A, \hat{c}_B))\cr
&\ \texttt{return } d[(\hat{c}_A, \hat{c}_B)] \cr
\end{aligned}
}
\circ \text{IND-CCA-1}_0(A)
$$

With this game, we've extracted out the $A$ KEM completely.
One key thing which enabled us to do this was checking that
$\hat{c}_A = c_A$ ourselves, to avoid hitting the assertion inside
of $A$'s $\texttt{Decap}$ oracle.

Naturally, we'll be able to make the jump from $\text{IND-CCA-1}_0(A)$
to $\text{IND-CCA-1}_1(A)$, but that won't be enough to complete our proof.
We still need to make use of our PRF assumption.

To that effect, let's write a game which explicitly captures what
happens after making the switch to an ideal KEM:

$$
\Gamma^1_b \circ \text{IND-CCA-1}_1(A) =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^2_b$
}\cr
\cr
&(\text{sk}_B, \text{pk}_B) \xleftarrow{R} \text{B.Gen}()\cr
&\ (\cdot, c_B) \gets \text{A.Encap}(\text{pk}_A) \cr
&\ k_A \xleftarrow{R} A.\bold{K}\cr
&\ (k_B, c_B) \gets \text{B.Encap}(\text{pk}_B) \cr
&\ k_0 \gets F(k_A, k_B, (c_A, c_B))\cr
&\ k_1 \xleftarrow{R} \bold{K} \cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \text{pk}_A \gets \texttt{super.GetPk}()\cr
&\ \texttt{return } (\text{pk}_A, \text{pk}_B) \cr
\cr
&\underline{(1)\mathtt{Challenge}():}\cr
&\ \texttt{return } (k_b, (c_A, c_B)) \cr
\cr
&d[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Decap}((\hat{c}_A, \hat{c}_B)):}\cr
&\ \texttt{assert } (\hat{c}_A, \hat{c}_B) \neq (c_A, c_B)\cr
&\ \hat{k}_B \gets \text{B.Decap}(\text{sk}_B, \hat{c}_B)\cr
&\ \texttt{if } \hat{k}_B = \bot\cr
&\ \quad \texttt{return } \bot\cr
&\ \texttt{if } \hat{c}_A = c_A\cr
&\ \quad\texttt{if } (\hat{c}_A, \hat{c}_B) \notin d\cr
&\ \quad\quad d[(\hat{c}_A, \hat{c}_B)] \gets F(k_A, \hat{k}_B, (\hat{c}_A, \hat{c}_B))\cr
&\ \quad\texttt{return } d[(\hat{c}_A, \hat{c}_B)] \cr
&\ \texttt{else }\cr
&\ \quad \hat{k}_A \gets \texttt{super.Decap}(\hat{c}_A)\cr
&\ \quad \texttt{if } \hat{k}_A = \bot\cr
&\ \quad\quad\texttt{return } \bot \cr
&\ \quad \texttt{return } F(\hat{k}_A, \hat{k}_B, (\hat{c}_A, \hat{c}_B))\cr
\end{aligned}
}
$$

At the start of the game, we can ignore whatever key we get from $A$'s
encapsulation, and instead use a random $k_A$.

At the end of the game, in decapsulation, we've shifted things around
to make how the PRF is queried more clear.
In one branch, we query the PRF using that initial $k_A$ key,
and so we want to make sure to cache results.
In the other branch, we use an unrelated key, and so we don't care.

Our next trick is going to be to replace calls to $F(k_A, \ldots)$
with oracle queries to the $\text{SPLIT-PRF}$ game.

$$
\Gamma^2_b =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^3_b$
}\cr
\cr
&(\text{sk}_B, \text{pk}_B) \xleftarrow{R} \text{B.Gen}()\cr
&\ (\cdot, c_B) \gets \text{A.Encap}(\text{pk}_A) \cr
&\ (k_B, c_B) \gets \text{B.Encap}(\text{pk}_B) \cr
&\ k_0 \gets \texttt{super.QueryF}(k_B, (c_A, c_B))\cr
&\ k_1 \xleftarrow{R} \bold{K} \cr
\cr
&\underline{\mathtt{GetPk}():}\cr
&\ \text{pk}_A \gets \texttt{super.GetPk}()\cr
&\ \texttt{return } (\text{pk}_A, \text{pk}_B) \cr
\cr
&\underline{(1)\mathtt{Challenge}():}\cr
&\ \texttt{return } (k_b, (c_A, c_B)) \cr
\cr
&d[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Decap}((\hat{c}_A, \hat{c}_B)):}\cr
&\ \texttt{assert } (\hat{c}_A, \hat{c}_B) \neq (c_A, c_B)\cr
&\ \hat{k}_B \gets \text{B.Decap}(\text{sk}_B, \hat{c}_B)\cr
&\ \texttt{if } \hat{k}_B = \bot\cr
&\ \quad \texttt{return } \bot\cr
&\ \texttt{if } \hat{c}_A = c_A\cr
&\ \quad\texttt{if } (\hat{c}_A, \hat{c}_B) \notin d\cr
&\ \quad\quad d[(\hat{c}_A, \hat{c}_B)] \gets \texttt{super.QueryF}(\hat{k}_B, (\hat{c}_A, \hat{c}_B))\cr
&\ \quad\texttt{return } d[(\hat{c}_A, \hat{c}_B)] \cr
&\ \texttt{else }\cr
&\ \quad \hat{k}_A \gets \texttt{super.Decap}(\hat{c}_A)\cr
&\ \quad \texttt{if } \hat{k}_A = \bot\cr
&\ \quad\quad\texttt{return } \bot \cr
&\ \quad \texttt{return } F(\hat{k}_A, \hat{k}_B, (\hat{c}_A, \hat{c}_B))\cr
\end{aligned}
} \circ \text{SPLIT-PRF}_0
$$

One interesting thing is that we keep calls to the actual function $F$,
we just use the oracle queries for when the key $k_A$ was used before.

Now, the next question is: what happens when our PRF is perfect?
Well, we get the following situation:

$$
\Gamma^3_b \circ \text{SPLIT-PRF}_1 =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^4_b$
}\cr
\cr
&(\text{sk}_B, \text{pk}_B) \xleftarrow{R} \text{B.Gen}()\cr
&\ (\cdot, c_B) \gets \text{A.Encap}(\text{pk}_A) \cr
&\ (k_B, c_B) \gets \text{B.Encap}(\text{pk}_B) \cr
&\ k_0 \xleftarrow{R} \bold{K} \cr
&\ k_1 \xleftarrow{R} \bold{K} \cr
&\ldots\cr
\end{aligned}
}
$$

The rest of the game doesn't matter.
The important part is that now there's no difference between $k_0$
and $k_1$, and so we have $\Gamma^4_0 = \Gamma^4_1$.
Now, we can do the walk backwards, chaining everything together to get
our result:

$$
\begin{aligned}
\text{IND-CCA-1}_0 &= \Gamma^0_0\cr
&= \Gamma^1_0 \circ \text{IND-CCA-1}_0(A)\cr
&\stackrel{\epsilon_1}{\approx} \Gamma^1_0 \circ \text{IND-CCA-1}_1(A)\cr
&= \Gamma^2_0\cr
&= \Gamma^3_0 \circ \text{SPLIT-PRF}_0\cr
&\stackrel{\epsilon_2}{\approx} \Gamma^3_0 \circ \text{SPLIT-PRF}_1\cr
&= \Gamma^4_0\cr
&= \Gamma^4_1\cr
&= \Gamma^3_1 \circ \text{SPLIT-PRF}_1\cr
&\stackrel{\epsilon_2}{\approx} \Gamma^3_1 \circ \text{SPLIT-PRF}_0\cr
&= \Gamma^2_1\cr
&= \Gamma^1_1 \circ \text{IND-CCA-1}_1(A)\cr
&\stackrel{\epsilon_1}{\approx} \Gamma^1_1 \circ \text{IND-CCA-1}_0(A)\cr
&= \Gamma^0_1\cr
&= \text{IND-CCA-1}_1\cr
\end{aligned}
$$

$\square$

## Constructing Split-Key PRFs

In the KEM combiner paper {{<ref-link "[GHP18]">}}, they present many
examples of split-key PRFs.
As an illustration, I'll choose one of the simplest ones, which
can be proven secure even without random oracles.
I'd recommend checking out the papers for other more efficient constructions.

### Xoring Two PRFs

One simple construction involves combining two PRFs $F_0$ and $F_1$ together:

$$
F(k_0, k_1, x) := F_0(k_0, x) \oplus F_1(k_1, x)
$$

This PRF is split-key because even if you can influence one of the keys,
the other side looks completely random, and thus so is the final result.
Let's prove this a bit more formally.

Without loss of generality, we can simply prove that it's split
key for the first key, $k_0$, since the function is symmetric.

$$
\text{SPLIT-PRF}(0) \leq \text{PRF}(F_0)
$$

The high level proof idea is that what happens with $F_1$ can't affect
the outcome, because $F_0$ being a PRF is enough to guarantee that the
output is random.
In our split-key game, we'll easily be able to

First, explicitly write the split-key game:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{SPLIT-PRF}_0$
}\cr
\cr
&\ k\_0 \xleftarrow{R} \bold{K}_0\cr
&\ \text{seen} \gets \emptyset\cr
\cr
&\underline{\texttt{QueryF}(k_1, x):}\cr
&\ \texttt{assert } x \notin \text{seen}\cr
&\ y \gets F(k_0, x) \oplus F(k_1, x)\cr
&\ \texttt{return } y
\end{aligned}
}
$$

Next, extract out $F_0$:

$$
\text{SPLIT-PRF}_0 =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma$
}\cr
\cr
&\underline{\texttt{QueryF}(k_1, x):}\cr
&\ y \gets \texttt{super.QueryF}(x) \oplus F(k_1, x)\cr
&\ \texttt{return } y
\end{aligned}
}
\circ
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{PRF}_0$
}\cr
\cr
&\ k \xleftarrow{R} \bold{K}_0\cr
&\ \text{seen} \gets \emptyset\cr
\cr
&\underline{\texttt{QueryF}(k_1, x):}\cr
&\ \texttt{assert } x \notin \text{seen}\cr
&\ y \gets F(k_0, x) \cr
&\ \texttt{return } y
\end{aligned}
}
$$

Next, notice that if we switch to $\text{PRF}_1$, then the output
of $\Gamma$ becomes completely random:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma$
}\cr
\cr
&\underline{\texttt{QueryF}(k_1, x):}\cr
&\ y \gets \texttt{super.QueryF}(x) \oplus F(k_1, x)\cr
&\ \texttt{return } y
\end{aligned}
}
\circ
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{PRF}_1$
}\cr
\cr
&\ \text{seen} \gets \emptyset\cr
\cr
&\underline{\texttt{QueryF}(k_1, x):}\cr
&\ \texttt{assert } x \notin \text{seen}\cr
&\ y \xleftarrow{R} \bold{Y}\cr
&\ \texttt{return } y
\end{aligned}
} =
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{SPLIT-PRF}_1$
}\cr
\cr
&\text{seen} \gets \emptyset\cr
\cr
&\underline{\texttt{QueryF}(k_1, x):}\cr
&\ \texttt{assert } x \notin \text{seen}\cr
&\ y \xleftarrow{R} \bold{Y}\cr
&\ \texttt{return } y
\end{aligned}
}
$$

This is because xoring any value with a (uniformly) random value
returns another random value.
Thus, xoring $F(k_1, x)$ with a random $y$ gives us a random result.

To summarize, we have:

$$
\text{SPLIT-PRF}_0 = \Gamma \circ \text{PRF}_0 \stackrel{\epsilon_1}{\approx} \Gamma \circ \text{PRF}_1 = \text{SPLIT-PRF}_1
$$

which concludes our proof.

$\square$

### Other Methods

The kem combiner paper {{<ref-link "GHP18">}} also provides various
other methods for creating split-key PRFs.

One method is to do $F(k_A, c_A) \oplus F(k_B, c_B)$, including only
one of the ciphertexts.

Another method, which requires the random oracle model, is to do:
${H(k_A \oplus k_B, c_A, c_B)}$.

And this is just a small sample; I really recommend checking out the paper
if you want to know more; it's a very well written paper!

# Conclusion

While there weren't that many topics covered in thist post, I felt
that stopping it here made for a hefty post already, especially
with the focus on proofs.
I think state-separable proofs are quite fun to write, and hopefully
they were enjoyable to read as well.
A secret about this post is that KEMs are really just an excuse
to write more state-separable proofs, since that's what motivated
me to write about the topic.

There's still a lot more interesting things to say about KEMs though.
One notion I didn't touch on at all here is that of _authenticated_
KEMs, where you also want to authenticate the sender in some way.
Neil Madden wrote [a post](https://neilmadden.blog/2021/02/16/when-a-kem-is-not-enough/) on this topic as well,
and you might want to check out {{<ref-link "ABHKLR20">}},
which analyzes the HPKE standard for authenticated KEMs, outlining
various notions of security for this construction.

Another thing that would be neat to go over is the notion of
_deniable_ authenticated KEMs, wherein it's possible to forge ciphertexts
which claim to have been sent by another person to yourself.
This makes it so that KEM ciphertexts can't be used as evidence
of communication between two parties.

This also butts heads against the notion of _insider security_,
which Yolan Romailler [brought to my attention on twitter](https://twitter.com/AnomalRoil/status/1556022245109235712).
See also [his blog post](https://romailler.ch/2021/08/18/crypto-why-ephemeral-keys/) touching on ephemeral keys and insider
security in key exchanges.

# References

{{<ref
  "[ABHKLR20]"
  "https://eprint.iacr.org/2018/024"
  "[ABHKLR20] Analysing the HPKE standard - Joël Alwen, Bruno Blanchet, Eduard Hauck, Eike Kiltz, Benjamin Lipp, Doreen Riepel">}}
{{<ref
  "[GHP18]"
  "https://eprint.iacr.org/2018/024"
  "[GHP18] KEM Combiners - Federico Giacon, Felix Heuer, Bertram Poettering">}}
