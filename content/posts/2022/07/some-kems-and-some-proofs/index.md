---
title: "Some KEMs and Some Proofs"
date: 2022-07-29T18:01:33+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Foundations"
  - "KEMs"
---

# Defining KEMs

A key encapsulation mechanism (KEM), is a scheme similar to public key encryption.
This scheme consists of three algorithms:

$$
\begin{aligned}
\text{Gen} &: () \to (\bold{SK}, \bold{PK})\cr
\text{Encap} &: \bold{PK} \to (\bold{K}, \bold{C})\cr
\text{Decap} &: (\bold{SK}, \bold{C}) \to \bold{K}
\end{aligned}
$$

The first algorithm, $\text{Gen}$ creates new keypair, consisting
of a private key, and its corresponding public key.

The second algorithm, $\text{Encap}$, takes in a public key,
for the recipient, and results in a symmetric key, and an encapsulation,
or ciphertext.

With $\text{Decap}$, a recipient can use their private key to extract
this symmetric key from the ciphertext.

This scheme also needs to satisfy some notion of correctness.
Intuitively, decapsulating should yield the same key that was encapsulated.

More formally, the following procedure must always succeed:

$$
\begin{aligned}
&(\text{sk}, \text{pk}) \gets \text{Gen}()\cr
&(k_S, c) \gets \text{Encap}(\text{pk})\cr
&k_R \gets \text{Decap}(\text{sk}, c)\cr
&k_S \stackrel{?}{=} k_R
\end{aligned}
$$

## $\text{IND}$ Security

Naturally, having a scheme that's correct is easy if you don't care about
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
&\colorbox{#dbeafe}{\large
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

In one version of the game, we get the key related to the encapsulation,
and in the other version of the game, we get a completely random key.
An adversary should not be able to distinguish between the two games.

{{<note>}}
In this variant, you can query the challenge multiple times.
This is equivalent to only being able to query once, although the security
gets worse as a linear function of the number of queries you do.
For simplicity, I'll stick with the multi query variants for the rest
of this post.
{{</note>}}

This variant of security is also sometimes referred to as $\text{IND-CPA}$.
This is because the adversary is able to create encapsulations themselves,
by virtue of having the public case.
This kind of query is a "chosen plaintext", hence the "chosen plaintext attack (CPA) in the name.
With symmetric encryption, you need to have a secret key to even
_encrypt_ data, so the $\text{CPA}$ capability is a meaningful distinction.

## $\text{IND-CCA}$ Security

An even stronger variant of security also allows the adversary to make
_decapsulation_ queries on ciphertexts of their choice.
We call these _chosen ciphertext attacks_ (CCA).
This models situations where we might know the keys corresponding
with certain ciphertexts.
While in practice these leakages could be quite limited, having a more
expansive model of security covers many more situations,
and we can construct schemes which satisfy this general model.

We model this with a pair of games, like before:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
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
&\colorbox{#dbeafe}{\large
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

One natural question is whether or not this new game is equivalent
to our previous definitions.

It is.

As a bit of a warmup, let's prove this equivalence.

### $\text{IND-Both-CCA} \leq 2 \cdot \text{IND-CCA}$

The idea of this reduction is that we can replace $k_0$ from the first
encapsulation with a random key, because of $\text{IND-CCA}$ security,
thus then allows us to swap $k_0$ with $k_1$, and then walk our way backwards,
giving us a bound for $\epsilon(\text{IND-Both-CCA}_b)$.

We start by extracting out the encapsulation in $\text{IND-Both-CCA}_b$,
using $\text{IND-CCA}$:

$$
\text{IND-Both-CCA}_b =
\begin{aligned}
&\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
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
W_b \circ \text{IND-Both-CCA}_0 = \text{IND-CCA}\_{(1 - b)}
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

Both of these definitions turned out to be equivalent, but both
fit into a kind of "real-or-random" paradigm, in that one of the keys
is randomly selected, while the other is connected to the ciphertext.
This is a general kind of paradigm, used for defining the security of other
schemes as well.
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
&\colorbox{#dbeafe}{\large
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

Let's start with [RSA](https://www.wikiwand.com/en/RSA_(cryptosystem)).
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
&\colorbox{#dbeafe}{\large
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
The idea is that we want the pre-image $x$ to be our key, and then
$y := x^e \mod N$ is our ciphertext, hiding the key.
The recipient can use their secret to unwrap $x$ from this ciphertext.
Now, because $\mathbb{Z}/(N)$ may not be a very useful key by itself,
we also make use of a hash function $H : \mathbb{Z}/(N) \to \bold{K}$,
so that we can derive a more useful key from $x$.

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
First, we'll model our hash function $H$ as a *random oracle*, where
the outputs will be generated perfectly at random, on demand.
Second, rather than considering normal $\texttt{IND-CCA}$ security,
instead we'll consider $\texttt{IND-CCA-1}$ security, where the adversary
can only make a single challenge query.
Making $Q$ challenge queries instead of just a single one only decreases
security by a factor of $Q$, as can be shown via a hybrid argument.

### $\text{IND-CCA-1} \leq 2 \cdot \text{RSA}$

First, let's write down the $\text{IND-CCA-1}$ game in this context.

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
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
&\colorbox{#dbeafe}{\large
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
&\colorbox{#dbeafe}{\large
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
&\colorbox{#dbeafe}{\large
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
and a point $A := a \cdot G$, for the public key.
In order to encapsulate a key, we generate a random scalar $b$,
set $B := b \cdot G$ as our encapsulation, and $H(b \cdot A)$ as the key.
The receiver can set $H(a \cdot B)$ as their key, arriving at the same
result because of commutativity.

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
&\colorbox{#dbeafe}{\large
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

We'll be working in the random oracle model, for our hash function.

Let's start by explicitly writing down the $\text{IND-CCA-1}$ game
in this context:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
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
&\colorbox{#dbeafe}{\large
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

Notice that the query $a \cdot \hat{B} = \hat{C}$ is the kind of query
we can do in the $\text{ICDH}$ game.
The next step is to extract out this game:

$$
\Gamma^0_b =
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
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

This is enough to allow us to make our full walk, and tie everything together:

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

# Authenticated KEMs

## Generic Construction via Signatures

# Deniable Authenticated KEMs

## Construction from Groups

# Conclusion
