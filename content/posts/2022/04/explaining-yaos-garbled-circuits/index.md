---
title: "Explaining Yao's Garbled Circuits"
date: 2022-04-26T21:24:02+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "MPC"
---

# MPC in a Nutshell

Garbled Circuits are a special case of a more general idea called
(secure) Multi-Party Computation (MPC).

The premise is that you have a group of parties $P_1, \ldots, P_n$,
with each party $P_i$ having their own secret input $x_i$. The
players want to compute some function $f(x_1, \ldots, x_n)$ on their inputs,
learning the result $y$.

The simplest way to do this would be for the parties to share their
inputs with each other. Each party would know all of the inputs $x_1, \ldots,
x_n$, and thus be able to compute $y = f(x_1, \ldots, x_n)$ on their own.

The problem with this naive approach is that each party would like
to keep their input $x_i$ _secret_. The difficulty in MPC is not
in computing the function $f$ among multiple parties, but rather in
keeping all of the inputs hidden while performing that computation.

## Example: The Millionaire Problem

One of the classic examples of a situation where MPC is useful
is [Yao's Millionaire Problem](https://doi.ieeecomputersociety.org/10.1109/SFCS.1982.88).

The premise is that several millionaires want to know which of
them is the richest. They could do this by revealing their exact
wealth to each other, but they'd like to each keep that amount
secret.
In other words, they'd like to learn who has the most money,
without revealing the amount of money that person, or any
other member of the group has.

This is a problem which can be solved through an MPC protocol.
The inputs $x_1, \ldots, x_n$ will be set to the wealth of each
party, and the function $f$ to compute will be:

$$
f(x_1, \ldots, x_n) := \argmax_{i} x_i
$$

which returns the index $j$ of the largest amount $x_j$.

An MPC protocol for $f$ would let the millionaires collaborate
by exchanging messages, eventually learning the result
$f(x_1, \ldots, x_n)$, but no other information about the inputs.

This is a somewhat artificial example, but there are plenty
of more realistic applications of MPC. Another example
people like to give is that of the [Danish Sugar Beet Auction](https://www.wikiwand.com/en/Danish_Sugar_Beet_Auction), where MPC was used to
organize an auction for sugar beet production contracts without
revealing individual bids.

Another interesting application of MPC is allowing companies to
collaborate to train Machine Learning models on private data, without sharing
that data with each other. For example, several hospitals could
collaborate to train a model for identifying cancer, without having
sensitive health information leave the individual hospitals.

{{<note>}}
Now, the result of such a process would be a Machine Learning model,
and it can be surprisingly easy to use such a model to extract
information about the data it was trained on. See:
["Extracting Training Data from Large Language Models"](https://www.usenix.org/system/files/sec21-carlini-extracting.pdf)
as a recent example.

MPC means you'll only learn the result of your function $f$,
but that result can sometimes imply a lot of information about
the inputs involved. Beware.
{{</note>}}

## Our Specific Setting

So far we've seen MPC in the context of having an
arbitrary number of parties compute some arbitrary function $f$.
Garbled Circuits is a technique for doing MPC in the case
where there are only two parties.

### Boolean Circuits

This technique nonetheless works with an arbitrary function $f$,
although we need to assume a more concrete representation for
that function. We assume that $f$ can be represented as a
*boolean circuit*. You can think of this circuit as a bunch of
wires conneced to boolean gates like $\\&, \oplus$, etc. A
more formal definition would model this circuit as a *graph*.
The nodes would be the inputs or gates of the circuit, and the edges
would the wires connecting gates together.

{{<todo>}}
Image of a stereotypical graph goes here.
{{</todo>}}

I think the most useful representation for this post is related
to the idea of graphs, but not exactly the same. I like
to think of a circuit as a collection of labeled wires. Each
wire either comes from an input value, or from the output
of another gate, like in this example:

{{<todo>}}
Example image with wire splitting.
{{</todo>}}

You can also model this idea as a little programming language,
involving variables, reading input, and boolean operators:

```txt
let x = input(0)
let y = input(1)
let z = x & y
let w = y | w
return (z, w)
```

But models are less important than what you do with them, so let's
mosey on and have a look at that.

# Functions as Lookup Tables

The core idea that the Garbled Circuits technique is based on is
a simple observation: you can represent a function as a lookup table.
We can use this observation to evaluate a function at some input,
without learn what that input is.

To illustrate the idea, let's start with some function $f : \\{0, \ldots, n\\}^2 \to Z$.
We can create a table mapping each input to its corresponding output:

$$
F := \begin{vmatrix}
f(0, 0)&f(0,1)&\cdots&f(0, n)\cr
f(1, 0)&f(1,1)&\cdots&f(1, n)\cr
\vdots&\vdots&\vdots&\vdots\cr
f(n, 0)&f(n, 1)&\cdots&f(n, n)\cr
\end{vmatrix}
$$

In other words, we have $F_i^j := f(i, j)$.

To evaluate $f$ on the input $(x, y)$, all we have to do is lookup
the corresponding entry of table. The problem with this approach
is that we need to know the input in order to evaluate the function.
We'd like to avoid this.

The key step here is that instead of hold the input $x$ and $y$,
we instead have a keys $k^x$ and $k^y$. For each possible
value $i$ that $x$ can have, a key $k^x_i$ will be generated.
Then, instead of having a the raw table as before, we instead
have an encrypted table $F$, where each entry is defined as follows:

$$
F_i^j := \text{Enc}((k^x_i, k^y_j), f(i, j))
$$

Each entry is encrypted with the pair of keys corresponding to that
entry.

The idea is then that our pair of keys $k^x$ and $k^y$ correspond
to one of the pairs used to encrypt an entry in the table, but
we don't know which entry. So, we scan through the table, decrypting
every entry, until we actually succeed in decoding one of the entries.
When then use that as the result of our function.
The value of the keys $k^x$ and $k^y$ should look random, and
we shouldn't be able to learn anything about the actual input values
they correspond to. Since there's a different key for each of the
possible input values, we also need to somehow receive these keys
before evaluating our function; we'll set that aside for now.

## Shuffling Entries

One problem with encrypting the table as is, is that we know
what the original layout of the table was. If we successfully
decrypt output $(i, j)$, then we know that the input was
$(i, j)$, based on the layout of the table.

To address this, we need to shuffle the table. One simple way
to do this is to choose a random permutation $\sigma_x$ on
the rows, and a random permutation on the columns $\sigma_y$.
Our shuffled and encrypted table then becomes:

$$
F_i^j := \text{Enc}((k^x_{\sigma_x(i)}, k^y_{\sigma_y(j)}), f(\sigma_x(i), \sigma_y(j)))
$$

This prevents the location of the entry we manage to decrypt from
revealing information about what the input to the function was.

## Pointed Keys

Our current approach to finding the right output is to simply
try and decrypt every entry of the table. This isn't very elegant,
and certainly not very fast. There's a simple fix to this issue
however: include the row / column of the entry in each key.

So, instead of just having a key $k_i$ for the value $i$, we also
include a value used to locate the row / column of that value:
$\sigma^{-1}(i)$. Because the entry $(i, j)$ contains
the output $f(\sigma_x(i), \sigma_y(j))$, our key pair needs
to use the inverse permutation, giving us $(k^x, \sigma^{-1}_x(i))$ and
$(k^y, \sigma^{-1}_y(j))$ as our "pointed" key-pair.

By storing these points along with the keys, we avoid having to
decrypt all of the entries in the table, while still hiding
what the input value was.

# Walking on Wires

So far, we have this little technique to evaluate
some function $f : \\{0, \ldots, n\\}^2 \to Z$, without learning
the input. This technique works by associating each of the possible
values for the two
inputs with some opaque key, and then encrypting a lookup table
with those keys. To evaluate the function at a given input,
you have to magically have the right pair of input keys (we'll
get to this later), and then you can use the pointers attached
to the keys to lookup and then decrypt the output.

The big flaw in this approach is that our table will get quite
large very quickly. For example, if we're evaluating
a function on two 32 bit inputs, our lookup table will have
$2^{64}$ entries, which isn't great.

On the other hand, this technique is very convenient for
the case of a single boolean gate. In this case, we have
a function $f : \\{0, 1\\}^2 \to \\{0, 1\\}$, needing only
4 entries.

This would be fine if our circuit only had a single gate,
but that's not going to be the case. We need some way of chaining
the gates together, making the full circuit. Notice that
with our lookup table approach, the output value doesn't
really affect how we're doing things. Our initial description
was for some abstract type $Z$, and a boolean gate has
the output $\\{0, 1\\}$, but we could easily put something else
there. For example, we could put one of our fancy input keys
as the output:

$$
f : \\{0, 1\\}^2 \to \text{Key}
$$

This allows us to string together multiple gates. We want
the output of one gate to be used as the input to another.
For each input we need to have an encryption key, so if
our function spits out a key, we can then use that as
a perfectly valid input.

## In More Detail

Well, that's the idea, but I think the description could
be a bit clearer if we take a step back.

Our circuit has a set of wires in it. The value on each wire
can only come from one of two places:

- The wire is connected to the input of the circuit
- The wire is connected to the output of some other gate

# Delivering The Input

- How to get the keys to the user.
## Oblivious Transfer

{{<todo>}}
Explain how Oblivious Transfer works, at a high level, and then
link to an example with Simplest OT.
{{</todo>}}

# Summarizing

- Go over the protocol entirely.

# Conclusion
