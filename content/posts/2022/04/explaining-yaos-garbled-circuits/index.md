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
\begin{vmatrix}
f(0, 0)&f(0,1)&\cdots&f(0, n)\cr
f(1, 0)&f(1,1)&\cdots&f(1, n)\cr
\vdots&\vdots&\vdots&\vdots\cr
f(n, 0)&f(n, 1)&\cdots&f(n, n)\cr
\end{vmatrix}
$$

To evaluate $f$ on the input $(x, y)$, all we have to do is lookup
the corresponding entry of table.

## Pointed Keys

# Walking on Wires

# Delivering The Input
## Oblivious Transfer

{{<note>}}
I wasn't sure where to put this section, but felt it would be a simpler
read for most people if we took care of explaining the idea of Oblivious
Transfer first. You may not be among these people, so feel free to come
back to this section later if that works better for you.
{{</note>}}

{{<todo>}}
Explain how Oblivious Transfer works, at a high level, and then
link to an example with Simplest OT.
{{</todo>}}

# Summarizing

# Conclusion
