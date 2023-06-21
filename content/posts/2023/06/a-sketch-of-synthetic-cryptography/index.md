---
title: "A Sketch of Synthetic Cryptography"
date: 2023-06-17T21:31:27+01:00
katex: true
draft: true
tags:
  - "Category Theory"
  - "Cryptography"
  - "Foundations"
---

This is.

<!--more-->

# String Diagrams

To start, we'll first go over the general rules for manipulating diagrams
representing cryptographic games.
This presentation tries to be intuitive rather than overly formal,
so the precise definition or rules are less important than developing
a "feel" for how to play with the diagrams.
We leave a formal presentation of string diagrams in general
to other resources, such as {{<ref-link "PZ23">}}.

Our starting point will be a "process":

{{<img "01/001.png">}}

In other frameworks, one might call this a cryptographic game,
or a package.
Because our level of granularity is very fine, we simply refer to it
as a process instead.
A process is connected to several input wires, on the left,
and several output wires, on the right.
Each of these wires has a type, giving the process as a whole
a signature, which we'd write in this case as:
$$
A : X_1 \otimes X_2 \otimes X_3 \to Y_1 \otimes Y_2
$$
As this notation might suggest, we can think of a process
as a kind of function, taking in some inputs,
and producing some outputs.

Some important differences between a process and a function are:
- A process is *randomized*.
- A process may not use all of its inputs.
- A process may produce some outputs before others.

The last two points are actually quite important.
Another way to think of them is that wires connected
to a process are either "dead", carrying a dummy value $\bot$
indicating this, or "alive", then carrying a value
of the type that wire is.
A wire can change its value from dead to alive,
but once it is alive, it will never change its value again.

Each output wire has a set of input wires it depends
on, and as soon as all of these wires are alive,
the process will produce an output on *this* wire
in particular, in a randomized way.
Any given output wire may have their value
ready before other output wires.

If this doesn't make complete sense in the abstract, that's fine,
we'll be highlighting how this point shows up as necessary.

In the next subsections, we look at various rules for gluing
processes together, and manipulating diagrams of connected processes.

### Shorthand for Products

The type $A \otimes B$ denotes the type of "both $A$ and $B$".
Because of this, we can treat two wires as one, like this:
{{<img "01/002.png">}}
This just a notational convenience,
the meaning of the process is otherwise the same.

Sometimes processes will consume the wires split, and other times,
some processes will be written with the wires joined.
To convert between the two, we use the following bits of notation:
{{<img "01/004.png">}}
The first takes multiple wires, and combines them into one
wire with $\otimes$.
The latter does the opposite, taking one wire with some types connected
by $\otimes$, and separates them out into parts.
In isolation, these might be a bit ambiguous, but they
usually become clear in context.

As a shorthand, we also write $A^n$ to denote $A \otimes A \otimes \ldots \otimes A$,
$n$ times.

### Back-to-Back

The first way to connect two processes is via their wires:

{{<img "01/003.png">}}

In terms of the underlying math,
given processes $A : X \to Y$, and $B : Y \to Z$, we can define
the process:
$$
A \rhd B : X \to Z
$$

One crucial aspect of composition is that the order in which we compose
doesn't matter:

{{<img "01/005.png">}}

This is also suggested by the notation we use.
With our notation, it's not easy to distinguish between the two orders
of composing, which is fine, since the order doesn't matter!
This is the first in an instance of many notational choices in our
diagram system which reflect equivalences in the underlying processes.

### Side-by-Side

The other way to compose protocols is via *tensoring*, or, side-by-side:
{{<img "01/006.png">}}
In terms of the math, given
$A : X_1 \to Y_1$ and $B : X_2 \to Y_2$, we write:
$$
A \otimes B : X_1 \otimes X_2 \to Y_1 \otimes Y_2
$$

It's important to note that the point about output wires being
able to have values independently of each-other is important.
The output on $A$'s section of the combined process
will be ready as soon as the input on $A$ is,
and ditto for $B$.
Each half doesn't need to "wait" for the other half to be ready.

As with composition, the order in which we tensor doesn't matter,
as the notation suggests:

{{<img "01/007.png">}}

### Interchange

Composition and tensoring also play very nicely with eachother.
If you have processes $A, B, C, D$, boxed up like this:
{{<img "01/008.png">}}
Then there are two ways of looking at this diagram:
{{<img "01/009.png">}}
In the first way, you tensor first, then compose,
in the second way, you compose first, and then tensor.

The order you do these operations does not matter!
Once again, the notation expresses this fact by not reflecting
any differences in terms of the diagram.

### Sliding

Next, we introduce little gadgets we can use.
The first of these gadgets is "$1$", a process
which simply returns its input:
{{<img "01/010.png">}}
In math, we have $1_X : X \to X$ (for any type $X$).

The defining property is that $1$ composed with any other process,
in front, or in back, is just the same process:
{{<img "01/011.png">}}

Combining this property with that of interchange allows
us to "slide" processes along wires:

{{<img "01/012.png">}}

### Swapping

Next, we introduce another gadget, which is a process which
swaps its inputs:
{{<img "01/013.png">}}
Once again, the outputs here will be ready as soon as the corresponding
input is, without needing to wait for the other,
just as in an electrical circuit, or something like that.

One key property of swapping is that doing it twice is the same
as not swapping:
{{<img "01/014.png">}}
(As a consequence, swapping an even number of times does nothing.)

### Backwards and Forwards

Next, we also consider processes which may also have inputs
on the right, and outputs on the left:
{{<img "01/015.png">}}
Thus, on certain wires, information flows "backwards".
To make this clear, we make sure to write the direction on those arrows.

There are two fundamental gadgets which reverse the flow of arrows:
{{<img "01/016.png">}}
Intuitively, these just take the information on one wire and
put it on another, flowing in the other direction.

As one might expect, these gadgets satisfy a natural law:
{{<img "01/017.png">}}
In other words, changing the direction of flow twice does nothing.

### Copying and Deleting

The final gadgets we'll look at our processes which copy
and discard information:

{{<img "01/018.png">}}

We can think of the first process as copying its input,
and outputting it on two wires,
and the second process as ignoring its input,
producing no output.

These satisfy a few properties.

First, when we copy twice to get three outputs,
the order we do this in does not matter:
{{<img "01/019.png">}}
Second, when we copy, swapping the two outputs does nothing:
{{<img "01/020.png">}}
Finally, if we copy and discard either output,
the end result is the same as just returning the input:
{{<img "01/021.png">}}

### Summary

To summarize, processes are boxes, with inputs and outputs,
representing a randomized machine which can write values,
once, to its output wires, by reacting to inputs
provides on its input wires.

Inputs and outputs can appear on other side,
and we can move boxes around freely, as long as the connections
between inputs and outputs are preserved.

As an example of this kind of easy manipulation, the following
two diagrams represent the same process:
{{<img "01/022.png">}}

The notation we've chosen is such that it reflects this kind of equality
very easily, freeing us to not really have to think all that much
about these low level details.

# Properties and Rewrites

One crucial aspect of the calculus we're developing is that if
a process is equal to another in isolation,
then it will also allow us to substitute that process
for the other in an arbitrary context:
{{<img "02/001.png">}}
For the rules we've seen so far, we can apply them without
really caring about how many times we do so,
and assuming them to be "true".

In cryptography, often times we have rules that we want to play
around with, but that we don't assume to be absolutely true.
Rather, we care about what rules can be deduced from other rules.
For example, if some problem (e.g. RSA) is hard,
then we can build secure public key encryption.

In these deductions, we also care how many times we use a given
assumption.
This allows us to work backwards, and figure
out what parameters we need to use in the assumptions
to get enough security in the deductions.

So, to do so, we define the notion of a "property",
which is just a named rule.

As an example, consider the following property,
$\Pi[\text{AB}]$:
{{<img "02/002.png">}}

We also have deductions, which are of the form:
$$
\Pi_1 \times \Pi_2 \times \ldots \multimap \Pi
$$
The rule is that each assumed property on the left can only be used a single time in the deduction.
As a shorthand, we write $\Pi^n := \Pi \times \ldots \times \Pi$, $n$ times.

As an example deduction,
consider $\Pi[\text{Example}]$, defined via:
{{<img "02/003.png">}}

The following deduction holds:
$$
\Pi[\text{AB}]^2 \multimap \Pi[\text{Example}]
$$
As demonstrated by the following proof:
{{<img "02/004.png">}}
We use $\epsilon_1$ to denote which assumption we use, and
we do indeed use it (at most) twice.
(Another convention we use is that green is used to highlight
changes we've made, which has no impact on the actual semantics
of a diagram).

# Booleans

Now, we move from a very abstract context, and towards actual
concrete types that exist in cryptography.

The first such type we look at is that of *booleans*, or
the set $\\{0, 1\\}$, also written $\texttt{01}$ for convenience.

We define booleans via their effect on a selector function "$?$":
{{<img "02/005.png">}}
This function takes a condition, in its first input,
which it uses to select one of two inputs.
It takes the top one when the condition is true, and the bottom
one when the condition is false.


## Gates

The selector functions are sufficient to define logic gates.

For example, here's negation ($\neg$):
{{<img "02/006.png">}}

And, here's logical and ($\land$):
{{<img "02/007.png">}}

Naturally, we can then define all other logical operators
by combining these two operations.

We also define natural multivariate versions of these gates,
taking more than one input, by chaining them together.
E.g. the and of several variables is $((x_1 \land x_2) \land x_3) \land \ldots$.

## Equalities

Booleans can be produced by different processes,
but usually a boolean is produced as the result of some kind of comparison.
One very important kind of comparison we'll be needing is that of *equality*.

Some types are able to be compared for equality, in which case
the operator "$=$" is defined:
{{<img "02/008.png">}}

Many times, we'll want to compare multiple things
against a single thing, which we define via the operator "$=_0$":
{{<img "02/009.png">}}

This is also our first instance of defining a construction by induction.
We define the general case for $N$ inputs by looking
at the case with $0$ an inputs, and then at the case where
we have one additional input.

Finally, the last kind of equality we have will check
if any among a list of values is equal to another, via "$=_\exists$":
{{<img "02/010.png">}}
Another way of looking at it is that this operation checks
if there exists a collision between the variables.
The inductive definition says that a single variable
has no collisions,
and that checking a collision for $1 + N$ variables
amounts to checking if any of the $N$ are equal to the first one,
or collide amongst themselves.

### Equality of Copied Values

Another natural property of equality is that comparing duplicated
elements for equality will always return true:
{{<img "02/011.png">}}

# Randomness

What differentiates cryptography from other fields is ultimately that of
randomness.
In this section we define some basic properties
of randomness, which will be the foundation for the rest of the
schemes we see in this post.

Some types are said to be *sampleable*, when there exists
a process of the form:
{{<img "03/001.png">}}
This process should be understood as being a way of sampling a uniform
value of this type.

Furthermore, some types have some addition operation, $+$,
which as one might expect with randomness:
{{<img "03/002.png">}}
In other words, a random value added to any value is the same
as a fresh random value.

## Guessing

For some types, it's difficult to guess a value sampled at random,
without first seeing that value.

### Barriers

In order to formalize "without first seeing", we introduce the notion of barriers:
{{<img "03/003.png">}}
A barrier is simply a process, denoted by this bar, which
waits for *all* of its inputs before returning them on its outputs.

Here are some properties of barriers:

{{<img "03/004.png">}}

Intuitively, these capture the fact that a barrier can't be "crossed",
unlike a standard wire, but that a barrier only cares about the "dependencies"
of its wires.

### Back to Guessing

Now, having defined barriers, let's go back to the task we had before,
which is that of defining what it means to have values
which are hard to "guess" if sampled at random.
We call such types *large*,
as defined by the following property:

**Property:** For a type $X$, $\Pi[\text{Guess}(X)]$ is defined via:
{{<img "03/005.png">}}

$\square$

In other words, for large types, this property will hold.
We usually consider it as an explicit assumption though,
allowing us to keep track of how many times the assumption
is used, and thus choosing
a concrete size for the type for it to be large enough
in the context of a given system.

The barrier here is crucial, otherwise one could show this property
to never hold, by copying the random value
and using it as the guess.
The barrier prevents this, by forcing the guess to be chosen
before the random value is seen.

### Implied Equalities

We've shown a basic guessing property for a simple equality,
but what can we say about guessing properties
for more complicated equalities?

First, for $=_0$, we can set up the property $\Pi[\text{MultiGuess}(N)]$,
saying that guessing is hard, even with multiple guesses:
{{<img "03/006.png">}}

As one might expect, if guessing with one try is hard,
so is it with multiple tries.

**Claim:**
$$
\Pi[\text{Guess}]^N \multimap \Pi[\text{MultiGuess}(N)]
$$
**Proof:**

We proceed by induction.

First, we prove that $\Pi[\text{Guess}] \multimap \Pi[\text{MultiGuess}(1)]$:
{{<img "03/007.png">}}

Next, we prove that $\Pi[\text{Guess}] \otimes \Pi[\text{Guess}]^N \multimap \Pi[\text{MultiGuess}(1 + N)]$:
{{<img "03/008.png">}}

$\blacksquare$

This property will be useful in many contexts where we want
to use a random value more than once.

## Random Functions

Next, we develop a little gadget we'll be using a few times throughout this
post: random functions.
Random functions are useful to define the random oracle model of security,
and to define pseudo-random functions, which we'll use for encryption.

Mathematically, a random function $f : X \to Y$
is like sampling a value from that set of functions randomly.
The outputs of $f$ will be random, subject to the condition
that $x = x' \implies f(x) = f(x')$.

This perspective is a bit cumbersome in that it doesn't give us an easy
algorithmic recipe to construct such a random function.
We need a succinct way to do that.
A useful perspective here is that we can look at making a random
function a lazy mapping.
Each time we produce an output, we check if the input has been
seen before,
in which case we use a previous output,
otherwise we generate a fresh random output.

This leads us to the following definition of a random function
over $N$ inputs, $\rho$:
{{<img "03/009.png">}}
In other words, the first output will always be random,
and the subsequent outcomes will use previous outputs
if they match.

### Random Functions on Random Inputs

The outputs of a random function are basically random,
except if the inputs have collisions.
If the inputs are random, then collisions will be unlikely,
as the following property,$\Pi[\rho\text{Rand}(N)]$, claims:
{{<img "03/010.png">}}

This claim can be proven for large types.

**Claim:**
$\bigotimes_{i = N,\ldots,1}\Pi[\text{MultiGuess}(i)] \multimap \Pi[\rho\text{Rand}(N)]$

**Proof:**

By induction.

$\Pi[\text{MultiGuess}(1)] \multimap \Pi[\rho\text{Rand}(1)]$:
{{<img "03/012.png">}}

$\Pi[\text{MultiGuess}(1 + N)] \otimes \Pi[\rho\text{Rand}(N)] \multimap \Pi[\rho\text{Rand}(1 + N)]$:
{{<img "03/013.png">}}

$\blacksquare$

If we combine this with what we know about $\Pi[\text{MultiGuess}(N)]$,
we get that $\Pi[\text{Guess}]^{N^2} \multimap \Pi[\rho\text{Rand}(N)]$.

This is a very useful property, as it shows that applying
a random function to random inputs produces completely
independent random outputs as well.

# Encryption

Next, we turn our eyes towards symmetric-key encryption.
In this kind of encryption, both the sender and the receiver
share a random value: the key.
Using the key, there's an algorithm to encrypt a message,
producing a ciphertext.
Given a ciphertext, one can decrypt it using the key to recover
the message.
For security, one shouldn't be able to learn any information
about the message just by looking at the ciphertext.

More formally, an encryption scheme is a randomized function:
$$
E : K \otimes M \xrightarrow{\\$} \to C
$$
for a type of keys $K$, of messages $M$, and ciphertexts $C$,
along with a deterministic function:
$$
D : K \otimes C \to M
$$

We require the following correctness property of an encryption scheme:
{{<img "04/002.png">}}
In other words, if we use the same key to encrypt and decrypt
many messages, we get the same messages out that we put in:
we can recover messages through decryption.

## Indistinguishability

Informally, the security of encryption says that "no information"
can be extract from a ciphertext.
Formally, we model this via the property $\Pi[\text{IND-CPA}(N)]$:
{{<img "04/003.png">}}
This says that one can't tell the difference between decryptiong
one set of messages (on the top), or another set of messages
(on the bottom).
This means that no information about the messages leaks through the ciphertexts.


Often we refer to the property $\Pi[\text{IND-CPA}(1)]$
as $\Pi[\text{IND}]$.
Achieving the latter is much easier than achieving
the former for arbitrary $N$,
as we'll see later.

### Left or Right vs Real or Random

Another variant of security for encryption involves
comparing the encryption of a chosen set of messages,
and that of a random set of messages, as described by the property
$\Pi[\text{\\$IND-CPA}(N)]$:
{{<img "04/004.png">}}

This turns out to be equivalent.

The first direction is trivial:
**Claim:**
$$
\Pi[\text{IND-CPA}(N)] \multimap \Pi[\text{\\$IND-CPA}(N)]
$$
**Proof:**
{{<img "04/005.png">}}
$\blacksquare$

The second direction is a bit less trivial,
in that we need to use the assumption twice.

**Claim:**
$$
\Pi[\text{\\$IND-CPA}(N)]^2 \multimap \Pi[\text{IND-CPA}(N)]
$$
**Proof:**
{{<img "04/006.png">}}
$\blacksquare$

Because of this, we'll often use one of these properties as is more
convenient, since the difference between them doesn't really matter.

## One-Time Pad

Now, we construct an example of a scheme that satisfies
the $\Pi[\text{IND}]$ property.

To do so, we introduce the notion of a "binary" type.
This is a large type $X$, along with an operation
$\oplus : X \otimes X \to X$,
and an element $0 : X$, satisfying the following properties:
{{<img "04/007.png">}}

An example of this would be some set of binary strings $\\{0, 1\\}^\lambda$,
with the usual xor operation.
The rules above just say that $\oplus$ is an associative and commutative
operator, with the extra property
that any element $\oplus$ itself is the identity.
Also, we have a property related to randomness,
which says that xoring a random value with any other value results
in a random value.

This property is precisely what we'll use to make an encryption scheme.
The idea is that our keys, messages, and ciphertexts
will all be of type $X$, and we encrypt a message by xoring it with a key.

We can check that this satisfies the correctness property for encryption:
{{<img "04/008.png">}}
Furthermore, this is secure against a single query:
{{<img "04/009.png">}}

Against multiple queries, we do run into a problem though.
Intuitively, with multiple ciphertexts, we can calculate
the xor of two messages:
{{<img "04/010.png">}}
This obviously leaks information about the underlying messages,
which is not good.

## Deterministic Schemes aren't IND-CPA Secure

# Achieiving Multiple Encryptions

## Pseudo-Random Functions

## IND-CPA Encryption from PRFs

# Public-Key Encryption

## KEMs

## KEM-DEM paradigm

# KEMs from Group Assumptions

## Groups

## CDH Assumption

## ElGamal KEM

## Random-Oracle Model

## Proving Security

# Conclusion

# References

{{<ref
  "PZ23"
  "https://arxiv.org/abs/2305.08768"
  "[PZ23] An Introduction to String Diagrams for Computer Scientists">}}