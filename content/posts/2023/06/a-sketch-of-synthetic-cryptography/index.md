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

# Booleans

# Randomness

## Random Functions

# Pseudo-Randomness

# Encryption

# Public-Key Encryption

## KEMs

## KEM-DEM paradigm

# KEMs from Group Assumptions

## Groups

## CDH Assumption

## ElGamal KEM

## Random-Oracle Model

## Proving Security

# How To Formalize This

# Conclusion

# References

{{<ref
  "PZ23"
  "https://arxiv.org/abs/2305.08768"
  "[PZ23] An Introduction to String Diagrams for Computer Scientists">}}