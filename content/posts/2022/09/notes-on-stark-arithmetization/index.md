---
title: "Notes on STARK Arithmetization"
date: 2022-09-22T08:51:31+00:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "ZK Proofs"
---

I've been playing around with [STARKs](https://eprint.iacr.org/2018/046) lately,
meandering my way towards a toy implementation.
One of the things I struggled with initially was the way STARKs structure
their arithmetization: the way computation is laid out to make it easier
to prove things about.

While a bit hard to grasp at first, there was a definite moment
where my understanding clicked into place, and I finally "got" what
the arithmetization was all about, and how it worked.

Hopefully I can share a bit of this insight with you through this post.

# What is Arithmetization?

First, what is *arithmetization* anyways?

For any proving system, you need some way to represent your computation.
The way most computers end up representing computation is with
some kind of assembly language, which describes stateful operations
over registers and memory, at least with most architectures we use today.
These operations are usually defined over bits.

Most SNARKs, on the other hand, would rather work with operations
over large *fields*.
There are many reasons for this, but the basic reason is that it's
much easier to create succinct proofs using fields and polynomials,
because of the nice error-correcting properties they have.

The basic way to express computation using fields would be as
an *arithmetic circuit*.
You can think of this as a big expression graph, where the basic
operations are addition and multiplication.

While this circuit is enough to express any computation, the unstructured
nature of a raw circuit makes it difficult to implement SNARKs directly.
Instead, one usually choose a more structure format, like R1CS,
Plonkish arithmetization, etc.

# Execution Traces and Constraints

The kind of arithmetization used in STARKs is called an
*Algebraic Intermediate Representation* (AIR).
The starting point for an AIR is an *execution trace*.
Conceptually, this represents the evolution
of some computation through time.

More concretely, an exection trace is a matrix
of field elements, with $w$ columns and $T$ rows.
You can think of each column as a register,
and each row as a point in time.
The execution trace thus represents the value of
a set of registers at different points in time.

It's common to think of each register as
a function $f_i : [T] \to \mathbb{F}$, mapping
each time step to a field element.
We thus have $w$ column functions, $f_1, \ldots, f_w$.

When doing any kind of ZK proof, we care about
proving that some kind of statement holds.
This means that we want to prove that the execution
trace satisfies certain properties, which will
then convince us that a certain computation
was performed correctly.

These constraints come in two basic forms:
- Boundary Constraints.
- Transition Constraints.
Later in this post we'll see how to generalize
this to a broader kind of constraint, but for
now let's just focus on these two.

## Boundary Constraints

This kind of constraint asserts that a register
takes on a certain value at some point in time.
This is useful for asserting that the correct
input was used, or that the computation produced
a certain output, among other things.

Concretely, a boundary constraint is a tuple
$(i, j, \alpha)$, which represents the assertion:
$$
f_i[j] = \alpha
$$
In other words, the $i$-th register is equal
to $\alpha$ at time $j$.

## Transition Constraints

This kind of constraint is about the *evolution*
of the trace.
It asserts that two subsequent time steps of the
computation satisfy a certain relation.
One way this can be used is to check that a certain
program is being used to move the state forward.

Concretely, a transition constraint is
a polynomial $P$ over $2w$ variables.
This represents the assertion:
$$
\forall j \in [T - 1].\ P(f_1[j], \ldots, f_w[j], f_1[j + 1], \ldots, f_w[j + 1]) = 0
$$
In other words, at each time step, a certain relation
holds between the current state, and the next state.

In the common case that each state is a function
of the previous state, we can use a simple relation,
which checks that $s_{j + 1} = f(s_j)$.

That said, the transition constraints are actually
more general. For example, we could instead
check that $s_{j} = f(s_{j + 1})$, in effect
running the computation "backwards".

This bidirectionality is very useful.
For example, if we want to calculate the square root
of some number, we can do that outside of the trace,
and then assert that $s_{i + 1}^2 = s_i$,
which is simpler to check than $\sqrt{s_i} = s_{i + 1}$.

## Why both are necessary

The interplay between both kinds of constraints is what makes an AIR powerful.
Having just one of the constraints would make a anemic constraint system.

If you only had transition constraints, but no boundary constraints,
then it would be very difficult to enforce that the the computation
have a certain input or output.
You'd be able to control the relation between adjacent points in time,
but not specific values.

If you only had boundary constraints, it would be difficult to express
relations between values.
Also, you need one constraint at each point in the trace you want to control.
If you want to control some global property of the trace, then you
need one constraint at each point in time, which makes a lot
of constraints, and makes it difficult for the verifier to have
a speed advantage over the prover.

# Arithmetized Traces

In some sense, our current understanding of an AIR is complete.
You can use just the constraints we've defined so far, and capture any
program.

That said, you can in fact create more general constraints, which allows
expressing complicated computation with fewer constraints.
One thing you can do is to have a periodic constraint,
allowing you to claim that every 4th row in some column takes on a particular
value, for example.

To understand how these general constraints work, however, you need
to first understand how these traces are represented as polynomials
when implementing STARKs.

First, rather than having our columns be merely functions,
we'll have them be polynomials in $\mathbb{F}[X]$.
The way the interpolation works is that we take a generator $\omega$
of a subgroup $H \subseteq \mathbb{F}^*$, of order $T$.
Then, our polynomial $\tilde{f}_i$ will be such that:
$$
f_i[t] = \tilde{f}_i(\omega^t)
$$

So rather than having a column as a list of values $f_i[0], f_i[1], \ldots$,
our column is now a polynomial, and we get the values of the trace
by evaluating it at successive points: $f_i(\omega^0), f_i(\omega^1), \ldots$.

# Generalized Constraints

The general pattern of constraints in this framework is to assert
some property that must hold, along with where the property must hold.
For example, a boundary constraint says that a particular column
must have a certain value, but only at a particular point.
A transition constraint relates the value of columns between time steps,
but at every point.
A periodic constraint requires a column to have a particular value,
but only at a fixed period of steps.
Etc.

The way you express this is by using some expression involving the polynomials
to describe the what. e.g.

$$
f_1(X)^2 = f_1(\omega \cdot X)
$$

to say that the next value of a column is the square of the previous value.

Then, to describe where this value must hold, you use a polynomial
whose roots are those points.
So, if we want the above constraint to hold only at the first point in time,
we'd use the quotient $(X - \omega^0)$.

Then our constraint would be:

$$
f_1(X)^2 - f_1(\omega \cdot X) \equiv 0 \mod (X - \omega^0)
$$

If the polynomial on the left is $0$ at $\omega^0$, then the constraint
holds, and $(X - \omega^0)$ will divide it, since all the roots in this
quotient are also present in the constraint polynomial.

The way we check constraint like this in STARKs is by checking that:
$$
\frac{C(X)}{Q(X)}
$$
has low degree, which will only happen if $Q$ actually divides $C$,
essentially.

So, in general, we have some constraint $C$, which is a polynomial
expression involving all the $f_i(\omega^j X)$, to talk about
the values of the trace.
Using $\omega^j$ lets us relate two values in the trace that are separated
in time by $j$ steps.
We also have a quotient $Q(X)$, whose roots should be all the points
where the constraint has to be satisfied.

Let's see how this framework is used for various concrete constraints.

## Boundary Constraints

A boundary constraint $(i, j, \alpha)$ says that $f_i[j] = \alpha$.
In the language of polynomials, this becomes $f_i(\omega^j) = \alpha$.
Then, in our constraint framework, this becomes:

$$
f_i(X) - \alpha \equiv 0 \mod (X - \omega^j)
$$

In other words $f_i$ has to equal $\alpha$ at $\omega^j$.

## Transition Constraints

For transition constraints, we use a polynomial $P$ to relate
the values of a trace at adjacent points in time.
To relate these two sets of values, we multiply by $\omega$.
In terms of where this constraint needs to hold, we want it to hold
everywhere, except the last point, $\omega^{T - 1}$ (we want to avoid wrapping around, usually).

This is where the fact that $\omega$ generates a group of size $T$ is very
useful.
This means that $\omega^0, \ldots, \omega^{T - 1}$ are all of the values
such that $\omega^T = 1$.
In other words, the polynomial $X^T - 1$ has as roots exactly these powers
of omega!

This allows us to write our transition constraint thusly:

$$
P(f_1(X), \ldots, f_w(X), f_1(\omega \cdot X), \ldots, f_w(\omega \cdot X)) \equiv 0 \mod (X^T - 1) / (X - \omega^{T - 1})
$$

Crucially, the quotient polynomial is succintly expressable, which
is important to make the verifier fast.

## Periodic Constraints

Finally, to illustrate how this framework can generalize beyond
the standard constraints we've seen so far, let's look at how periodic
constraints might work.

The technique we use exploits the fact that if $\omega$ has order $T$,
then $\omega^k$ should have order $T / k$, provided $k$ divides $T$.
This means that the polynomial $(X^{T / k} - 1)$ would have a root
at every $k$-th row.

This allows us to express a period constraint as:

$$
f_i(X) - \alpha \equiv 0 \mod (X^{(T / k)} - 1)
$$

We can also use $f_i(\omega^j \cdot X)$ to shift the rows where we
want the constraint to happen.

# What Costs You?

We've discussed the different kinds of constraints you can hav
in a generalized AIR, but only briefly mentioned what makes some
constraints more expensive than others.

Here's the basic rundown:

- The quotient polynomial needs to have a short description.
- The constraint polynomial needs to have low degree.

It's important that the quotient polynomial have a short description,
because the verifiers needs to be able to evaluate it quickly.
Something like $(X^T - 1)$ is fine, because you can evaluate $X^T$
at a point in $\lg T$ steps.
On the other hand, if the polynomial were described as
$(X - \omega^0)(X - \omega^1)\ldots$,
then that would be problematic, since the verifier would need to
do $T$ steps, removing its logarithmic advantage.

The constraint polynomial should have low degree, mainly to lower prover
costs.
In essence, the prover needs to do work proportional to this degree.
So a constraint like:

$$
f_0(X)^4 - \alpha
$$

is 4 times more expensive than:

$$
f_0(X) - \alpha
$$

It's better to have many constraints of low degree than a single constraint
of large degree.
In practice, all of the constraints get coalesced during the protocol.

# A Comparison with (Turbo)-PLONK

Let's compare this, briefly, with [(Turbo)-PLONK](https://docs.zkproof.org/pages/standards/accepted-workshop3/proposal-turbo_plonk.pdf),
which I am admittedly less well-versed in.

If you read the document linked above, you'll notice that the idea
of an execution trace is also present.

One main difference is that PLONK has the notion of *selectors*
which aren't, by themselves, present in the AIR arithmetization.

Next, rather than having quotient polynomials which indicate where
a constraint should hold, PLONK instead uses wiring constraints.
These allow for computation with less repeated structure, because you
can have arbitrary gaps in your wiring, whereas AIR really needs
a regular constraint, in order to have a simple quotient.

Another difference is the use of selectors, which lets PLONK merge
several constraints into a single one, by using the selectors
to indicate which constraint is "active".

AIR instead opts to use quotient polynomials here, which is essentially
like having a selector with a regular pattern of occurrence,
rather than being able to occurr arbitrarily like with PLONK.

# Conclusion

I think the best resource for really diving deep into how STARKs
work is the [ethSTARK paper](https://eprint.iacr.org/2021/582),
and is probably what solidified my own knowledge the most.

Otherwise, there are plenty of more introductory material on STARKs.
In particular, I like the [STARK Anatomy](https://neptune.cash/learn/stark-anatomy/) series of posts by Alan Szepieniec.