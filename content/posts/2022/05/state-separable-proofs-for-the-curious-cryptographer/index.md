---
title: "State-Separable Proofs for the Curious Cryptographer"
date: 2022-05-20T18:02:59+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Foundations"
---

This blog post is an introduction to *state-separable* proofs,
a technique for proving the security of Cryptographic proofs.

<!--more-->

The target audience for this post is me, a month ago. As in,
I expect the reader to have some background knowledge
around game-based security in Cryptography. The goal of
this post is to show people familiar with proving security
using games what state-separable proofs bring to the table,
and how they offer a neat way of making proof writing
a lot easier.

If you're not familiar with game-based security, I'm not sure
if this post will be easy to understand, but I did
write [a list of Cryptography book recommendations](/posts/2022/05/some-cryptography-books-i-like/) a couple weeks ago, so that
may be a good starting point if you'd like to learn more. I'm
also planningon writing a more beginner-friendly introduction
to provable security, so keep your eyes peeled for if I follow
through on that promise.

# Introduction

The security of Cryptographic schemes is usually reasoned
about with games. In these games, an adversary tries to
break some scheme, by interacting with a challenger making
use of that scheme. State-separable proofs are a technique
to make write this style of proof simpler.

The gist of the technique is that it allows you to turn a complicated proof
into a series of much simpler steps. This is done
by decomposing large monolithic games, into smaller,
more composable snippets of code, which we call *packages*.

## Why We Care

Personally I care about state-separable proofs because
I've found them to be much easier to work with. Hopefully
I can impart some of this feeling onto you, but ultimately
this is somewhat subjective.

A more objective argument in favor of this technique is that
it makes it much easier to create reusable proof techniques.
With traditional game-based proofs, certain arguments,
like hybrid arguments, are often repeated each time they're used.
You remember the basic technique of a hybrid argument, but you
have to spell it out each time you want to use it.

With state-separable proofs, a lot more of the mechanics of
writing proofs is encoded in the formalism you use. More steps
of the proof correspond to concrete actions with the mathematical
objects representing various components of games, if that makes sense. This means that it's a lot easier to formulate
things like a hybrid argument in a way which is generic,
and thus immediately reusable in different contexts.

Continuing with this example, in this post we'll see
a "generic hybrid argument" which just requires a few bits
of structure to instantiate, and then the result can immediately
be reused, without having to invoke or reuse the details
of the argument.

I've been somewhat vague in this section, but hopefully
the rest of this post will be able to better illustrate
how this reusability plays out in practice.

## Overview

In the rest of this post, we'll first briefly review
how traditional game-based proofs work, and then learn the formalism
we use for state-separable proofs. We'll then see
how reductions work with this technique, using
the example of reduction an encryption scheme to the
security of a pseudo-random-function (PRF).
We'll then see how to make a generic hybrid argument,
applying that to multiple vs single-query encryption security.
Finally, we'll conclude with an example on hybrid encryption,
which lets us see how to model "computational problems"
like the computational Diffie Hellman problem (CDH),
and how random oracles work with this technique.

# State-Separable What?

I've praised the benefits of state-separable proofs quite
a bit already, so you might be impatient to learn
exactly what it is I'm talking about.

What they boil down to is really a formalism for games,
which lends itself better to composability. They're
sort of an alternative to game-based security, in the
sense that they have their own formalism for various
aspects of that technique. On the other hand,
the strategy for proving things is still somewhat similar,
so in other sense state-separable proofs are
more of an extension of traditional game-based proofs.

## Security Games

Before we get to state-separable proofs, it might be useful
to recall how traditional game-based proofs work.

The basic idea is that the security of some scheme, say,
encryption, takes the form of a *game*. This game is played
between a challenger, $\mathcal{C}$, and an adversary $\mathcal{A}$.
The challenger sort of resembles the user of a scheme, in
that they have private information, like keys, associated
with the scheme. The adversary plays the role of a malicious
actor trying to break the scheme.

These two players interact, and at the end of the game,
it's clear whether or not the adversary has won.

As an example, you might have a game where an adversary
can ask the challenger to encrypt messages, and the goal
of the adversary is to guess the secret key.

There are different ways to formalize and write this game
down. One way I liked was to write down what messages
get exchanged, like this:

$$
\boxed{
\begin{aligned}
&k \xleftarrow{R} \mathcal{K}\cr
&&\xleftarrow{m_i}\cr
&c_i \gets \text{Enc}(k, m_i)\cr
&&\xrightarrow{c_i}\cr
&&\xleftarrow{\hat{k}}\cr
&\text{win} \gets \hat{k} = k\cr
\end{aligned}
}
$$

The challenger is sort of like a box, and the adversary
interacts with them by sending and receiving messages.

{{<todo>}}
Illustration
{{</todo>}}

Since the adversary can win, and the game involves randomness,
we usually talk about the *advantage* of an adversary
based on the probability that they win the game. Sometimes
you can win relatively often by just playing randomly,
so we often subtract this "trivial win rate" in order to get
the advantage.

In this example, we might define the advantage as:

$$
\text{Adv}[\mathcal{A}] := |P[\text{win} = 1| - |\mathcal{K}||
$$

Sometimes security involves several games, and then the
advantage is defined as something like:

$$
\text{Adv}[\mathcal{A}] := |P[\text{out} = 1\ |\ \text{Game}_0] -
P[\text{out} = 1\ |\ \text{Game}_1] |
$$

The idea in this case is that you want to capture how
well the adversary does at distinguishing between the two games.
For example, can the adversary distinguish
a real encryption scheme from a perfect scheme
which just returns random ciphertexts.

Reductions work by taking an adversary for one game,
and creating an adversary for a different game.
This often takes the form of what I call a "three-column proof"
where you have the challenger on the left, the adversary
on the right, and then some wrapping code in the middle:

$$
\boxed{
\begin{aligned}
&k \xleftarrow{R} \mathcal{K}\cr
&&&&\xleftarrow{m_i}\cr
&&&m_i \gets m_i \oplus 1&\cr
&&\xleftarrow{m_i}\cr
&c_i \gets \text{Enc}(k, m_i)\cr
&&\xrightarrow{c_i}\cr
&&&&\xrightarrow{c_i}\cr
&&&&\xleftarrow{\hat{k}}\cr
&&\xleftarrow{\hat{k}}\cr
&\text{win} \gets \hat{k} = k\cr
\end{aligned}
}
$$

The idea is that the wrapping code takes the adversary on
the right, and then plays with its messages, in order
to play the game on the left. But this is just a convenient
formalism to describe an adversary for the game on the left.
You could describe it in other ways, and there's not
really a standard way of describing this process of
creating new adversaries when doing reductions.

For many schemes, you'll need multiple reductions,
since the security of the scheme might depend on
several different assumptions. For example, 
a public key encryption scheme might depend on a symmetric
encryption scheme, and also the RSA assumption.

## Packages

With state-separable proofs, we decompose large security games into
smaller units, called *packages*. A package consists of
pseudo-code. This is similar to the kind of code we use
to describe games.
How we interpret this code isn't very important.
We can give it a precise semantics with Turing machines,
or some other formalism, but all that matters is that
we agree on what the code means.

Part of this code is dedicated to the *state* of the package,
with code dedicated to defining and initializing this state.
The rest of this package is dedicated
to *functions*. As an example, consider this package:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\mathcal{L}_F$
}\cr
\cr
&k \xleftarrow{R} \\{0, 1\\}^\lambda\cr
\cr
&\underline{\mathtt{F}(m):}\cr
&\ \texttt{return } m \oplus k\cr
\end{aligned}
}
$$

The state of this package is the variable $k$. The set of functions exposed
by this package is $\\{\texttt{F}\\}$. We
say that the *exports* of $\mathcal{L}_F$ are $\text{out}(\mathcal{L}_F) = \\{\texttt{F}\\}$. Before any of the functions
in this package can be called,
the initialization
code is executed. This will set $k$ to a (uniform) random value in $\\{0, 1\\}^\lambda$.

Packages are not limited to just *exporting* functions. They can
also *import* functions. For example:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\mathcal{L}_G$
}\cr
\cr
&k \xleftarrow{R} \\{0, 1\\}^\lambda\cr
\cr
&\underline{\mathtt{G}(m):}\cr
&\ \texttt{return } \texttt{F}(m) \oplus k\cr
\end{aligned}
}
$$

This package is similar to the previous one, except that it relies
on a function $\texttt{G}$, which isn't defined inside of the package.
Instead, this package depends on some external function. Think of
this like a computer program which depends on another library.
We say that the *imports* of $\mathcal{L}_G$ are ${\text{in}(\mathcal{L}_G) =
\\{\texttt{G}\\}}$.

## Composition

Since $\mathcal{L}_G$ imports the function $\texttt{F}$, and
$\mathcal{L}_F$ exports that same function,
a natural construction would be to
create a larger package by linking the two packages together. Whenever
$\mathcal{L}_G$ would make a call to $\texttt{F}$, the code inside
of $\mathcal{L}_F$ would be executed. This linking is defined
as *sequential composition*.

Whenever we have two packages $A$ and $B$, such that $\text{in}(A) \subseteq \text{out}(B)$, we can define their sequential composition:

$$
A \circ B
$$

This package has the same exports as $A$, with $\text{out}(A \circ B) = \text{out}(A)$,
and the same imports as $B$, with $\text{in}(A \circ B) = \text{in}(B)$.

This composition defines a new package whose state is the combination
of the states in $A$ and $B$. We replace a call to a function in $B$
by inlining the code of that function directly inside of $A$.

Let's take the example of $\mathcal{L}_G$ and $\mathcal{L}_F$.
One slight issue is that their internal state $k$ shares a name,
so simply inlining the code in $\mathcal{L}_F$ wouldn't work. To get
around this, one convention I like to use is that everything inside
of a package is implicitly namespaced by that package. So the $k$
inside of $\mathcal{L}_F$ is really $\mathcal{L}_F.k$, and the
$k$ inside of $\mathcal{L}_G$ is also shorthand for $\mathcal{L}_G.k$.
The same goes for function names.
With this convention, we can explicitly describe their composition:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\mathcal{L}_G \circ \mathcal{L}_F$
}\cr
\cr
&\mathcal{L}_F.k \xleftarrow{R} \\{0, 1\\}^\lambda\cr
&\mathcal{L}_G.k \xleftarrow{R} \\{0, 1\\}^\lambda\cr
\cr
&\underline{\mathtt{G}(m):}\cr
&\ \texttt{return } m \oplus \mathcal{L}_F.k \oplus \mathcal{L}_G.k\cr
\end{aligned}
}
$$

Instead of the call to $\texttt{F}$ we had before, we've now inlined
that code directly in the package. To resolve ambiguities in the variable
names, we've also explicitly written them down with their namespace.

### Associativity

One interesting aspect of composition is that it's *associative*. In other
words, $(A \circ B) \circ C$ is the exact same package as $A \circ (B \circ C)$. The order in which you inline function definitions doesn't matter.
When you first inline the definitions in $B$, and then those in $C$,
this yields the result as first inlining those in $C$, and then inlining
all of that inside of $A$.

Now, I've said that these packages are "the same", but I mean this
in a specific way. In this case I mean that $(A \circ B) \circ C$
and $A \circ (B \circ C)$ are the exact same package, the state and
code are all exactly the same, up to a potential renaming of variable
and function names. We'll refer to this kind of relation
between packages as *definitional equality*, and denote it
by $\equiv$. Continuing with this example, we have:

$$
(A \circ B) \circ C \equiv A \circ (B \circ C)
$$

Another silly example is that if we append the word $\text{foo}$ to every
name inside of the package $A$, in order to get the package $A_{\text{foo}}$,
then we'd have:

$$
A \equiv A_{\text{foo}}
$$

because definitional equality doesn't care about variable or function names.

## Parallel Composition

So far we've seen how to link packages together, using the exports
of one package to satisfy the imports of another. We called this *sequential composition*.
There's another form of composition which isn't as useful, but is still
interesting to look at. This is *parallel composition*. The idea is that
if the functions exported by two packages are distinct, then we can
form a new package by putting the state of these packages
side-by-side, and
exporting all the functions these packages provide. We denote
this composition by $\begin{matrix}A\cr \hline B \end{matrix}$ or $A \otimes B$.

As an example, we have:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $A$
}\cr
\cr
&k \xleftarrow{R} \mathbb{Z}/(q)\cr
\cr
&\underline{\mathtt{F}(m):}\cr
&\ \texttt{return } m + k\cr
\end{aligned}
}
\otimes
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $B$
}\cr
\cr
&k \xleftarrow{R} \mathbb{Z}/(q)\cr
\cr
&\underline{\mathtt{G}(m):}\cr
&\ \texttt{return } m \cdot k\cr
\end{aligned}
}
\equiv
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $A \otimes B$
}\cr
\cr
&A.k \xleftarrow{R} \mathbb{Z}/(q)\cr
&B.k \xleftarrow{R} \mathbb{Z}/(q)\cr
\cr
&\underline{\mathtt{F}(m):}\cr
&\ \texttt{return } m + A.k\cr
&\underline{\mathtt{G}(m):}\cr
&\ \texttt{return } m \cdot B.k\cr
\end{aligned}
}
$$

The parallel composition combines both states, and exposes both of the functions.

More formally, this composition is well defined when
$\text{out}(A) \cap \text{out}(B) = \emptyset$, and gives us:

$$
\begin{aligned}
\text{out}(A \otimes B) &= \text{out}(A) \cup \text{out}(B)\cr
\text{in}(A \otimes B) &= \text{in}(A) \cup \text{in}(B)\cr
\end{aligned}
$$

One property of parallel composition which is apparent from the definition
is that it's associative and commutative. You have:

$$
(A \otimes B) \otimes C \equiv A \otimes (B \otimes C)
$$

and

$$
A \otimes B \equiv B \otimes A
$$

### Combining Sequential and Parallel Composition

These two notions of composition actually work quite well together.
If you have 4 packages $A, A', B, B'$, then you have:

$$
\frac{A}{A'} \circ \frac{B}{B'} \equiv \frac{A \circ B}{A' \circ B'}
$$

provided that the individual compositions are well defined, based on the imported and exported functions.

This equality holds because inlining one function doesn't affect
any of the other functions. This equality is very useful
when linking packages defined via parallel composition.

## Adversaries

So far, we've defined packages which import and export functions,
and can be composed together in various ways. While this is very neat,
it's not yet "praktisch" (as they'd say in German); we're still
pretty far away from capturing the notion of security, as we
did with the more traditional game-based definition. To do this, we
need to introduce a few more notions based on packages.

The first notion is that of a *game*. A game is simply a package $G$
with no imports, i.e. $\text{in}(G) = \emptyset$. This captures
the idea that a game is something you can interact with directly.
If the game had any imports, there wouldn't be a well defined notion
of what it does, because the imported functions could significantly
change the behavior.

The second notion is that of an *adversary*. An adversary is a package $\mathcal{A}$
with many potential imports, but only a single exported function. This
function, commonly named $\texttt{run}(): \\{0, 1\\}$ is used to execute the adversary.

The idea here is that each adversary plays against some interface of functions
it can interact with. When we link an adversary $\mathcal{A}$ with
a game $G$ implementing the right interface, we end up with a package
$\mathcal{A} \circ G$, exposing a single function $\texttt{run}$.
Executing this function makes the adversary and the game interact,
and finishes with a single bit $0$ or $1$. In fact, because
of the random choices taken by these packages, we have a probability
distribution over these outputs.

### Advantages

This leads us to the natural notion of *advantage*. The idea is that
we have a pair of games $G_0$ and $G_1$, with $\text{out}(G_0) = \text{out}(G_1)$. The advantage of an adversary $\mathcal{A}$ interacting with $G$
tells us how well that adversary can distinguish $G_0$ from $G_1$.

We define the *advantage* of such an adversary as:

$$
\epsilon(\mathcal{A} \circ G_b) := |P[1 \gets \mathcal{A} \circ G_0] - P[1 \gets \mathcal{A} \circ G_1]|
$$

We look at the probability that $\texttt{run}$ returns $1$ in either situation,
and the advantage measures how well the adversary is able to separate
the two cases.

One useful game I like to define is $?_b(A, B)$, which behaves
like $A$ when $b = 0$, and $B$ otherwise. In other words:

$$
\begin{aligned}
?_0(A, B) &\equiv A\cr
?_1(A, B) &\equiv B
\end{aligned}
$$

{{<note>}}
I've referred to $?_b(A, B)$ as a "game", but strictly
speaking it's a pair of games. These two notions are often
conflated, without much harm.
{{</note>}}

### Equality

Sometimes games might have different code, but behave
in exactly the same way.

For example, consider these two games:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $G_0$
}\cr
\cr
&k \xleftarrow{R} \\{0, 1\\}^\lambda\cr
\cr
&\underline{\mathtt{F}(m):}\cr
&\ \texttt{return } m \oplus k\cr
\end{aligned}
}
\quad \quad
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $G_1$
}\cr
\cr
&k_0, k_1 \xleftarrow{R} \\{0, 1\\}^\lambda\cr
\cr
&\underline{\mathtt{F}(m):}\cr
&\ \texttt{return } m \oplus k_0 \oplus k_1 \cr
\end{aligned}
}
$$

It's not true that $G_0 \equiv G_1$, because the code is cleary
different, even after potential renaming. On the other hand,
the result is the same, because $k_0 \oplus k_1$ has the same distribution
as $k$.

Capturing what it means to have "the same behavior" is tricky. One way
to do this is to lean on the notion of advantage we've just defined.

We say that two games $A, B$ are *equal*, denoted $A = B$, when
for any unbounded adversary, we have:

$$
\epsilon(\mathcal{A} \circ ?_b(A, B)) = 0
$$

In other words, no adversary, even with an unbounded amount of computation,
can distinguish between the two games. This captures the intuitive idea
that their behavior is the same.

In our previous example, we had $G_0 = G_1$, because the distributions
were indeed the same.

In most situations, $=$ is much more useful than $\equiv$, since it's
more robust to unimportant changes, like moving variables around,
or replacing a complicated expression with something having the same
distribution. In fact, outside of the basic definitions, $\equiv$
is rarely needed, and $=$ is used instead.

### Indistinguishability

Now, this notion of equality is much too strong. It requires games
to look exactly the same, even under the scrutiny of an adversary
with no limits on their computation time. There's not a lot
of Cryptography which can be done under these constraints.

We relax this notion by only considering
"efficient" adversaries. These adversaries have a runtime polynomial
in some ambient security parameter $\lambda$. In fact, when
we refer to "adversaries" we usually mean "efficient adversaries".

This leads us to the notion of $\epsilon$-indistinguishability.
We say that two games $A$ and $B$ are $\epsilon$-indistinguishable
with respect to $\mathcal{A}$,
or $A \stackrel{\epsilon}{\approx} B$ when we have

$$
\epsilon(\mathcal{A} \circ ?_b(A, B)) \leq \epsilon
$$

(the adversary is usually left implicit)

In other words, this adversary can only
distinguish the two games with advantage at most $\epsilon$.

One useful property of this definition comes from the triangle inequality:

$$
A \stackrel{\epsilon_1}{\approx} B, \ B \stackrel{\epsilon_2}{\approx} C
\implies A \stackrel{\epsilon_1 + \epsilon_2}{\approx} C
$$

This property allows us to do "game-hopping" where we chain small
differences together to examine the indistinguishability of a larger
game.

Finally, we say that two games are *indistinguishable* when
for every efficient adversary $\mathcal{A}$, there exists an $\epsilon$,
which is a negligeable function of $\lambda$,
such that these games
are $\epsilon$-indistinguishable.

# Defining Security

This notion of *indistinguishability* is what we'll use to
define the security of different constructions.
Whenever we have a construction, like a signature scheme,
or an encryption scheme, and we want to reason about it's
security, we'll do so with a pair of games $G_b$. The scheme
will be considered secure if these games are indistinguishable.

One slight difference with game-based security is that we'll
have a strong preference for games in a "real vs ideal" style.
We'll want one game to be based on interacting with our
actual scheme, and another to be based on interacting with
an ideal version of our scheme.

For example, with encryption, you might consider a real
game where you use your cipher, as compared to an ideal
game, where encryption returns a random ciphertext.
If you can't distinguish between the two, then the encryption
scheme is secure.

With traditional game-based security, sometimes you have other
styles of game definition which also work. For example,
the $\text{IND}$ notion security for encryption involves
asking for the encryption of one of two messages. In one game,
you get the left message, in the other, the right message.

The reason we prefer real vs ideal is that it allows us
to consistently define games in this paradigm, which
is much more amenable to composition. Using small ideal components,
you build up a large ideal construction.

# Reductions

Taking our notion of security at face value, to prove
that a scheme is secure, we'd prove that two games $G_0, G_1$
are indistinguishable. To do that, we'd need to show
that for every adversary $\mathcal{A}$, we have
$\epsilon(\mathcal{A} \circ G_b) \leq \epsilon$, for some negligeable $\epsilon$.

Proving that every adversary has a negligeable advantage is
essentially impossible for anything but the simplest of schemes.
In fact, in many cases, such a proof would immediately
lead you to conclude that $P \neq \text{NP}$, which a lot
of people have spent a considerable amount of time trying
to prove.

What you do instead is rely on the hardness of some
other problem. For example, you may assume that factoring
integers is hard, and use that to build an encryption scheme
which is also hard.

The way this linking is done, is via reductions. A reduction
uses an adversary $\mathcal{A}$ for some game $A_b$
to build other adversaries $\mathcal{B}_i$
for games $B^i_b$. If $A_b$ is broken, then all of the $B^i_b$
games are also broken. Conversely, if all of these games
are secure, then $A_b$ must be as well.

More concretely, a reduction is a statement (and corresponding proof) of the form:

For all adversaries $\mathcal{A}$ against $A_b$, there
exists adversaries $\mathcal{B}_i$ against $B^i_b$ such that:
$$
\epsilon(\mathcal{A} \circ A_b) \leq f(\epsilon(\mathcal{B}_0 \circ B^0_b), \ldots, \epsilon(\mathcal{B}_N \circ B^N_b))
$$

Now, $f$ is usually some very simple function like $f(x, y) = 2 \cdot x + y$, or something like that. It should be the case
that if the arguments to $f$ are negligeable, then so will
its output be.

This equation has the nice property that if the right side
is negligeable, then so is the left side. In other words,
if all the games on the right are secure, then so is the game
on the left. Conversely, if the game on the left is broken,
then one of the games on the right is too.

A shorthand notation I like to use for this statement is:

$$
A_b \leq f(B^0_b, \ldots, B^N_b)
$$

This is just shorthand for the above statement about adversaries
and advantages, and allows a concise overview of the relation
between different games.

## Building Reductions

Our notion of reduction is basically the same as
with traditional game-based security, so it isn't exactly
clear what advantage is gained by using state-separable proofs.

This advantage comes from *associativity*.

Let's say we have an adversary $\mathcal{A}$ for $G_b$,
and we want to use this adversary to attack $H_b$.
To do that, we need to build an adversary $\mathcal{B}$
against $H_b$. What we
can do is build a shim package $S$, such that $\text{in}(S) = \text{out}(H_b)$ and $\text{out}(S) = \text{in}(G_b)$. We then have
a game:

$$
S \circ H_b
$$

The advantage of $\mathcal{A}$ against this game is:

$$
\epsilon(\mathcal{A} \circ (S \circ H_b))
$$

But, because of associativity, this is the same quantity as:

$$
\epsilon((\mathcal{A} \circ S) \circ H_b)
$$

We can see this situation as either $\mathcal{A}$ playing against
$(S \circ H_b)$, or the adversary $(\mathcal{A} \circ S)$
playing against $H_b$.

In this example, our reduction would proceed straightforwardly:

$$G_0 = S \circ H_0 \stackrel{\epsilon_0}{\approx} S \circ H_1 = G_1$$

with $\epsilon_0 := \epsilon(\mathcal{B} \circ H_b)$,
and $\mathcal{B} := (\mathcal{A} \circ S)$.

From this equation, we see that
$G_0 \stackrel{\epsilon_0}{\approx} G_1$, and thus conclude that:

$$
\epsilon(\mathcal{A} \circ G_b) \leq \epsilon(\mathcal{B} \circ H_b)
$$

or, in shorthand:

$$
G_b \leq H_b
$$

This example really captures the essence of what makes
state-separable proofs nice. Our reductions will be made
through the use of clever shim packages like $S$,
which implicitly define new adversaries for us to use. Our
full reduction then becomes a matter of constructing
a series of small hops, to navigate our way from $G_0$ to $G_1$.

In traditional game-based security, you often need to
explicitly construct the adversaries you use in a reduction,
which is more cumbersome, and less amenable to the local reasoning
you get with state-separable proofs.

In the rest of this post, we'll go over several more examples
of how reductions work, so hopefully that will clarify
things quite a bit more, and give you an even better feel
for how reductions work in practice.

# Example: Encryption with a PRF

As an example, let's consider the case of using a Pseudo-Random Function (PRF)
in order to build an encryption scheme secure under chosen plaintext attack.

### PRFs

A PRF $F : \mathcal{K} \times \mathcal{X} \to \mathcal{Y}$ is function
from $\mathcal{X} \to \mathcal{Y}$ which also takes in a key. The idea
is that if you don't know the key, you shouldn't be able to tell this
function apart from a random function. We capture this notion of
security through a pair of games:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{PRF}_0$
}\cr
\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{QueryF}(m : \mathcal{X}): \mathcal{Y}}\cr
&\ \texttt{return } F(k, m)\cr
\end{aligned}
}
\quad \quad
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{PRF}_1$
}\cr
\cr
&\text{out}[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{QueryF}(m : \mathcal{X}): \mathcal{Y}}\cr
&\ \texttt{if } m \notin \text{out}:\cr
&\ \quad \text{out}[m] \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } \text{out}[m] \cr
\end{aligned}
}
$$

In one game, we actually query the PRF, and in the other, we're querying
a random function. We say that the PRF $F$ is secure if
$\text{PRF}_0$ and $\text{PRF}_1$ are indistinguishable.

### Encryption

A symmetric encryption scheme consists of two functions:

$$
\begin{aligned}
E &: \mathcal{K} \times \mathcal{M} \xrightarrow{R} \mathcal{C}\cr
D &: \mathcal{K} \times \mathcal{C} \to \mathcal{M}
\end{aligned}
$$

Both of them take a key. One encrypts a message with a key,
producing a ciphertext, and the
other decrypts a ciphertext with a key, producing a message.
Encryption can be randomized, while decryption should be deterministic.

For the scheme to be considered correct, encrypting a message and then
decrypting the ciphertext should return the same message.

In terms of security, the intuitive idea is that an adversary shouldn't
be able to distinguish between an encryption of an actual message and
an encryption of a random message.

Using a real vs a random message is often referred to as $\text{IND}\text{\textdollar}$, with $\text{IND}$ being reserved for a variant of the game
where you submit two messages, and receive the encryption of one of the messages.

With state-separable proofs, you really want to use "real vs ideal" as the
defining principle for all of your games, since that makes composition
a lot easier.

Because of this, I'll refer to this notion as $\text{IND}$. 
We'll also also let the adversary encrypt messages of their choice.
This addition is usually referred to as "chosen plaintext attack" (CPA).

Thus, $\text{IND-CPA}$ security is defined by a pair of games:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{IND-CPA}_b$
}\cr
\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \texttt{return } \texttt{Encrypt}(m_b)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } E(k, m)\cr
\end{aligned}
}
$$

An encryption scheme is $\text{IND-CPA}$ secure if these two games
are indistinguishable.

### Encryption with a PRF

The simplest encryption scheme is the one-time pad. One problem with this
scheme is that we can only encrypt one message. A PRF bypasses
this limitation, by letting us generate new pads on demand.

Given a PRF over $\mathcal{X}$ and $\mathcal{Y}$,
with $\mathcal{Y}$ some type where $\oplus$ makes sense, we can define
an encryption scheme with $\mathcal{M} = \mathcal{Y}$  and $\mathcal{C} = \mathcal{X} \times \mathcal{Y}$ as follows:

$$
\begin{aligned}
&\underline{E(k, m):}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ \texttt{return } (x, m \oplus F(k, x))\cr
\cr
&\underline{D(k, (x, c)):}\cr
&\ \texttt{return } c \oplus F(k, x) \cr
\end{aligned}
$$

What we want to show is that the security of this scheme reduces
to the security of the underlying PRF.

In particular, we want to show that $\text{IND-CPA}_b$ reduces to
$\text{PRF}_b$. The precise statement is that:

$$
\text{IND-CPA}_b \leq \frac{Q^2}{|\mathcal{X}|} + 2\text{PRF}_b
$$

using our short-hand notation for reductions from earlier, and with $Q$
denoting the number of queries made to the $\texttt{Encrypt}$ function
in the $\text{IND-CPA}_b$ game.

{{<note>}}
We'll use this $Q$ in a somewhat informal way, but you could also explicitly
modify the $\text{IND-CPA}_b$ game to take in this $Q$ as a parameter
to the game, and have $\texttt{Encrypt}$ return $\bot$ after $Q$ queries
have been made.
{{</note>}}

Our goal is to start with $\text{IND-CPA}_0$, and then hop our way
over to $\text{IND-CPA}_1$, quantifying the distinguishing advantage
along the way, based on the security of the underlying PRF.

First, note that:

$$
\text{IND-CPA}_1 =
\boxed{
\begin{aligned}
&\underline{\mathtt{Challenge}(m_0 : \mathcal{Y}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ y \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, y)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{Y}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ \texttt{return } (x, m \oplus \texttt{QueryF}(x))\cr
\end{aligned}
}
$$

This is because xoring with a random message is equivalent to just sampling
a random output.

Second, note that we can abstract out the use of the PRF inside
of these games:

$$
\text{IND-CPA}_b =
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^0_b$
}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \texttt{return } \texttt{Encrypt}(m_b)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ \texttt{return } (x, m \oplus \texttt{QueryF}(x))\cr
\end{aligned}
}
\circ \text{PRF}_0
$$

Then we have $\Gamma^0_b \circ \text{PRF}_0 \stackrel{\epsilon_0}{\approx} \Gamma^0_b \circ \text{PRF}_1$, with:

$$
\epsilon_0 = \epsilon(\mathcal{B} \circ \text{PRF}_b)
$$

for some adversary $\mathcal{B}$.

{{<note>}}
This adversary $\mathcal{B}$ is actually subtle to define.

We'll use this $\epsilon_0$ at two points in the game.
- Once for $\Gamma^0_0 \circ \text{PRF}_0 \stackrel{\epsilon_0}{\approx} \Gamma^0_0 \circ \text{PRF}_1$
- And another time for $\Gamma^0_1 \circ \text{PRF}_0 \stackrel{\epsilon_0}{\approx} \Gamma^0_1 \circ \text{PRF}_1$

There's a slight issue in that there's no single adversary here.
We have ${\mathcal{B}_0 := \mathcal{A} \circ \Gamma^0_0}$ and
${\mathcal{B}_1 := \mathcal{A} \circ \Gamma^0_1}$. At a first
glance, it would seem that we need to use a different $\epsilon$
in both situations.

We can get around this technical impass by picking the adversary
between $\mathcal{B}_0$ and $\mathcal{B}_1$ with the largest
advantage, and using this as our $\mathcal{B}$. This works
because the $\stackrel{\epsilon}{\approx}$ only requires that
the distinguishing adversary $\mathcal{A}$ has an advantage
*bounded by* $\epsilon$, which gives us the flexibility we need.

This trick works in many other situations. The general lesson
is that if you have some underlying game $G_b$ which you
use in several different hops, you can refer to all of its uses
with a single $\epsilon$, implicitly taking the adversary
against $G_b$ with the best advantage.
{{</note>}}

Now, if we inline the random function in $\text{PRF}_1$, we have:

$$
\Gamma^0_0 \circ \text{PRF}_1 =
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^1$
}\cr
\cr
&\text{out}[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Challenge}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } \texttt{Encrypt}(m)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ \texttt{if } x \notin \text{out}:\cr
&\ \quad \text{out}[x] \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, m \oplus \text{out}[x])\cr
\end{aligned}
}
$$

Now, if we were to remove the $x \notin \text{out}$ check, this would
only change the behavior if the same $x$ were generated twice.
This happens with probability at most $\frac{Q^2}{2 |\mathcal{X}|}$, by a standard birthday paradox argument. Thus, we have
$\Gamma^1 \stackrel{Q^2 / 2 |\mathcal{X}|}{\approx} \Gamma^2$,
where $\Gamma^2$ is defined as:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^2$
}\cr
\cr
&\underline{\mathtt{Challenge}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } \texttt{Encrypt}(m)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ y \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, m \oplus y)\cr
\end{aligned}
}
$$

An equivalent way to write this would be:

$$
\Gamma^2 = 
\boxed{
\begin{aligned}
&\underline{\mathtt{Challenge}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ y \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, y)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ y \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, m \oplus y)\cr
\end{aligned}
}
$$

We can then add in a random table again, to get:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^3$
}\cr
\cr
&\text{out}[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{Challenge}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ y \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, y)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ x \xleftarrow{R} \mathcal{X}\cr
&\ \texttt{if } x \notin \text{out}:\cr
&\ \quad \text{out}[x] \xleftarrow{R} \mathcal{Y}\cr
&\ \texttt{return } (x, m \oplus \text{out}[x])\cr
\end{aligned}
}
$$

By the same difference lemma argument as before, we have
$\Gamma^3 \stackrel{Q^2 / 2 |\mathcal{X}|}{\approx} \Gamma^2$.

Notice that we have:

$$
\Gamma^3 = \Gamma^0_1 \circ \text{PRF}_1
$$

since in the case where we use a random message, the call to $\texttt{Encrypt}$
doesn't matter, as we saw earlier. From this point, we can reach
$\text{IND-CPA}_1$, so we've done all the hops we need. Putting everything together, we have:

$$
\begin{aligned}
\text{IND-CPA}_0 &= \Gamma^0_0 \circ \text{PRF}_0\cr
&\stackrel{\epsilon_0}{\approx} \Gamma^0_0 \circ \text{PRF}_1\cr
&= \Gamma^1\cr
&\stackrel{Q^2 / 2 |\mathcal{X}|}{\approx} \Gamma^2\cr
&\stackrel{Q^2 / 2 |\mathcal{X}|}{\approx} \Gamma^3\cr
&= \Gamma^0_1 \circ \text{PRF}_1\cr
&\stackrel{\epsilon_0}{\approx} \Gamma^0_1 \circ \text{PRF}_0\cr
&= \text{IND-CPA}_1
\end{aligned}
$$

So all we have to do is apply the triangle lemma, to get:

$$
\text{IND-CPA}_0 \stackrel{\epsilon}{\approx} \text{IND-CPA}_1
$$

with:

$$
\epsilon \leq \frac{Q^2}{|\mathcal{X}|} + 2 \epsilon(\mathcal{B} \circ \text{PRF}_b)
$$

In more words, we've shown that given an adversary $\mathcal{A}$
for $\text{IND-CPA}_b$, there exists an adversary $\mathcal{B}$
for $\text{PRF}_b$ such that:

$$
\epsilon(\mathcal{A} \circ \text{IND-CPA}_b) \leq
\frac{Q^2}{\mathcal{|X|}} + 2\epsilon(\mathcal{B} \circ \text{PRF}_b)
$$

with $Q$ the number of queries made to the encryption oracle.

$\square$


# Hybrid Arguments

In our definition of the $\text{IND-CPA}$ game, we allowed adversaries
to make multiple queries to $\texttt{Challenge}$. A different
variant of the game only allows one query to be made:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{IND-CPA-1}_b$
}\cr
\cr
&\text{count} \gets 0\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{assert } \text{count} = 0\cr
&\ \text{count} \gets \text{count} + 1\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \texttt{return } E(k, m_b)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}) : \mathcal{C}}\cr
&\ \texttt{return } E(k, m)
\end{aligned}
}
$$

The $\texttt{assert}$ will make sure that only one query can be made,
returning $\bot$ if further queries are attempted.

A natural question is how much being able to make multiple queries helps.
An adversary for $\text{IND-CPA-1}$ can obviously break $\text{IND-CPA}$,
since the latter allows them to make the one query they need.
In the other direction, the question is more subtle. It turns out
that if we make $Q$ queries in the $\text{IND-CPA}$ game, then our advantage
is only larger by a factor of $Q$, compared to the $\text{IND-CPA-1}$ game.

The intuition behind the proof is that we bridge that gap
between $\text{IND-CPA}_0$ and $\text{IND-CPA}_1$, we create a series of
hybrid games $H_0, \ldots, H_Q$. The first hybrid starts
at $\text{IND-CPA}_0$, and the last ends at $\text{IND-CPA}_1$. At each step,
instead of moving from $m_0$ to $m_1$ in all of the queries, we only
change to $m_1$ in a single query:

{{<todo>}}
illustration
{{</todo>}}

Then the idea is that distinguishing between two successive games is like
distinguishing between $\text{IND-CPA-1}_0$ and $\text{IND-CPA-1}_1$:

{{<todo>}}
illustration
{{</todo>}}

Since there are $Q$ hops, that's how we get the factor of $Q$ in our
advantage.

## The General Hybrid Argument

We can formalize this argument in a general way, which will be useable
for a wide variety of games.

The basic setup is that we have two different game pairs $G_0$ and $G_1$,
with $\text{out}(G_0) = \text{out}(G_1)$,
which represent our "single query" games, and $M_0$ and $M_1$,
with $\text{out}(M_0) = \text{out}(M_1)$, which
represent our "multi query" games. We then have a series of hybrid games
$H_0, \ldots, H_Q$, satisfying $\text{out}(H_i) = \text{out}(M_b)$.
Finally, we have a series of "shims" $R_0, \ldots, R_{Q - 1}$,
with $\text{in}(R_i) = \text{out}(G_b)$ and $\text{out}(R_i) = \text{out}(M_b)$.

If it holds that:

$$
\begin{aligned}
H_0 &= M_0\cr
H_Q &= M_1
\end{aligned}
$$

and that for all $i \in \\{0,\ldots, Q - 1\\}$, we have:

$$
\begin{aligned}
R_i \circ G_0 &= H_i\cr
R_i \circ G_1 &= H_{i + 1}\cr
\end{aligned}
$$

Then for any adversary $\mathcal{A}$ against $M$, there exists an adversary
$\mathcal{B}$ against $G$, satisfying:

$$
\epsilon(\mathcal{A} \circ M_b) = Q \cdot \epsilon(\mathcal{B} \circ G_b)
$$

**Proof:**

First, we have:

$$
\epsilon(\mathcal{A} \circ M_b)
= |P[1 \gets \mathcal{A} \circ M_0] - P[1 \gets \mathcal{A} \circ M_1]|
$$

Because of the properties of $H_0$ and $H_Q$, we can write this as:

$$
|P[1 \gets \mathcal{A} \circ H_0] - P[1 \gets \mathcal{A} \circ H_1]|
$$

We can then write this as a telescoping sum:

$$
\left|\sum_{i = 0}^{Q - 1} P[1 \gets \mathcal{A} \circ H_i] -
P[1 \gets \mathcal{A} \circ H_{i + 1}]\right|
$$

This works because the intermediate terms cancel each other out,
giving us the final result.

Next, we apply the properties of $R_i$, giving us:

$$
\left|\sum_{i = 0}^{Q - 1} P[1 \gets \mathcal{A} \circ R_i \circ G_0] -
P[1 \gets \mathcal{A} \circ R_i \circ G_1]\right|
$$

At this point, we need to introduce a little gadget. We define
a new package $R$, which samples a random $j \in \\{0, \ldots, Q - 1\\}$,
and then behaves like the package $R_j$.

Conditioning on $j$, we can write our current value as:

$$
\left|\sum_{i = 0}^{Q - 1} P[1 \gets \mathcal{A} \circ R \circ G_0 \ |\ j = i] -
P[1 \gets \mathcal{A} \circ R_i \circ G_1 \ |\ j = i]\right|
$$

Now, by basic probability, we have that:

$$
\frac{1}{Q} \sum_{i = 0}^{Q - 1} P[1 \gets \mathcal{A} \circ R \circ G_b \ |\ j = i] = P[1 \gets \mathcal{A} \circ R \circ G_b]
$$

since $P[j = i] = \frac{1}{Q}$.

Multiplying our current telescoping sum by $\frac{1}{Q}$ inside,
and $Q$ on the outside, we get:

$$
Q \cdot \left|P[1 \gets \mathcal{A} \circ R \circ G_0] -
P[1 \gets \mathcal{A} \circ R \circ G_1]\right|
$$

Denoting the package $\mathcal{A} \circ R$ as an adversary $\mathcal{B}$,
This is just the value:

$$
Q \cdot \epsilon(\mathcal{B} \circ G_b)
$$

Tying everything together, we conclude that:

$$
\epsilon(\mathcal{A} \circ M_b) = Q \cdot \epsilon(\mathcal{B} \circ G_b)
$$


$\square$

## Our Specific Case

Now, let's recall our specific case. We have the following game:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{IND-CPA}_b$
}\cr
\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \texttt{return } E(k, m_b)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } E(k, m)\cr
\end{aligned}
}
$$

As well as a variant $\text{IND-CPA-1}_b$, which only allows a single
query to challenge. $\text{IND-CPA-1}_b$ will play the role of $G_b$,
and $\text{IND-CPA}_b$ the role of $M_b$. We'll implicitly limit
$\text{IND-CPA}_b$ to $Q$ queries, for the sake of the argument.

The next step is to define $H_0, \ldots H_Q$:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{H}_i$
}\cr
\cr
&\text{count} \gets Q\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ b \gets \text{count} \geq i\cr
&\ \text{count} \gets \text{count} - 1\cr
&\ \texttt{return } E(k, m_b)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } E(k, m)\cr
\end{aligned}
}
$$

For $H_0$, we always use $m_0$, for $H_1$ we start using $m_1$ only
for the last query, when count is $0$, for $H_2$ we use $m_1$
for the last two, etc., until we reach $H_Q$, which always uses
$m_1$. Thus, we clearly have $H_0 = \text{IND}_0$ and $H_1 = \text{IND}_1$.

The final step is to construct our shims $R_0, \ldots, R_{Q - 1}$.
The idea is that we use the $\text{IND-CPA-1}$ game to make the transition
between choosing $m_0$ and choosing $m_1$.

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{R}_i$
}\cr
\cr
&\text{count} \gets Q\cr
&k \xleftarrow{R} \mathcal{K}\cr
\cr
&\underline{\mathtt{Challenge}(m_0 : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{if } \text{count} > i:\cr
&\ \quad \texttt{return } \text{IND-CPA-1}.\texttt{Encrypt}(m_0)\cr
&\ \texttt{elif } \text{count} = i:\cr
&\ \quad \texttt{return } \text{IND-CPA-1}.\texttt{Challenge}(m_0)\cr
&\ \texttt{else } \text{count} < i:\cr
&\ \quad m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \quad \texttt{return } \text{IND-CPA-1}.\texttt{Encrypt}(m_1)\cr
\cr
&\underline{\mathtt{Encrypt}(m : \mathcal{M}): \mathcal{C}}\cr
&\ \texttt{return } \text{IND-CPA-1}.\texttt{Encrypt}(m)\cr
\end{aligned}
}
$$

For example, in the game $R_0$, we always encrypt $m_0$ until
we reach the $Q$th query. At this point, if we're against
$\text{IND-CPA-1}_0$, then we'll encrypt $m_0$, otherwise, we
end up encrypting $m_1$. And the linking works fine, since we
only make a single call to $\texttt{Challenge}$.

Thus, we have that:

$$
\begin{aligned}
R_i \circ \text{IND-CPA-1}_0 &= H_i\cr
R_i \circ \text{IND-CPA-1}_0 &= H\_{i+1}\cr
\end{aligned}
$$

At this point, all of the conditions are satisfied for us to apply
the hybrid argument lemma we proved earlier, to conclude that
for every adversary $\mathcal{A}$ against $\text{IND-CPA}$,
making $Q$ challenge queries,
there exists an adversary $\mathcal{B}$ against $\text{IND-CPA-1}$
satisfying:

$$
\epsilon(\mathcal{A} \circ \text{IND-CPA}_b) \leq
Q \cdot \epsilon(\mathcal{B} \circ \text{IND-CPA-1}_b)
$$

This means that whether or not you allow multiple challenge
queries doesn't fundamentally change the security of the scheme.
You just get a linear increase in advantage by being able to make
more queries. Thus, you can safely allow multiple challenge queries
if that's more convenient.

{{<note>}}
It's important to note that this hybrid argument wouldn't work
without having the $\texttt{Encrypt}$ queries available to us.
This is an essential difference, in fact. If the encryption
scheme were deterministic, then $\text{IND-1}$ might be secure,
but $\text{IND}$ would fail to be, because we could call
$\texttt{Challenge}$ multiple times with the same message, and figure
out whether the game is encrypting real messages or not.
{{</note>}}

# Hybrid Encryption

As a final example, I'd like to explore hybrid encryption,
in the form of ElGamal encryption.
This will let us explore a few interesting techniques in proofs,
including random oracles, and modelling games where the adversary
is expected to return a complex answer, like a group element.

First, let's recall how a the ElGamal encryption scheme works.
The basic idea is to combine a Diffie-Hellman key exchange with
an encryption scheme. First, we need some Cryptographic
group $\mathbb{G}$, generated by $G$, and with order $q$.
We also need an underlying symmetric encryption scheme $\Sigma$,
defined by:

$$
\begin{aligned}
\Sigma.E &: \mathcal{K} \times \mathcal{M} \to \mathcal{C}\cr
\Sigma.D &: \mathcal{K} \times \mathcal{C} \to \mathcal{M}\cr
\end{aligned}
$$

Finally, we need a hash function $H : \mathbb{G} \to \mathcal{K}$.

With this, we can define the ElGamal encryption scheme:

$$
\begin{aligned}
&\underline{\text{Gen}():}\cr
&\ a \xleftarrow{R} \mathbb{Z}/(q)\cr
&\ A \gets a \cdot G\cr
&\ \texttt{return } (a, A)\cr
\cr
&\underline{E(A, m):}\cr
&\ b \xleftarrow{R} \mathbb{Z}/(q)\cr
&\ k \gets H(b \cdot A)\cr
&\ \texttt{return } (b \cdot G, \Sigma.E(k, m))\cr
\cr
&\underline{D(a, (B, c)):}\cr
&\ k \gets H(a \cdot B)\cr
&\ \texttt{return } \Sigma.D(k, m)\cr
\end{aligned}
$$

Encryption works by generating a new key pair, and then performing
a key exchange with the receiver. This ephemeral key is included
with the ciphertext, to allow the receiver to perform their end
of the key exchange, deriving the symmetric key needed to decrypt
the ciphertext.

## The CDH Problem

When trying to break this scheme, you see the public key $A = a \cdot G$, the ephemeral key $B = b \cdot G$. If you could derive $ab \cdot G$
from these values, you would be able to decrypt ciphertexts.
This problem is referred to as the Computational Diffie-Hellman (CDH)
problem.

In traditional game-based security, the way you'd model
the security of the CDH problem is with a simple game, wherein
an adversary tries to guess $ab \cdot G$ from $A$ and $B$:

$$
\boxed{
\begin{aligned}
&a, b \xleftarrow{R} \mathbb{Z}/(q)&\cr
&A, B, C \gets a \cdot G, b \cdot G, ab \cdot G&\cr
&&\xrightarrow{(A, B)}\cr
&&\xleftarrow{C'}\cr
&\text{win} \gets C = C'\cr
\end{aligned}
}
$$

The advantage in this game would be defined as the probability
that $\text{win}$ gets set to $1$; in other words, the probability
that the adversary succeeds in finding $C$.

For this to be useful in a state-separable proof setting,
we'd need to make this into a pair of games, modelling a
"real vs ideal" situation. It's not immediately clear how to do that.

We can emulate the first message sent to the adversary with a method
returning that information, so that's not the issue. In order
to emulate the winning aspect, we can have a method which lets
the adversary make an attempt, and tell them whether or not they've
guess correctly. If in the ideal case, we make it impossible for the adversary
to win, then their distinguishing ability is related to whether
or not they can guess correctly. Putting this into a package, we have:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{CDH}_b$
}\cr
\cr
&a, b \xleftarrow{R} \mathbb{Z}/(q)\cr
&A, B \gets a \cdot G, b \cdot G \cr
\cr
&\underline{\mathtt{Pair}():}\cr
&\ \texttt{return } (A, B)\cr
\cr
&\underline{\mathtt{Challenge}(C):}\cr
&\ \texttt{return } b = 0 \land C = ab \cdot G\cr
\end{aligned}
}
$$

In the case that $b = 1$, $\texttt{Challenge}$ always returns $0$,
otherwise, it directly tells us whether or not the guess was correct.
This means that the difference between the two games is bounded
by the probability that an adversary makes a correct guess.
If they can't make a correct guess, then they won't be able to distinguish the two games.

One subtle difference we've introduced with this game is that
the adversary is allowed to make multiple different guesses
for $C$, whereas in the traditional game, the adversary can only
make a single guess. We can fix this by modifying $\texttt{Challenge}$
to return $\bot$ after the first guess, giving us a $\text{CDH-1}$ game.
It turns out that allowing $Q$ queries instead of a single one
only increases the advantage by a factor of $Q$, so we can use
the multi-query $\text{CDH}$ variant without hesitation. Proving
this, on the other hand, relies on a somewhat technical argument,
so the next subsection can safely be skipped.

### Single vs Multiple Guesses

We can generalize this proof to many situations beyond just
the CDH problem. In general, we define a *guessing scheme*
as a set of types $\mathcal{P}, \mathcal{S}, \mathcal{G}$, along
with functions:

$$
\begin{aligned}
\text{Setup} &: \bullet \xrightarrow{R} \mathcal{P} \times \mathcal{S}\cr
\text{Correct} &: \mathcal{P} \times \mathcal{S} \times \mathcal{G} \to \\{0, 1\\}
\end{aligned}
$$

We have some public information $\mathcal{P}$, some secret information
$\mathcal{S}$, as well as a type of guesses $\mathcal{G}$. Given
all of the information, and a guess, we can determine if that
guess is correct. In the case of the CDH problem, the public
information is $A, B$, the secret information $a, b$, and the guess
$C$ is correct when $C = ab \cdot G$.

Given a guessing scheme, we have the associated guessing game:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{Guess}_b$
}\cr
\cr
&(p, s) \xleftarrow{R} \text{Setup}()\cr
\cr
&\underline{\mathtt{Public}():}\cr
&\ \texttt{return } p\cr
\cr
&\underline{\mathtt{Guess}(g):}\cr
&\ \texttt{return } b = 0 \land \text{Correct}(p, s, g)\cr
\end{aligned}
}
$$

We also have a restricted version of the game, $\text{Guess-1}_b$,
where only a single guess is allowed; subsequent guesses return
$\bot$.

We prove that $\text{Guess}_b \leq Q \cdot \text{Guess-1}_b$
with $Q$ the number of guesses made.

**Proof:**

We start with an adversary $\mathcal{A}$ against $\text{Guess}_b$.
Our adversary $\mathcal{B}$ will not be constructed using
a wrapper package around $\text{Guess-1}_b$, like we've done
with everything else so far. Instead, $\mathcal{B}$ will itself
run $\mathcal{A}$, with a simulated version of $\text{Guess}_b$,
and use its behavior to make a single guess. More formally,
$\mathcal{B}$ plays against $\text{Guess-1}_b$,
with $\texttt{run}$ behaving as follows:

1. $\mathcal{B}$ calls $\texttt{Public}$, receiving $p$.
2. $\mathcal{B}$ runs $\mathcal{A}$ with a simulated version of $\text{Guess-1}_1$. In this simulation, $\texttt{Public}$ returns the $p$ $\mathcal{B}$ saw earlier, and $\texttt{Guess}$ always returns $0$. The guesses $\\{g_0, \ldots, g_Q\\}$ made by $\mathcal{A}$ are recorded in a list.
3. $\mathcal{B}$ picks one of the $Q$ guesses at random, and calls $\texttt{Guess}$ with that guess, and then returns $1$ if $\texttt{Guess}$ does.

Since $\mathcal{B}$ will never return $1$ against $\text{Guess-1}_1$,
we have:

$$
\epsilon(\mathcal{B} \circ \text{Guess-1}_b) = P[1 \gets \mathcal{B} \circ \text{Guess-1}_0]
$$

This probability satisfies:

$$
P[1 \gets \mathcal{B} \circ \text{Guess-1}_b] \geq P[\exists i.\ g_i \text{ is correct},\ j = i]
= \frac{1}{Q}P[\exists i.\ g_i \text{ is correct}]
$$

this means that:

$$
Q\cdot \epsilon(\mathcal{B} \circ \text{Guess-1}_b) \geq P[\exists i.\ g_i \text{ is correct}]
$$

But, unless that last even happens, then $\mathcal{A}$ won't be
able to distinguish between $\text{Guess}_0$ and $\text{Guess}_1$.
Thus we have:

$$
\epsilon(\mathcal{A} \circ \text{Guess}_b) \leq P[\exists i.\ g_i \text{ is correct}]
$$

Putting this all together, we have:

$$
\epsilon(\mathcal{A} \circ \text{Guess}_b) \leq Q \cdot \epsilon(\mathcal{B} \circ \text{Guess-1}_b)
$$

$\square$

## The $\text{IND-CPA}$ Game, with Random Oracles

After that little detour, let's get back to the main course.
We want to investigate the $\text{IND-CPA}$ security of our
hybrid encryption scheme. In this model of security, the adversary
tries to distinguish between encryptions of a real message,
and encryptions of a random message. In principle, they're
also allowed to make queries for encryptions of messages on their
choice, hence the $\text{CPA}$. One neat aspect of public-key
encryption is that we don't need to allow for these,
since the adversary can use the public key to do encryption by
themselves. We also need to model the hash function used inside
of our encryption scheme. We model this as a random oracle,
making $H$ a perfectly random function, which we allow the
adversary to query. Putting this all together, our game pair looks
like this:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{IND-CPA}_b$
}\cr
\cr
&a \gets \mathbb{Z}/(q)\cr
&A \gets a \cdot G\cr
\cr
&\underline{\mathtt{Pk}():}\cr
&\ \texttt{return } A\cr
\cr
&\underline{\mathtt{Challenge}(m_0):}\cr
&\ b \gets \mathbb{Z}/(q)\cr
&\ k \gets \texttt{H}(b \cdot A)\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \texttt{return } (b \cdot G, E(k, m_b))\cr
\cr
&h[\cdot] \gets \bot\cr
\cr
&\underline{\mathtt{H}(P):}\cr
&\ \texttt{if } P \notin h:\cr
&\ \quad h[P] \xleftarrow{R} \mathcal{K}\cr
&\ \texttt{return } h[P]\cr
\end{aligned}
}
$$

Instead of actually querying a hash function, instead we treat
it as a completely random function, which we also allow the adversary
to query. This is the essence of proofs on the "random oracle model".

## Reducing to CDH

We'll show that the $\text{IND-CPA}$ security of this scheme
reduces to the $\text{CDH}$ security of the underlying group,
as well as the $\text{IND}$ security of the underlying symmetric
encryption scheme.

Using a hybrid argument, we can instead work with $\text{IND-CPA-1}$,
since this only decreases the advantage by a factor of $Q$, the
number of challenge queries made.

First, let's separate out the $\text{CDH}$ game:

$$
\text{IND-CPA-1}_b =
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^0_b$
}\cr
\cr
&h[\cdot] \gets \bot\cr
&k \xleftarrow{R} \mathcal{K}\cr
&\text{count} \gets 0\cr
\cr
&\underline{\mathtt{Pk}():}\cr
&\ (A, \cdot) \gets \texttt{Pair}()\cr
&\ \texttt{return } A\cr
\cr
&\underline{\mathtt{Challenge}(m_0):}\cr
&\ \texttt{assert } \text{count++} = 0\cr
&\ (\cdot, B) \gets \texttt{Pair}()\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ \texttt{return } (B, E(k, m_b))\cr
\cr
&\underline{\mathtt{H}(P):}\cr
&\ \texttt{if } \text{CDH}.\texttt{Challenge}(P):\cr
&\ \quad h[P] \gets k\cr
&\ \texttt{if } P \notin h:\cr
&\ \quad h[P] \xleftarrow{R} \mathcal{K}\cr
&\ \texttt{return } h[P]\cr
\end{aligned}
}
\circ \text{CDH}_0
$$

The basic idea here is that since the adversary only makes
a single challenge query, we can use a hardcoded ephemeral key $B$.
We can also generate the single symmetric key we need in advance.
We need to make sure that when the hash function is evaluated
at $ab \cdot G$, that this symmetric key is what comes out. We
do this by deferring to $\text{CDH}_0$, which will give
us that information.

Now, when we're playing against $\text{CDH}_1$ instead, then
this check will never hold, and thus $k$ is completely
unliked from $\texttt{H}$. We can simplify this situation as follows:

$$
\Gamma^0_b \circ \text{CDH}_1 =
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^1_b$
}\cr
\cr
&h[\cdot] \gets \bot\cr
&A, B \xleftarrow{R} \mathbb{G}\cr
&\text{count} \gets 0\cr
\cr
&\underline{\mathtt{Pk}():}\cr
&\ \texttt{return } A\cr
\cr
&\underline{\mathtt{Challenge}(m_0):}\cr
&\ \texttt{assert } \text{count++} = 0\cr
&\ m_1 \xleftarrow{R} \mathcal{M}\cr
&\ k \xleftarrow{R} \mathcal{K}\cr
&\ \texttt{return } (B, E(k, m_b))\cr
\cr
&\underline{\mathtt{H}(P):}\cr
&\ \texttt{if } P \notin h:\cr
&\ \quad h[P] \xleftarrow{R} \mathcal{K}\cr
&\ \texttt{return } h[P]\cr
\end{aligned}
}
$$

Since $\text{CDH}_1$ doesn't give us any useful information about
$A$ and $B$, we might as well just generate them ourselves.
Because our key $k$ has no relation with the rest of our package,
we can now unlike the symmetric encryption aspect, deferring
to the $\text{IND}$ game for symmetric encryption:

$$
\Gamma^1_b =
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\Gamma^2$
}\cr
\cr
&h[\cdot] \gets \bot\cr
&A, B \xleftarrow{R} \mathbb{G}\cr
&\text{count} \gets 0\cr
\cr
&\underline{\mathtt{Pk}():}\cr
&\ \texttt{return } A\cr
\cr
&\underline{\mathtt{Challenge}(m_0):}\cr
&\ \texttt{assert } \text{count++} = 0\cr
&\ \texttt{return } (B, \text{IND}.\texttt{Challenge}(m_0))\cr
\cr
&\underline{\mathtt{H}(P):}\cr
&\ \texttt{if } P \notin h:\cr
&\ \quad h[P] \xleftarrow{R} \mathcal{K}\cr
&\ \texttt{return } h[P]\cr
\end{aligned}
}
\circ \text{IND}_b
$$

And now all we have to do this is tie all of these games together,
to bridge $\text{IND-CPA-1}_0$ and $\text{IND-CPA-1}_1$:

$$
\begin{aligned}
\text{IND-CPA-1}_0
&= \Gamma^0_0 \circ \text{CDH}_0\cr
&\stackrel{\epsilon_1}{\approx} \Gamma^0_0 \circ \text{CDH}_1\cr
&= \Gamma^1_0\cr
&= \Gamma^2 \circ \text{IND}_0\cr
&\stackrel{\epsilon_2}{\approx} \Gamma^2 \circ \text{IND}_1\cr
&= \Gamma^1_1\cr
&= \Gamma^0_1 \circ \text{CDH}_1\cr
&\stackrel{\epsilon_1}{\approx} \Gamma^0_1 \circ \text{CDH}_0\cr
&= \text{IND-CPA-1}_1
\end{aligned}
$$

This gives us:

$$
\epsilon(\mathcal{A} \circ \text{IND-CPA-1}_b) \leq
2 \cdot \epsilon(\mathcal{B} \circ \text{CDH}_b) +
\epsilon(\mathcal{C} \circ \text{IND}_b)
$$

for some adversaries $\mathcal{B}$ and $\mathcal{C}$, which is
what we set out to prove.

$\square$

# Conclusion

Hopefully these examples have helped you get a feeling for how
state-separable proofs work. A lot of the understanding comes
from wrestling with proofs yourself, so hopefully you get
a chance to try out the technique in an example that
matters to you.

I've found working with state-separable proofs to be much
easier, and quite a bit of fun. This blog post wasn't something
I would've thought to write about, but I felt compelled
to after playing around with this technique for a while.
I didn't expect this post to be so long either; hopefully
its length was warranted.

I'm also planning to write a more beginner-friendly
introduction to provable security.
This would be relevant to a larger audience, but I think there's
enough people who could benefit from
a more advanced post like this one.
I would have liked to have a resource like
this when first wrapping my head around state-separable
proofs a month ago.

## Resources

[Mike Rosulek's The Joy of Cryptography](https://joyofcryptography.com/) is a neat book which uses this technique pervasively,
so this can be an interesting read if you're looking for more
examples.

[State Separation for Code-Based Game-Playing Proofs](https://eprint.iacr.org/2018/306) is the original paper introducing the technique, and might be an interesting read if you want to
know more details about the formalism.
