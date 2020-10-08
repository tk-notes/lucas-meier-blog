---
title: "A Category of Turing Machines"
date: 2020-08-03
draft: true
description: "A formulation of Turing Machines as a Cartesian Closed Category"
path: "/posts/2020/08/a-category-of-turing-machines"
type: post
image: "/posts/2020/08/a-category-of-turing-machines/cover.png"
tags:
  - Math
  - Category Theory
  - Computer Science
---

This post is about describing a way of organizing Turing Machines into a
*category*, specifically, one with the property of being *cartesian closed*. This organization
elucidates the correspondance between Turing Machines and the lambda calculus, and can
be seen as a justification for the Church Turing thesis.

# Models of Computation

Back in the 1930s, two different models of computation were proposed. The classic
Turing Machine by Alan Turing, and the lambda calculus by Alonzo Church. It was quickly
postulated that these two models of computation were in fact equivalent.

The lambda calculus is not as well known as the model of the Turing Machine, but a bit
more familiar to programmers. The calculus is based on being able to create and apply
functions, to create sentences like:

$$
\lambda f. \lambda x. f x
$$

This corresponds to a term in a programming language like:

```haskell
\f -> \x -> f x
```

Turing Machines, on the other hand, are best described as analogous to mechanical processes.
The basic definition of a Turing Machine involves a finite internal state machine, with
access to an infinite tape, which provides input to the machine, and acts as the working
memory the machine can use to write the output.

At each step, the machine looks at the current cell of the tape, and can change its internal
state, move left or right along the tape, and output a symbol to the tape. This mechanical
model of computation has the advantage of being a bit easier to analyze in terms of proving
things about it.

The disadvantage of the Turing Machine model is that it's difficult to see the connection
between the mechanical description of a machine and terms in a programming language,
unlike the lambda calculus, where this connection is much clearer.

It's also hard to see any connective structure or compositionality to turing machines. Described
this way, it seems that these machines are big blobs, and there's no obvious way to build up
large machines from smaller ones, unlike in the lambda calculus, which is all about
building up functions and encodings of things from those functions.

The traditional presentation of turing machines as single tape behemoths obscures the compositional
aspects of them, but if you allow for a slight generalization (with equivalent power) you can
see the connective tissue between machines, and the connection with the lambda calculus becomes
apparent.

# A Definition of a Turing Machine

I glossed over the definition of a turing machine quite a bit earlier, but now it's time
to give a formal definition.

The idea we'll develop here is that a turing machine has access to an input tape, and an output
tape. The input tape stretches out infinitely in both directions, and the output tape
stretches out infinitely in one direction. Both are filled with symbols:

![](/posts/2020/08/a-category-of-turing-machines/1.png)

At each step, the machine gets to look at a single cell of the input tape. Using that
information, it can change its position on the input tape (in either direction). It
can also choose what symbol it will write on the output tape, before moving right there.
It can also decide to halt, and stop processing input.

Let's describe this formally:

A turing machine is a tuple:

$$
(Q, H, \Sigma, \delta)
$$

$Q$ is a set of *states* that the machine can be in.

$H \in Q$ is the halt state. Once the machine reaches this state, we can consider it to have
halted.

$\Sigma$ is the set of symbols that can appear on the tapes

$\delta : Q \times \Sigma \to Q \times \{L, R\} \times \Sigma$ is
the transition function. Given the current state, and the current symbol on the input tape,
we either halt, or move to a new state, move our current position on the input tape
in one direction or the other, and write a symbol to the output tape.

We always move one cell to the right on the output tape.

We can describe the current state of the input tape as a function $\mathbb{Z} \to \Sigma$,
and the output tape as $\mathbb{N} \to \Sigma$.


Using $\delta$, we can provide functions
that describe the state of each tape, as well as the current state of the machine,
at each timestep $t$. Thus we can provide a (partial) function
$(\mathbb{Z} \to \Sigma) \to (\mathbb{N} \to \Sigma)$
which, given the state of the input tape, provides us with the final state of the output
tape after the machine halts. Of course, it's possible that for a given input, the machine
never halts. That's why this function must be *partial*.

## Equivalent Models

This formulation is pretty specific, and somewhat idiosyncratic. The usual definition
of a Turing Machine usually only has a single tape, on which the input is initially provided,
and that the machine uses as a working memory as well. I find it much clearer to separate out
the input from the output, and it leads to a more composable definition as well.

Another thing absent from our definition is the concept of *accept states*. The idea is that
the machine halts at some point, but depending on what state the machine is in, we say that
the machine *accepted* or *rejected* the input. This allows us to see the turing machine
as a generalization of other models of computation that accept or reject strings.

This is subsumed by the concept of the output tape, since we can model any accept or reject process
by writing a $1$ or a $0$ on the tape before halting. We can just write blank symbols before then,
so that the only input on the tape is just the result of our acceptance process.

Another question is whether or not we get a more "powerful" model by adding more symbols
to $\Sigma$? Any $\Sigma$ with at least two symbols has the same power. The reason for
this is that any other set of symbols can be encoded in binary, and we can always write a machine
that reads two symbols at once, and outputs two symbols at once, and this will be a machine
operating on $\Sigma = \{0, 1\}$.

If $\Sigma$ only contains a single symbol, we run into an exponential explosion in the number
of symbols it takes to encode things, which poses a bit of a problem.

Since different choices of $\Sigma$ are equivalent, so are models in which we're not forced
to always move right on the output tape. This is because we can add an output symbol
indicating that we would rather have stayed still, e.g. $x$. Then a sequence $0xxx10$
has all of the $x$ placeholders removed, to be interpreted as $010$, the correct output
had we had the ability to stay still on the output tape.

It's often very convenient to assume that $\Sigma$ contains the binary digits $0, 1$,
along with some symbol represented a blank space on the tape. Of course, this is equivalent
to an encoding using two binary symbols for each of these three options, as we saw earlier.

## The Identity Machine

Let's write a simple (but important) machine. The machine which simply copies the input
it sees to the output. The idea is that the input is encoded with a binary string with no spaces,
and the machine starts at the beginning of that string, and copies the string to the output until
it sees a space, which means that the string is finished.

The machine has a single state, and we can visualize what it does through this diagram:

![](/posts/2020/08/a-category-of-turing-machines/2.png)

Whenever it sees a blank symbol, it will halt. Otherwise, if it sees a $0$, it will move
right on the input tape, and write down a $0$, ditto for $1$.

It's easy to see that the result of running this machine on some input string will simply
end up with the input copied directly to the output tape.

# Multiple Tapes

We can naturally extend this definition to the case where we have multiple input tapes,
and multiple output tapes. Instead of reacting to a single symbol, we react to $m$ symbols,
where $m$ is the number of input tapes. Instead of outputing a single symbol, we output
$n$ symbols, etc.

Formally, an $(m, n)$ turing machine is:

$$
(Q, H, \Sigma, \delta : Q \times \Sigma^m \to Q \times \{L, R\}^m \times \Sigma^n)
$$

The change is only $\delta$, which now accepts multiple symbols, outputs multiple directions
on each input tape, and outputs multiple symbols.

Otherwise, things stay exactly the same.

Instead of a function $(\mathbb{Z} \to \Sigma) \to (\mathbb{N} \to \Sigma)$, we get
a function $(\mathbb{Z} \to \Sigma)^m \to (\mathbb{N} \to \Sigma)^n$, where given inputs
on $m$ tapes, we get outputs on $n$ tapes.

If we set $m = n = 1$, we recover the original notion of a turing machine with a single
input and output tape.

## Composing Machines

Given an $(m, k)$ machine, and a $(k, n)$ machine, can we get an $(m, n)$ machine?

Well, one idea is to run the first machine on the $m$ inputs, producing $k$ outputs.
Then you can run the second machine on those $k$ outputs, producing $n$ inputs. Intuitively
we can compose different machines together.

We can visualize composition as putting one machine after another:

![](/posts/2020/08/a-category-of-turing-machines/3.png)

The second machine uses the output of the first to do its work.

It's clear that this kind of machine is a thing that should exist, but giving a formal
description in terms of the states, transition function, etc. is tedious, and not
that illuminating.

In fact, you probably need to introduce the concept of *work tapes*, on which you can
both read and write, to our model of a turing machine, in order to define this composition
in a straight-forward way.

For our purposes, we don't care how many work tapes a turing machine might end up using.

# A Category of Machines

Now that we've defined an identity machine, along with a way to compose multiple machines,
we have the ingredients necessary to form a category of turing machines.

This category will have objects natural numbers, and a morphism $m \to n$ is a turing
machine with $m$ input tapes and $n$ output tapes. The identity morphisms are given
by the identity machines we gave earlier, which simply copy their inputs to their outputs.

Composition is defined by the construct we had earlier.

One subtlety we're seriously glossing over, is under what conditions we consider two machines to be equal. The problem with
equality as defined by looking at the internals of a machine is that two machines ostensibly
performing identical manipulations of a tape can have different structure. Because of this,
we need to relax our morphisms slightly, to be equivalence classes of machines instead of
machines per se.

That, or we can say that associativity of composition is guaranteed up to a certain
kind of isomorphism, which is the approach in [[2]](/posts/2020/08/a-category-of-turing-machines/#ref-2)

## Products

# References

<p id="ref-1">
[1] Joachim Lambek, From lambda calculus to Cartesian closed categories, in To H. B. Curry: Essays on Combinatory Logic, Lambda Calculus and Formalism, eds. J. P. Seldin and J. Hindley, Academic Press, 1980, pp. 376-402.
</p>

<p>
  <a id="ref-2" href="http://www.sci.brooklyn.cuny.edu/~noson/TCStext.html">
    [2] Noson S. Yanofsky, Theoretical Computer Science for the Working Category Theorist
  </a>
</p>

<p>
  <a id="ref-3" href="https://golem.ph.utexas.edu/category/2006/08/cartesian_closed_categories_an_1.html">
    [3] John Baez, CCCs and the λ-calculus
  </a>
</p>

<p>
  <a id="ref-4"  href="https://books.google.com/books?hl=en&lr=&id=1aMKAAAAQBAJ&oi=fnd&pg=PP1&dq=introduction+to+the+theory+of+computation&ots=iEZTkLPfaM&sig=Lm7Ch15cZO96w9RoIZ9FNMZgUxI">
    [4] Michael Sipser, Introduction to the Theory of Computation, 2012
  </a>
</p>
