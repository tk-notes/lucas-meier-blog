---
title: "Quantum Computing: Some Analogies"
date: 2021-07-10T09:34:05+02:00
draft: true
katex: true
tags:
  - "Math"
  - "Quantum Computing"
---

I've come up with a few intuitive ways of thinking about algorithms 
on quantum computers. This post is about explaining a few of those.

<!--more-->

# Treasure in a Pond

Here's an analogy that came to me when I was taking a walk in the forest.
This is now my go-to analogy to explain the difference between classical
computing and quantum computing.

Let's say there's a shallow pond. There's a treasure chest hidden somewhere
in the pond. The water is so murky that you can't see the chest by looking
through the water.

{{<img "1.png">}}

The classical computing approach to finding the chest is to
use a stick to prod the pond at different locations,
until you end up hitting the chest.

{{<img "2.png">}}

The quantum computing approach is to throw a stone in the pond, and then
observe how the ripples behave. The chest will cause a perturbation in
the ripples, revealing its location.

{{<img "3.png">}}

This analogy illustrates the difference in thinking between these models
of computation, and how quantum computing might provide an advantage
for some problems.

The key difference is that classical computing has to work
with local information. You can only prod the pond at selected points.
Quantum computing, on the other hand, can make use of global information
about the problem. At least, in some situations, the problem yields
itself to global investigation. In this situation, the chest is kind
enough to perturb the ripples in the pond.

On the other hand, if we were searching for a chest at the bottom of a lake,
then our ripples wouldn't be affected at all, and we'd be back to prodding,
albeit with a longer stick.

I wrote this post for the sole purpose of getting this analogy of my chest.
The rest of this post serves to anchor this analogy through a concrete
illustration of some problems in quantum computing.

# No, you can't just try all possibilities at once

A classical bit is either $0$ or $1$. A qubit can be $\ket{0}$, or $\ket{1}$,
but more generally, an arbitrary superposition of the two:

$$
\alpha \ket{0} + \beta \ket{1}
$$

This is true.

What is not true, however, is that you can instantly solve any classical
problem by "simply trying all possibilities at once". It is true that
you can create a superposition of all possibilities. For example,
using the Hadamard gate:

$$
\begin{aligned}
&H \ket{0} := \frac{1}{\sqrt{2}}(\ket{0} + \ket{1}) \cr
&H \ket{1} := \frac{1}{\sqrt{2}}(\ket{0} - \ket{1})
\end{aligned}
$$

You can get a superposition of all possible $n$-bit values by applying
this gate over $n$ zeroes:

$$
H^{\otimes n} \ket{0 \cdots 0} = \frac{1}{\sqrt{2}^n} \sum_{i = 0}^{n - 1} \ket{i}
$$

The problem is that if you measure this state, you don't get information
about all of these possibilities. You just sample one of these possibilities
uniformly.

If a create a super position of my inputs, apply a function, and then measure,
all I get is information about one application of the function. This is
no better than the classical case.

What allows quantum computing to gain any advantage is *interference*.
The difference between quantum superpositions and simple stochastic
variables lies in negative probabilities. A superposition can have
negative probabilities for certain states. This allows the transformation
on your superposition to have certain states cancel each other out.
This cancellation, crucially, will depend on the *global* properties
of the function you're applying to this superposition.

This is what allows you to use quantum computing to gain global information
about a function. If you apply your function to a superposition,
then cancellation might lead to valuable information about how that function behaves.
Of course, the function needs to be amenable to this kind of cancellation.

Back to the lake analogy, the quantum approach involves the
entire pond as input, but it can't magically prod the entire pond at once.
Thankfully, when the entire pond is shaken up, the treasure chest perturbs
the shaking in just the right way, so as to reveal its location.

The takeaway is that quantum computing can explore certain global properties
of functions
more efficiently.

# Phase Finding

Let's move from an abstract point of view to a more concrete one.
Here, I'll try to illustrate the *phase finding* algorithm, in a conceptual
way. Going over the complete technical details would be beyond
the scope of this post, but we can taste some of the algorithm's
flavor nonetheless.

We start with some unitary operator $U$, and an eigenvector $\ket{u}$
of this operation, such that:

$$
U \ket{u} =\ \circlearrowleft(\varphi) \ket{u}
$$

{{<note>}}
Here I use the notation:

$$
\circlearrowleft(\varphi) := e^{2 \pi i \varphi}
$$
{{</note>}}

The phase finding algorithm takes in this eigenvector $\ket{u}$,
and returns a ket $\ket{\hat{\varphi}}$, an $N$ bit approximation
to the phase $\varphi$. 

# Grover's Search

# Conclusion
