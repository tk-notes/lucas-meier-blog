---
title: "Quantum Computing: Some Analogies"
date: 2021-07-10T08:33:44+02:00
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

# No, you can't just try all possibilities at once

# Phase Finding

# Grover's Search

# Conclusion
