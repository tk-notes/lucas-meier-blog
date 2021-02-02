---
title: "Tychonoff's Theorem and Zorn's Lemma"
date: 2021-02-02T11:20:32+01:00
draft: true
katex: true
tags:
  - Math
  - "Set Theory"
  - Topology
---

Tychnoff's theorem proves that the product (even infinite) of compact spaces
is also compact. The proof makes judicious use of Zorn's lemma, and made
me appreciate how fun its application can be.

<!--more-->

In this post, I'll be going over the notion of *Product space* and *Compact space*,
building up to the final result.

I think that the final proof makes elegant use of a powerful tool:
Zorn's lemma. This use is so elegant, in fact, that as soon as I first read
the proof, I felt compelled to write a blog post exposing the proof,
and trying to describe its elegance to others.

All the proofs are inspired by Munkres {{<ref-link "1">}}, which is the standard
introductory text on Topology. I won't be going over the basics of Topological
spaces and continuous functions here, so I recommend this book if you'd like
to see these fundamentals, or learn more about Topology.

# Products

## Finite Products

## Extending to the infinite case

# Compactness

## A notion of smallness

**TODO: Add prerequisite theorems here**

# Tychonoff's Theorem

We've seen the notion of a *Product* and of *Compactness*. A natural
question to ask is whether these properties are compatible:

Given a collection $\\{X_\alpha\\}$ of compact spaces, is the product $\prod_{\alpha} X_\alpha$
also a compact space?

Tychnoff's theorem answers this with a definitive **yes**.

## A rough idea, and a problem

It's more convenient to use the finite intersection version of compactness here.
Given a collection $\mathcal{A}$ of closed subsets of our product space, with
the finite intersection property, we need to find a point in the intersection $\bigcap \mathcal{A}$.

The idea is that we can find this point component by component, using that fact that each part
of our product space is already compact.

Concretely, consider the product $[0, 1] \times [0, 1]$. Let $a = (\frac{1}{3}, \frac{1}{3})$
and $b = (\frac{2}{3}, \frac{2}{3})$. Our collection $\mathcal{A}$ consists of rectangles
centered along the segment $ab$:

{{<img "1.png">}}

This has the finite intersection property. This is because for any two rectangles centered
on this line, the
smaller one will always be contained in the larger one.

In fact, we can see the final intersection of this collection. It is the
segment $ab$ itself:

{{<img "2.png">}}

If we project each rectangle down to one of the intervals, we get a nested
set of closed intervals, eventually reaching $[\frac{1}{3}, \frac{2}{3}]$:

{{<img "3.png">}}

Now, the rough idea we had earlier was to pick a point in the intersection
of each projection, and then use that. Let's say we pick $\frac{1}{3}$ and $\frac{2}{3}$.
The problem is that $(\frac{1}{3}, \frac{2}{3})$ is not in the intersection
$\bigcap \mathcal{A}$,
in the product $[0, 1] \times [0, 1]$:

{{<img "4.png">}}

It's possible to make the "wrong" choice when considering each of the points individually.
We need to use the way that our collection $\mathcal{A}$ puts constraints
on each component *at the same time*, when considering how to make each component.
We've gotten rid of this global information by limiting ourselves to considering
just the component collections $\pi_X(\mathcal{A})$, and $\pi_Y(\mathcal{A})$.

We can amend this by choosing a collection of sets that contains $\mathcal{A}$.
Let's now add in all the rectangles centered on the same segment $ab$,
but where the "top" can vary as well:

{{<img "5.png">}}

Now this collection "hones in" precisely on the point $(\frac{1}{3}, \frac{1}{3})$,
and our method of construction works, since the projections of this set have
to end up at $\frac{1}{3}$ as well.

The reason our original collection didn't work is because
$\pi_1(C) \times \pi_2(C) \neq C$ in general, and especially not in this case.
When we project down the line segment, and then glue each projection back up,
we end up with something way too big:

{{<img "6.png">}}

On the other hand, when we have a single point, things work out just fine:

{{<img "7.png">}}

### Lessons Learned

In a sense, the collection $\mathcal{A}$ was not strict enough to force
each component to be coherent, and so it was possible to make the "wrong" choice.
We have too much freedom in our choice for each component, so it's possible
to not respect the conditions that $\mathcal{A}$ imposes only on the *product space*.

Now, in this specific case, it's easy to find a collection $\mathcal{B} \supseteq \mathcal{A}$
that that's strong enough to force our components to be coherent. But in general,
we can't leave this up to chance or observation, we need a fool-proof method
to make this work.

What we're going to do is simply consider all such collections. If we consider
the biggest set with the finite intersection property that contains $\mathcal{A}$,
we'll be able to use all of the insight we could ever gleam from picking
a collection by hand.

Another way to think about this is that for any collection $\mathcal{C}$,
we can find an intersection of $\overline{\pi_\alpha(\mathcal{C})}$
by compactness. If we restrict ourselves to just doing this for the one collection
we've been given, we're not exercising the full power of compactness.
The more sets are in a collection the more "difficult" it is to find a point
in the intersection of these sets. By using the biggest possible collection
containing $\mathcal{A}$, we exercise the full power of our assumption that
$X_\alpha$ is compact.

## Zorn's Lemma

Given some collection $\mathcal{A}$, with the finite intersection property,
we'd like to show that there's a "biggest" collection $\mathcal{D}$ containing
$\mathcal{A}$, and satisfying the finite intersection property.

The problem with trying to construct this object is that the "super-collection":
$$
\\{\mathcal{D} \ |\ \mathcal{D} \supseteq \mathcal{A},\ \mathcal{D} \text{ FIP} \\}
$$
is pretty unwieldy. In fact, it's very uncountable. Super uncountable, if you will.
Trying to build it directly, or to use induction are going to fail immediately.

On the other hand, there's never any obstacle to us continuing to build this
mega-set. If at some point we're working on our collection $\mathcal{D}$, and we
realized that there's an even bigger $\mathcal{D'}$ that works, then we can just
use that instead, and keep working.

If we ever encounter some kind of obstacle, we can swiftly adjust our course around it,
or incorporate whatever information it's signalling to us.

This is the kind of situation in which Zorn's lemma applies. We're trying to build
a big object, that requires considering an infinite number of steps, but where
no obstacle to our construction ever pops up.

**Theorem: (Zorn's Lemma)** Given a partially ordered set $(X, \leq)$, if
every chain (totally ordered subset) $C \subseteq X$ has an upper bound in $X$,
then $X$ has a maximal element.

{{<note>}}
Note that "maximal" simply means that nothing is bigger. i.e. $m \in X$ maximal $\iff$
$x \geq m \implies x = m$.

In particular, this *does not imply* $\forall x. \ x \leq m$.
{{</note>}}

Proving this is far beyond the scope of this post. Essentially,
this theorem is taken as an axiom. At least, that's how I think of it.
Using set theory, and *assuming* the axiom of choice, you can prove this lemma. In fact,
this lemma is *equivalent* to the axiom of choice. Morally speaking,
the axiom of choice is inoffensive, and used constantly throughout Topology,
but Zorn's lemma seems much more powerful, and used, just not constantly.

But, this feeling is just an illusion, because once you've assumed the axiom of choice,
you have no choice but to accept Zorn's lemma as well.

Thankfully, I have no moral qualms about using Zorn's lemma, and I actually find it kind
of cool when I do get to use it, so let me show you how to apply it in our case.

## The biggest FIP

Concretely, we'll be proving the following theorem:

**Theorem:** Given a set $X$, and a collection of subsets $\mathcal{A}$ of $X$, with the FIP property,
there exists a collection $\mathcal{D}$ containing $\mathcal{A}$ and with the FIP property,
that is not strictly contained in any other such collection

**Proof:**

As hinted at, we'll be using Zorn's lemma. The first step in applying it is to find
a partially ordered set. Consider:

$$
\mathcal{A}_\subseteq := \\{\mathcal{B} \ |\ \mathcal{A} \subseteq \mathcal{B},\ \mathcal{B} \text{ FIP}\\}
$$

For the partial order $\leq$, we'll be using $\subseteq$.

A maximal element $\mathcal{D}$ of this set would be a collection satisfying all the properties
we want. We can use Zorn's lemma to find this collection, provided that every chain has an upper bound.

Let's say we have a chain $\frak{B}$. This means that for any
$\mathcal{B}, \mathcal{C} \in \frak{B}$ either $\mathcal{B} \subseteq \mathcal{C}$ or $\mathcal{C} \subseteq \mathcal{B}$.
We need to show that this super-collection has an upper bound in $\mathcal{A}_\subseteq$.

If this chain is empty, simply take $\mathcal{A}$ as an upper bound.

If it is not empty,
let's just take the union of all of the collections:

$$
\mathcal{U} := \bigcup_{\mathcal{B} \in \frak{B}} \mathcal{B}
$$

Certainly, this is an upper bound, so we just need to check that
it is contained in $\mathcal{A}_\subseteq$.

First, it obviously contains $\mathcal{A}$. Because the chain is non-empty,
we have some $\mathcal{B} \subseteq \mathcal{U}$. By assumption $\mathcal{A} \subseteq \mathcal{B}$,
since we've taken a chain in $\mathcal{A}_\subseteq$.

Secondly, it satisfies the FIP property.
Given two sets $C_1$ and $C_2$ in $\mathcal{U}$, there exists,
by definition, two collections $\mathcal{B}_1$ and $\mathcal{B}_2$, with
$C_1 \in \mathcal{B}_1$ and $C_2 \in \mathcal{B}_2$. Without loss of generality,
assume $\mathcal{B}_1 \subseteq \mathcal{B}_2$. Then,
$C_1 \in \mathcal{B}_2$ as well, alongside $C_2$. Their intersection,
$C_1 \cap C_2$ is also in $\mathcal{B}_2$, and therefore not empty.

Having shown that every chain in $\mathcal{A}_\subseteq$ has an upper bound,
we can now apply Zorn's lemma, and find our maximal collection $\mathcal{D}$.

$\square$

This collection is so all-encompassing, that if a set touches
every set in the collection, it must *itself* be in the collection.

Concretely

**Lemma:** If $B$ is a subset of $X$, intersecting every element of $\mathcal{D}$, then $B \in \mathcal{D}$
**Proof:**
We show that $\mathcal{D} \cup \\{B\\}$ has the FIP, which makes it equal
to $\mathcal{D}$, meaning $B \in \mathcal{D}$ in the first place.

Given $D_1, D_2 \in \mathcal{D} \cup \\{B\\}$, either both are drawn from $\mathcal{D}$,
in which case their intersection is in this collection, since $\mathcal{D}$ has the FIP,
or both are $B$, in which case $B \cap B = B$ is evidently in the collection, *or*
we have $D \in \mathcal{D}$ and $B$.

But, $B \cap D$ is non-empty by assumption, since $B$ intersects every $D \in \mathcal{D}$.

$\square$

## Proving our Theorem

We can now use this strategy to prove our theorem:

**Tychnoff's Theorem:** Given a collection $\\{X_\alpha\\}$ of Compact spaces,
the product $\prod_\alpha X_\alpha$ is also a Compact space.

**Proof:**
Let $\mathcal{A}$ be a collection of closed sets with the FIP. We show that
the intersection:

$$
\bigcap_{A \in \mathcal{A}} \overline{A}
$$

is non-empty. If this were a collection of closed sets, this would be the intersection $\bigcap \mathcal{A}$.
Compactness thus follows from showing this.

Now, we can find a maximal collection $\mathcal{D}$ containing $\mathcal{A}$, and satisfying the FIP,
as we've developed earlier.

If $\bigcap_{D \in \mathcal{D}} \overline{D}$ is non-empty,
then certainly $\bigcap_{A \in \mathcal{A}} \overline{A}$
would also be non-empty.

Consider the collection:

$$
\\{\pi_\alpha(D) \ |\ D \in \mathcal{D}\\}
$$

This set satisfies the FIP, because:

$$
\pi_\alpha(D_1 \cap D_2) \subseteq \pi_\alpha(D_1) \cap \pi_\alpha(D_2)
$$

(this observation holds for any function $f$)

By compactness of each $X_\alpha$, we can then choose
a point $x_\alpha$ such that:

$$
x_\alpha \in \bigcap_{D \in \mathcal{D}} \overline{\pi_\alpha(D)}
$$

Let $\bold{x} = (x_\alpha)$ by the product of these components.
We need to show that
$\bold{x} \in \bigcap_{D \in \mathcal{D}} \overline{D}$.

If we have a sub-basis element $\pi_\alpha^{-1}(U_\alpha)$,
then we can show that it intersects every $D \in \mathcal{D}$.

Since $x_\alpha \in \overline{\pi_\alpha(D)}$ by definition,
so we have $U_\alpha$ intersecting $\pi_\alpha(D)$
in some point $\pi_\alpha(\bold{y})$,
with $\bold{y} \in D$.
It's clear that $\bold{y} \in \pi_\alpha^{-1}(U_\alpha) \cap D$.

Since $\pi_\alpha^{-1}(U_\alpha)$ intersects every
$D \in \mathcal{D}$, it belongs to $\mathcal{D}$,
because of $\mathcal{D}$ being maximal,
as we proved earlier.

Since basis elements in the Product Topology
consist of finite intersections of these subsets,
basis elements containing $\bold{x}$ are also
in $\mathcal{D}$, since $\mathcal{D}$ is closed under
finite intersection, as we showed earlier.

Since $\mathcal{D}$ has the FIP, any element in $\mathcal{D}$
intersects every other element. Thus,
any basis element containing $\bold{x}$
intersects every $D \in \mathcal{D}$. This means
that any neighborhood of $\bold{x}$ intersects
every $D \in mathcal{D}$, that is to say:

$$
\bold{x} \in \bigcap_{D \in \mathcal{D}} \overline{D}
$$

This means that
$$
\bigcap_{A \in \mathcal{A}} \overline{A} \neq \emptyset
$$
which implies that $\prod_\alpha X_\alpha$ is compact.

$\square$

# Conclusion

# References

{{<ref
  "1"
  "https://www.pearson.com/us/higher-education/product/Munkres-Topology-2nd-Edition/9780131816299.html"
  "[1] Munkres, James R. Topology; a First Course. Prentice-Hall, 1974.">}}