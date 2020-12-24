---
title: "Topological Groups are Hausdorff"
date: 2020-12-24T12:50:13+01:00
type: note
note-tags:
  - "Math"
  - "Topology"
  - "Algebra"
katex: true
---

A *Topological Group* is a *Group Object* in the Category $\bold{Top}$ of *Topological Spaces*
and *Continuous Functions*.

Concretely, this is a topological space $G$, endowed with two continuous functions:

$$
\begin{aligned}
\bullet &: G \times G \to G \cr
()^{-1} &: G \to G
\end{aligned}
$$

and a distinguished element $e \in G$, satisfying the usual group axioms:

$$
\begin{aligned}
e \bullet x = x = x \bullet e \cr
x^{-1} \bullet x = e = x \bullet x^{-1} \cr
a \bullet (b \bullet c) = (a \bullet b) \bullet c
\end{aligned}
$$

An extra assumption, made at least by **Munkres**, is that $G$ is a $T_1$ space.
This means that for any points $x, y \in G, x \neq y$, we have neighborhoods
$x \in U_x, y \in U_y$ that don't contain the other point, i.e.
$x \notin U_y, y \notin U_x$.

This extra assumption is enough to show that $G$ is a Hausdorff space.
When $x \neq y$, not only can we find neighborhoods $U_x, U_y$
that don't contain the other point, but that are distinct, with $U_x \cap U_y = \empty$.

# Hausdorff $=$ closed diagonal

Another characterization of Hausdorff is:

A space $X$ is *Hausdorff*, if and only if

The set:

$$
\Delta := \\{(x, x) | \ x \in X\\}
$$

is closed in $X^2$.

**Proof:**

$\implies$

We show that there exists an open neighborhood $N(x, y)$ around any point $(x, y)$ with $x \neq y$,
such that $N(x, y) \cap \Delta = \empty$. We then have that:

$$
\bigcup_{x \neq y} N(x, y) = X^2 - \Delta
$$

is open, meaning $\Delta$ is closed.

Since $X$ is Hausdorff, and $x \neq y$, there exists neighborhoods $U_x, U_y$
of $x$ and $y$, respectively, such that $U_x \cap U_y = \empty$.

$U_x \times U_y$ is evidently a neighborhood of $(x, y)$. Furthermore,
assume $(x', y') \in U_x \times U_y$. Because $U_x$ and $U_y$ are distinct,
we must have $x' \neq y'$. This means that $U_x \times U_y \cap \Delta = \empty$.

This is our neighborhood $N(x, y)$.

$\impliedby$

If $\Delta$ is closed, then the set $X^2 - \Delta$ is open. This means that
if $x \neq y$, then we can find a neighborhood $U_x \times U_y \subseteq X^2 - \Delta$ of $(x, y)$,
with $U_x$, $U_y$ open in $X$, by definition of the product topology.

Since $U_x \times U_y \subseteq X^2 - \Delta$, this means that

$$
x' \in U_x, y' \in U_y \implies (x', y') \notin \Delta
$$

That is to say:

$$
x' \in U_x, y' \in U_y \implies x' \neq y'
$$

Which means that $U_x \cap U_y = \empty$. Our space is thus Hausdorff.

$\square$

# $T_1 \implies$ Hausdorff, for a Group

First, note that the function $f = (x, y) \mapsto xy^{-1}$ is continuous. This is because
it is the composition of two continuous maps:

$$
\bullet \circ (id \times ()^{-1}) 
$$

For $G$ to be Hausdorff, we need $\Delta$ to be closed. Note
that $\Delta = f^{-1}(\\{e\\})$, since:

$$
f^{-1}(\\{e\\}) = \\{(x, y) | xy^{-1} = e\\} = \\{(x, y) | x = y\\} = \Delta
$$

It then suffices to show that $\\{e\\}$ is closed in $G$, since its preimage
under $f$ will also be closed, because $f$ is continuous.

This is easy to show, provided we assume that $G$ is a $T_1$ space.
This assumption implies that for each $x \neq e$, we can find a neighborhood
$U_x$ of $x$ with $e \notin U_x$.

If take the union of all of these neighborhoods, we have:

$$
\bigcup_{x \neq e} U_x = G - \\{e\\}
$$

Which means that $\\{e\\}$ must be closed.

$\square$

