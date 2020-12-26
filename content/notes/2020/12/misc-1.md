---
title: "Misc 0"
date: 2020-12-26T15:15:24+01:00
type: note
katex: true
note-tags:
  - "Programming"
  - "Green Threads"
  - "Topology"
  - "Math"
  - "Algebra"
  - "Misc"
---

This post has a lot of interesting information about green threads:

https://graphitemaster.github.io/fibers/

I kind of like the idea of using green threads for *Iku*, that programming
language I've been wanting to work on for a while. The implementation
of this seems a bit complicated though.

# Topological Groups

If $G$ is a Topological Group, then the connected component $C$
containing the identity $e$ is a normal subgroup of $G$.

First, note that for any $g \in G$, $gC$ is also a connected component.
If $gx, gy \in gC$, then $x, y \in C$. This means they share
a connected subspace $A$. The image $gA$ is also a connected subspace,
since the action of $g$ is a continuous function. Evidently,
$gA$ contains $gx$ and $gy$. This means that any two points
of $gC$ share a connected subspace.

A similar proof shows that $Cg$ is also a connected component.

It's easy to see that $C$ is a subgroup.

First, $e \in C$, by
assumption.

Second, we need to show that $a, b \in C \implies ab^{-1} \in C$.
Because $e \in C$, we have $ab^{-1} \in ab^{-1}C$. Since $a = ab^{-1}b$,
and $b \in C$,
we have $a \in ab^{-1}C$ as well. But since $ab^{-1}C$ is a connected
component, i.e. equivalence class, and it contains $a$,
it must be equal to $C$, since $C$ is an equivalence class containining
$a$. Since $ab^{-1}C = C$, $ab^{-1}$ must be in $C$.

To see that $C$ is normal, we show that $gC = Cg$. For this, note
that $gC$ and $Cg$ are both connected components, as
we showed earlier. But, since they both contain $g$,
they must be equal.

$\square$

# Font Rendering

For some reason I've gotten somewhat curious about font rendering as a potential project.
It falls under the "software I use without really understanding, for sure".

Here's some links I got on Discord:

https://gankra.github.io/blah/text-hates-you/
https://crates.io/crates/fontdue

