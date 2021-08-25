---
title: "Better Notation for Matrices"
date: 2021-08-25T08:38:01+00:00
type: note
katex: true
note-tags:
  - "Math"
---

Let $[l] := \\{1, \ldots, l\\}$.

Given an $r \times c$ matrix $M$, let $M^i$ denote the ith column,
with $i \in [c]$, and $M_j$ denote the jth row, with $j \in [r]$
and $M^i_j$ denotes a single entry. This makes
it alot easier to swap between transposes when doing calculations.
For example, if $\Delta_i$ is a vector of bits, with $\cdot$ denoting
multiplication, and $*$ denoting convolution, you can do:

$$
\begin{aligned}
&M^i = T^i + \Delta_i \cdot X^i \cr
&M^i_j = T^i_j + \Delta_i \cdot X^i_j \cr
&M_j = T_j + \Delta_i * X_j
\end{aligned}
$$

This is a lot more convenient.

The only issue is that this conflicts with tensor notation. Thankfully, there's not much overlap between general relativity and Cryptography.
