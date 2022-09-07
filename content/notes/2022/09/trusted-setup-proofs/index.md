---
title: "Powers of Tau Proofs"
date: 2022-09-07T22:43:30+02:00
type: note
note-tags:
  - "Cryptography"
  - "ZK Proofs"
katex: true
---

Here's a starting point for the proofs: [https://github.com/a16z/evm-powers-of-tau/blob/master/techreport/main.pdf](https://github.com/a16z/evm-powers-of-tau/blob/master/techreport/main.pdf).

You have $P_{1, j - 1}, P_{2, j - 1}, \ldots, P_{n, j - 1}; P_{+, j - 1}$.

Participant generates $r \xleftarrow{R} \mathbb{F}\_p^*$,
and publishes $P_{1, j}, P_{2, j}, \ldots; P_{+, j}$.
This should satisfy:
$$
\begin{aligned}
P_{1, j} &= r \cdot P_{1, j - 1}\cr
P_{2, j} &= r^2 \cdot P_{2, j - 1}\cr
&\ \vdots\cr
P_{+, j} &= r \cdot P_{+, j - 1}\cr
\end{aligned}
$$

The following properties need to be proved:

1. The prover knows $r$
2. The prover correctly used $r$, or at least, $P_{i, j}$ consist
   of a valid powers of tau setup, assuming the $P_{i, j-1}$ did.
3. $r \neq 0$

The protocol above does 2. with pairings.


# Using Maurer Proofs

c.f. [my blog post on Maurer proofs](/posts/2022/08/the-paper-that-keeps-showing-up/)


As a starting point, consider the following toy relation:

$$
\begin{aligned}
&\\{(A, B, G; a, b)\ |\\cr
&\quad a \cdot G = A\cr
&\quad b \cdot G = B\cr
&\quad b = a^2\cr
&\\}\cr
\end{aligned}
$$

The tricky aspect is the $b = a^2$ part.
The rest can be done with a Maurer proof.

One way around this is to tweak the relation slightly:

$$
\begin{aligned}
&\\{(A, B, G; a, b)\ |\\cr
&\quad a \cdot G = A\cr
&\quad a \cdot A = B\cr
&\quad b \cdot G = B\cr
&\\}\cr
\end{aligned}
$$

Because $b = a^2$, we have $a \cdot A = a^2 \cdot G = b \cdot G = B$,
so this relation is equivalent.
Notice also that this relation is captured by the homomorphism:

$$
\varphi(a, b) := (a \cdot G, a \cdot A, b \cdot G)
$$

A Maurer proof will work, wherein you check that
$\varphi(a, b) = (A, B, B)$

We can extend this to the degree $3$ case as well:

$$
\begin{aligned}
&\\{(A, B, C, G; a, b, c)\ |\\cr
&\quad a \cdot G = A\cr
&\quad b \cdot G = B\cr
&\quad c \cdot G = C\cr
&\quad b = a^2\cr
&\quad c = a^3\cr
&\\}\cr
\end{aligned}
$$

this time, we use the homomorphism:

$$
\varphi(a, b, c) := (a \cdot G, a \cdot A, b \cdot G, a \cdot B, c \cdot G)
$$

with expected output $(A, B, B, C, C)$.


### Tau Proof

The relation you want to prove is:

$$
\\{(P_{i, j}, P_{i, j - 1}; r)\ |\ r^i \cdot P_{i, j - 1} = P_{i, j}\\}
$$

(taking the convention $\tau^+ = \tau$)

If you had a vector $\bold{r}$ such that $\bold{r}_i = r^i$, then you could
just use a standard Maurer proof here.
The issue with that is that you need to enforce a specific relation
between the vector elements.

We can use the proof we developed in the previous section to get
around this restriction though.

We publish $\bold{R} := \bold{r} \cdot G$, and then create a proof
for the following relation:

$$
\begin{aligned}
&\\{(P_{i, j}, P_{i, j - 1}, \bold{R}; \bold{r})\ |\\cr
&\quad \forall i \in [n - 1].\ \bold{r}_1 \cdot \bold{R}_i = \bold{R}_{i + 1}\cr
&\quad \forall i \in [n].\ \bold{r}_i \cdot P_{i, j - 1} = P_{i, j}\cr
&\\}\cr
\end{aligned}
$$

This can be done via a Maurer proof for the following homomorphism:

$$
\varphi(\bold{r}) := ([\bold{r}\_i \cdot \bold{R}\_i], [\bold{r}\_i \cdot P\_{i, j - 1}])
$$

With the expected output being:

$$
(\bold{R}\_{i \geq 2}, [P\_{i, j}])
$$

The vector $\bold{r}$ has to be initialized to the powers of $r$.