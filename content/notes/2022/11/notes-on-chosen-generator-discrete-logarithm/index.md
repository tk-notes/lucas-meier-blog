---
title: "Notes on Chosen Generator Discrete Logarithms"
date: 2022-11-01T18:58:58+01:00
type: note
note-tags:
  - "Cryptography"
  - "Foundations"
  - "Math"
  - "Programming"
katex: true
---

Consider the basic discrete logarithm game:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{DLog}_b$
}\cr
\cr
&x \xleftarrow{\\$} \mathbb{F}_q\cr
\cr
&\underline{\mathtt{Pub}():}\cr
&\ \texttt{return } x \cdot G \cr
\cr
&\underline{\mathtt{Win}(\hat{x}):}\cr
&\ \texttt{return } b = 0 \land \hat{x} = x\cr
\end{aligned}
}
$$

Here, the generator is fixed as part of the configuration of the group.
One natural question is whether or not allowing the adversary
to choose the generator helps them.
This variant with a chosen generator could be described as follows:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{CLog}_b$
}\cr
\cr
&P, x \gets \bot\cr
\cr
&\underline{\mathtt{SetP}(P):}\cr
&\ \texttt{assert } P \neq 0\cr
&\ P \gets P\cr
&\ x \xleftarrow{\\$} \mathbb{F}_q\cr
&\ \texttt{return } x \cdot P \cr
\cr
&\underline{\mathtt{Win}(\hat{x}):}\cr
&\ \texttt{assert } P \neq \bot\cr
&\ \texttt{return } b = 0 \land \hat{x} = x\cr
\end{aligned}
}
$$

{{<note>}}
It is convenient to assume that the adversary cannot send a $P$
equal to the identity point, which makes the rest of the proofs smoother.
Sending the identity point also forces the adversary to guess randomly,
so it's not a good strategy anyhow.
{{</note>}}

Now, clearly $\text{DLog} \leq \text{CLog}$.
The question is, if $\text{DLog}$ is hard, what can we say about $\text{CLog}$?
Does being able to choose the generator help?

A little bit, at least with the techniques I've managed to gather together.

In particular, we can show that:

$$
\text{CLog} \leq \sqrt{\text{DLog}}
$$

If anyone knows how to get a tighter bound, I'm all ears!

{{<note>}}
I'm assuming that we're working in a group of prime order $q$,
and thus that all points besides the identity are generators.
{{</note>}}

# Reduction from Multi-CLog

Instead of reducing directly from $\text{CLog}$, we instead reduce
from a variant where the adversary needs to solve multiple challenges
on their provided generator:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\text{CLog}_b^n$
}\cr
\cr
&P, x_1, \ldots, x_n \gets \bot\cr
\cr
&\underline{\mathtt{SetP}(P):}\cr
&\ \texttt{assert } P \neq 0\cr
&\ P \gets P\cr
&\ x_1, \ldots, x_n \xleftarrow{\\$} \mathbb{F}_q\cr
&\ \texttt{return } x_1 \cdot P, \ldots \cr
\cr
&\underline{\mathtt{Win}(\hat{x}_1, \ldots, \hat{x}_n):}\cr
&\ \texttt{assert } P \neq \bot\cr
&\ \texttt{return } b = 0 \land \forall i \in [n]\ \hat{x}_i = x_i\cr
\end{aligned}
}
$$

Later in this post, we show that $\text{CLog} \leq \sqrt[n]{\text{CLog}^n}$.

In this section, we show that $\text{CLog}^2 \leq \text{DLog}$,
which gives us the aforementioned result that $\text{CLog} \leq \sqrt{\text{DLog}}$, combining the two reductions.

**Lemma:** $\text{CLog}^2 \leq \text{DLog}$

The basic idea is that two queries are enough to learn
the discrete logarithm of $G$ with respect to $P$, and some point
$X$ with respect to $P$.
We combine these two pieces of information to learn the discrete
logarithm of $X$ with respect to $G$.

In more detail, consider the following game:

$$

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^0$
}\cr
\cr
&P \gets \bot\cr
&X \gets \texttt{super}.\text{Pub}()\cr
&r \xleftarrow{\\$} \mathbb{F}_q\cr
&R \gets r \cdot G\cr
\cr
&\underline{\mathtt{SetP}(P):}\cr
&\ \texttt{assert } P \neq 0\cr
&\ P \gets P\cr
&\ \texttt{return } R, X \cr
\cr
&\underline{\mathtt{Win}(\hat{r}, \hat{x}):}\cr
&\ \texttt{assert } P \neq \bot\cr
&\ \texttt{return } \hat{r} \cdot P = R \land \texttt{super}.\text{Win}\left(\hat{x} \cdot \frac{r}{\hat{r}}\right)\cr
\end{aligned}
}
$$

I claim that $\Gamma^0 \circ \text{DLog}_b = \text{CLog}^2_b$

First, note that the adversary's perspective is the same.
They see two uniformly random group elements (thanks to our obligation
that $P$ is a generator).
So, it boils down to whether or not our check is equivalent
First, note that the check we're supposed to be doing is that:

$$
\begin{aligned}
\hat{r} \cdot P &= r \cdot G\cr
\hat{x} \cdot P &= x \cdot G\cr
\end{aligned}
$$

Or, considering $P$ to have discrete logarithm $p$, that:

$$
\begin{aligned}
\hat{r} \cdot p &= r\cr
\hat{x} \cdot p &= x\cr
\end{aligned}
$$

Note that we manually check the first equation, and so if that's
false, we behave in the same way, by rejecting.

So, assume that $\hat{r} p = r$.
In this case, $p = \frac{r}{\hat{r}}$,
and so applying that to the second equation gives us
$\hat{x} \frac{r}{\hat{r}} \stackrel{?}{=} x$,
and so our discrete logarithm game will tell us that correctly.

$\blacksquare$

# Reducing CLog to Multi-CLog

In this section, we prove that $\text{CLog} \leq \sqrt[n]{\text{CLog}^n}$.

The idea is simple, although unfortunately not a straight line reduction.
(I should encapsulate this into one of "my" "state-separable proof gadgets")

The idea is that given an adversary $\mathcal{A}$ for $\text{CLog}$,
you create an adversary $\mathcal{B}$ for $\text{CLog}^n$ by repeatedly rewinding
the adversary back to the point after sending $P$, you then feed them
a different $X_i$ each time, getting back $\hat{x}_1, \ldots, \hat{x}_n$
across the different forks.
Because each $X_i$ has the same distribution as in $\text{CLog}$,
the probability of succeeding is bounded below by $\text{CLog}^n$,
since we have $n$ forked instances which need to succeed.

This gives us our result.

# Putting everything together

To recap, we have:

$$
\begin{aligned}
\text{CLog} &\leq \sqrt{\text{CLog}^2}\cr
\text{CLog}^2 &\leq \text{DLog}
\end{aligned}
$$

which implies:

$$
\text{CLog} \leq \sqrt{\text{DLog}}
$$

I have a hunch that the square root loss in security is essential,
but getting this tighter would be quite interesting, imo.