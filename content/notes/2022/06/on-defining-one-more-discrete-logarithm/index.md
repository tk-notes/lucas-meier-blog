---
title: "On Defining the One More Discrete Logarithm Problem"
date: 2022-06-02T07:29:22+02:00
type: note
note-tags:
  - "Cryptography"
  - "Foundations"
katex: true
---

Here are three slightly different notions of the "One More Discrete Logarithm Problem" (OMDL), formulated as games.
<!--more-->

{{<note>}}
I'll be using the formalism of State-Separable proofs,
as [I wrote about recently](/posts/2022/05/state-separable-proofs-for-the-curious-cryptographer/).
{{</note>}}

First, the classic notion:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
$\text{OMDL-Adaptive}_b$
}\cr
\cr
&x\_0, \ldots, x\_{n + 1} \xleftarrow{R} \mathbb{Z}/(q)\cr
&X\_0, \ldots, X\_{n + 1} \gets x_i \cdot G\cr
\cr
&\underline{\texttt{Public()}:}\cr
&\ \texttt{return } (X\_0, \ldots, X\_{n + 1})\cr
\cr
&\text{count} \gets 1\cr
&\underline{\texttt{DLog(}i\texttt{)}:}\cr
&\ \texttt{assert } \text{count++} < n + 1\cr
&\ \texttt{return } x_i\cr
\cr
&\underline{\texttt{Challenge(}\hat{x}\_0, \ldots, \hat{x}\_{n + 1}\texttt{)}:}\cr
&\ \texttt{return } b = 0 \land \forall i.\ \hat{x}_i = x_i\cr
\end{aligned}
}
$$

In this game, the adversary is given $n + 1$ group elements,
and can query for $n$ of the discrete logarithms, adaptively.

Another variant allows queries for elements of the adversary's
choice, but not adaptively. This game is identical,
with the $\texttt{DLog}$ being the only change:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
$\text{OMDL-Dynamic}_b$
}\cr
&\ldots
\cr
&\text{count} \gets 1\cr
&\underline{\texttt{DLog(}\hat{i}\texttt{)}:}\cr
&\ \texttt{assert } \text{count++} = 1\cr
&\ \texttt{return } (x_i\ |\ i \neq \hat{i})\cr
\end{aligned}
}
$$

Finally, you can also make it so that the adversary doesn't
get to choose which group elements they get the logarithm
for:

$$
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
$\text{OMDL-Static}_b$
}\cr
&\ldots
\cr
&\underline{\texttt{DLog(}\texttt{)}:}\cr
&\ \texttt{return } (x_i\ |\ i \neq n + 1)\cr
\end{aligned}
}
$$

Now, because each of these gives progressively weaker
capabilities, we have:

$$
\text{OMDL-Static}_b \leq \text{OMDL-Dynamic}_b \leq \text{OMDL-Adapative}_b
$$

This can be proved via straight-line reductions.

The other direction is harder.

# $\text{OMDL-Dynamic}_b \leq \text{OMDL-Static}_b$

The idea behind this reduction is that if the adversary
$\mathcal{A}$ for $\text{OMDL-Dynamic}$ happens to ask
for $\hat{i} = n + 1$, you can satisfy their query immediately.
At first, this seems like it would reduce your advantage by
a factor of $n + 1$, since you need to get lucky.

The trick is that you can reset the adversary, and try again.
The adversary may not ever pick $n + 1$ in their strategy,
so instead you need to rearrange the group elements yourself,
picking where you place the "hole" randomly, so that there's
always a $1 / (n + 1)$ chance of the adversary picking it.

**Proof:**

More formally, given this adversary $\mathcal{A}$ for $\text{OMDL-Dynamic}$, we construct an adversary $\mathcal{B}$ for
$\text{OMDL-Static}$, as follows:

1. Query $\texttt{Public}$ and $\texttt{DLog}$ to learn $X_0, \ldots, X_{n + 1}$ and associated $x_0, \ldots, x_n$. Let $x_{n + 1} = \bot$.
2. Retry the following steps until they succeed (resetting any state modifications):
3. Choose random $r\_0, \ldots, r\_{n + 1} \xleftarrow{R} \mathbb{Z}/(q)$, and set $x_j \gets r_j \cdot x_j$, as well as $X_j \gets r_j \cdot X_j$.
4. Choose a random $j \xleftarrow{R} [1, \ldots, n + 1]$, and swap $X_j$ and $X\_{n + 1}$, as well as $x_j$ and $x\_{n + 1}$.
5. Run $\mathcal{A}$ using these group elements and scalars to answer their queries,
dividing by $r_j$ before forwarding their answers to $\texttt{Challenge}$, and abort if they call $\texttt{DLog}$ with $\hat{i} \neq j$.
6. Return what $\mathcal{A}$ returned.

The expected number of retries is $n + 1$. This is because the
behavior of $\mathcal{A}$ is completely independent
of $j$, particularly because we randomize the group elements
at each iteration, so that they're indistinguishable from
freshly sampled group elements. Thus, at each try,
there's at most an $n / (n + 1)$ chance that we have to abort
when running $\mathcal{A}$.

$\square$

# $\text{OMDL-Adaptive}_b \leq \text{OMDL-Static}_b$

It's actually not any easier to prove $\text{OMDL-Adaptive} \leq \text{OMDL-Dynamic}$,
so we'll prove this one instead.

The basic idea of the reduction is the same, it's just that the analysis is
more complicated. We re-randomize and shuffle the group elements as in the previous
reduction, and we abort the adversary $\mathcal{A}$ if it ever queries the group
element we have no scalar for. Analyzing this is trickier because of the adaptivity.

The idea is that given our choices of group elements $X\_0, \ldots, X\_{n + 1}$,
we can look at the sequence of queries made by $\mathcal{A}$, if they get
the answer each time. This defines a sequence of random variables $i_0, \ldots, i_n$,
which depends solely on $X_i$. We can also define a related random variable,
$\hat{i}$, represented the index which *isn't* queried. This is a function of $i_0, \ldots, i_n$,
and so depends solely on our choice of $X_i$.

In particular, $\hat{i}$ is independent from the random $j$ we choose at each iteration,
because of the re-randomization, and so the analysis from the previous reduction
applies exactly.

$\square$
