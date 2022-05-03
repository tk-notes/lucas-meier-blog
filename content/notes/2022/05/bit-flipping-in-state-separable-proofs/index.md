---
title: "Bit Flipping in State Separable Proofs"
date: 2022-05-03T21:20:48+02:00
type: note
note-tags:
  - "Cryptography"
  - "Foundations"
  - "Math"
katex: true
---

When working with [State Separable Proofs](https://eprint.iacr.org/2018/306.pdf), it's often easier to frame every game as distinguishing
between the "real world" and the "ideal world". This allows
keeping the framing consistent throughout the composition
of different games / packages.

For example, the security of a PRF is defined in this real ideal
paradigm, by distinguishing between the case of actually calling
the PRF with a key, and the case of a genuine random function.

If you then use this PRF to make an encryption scheme,
it's useful to also define security in terms of distinguishing
between encrypting messages, and encrypting random ciphertexts,
which helps the proofs go more smoothly when trying
to reduce the security of the larger encryption scheme
to the security of the smaller PRF.

Contrast this with the other common notion of encryption security,
in which the adversary sends two messages each round,
and has to figure out whether the challenger is consistently
encrypting the first or the second message.

This begs the question: can you reduce the real-or-random
notion of encryption to the left-or-right notion of encryption?
Yes.

# Background

For left-or-right, we have the following pair of games:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{LR}_{b}$
}\cr
\cr
&k \xleftarrow{R} K\cr
&\text{encrypted} \gets \emptyset\cr
\cr
&\underline{\mathtt{LR.Choose}(m_0, m_1):}\cr
&\ c \gets E(k, m_b)\cr
&\ \text{encrypted} \gets \text{encrypted} \cup \\{c\\}\cr
&\ \texttt{return } c\cr
\cr
&\underline{\mathtt{LR.Enc}(m):}\cr
&\ \texttt{return } E(k, m)\cr
\cr
&\underline{\mathtt{LR.Dec}(c):}\cr
&\ \texttt{require } c \notin \text{encrypted}\cr
&\ \texttt{return } D(k, c)
\end{aligned}
}
$$

and for real-or-random, we have:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
  $\text{RR}_{b}$
}\cr
\cr
&k \xleftarrow{R} K\cr
&\text{encrypted} \gets \emptyset\cr
\cr
&\underline{\mathtt{RR.Choose}(m_0):}\cr
&\ m_1 \xleftarrow{R} \\{0, 1\\}^{|m_0|}\cr
&\ c \gets E(k, m_b)\cr
&\ \text{encrypted} \gets \text{encrypted} \cup \\{c\\}\cr
&\ \texttt{return } c\cr
\cr
&\underline{\mathtt{RR.Enc}(m):}\cr
&\ \texttt{return } E(k, m)\cr
\cr
&\underline{\mathtt{RR.Dec}(c):}\cr
&\ \texttt{require } c \notin \text{encrypted}\cr
&\ \texttt{return } D(k, c)
\end{aligned}
}
$$

{{<note>}}
For full generality, you'd have a function which lets you
sample a random message of the same length as another message,
which lets you accomodate cases where the messages aren't
just simple bit-strings.
{{</note>}}

Note that because the oracles are the same in both cases,
I'll omit them in the reductions for simplicity.

# $\text{RR} \xrightarrow{\epsilon} \text{LR}$

In other words, for every adversary $\mathcal{A}$ against $\text{RR}_b$ with
advantage $\epsilon$, there exists an adversary $\mathcal{B}$
against $\text{LR}_b$ with advantage $\epsilon$ as well.

To show this, we provide a shim $S$ which provides
the interface for $\text{RR}_b$ over $\text{LR}_b$:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
S
}\cr
\cr
&\underline{\mathtt{RR.Choose}(m_0):}\cr
&\ m_1 \xleftarrow{R} \\{0, 1\\}^{|m_0|}\cr
&\ c \gets \mathtt{LR.Choose}(m_0, m_1)\cr
&\ \texttt{return } c\cr
\end{aligned}
}
$$

Then, we clearly have $S \circ \text{LR}_b = \text{RR}_b$,
in the sense of providing identical distributions.
Thus $\epsilon(\mathcal{A} \circ \text{RR}_b) = \epsilon(\mathcal{A} \circ S \circ \text{LR}_b) = \epsilon((\mathcal{A} \circ S) \circ \text{LR}_b)$, and $\mathcal{B} := \mathcal{A} \circ S$ is the adversary
breaking $\text{LR}_b$.

# The Other Direction

In the other direction, the idea is more subtle. We
can't directly translate an oracle encrypting either our message
or a random message into one encrypting one of two messages.

The idea of the reduction in standard game notation is
to choose the random bit for the message ourselves, inside
of the shim. Then, if the adversary is able to accurately
detect the bit, that tells us that we're in the
case where we're not seeing random ciphertexts from
the oracle.

There's actually a general lesson to be learned here.

Let's say you have two bit-indexed games, $G_{b_G}$ and $H_{b_H}$.

If we look at:
$$
H_{b_H} \circ G_{b_G}
$$

Then we can replace $H_{b_H}$ with a concrete game $H$,
where the bit is flipped on initialization. Ditto with $G$.

From a technical point of view, $H$ includes a special
function to read this bit, called $\texttt{get-b}$.
Then, given an adversary $\mathcal{A}$
for $H_{b_H}$, whose interface
doesn't include $\texttt{get-b}$, of course, we have
an adversary $\hat{\mathcal{B}}$ which runs $\mathcal{A}$,
and then checks if that bit is equal to $\texttt{get-b()}$,
returning $1$ if that's the case.

The question is about the advantage:

$$
\epsilon(\hat{\mathcal{B}} \circ (1(\texttt{get-b}) \otimes \mathcal{A}) \circ
H \circ G_{b_G})
$$

By definition, we have:

$$
\epsilon := |P[\hat{\mathcal{B}} \to 1 | b_G = 0] - P[\hat{\mathcal{B}} \to 1 | b_G = 1]|
$$

Letting $\hat{\mathcal{B}}(x_0, x_1) := P[\hat{\mathcal{B}} \to 1 | b_H = x_0, b_G = x_1]$, we can expand this to get:

$$
\epsilon = \frac{1}{2}|\hat{\mathcal{B}}(0, 0) + \hat{\mathcal{B}}(1, 0) - (\mathcal{B}(0, 1) + \mathcal{B}(1, 1))|
$$

Letting $\mathcal{A}(x_0, x_1)$ denote a similar quantity,
we have:

$$
\epsilon = \frac{1}{2}|(1 - \mathcal{A}(0, 0)) + \mathcal{A}(1, 0) - ((1 - \mathcal{A}(0, 1)) + \mathcal{A}(1, 1))|
$$

which we can reduce to:

$$
\epsilon = \frac{1}{2}|\mathcal{A}(1, 0) - \mathcal{A}(0, 0) +
\mathcal{A}(0, 1) - \mathcal{A}(1, 1)|
$$

Using the reverse triangle inequality, we can write this as:

$$
\epsilon \geq \frac{1}{2}||\mathcal{A}(0, 0) - \mathcal{A}(1, 0)| - |\mathcal{A}(0, 1) - \mathcal{A}(1, 1)||
$$

Or, in terms of advantages, we have:

$$
\epsilon \geq \frac{1}{2}|\epsilon(\mathcal{A} \circ H_b \circ G_0) - \epsilon(\mathcal{A} \circ H_b \circ G_1)|
$$

Which is actually a useful result. We can use an adversary
$\mathcal{A}$ which can break the inner part, and use
that to get an adversary breaking $G_b$.

A similar reasoning shows that:

$$
\epsilon(\hat{\mathcal{B}} \circ \mathcal{A} \circ H_{b_H} \circ G) 
\geq \frac{1}{2}|\epsilon(\mathcal{A} \circ H_0 \circ G_b) - \epsilon(\mathcal{A} \circ H_1 \circ G_b)|
$$

Let's summarize the main result. Given an adversary $\mathcal{A}$
playing against $H_{b_H} \circ G_{b_G}$, there exists
an adversary $\mathcal{B}$ such that:

$$
\epsilon(\mathcal{B} \circ G_b) \geq \frac{1}{2}|\epsilon(\mathcal{A} \circ H_b \circ G_0) - \epsilon(\mathcal{A} \circ H_b \circ G_1)|
$$

# $\text{LR} \xrightarrow{\frac{1}{2}\epsilon} \text{RR}$

If we let $G_b := \text{RR}_b$, then we can create a shim
$H_b$ such that $H_b \circ G_0 = \text{LL}_b$, as follows:

$$
\boxed{
\begin{aligned}
&\colorbox{#dbeafe}{\large
H$_b$
}\cr
\cr
&\underline{\mathtt{LR.Choose}(m_0, m_1):}\cr
&\ c \gets \mathtt{RR.Choose}(m_b)\cr
&\ \texttt{return } c\cr
\end{aligned}
}
$$

It should be clear that $H_b \circ \text{RR}_0$ provides
an equivalent game as $\text{LL}_b$. Note
also that $\text{LL}_0 \circ \text{RR}_1 = \text{LL}_1 \circ \text{RR}_1$, since the bit has no impact, since the ciphertext is always
random.

We can now apply the result we derived in the previous section.
Given an adversary $\mathcal{A}$ playing against $\text{LR}_b$,
there exists an adversary $\mathcal{B}$ playing against $\text{RR}$
such that:

$$
\epsilon(\mathcal{B} \circ \text{RR}_b) 
\geq \frac{1}{2}|\epsilon(\mathcal{A} \circ H_b \circ \text{RR}_0) -
\epsilon(\mathcal{A} \circ H_b \circ \text{RR}_1)|
= \frac{1}{2}|\epsilon(\mathcal{A} \circ \text{LL}_b) - 0|
$$

The last part is $0$, since the two games are equivalent.

And that implies the reduction.

$\square$
