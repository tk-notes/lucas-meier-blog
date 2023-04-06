---
title: "Cait-Sith Security (3): Multiplication and Triples"
date: 2023-04-05T18:22:14+02:00
type: note
note-tags:
  - "Cait-Sith"
  - "Cryptography"
  - "Protocols"
  - "TSS"
katex: true
---

Consider:

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{MTA}^2]$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&\underline{
  (1)\text{Start}\_{ij}(a, b):
}\cr
  &\enspace
    \text{StartMTA}^0_i(\text{Flip}\_{ij}(a, b))
  \cr
  &\enspace
    \text{StartMTA}^1_i(\text{Flip}\_{ij}(b, a))
  \cr
\cr
&\underline{
  (1)\text{EndMTA}\_{ij}():
}\cr
  &\enspace
    \texttt{return } \text{EndMTA}^0_i() + \text{EndMTA}^1_i()
  \cr
\cr
&\underline{
  (1)\text{Cheat}^\tau(\Delta)
}\cr
  &\enspace
    \text{Cheat}^\tau(\Delta)
  \cr
\end{aligned}
}
}
\end{matrix}
}
\lhd \mathscr{P}[\text{MTA}]^2
$$

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $F[\text{MTA}^2]$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&a_1, a_2, b_1, b_2, \beta_1, \beta_2 \gets \bot\cr
&\Delta \gets \bot\cr
\cr
&\underline{
  (1)\text{Start}_i(a, b):
}\cr
  &\enspace
    a_i \gets a,\ b_i \gets b
  \cr
\cr
&\underline{
  \text{Sample}():
}\cr
  &\enspace
    \texttt{assert } a_1, a_2, b_1, b_2, \Delta \neq \bot
  \cr
  &\enspace
    \texttt{if } \beta_1, \beta_2 = \bot:
  \cr
  &\enspace\enspace
    (\beta_1, \beta_2) \xleftarrow{\\$} \\{(\beta_1, \beta_2) \in \mathbb{F}_q^2 \mid \beta_1 + \beta_2 = a_1 \cdot b_2 + a_2 \cdot b_1 + \Delta \\}
  \cr
\cr
&\underline{
  (1)\text{End}_i():
}\cr
  &\enspace
    \texttt{wait}\_{(i, 0)} a_1, a_2, b_1, b_2, \Delta \neq \bot
  \cr
  &\enspace
    \text{Sample}()
  \cr
  &\enspace
    \texttt{return } \beta_i
  \cr
\cr
&\underline{
  (1)\text{Cheat}(\Delta)
}\cr
  &\enspace
    \Delta \gets \Delta
  \cr
\cr
&\underline{
  \text{Leak}(\Delta)
}\cr
  &\enspace
    \texttt{return } (a_1 \neq \bot, a_2 \neq \bot, b_1 \neq \bot, b_2 \neq \bot)
  \cr
\end{aligned}
}
}
\end{matrix}
}
$$

**Lemma:**
$$
\mathscr{P}[\text{MTA}^2] \leadsto F[\text{MTA}^2]
$$

**Proof:**

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^0_H$
}\cr
\cr
&\underline{
  (1)\text{Start}_1(a, b):
}\cr
&\enspace
  \ldots
\cr
\end{aligned}
}
}
\otimes
\boxed{\colorbox{FBCFE8}{\large
  $\Gamma^0_M$
} = 1
\begin{pmatrix}
    \text{StartMTA}^\tau_2
  ,\cr
    \text{EndMTA}^\tau_2
  ,\cr
    \text{Cheat}^\tau
\end{pmatrix}
}
\cr
\circ\cr
F[\text{MTA}] \otimes F[\text{MTA}]
\end{matrix}
$$

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^0_H$
}\cr
\cr
&\underline{
  (1)\text{Start}_1(a, b):
}\cr
&\enspace
  \ldots
\cr
\end{aligned}
}
}
\otimes
\boxed{\colorbox{FBCFE8}{\large
  $\Gamma^0_M$
} = 1
\begin{pmatrix}
    \text{StartMTA}^\tau_2
  ,\cr
    \text{EndMTA}^\tau_2
  ,\cr
    \text{Cheat}^\tau
\end{pmatrix}
}
\cr
\circ\cr
F[\text{MTA}] \otimes F[\text{MTA}]
\end{matrix}
$$

$$
\begin{matrix}
\boxed{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $\Gamma^1_H$
} = 1
\begin{pmatrix}
    \text{Start}_1
  ,\cr
    \text{End}_1
\end{pmatrix}
\end{aligned}
}
\otimes
\boxed{
\small{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $S$
}\cr
\cr
&\Delta^1, \Delta^2, \text{first} \gets \bot\cr
&\alpha \xleftarrow{\\$} \mathbb{F}_q\cr
\cr
&\underline{
  (1)\text{StartMTA}^\tau_2(a):
}\cr
  &\enspace
    \texttt{if } a^\tau = \bot:
  \cr
  &\enspace\enspace
    a^\tau \gets a
  \cr
  &\enspace
    \texttt{if } a^1, a^2 \neq \bot:
  \cr
  &\enspace\enspace
    \text{Start}(a^1, a^2)
  \cr
\cr
&\underline{
  (1)\text{EndMTA}^\tau_2():
}\cr
  &\enspace
    (r_1, \bullet, r_2, \bullet) \gets \text{Leak}()
  \cr
  &\enspace
    \texttt{wait}\_{(2, 0)} r\_\tau \neq \bot \land a^\tau, \Delta^\tau \neq \bot
  \cr
  &\enspace
    \texttt{if } \text{first} = \bot:
  \cr
  &\enspace\enspace
    \text{first} \gets \tau
  \cr
  &\enspace
    \texttt{if } \text{first} = \tau:
  \cr
  &\enspace\enspace
    \texttt{return } \alpha
  \cr
  &\enspace
    \texttt{return } \text{End}_2()
  \cr
\cr
&\underline{
  (1)\text{Cheat}^\tau(\Delta):
}\cr
  &\enspace
    \texttt{if } \Delta^\tau = \bot:
  \cr
  &\enspace\enspace
    \Delta^\tau \gets \Delta
  \cr
  &\enspace
    \texttt{if } \Delta^1, \Delta^2 \neq \bot:
  \cr
  &\enspace\enspace
    \text{Cheat}(\Delta^1 + \Delta^2 - \alpha)
  \cr
\end{aligned}
}
}
\cr
\circ\cr
F[\text{MTA}^2]
\end{matrix}
$$

$\blacksquare$

**Lemma:**
$$
\mathscr{P}[\text{Multiply}] \leadsto F[\text{Multiply}]
$$
**Proof:**

First, $\mathscr{P}[\text{Multiply}] = \mathscr{P}^0 \lhd \mathscr{P}[\text{MTA}^2]^{n^2}$,
where:

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}^0$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&\text{start}_i \gets \bot\cr
\cr
&\underline{
  (1)\text{StartMultiply}_i(a, b):
}\cr
  &\enspace
    \text{start}_i \gets \texttt{true}
  \cr
  &\enspace
    \forall j \neq i.\ \text{Start}_i^{ij}(a, b)
  \cr
\cr
&\underline{
  (1)\text{EndMultiply}_i():
}\cr
  &\enspace
    \texttt{assert } \text{start}_i
  \cr
  &\enspace
    \texttt{return } a \cdot b + \sum\_{j \neq i} \text{End}_i^{ij}()
  \cr
\end{aligned}
}
}
\end{matrix}
}
\lhd
\begin{matrix}
\mathscr{P}[\text{MTA}^2]^{n^2}\cr
\end{matrix}
$$

Then, it holds that:
$$
\mathscr{P}^0 \lhd \mathscr{P}[\text{MTA}^2]^{n^2/2}
\leadsto \mathscr{P} \lhd F[\text{MTA}^2]^{n^2/2}
= \mathscr{P}^1
$$

Unrolling $\mathscr{P}^1$, we get the following:

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^0_H$
}\cr
\cr
&\underline{
  (1)\text{StartMultiply}_i(a, b):
}\cr
&\enspace
  \ldots
\cr
\end{aligned}
}
}
\otimes
\boxed{\colorbox{FBCFE8}{\large
  $\Gamma^0_M$
} = 1
\begin{pmatrix}
    \text{Start}_k^{ki}
  ,\cr
    \text{End}_k^{ki}
  ,\cr
    \text{Cheat}^{ki}
\end{pmatrix}
}
\cr
\circ\cr
F[\text{MTA}^2]^{n^2/2}
\end{matrix}
$$
This is equivalent to:
$$
\begin{matrix}
\boxed{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $\Gamma^0_H$
} = 1
\begin{pmatrix}
    \text{StartMultiply}_i
  ,\cr
    \text{EndMultiply}_i
\end{pmatrix}
\end{aligned}
}
\otimes
\boxed{
\small{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $S$
}\cr
\cr
&\text{left} \gets \\{(k, i)\ k \in \mathcal{M}, i \in \mathcal{H} \\}\cr
&\alpha\_{ki} \xleftarrow{\\$} \mathbb{F}_q\cr
&a\_{ki}, b\_{ki}, \Delta\_{ki} \gets \bot\cr
\cr
&\underline{
  \text{Start}_k^{ki}(a, b)
}\cr
\cr
&\underline{
  \text{End}_k^{ki}
}\cr
\cr
&\underline{
  \text{Cheat}^{ki}(\Delta)
}\cr
  &\enspace
  f
  \cr
\end{aligned}
}
}
\cr
\circ\cr
F[\text{Multiply}]
\end{matrix}
$$

$\blacksquare$
