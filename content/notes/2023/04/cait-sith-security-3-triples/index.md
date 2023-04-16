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
\mathscr{P}[\text{Multiply}] \leadsto \mathscr{P}[\text{IdealMultiply}]
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
&a\_{ki}, b\_{ki}, \alpha\_{ki}, \Delta\_{ki} \gets \bot\cr
&a\_{kl}, b\_{kl} \gets 0\cr
\cr
&\underline{
  \text{Start}_k^{ki}(a, b)
}\cr
  &\enspace
    a\_{ki} \gets a, b\_{ki} \gets b
  \cr
  &\enspace
    \texttt{if } \forall i.\ a\_{ki}, b\_{ki} \neq \bot:
  \cr
  &\enspace\enspace
    \text{StartMultiply}_i(a\_\bullet, b\_\bullet)
  \cr
\cr
&\underline{
  \text{End}_k^{ki}()
}\cr
  &\enspace
    \texttt{if } (k, i) \notin \text{left}:
  \cr
  &\enspace\enspace
    \texttt{return } \alpha\_{ki}
  \cr
  &\enspace
    \texttt{wait}\_{(i, 0)} a\_{ki}, b\_{ki}, \Delta\_{ki} \neq \bot \land \text{Leak}(i, k)
  \cr
  &\enspace
    \text{left} \gets \text{left} / \\{(k, i)\\}
  \cr
  &\enspace
    \texttt{if } |\text{left}| = 0:
  \cr
  &\enspace\enspace
    \alpha_k \gets \text{EndMultiply}_k()\enspace (k \in \mathcal{M})
  \cr
  &\enspace\enspace
    \texttt{return } \sum\_{k \in \mathcal{M}} \alpha_k - \sum\_{\alpha\_{ki} \neq \bot} \alpha\_{ki}
  \cr
  &\enspace
    \texttt{else}:
  \cr
  &\enspace\enspace
    \alpha\_{ki} \xleftarrow{\\$} \mathbb{F}_q
  \cr
  &\enspace\enspace
    \texttt{return } \alpha\_{ki}
  \cr
\cr
&\underline{
  \text{Cheat}^{ki}(\Delta)
}\cr
  &\enspace
    \Delta\_{ki} \gets \Delta
  \cr
  &\enspace
    \texttt{if } \forall k, i.\ \text{Delta}\_{ki} \neq \bot
  \cr
  &\enspace\enspace
    \text{Cheat}\left(\sum\_{(k, i)} \Delta\_{ki}\right)
  \cr
\end{aligned}
}
}
\otimes
1
\begin{pmatrix}
\text{Start}^{kl}_k,\cr
\text{End}^{kl}_k,\cr
\text{Cheat}^{kl}\cr
\end{pmatrix}
\cr
\circ\cr
F[\text{Multiply}]
\end{matrix}
$$

$\blacksquare$

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{IdealTriple}]$
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
  (1)\text{Triple}_i():
}\cr
  &\enspace
    \text{Set}_i(\star)
  \cr
  &\enspace
    \text{WaitSet}_i(\star, \texttt{true})
  \cr
  &\enspace
    \text{Share}^0_i(\star)
  \cr
  &\enspace
    (a_i, b_i) \gets \text{WaitShares}^0_i(\texttt{true})
  \cr
  &\enspace
    \text{Share}^1_i(\star)
  \cr
  &\enspace
    c_i \gets \text{WaitShares}^1_i(\texttt{true})
  \cr
  &\enspace
    (A, B, C) \gets
    (\text{F}^{A,h}()(0), \text{F}^{B,h}()(0), \text{F}^{C,h}()(0))
  \cr
  &\enspace
    \texttt{return } (a_i, b_i, c_i, A, B, C)
  \cr
\end{aligned}
}
}
\quad
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F[\text{Convert}]$
}\cr
\cr
&f^{A,h}, f^{B,h}, \xleftarrow{\\$} \mathbb{F}_q[X]\_{\leq t - 1}\cr
&f^{A,h}, f^{B,h}, \xleftarrow{\\$} \\{f \in \mathbb{F}_q[X]\_{\leq t - 1} \mid f(0) = f^{A, h}(0) \cdot f^{B, h}(0)\\}\cr
&F^{A,m}, F^{B,m}, F^{C,m}, \text{ready}\_{ij}, \text{shared}^{\\{0, 1\\}}\_{ij}, a\_{i}, b\_{i}, c\_{i}, x^A_i, x^B_i, x^C_i \gets \bot\cr
\cr
&\underline{
  (1)\text{Set}_i(S):
}\cr
  &\enspace
    \text{ready}\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
\textcolor{ef4444}{
  (1)\text{Cheat}(F^A, F^B, F^C):
}
}\cr
  &\enspace
    \texttt{assert } F(0) = 0 \land \text{deg}(F) = t - 1
  \cr
  &\enspace
    F^{A, m}, F^{B, m}, F^{C, m} \gets F^A, F^B, F^C
  \cr
\cr
&\underline{
  \text{WaitSet}_i(S, m):
}\cr
  &\enspace
    \texttt{wait}\_{(i, 0)} \forall j \in S.\ \text{ready}\_{ji} \land (m \to F^{\bullet, m} \neq \bot)
  \cr
\cr
&\underline{
  \text{Share}^0_i(S):
}\cr
  &\enspace
    \text{shared}^0\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
\textcolor{ef4444}{
  \text{CheatShare}^0(S, \hat{x}^A\_\bullet, \hat{x}^B\_\bullet):
}
}\cr
  &\enspace
    \texttt{for } Y \in \\{A, B\\}:
  \cr
  &\enspace\enspace
    \texttt{assert } F^{Y,m} \neq \bot \land \forall j \in S.\ \hat{x}^Y_j \cdot G = F^{Y, m}(j)
  \cr
  &\enspace\enspace
    \texttt{for } j.\ x^Y_j \neq \bot: x^Y_j \gets \hat{x}^Y_j
  \cr
\cr
&\underline{
  \text{WaitShares}^0_i(h):
}\cr
  &\enspace
    \texttt{if } h:
  \cr
  &\enspace\enspace
    \texttt{wait}\_{(i, 1)} x^A_i, x^B_i \neq \bot \land \forall j. \land \forall j. \text{shared}^0\_{ji}
  \cr
  &\enspace\enspace
    \texttt{return } (f^{A,h}(i) + x^A_i, f^{B,h}(i) + x^B_i)
  \cr
  &\enspace
    \texttt{else}:
  \cr
  &\enspace\enspace
    \texttt{wait}\_{(i, 1)} \forall j \in \mathcal{H}. \text{shared}^0\_{ji} \neq \bot
  \cr
  &\enspace\enspace
    \texttt{return } (f^{A,h}(i), f^{B,h}(i))
  \cr
\cr
&\underline{
  \text{Share}^1_i(S):
}\cr
  &\enspace
    \text{shared}^1\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
\textcolor{ef4444}{
  \text{CheatShare}^1(S, \hat{x}^C\_\bullet):
}
}\cr
  &\enspace
    \texttt{assert } F^{C,m} \neq \bot \land \forall j \in S.\ \hat{x}^C_j \cdot G = F^{C, m}(j)
  \cr
  &\enspace
    \texttt{for } j.\ x^C_j \neq \bot: x^C_j \gets \hat{x}^C_j
  \cr
\cr
&\underline{
  \text{WaitShares}^1_i():
}\cr
  &\enspace\enspace
    \texttt{wait}\_{(i, 1)} x^C_i, \neq \bot \land \forall j. \land \forall j. \text{shared}^1\_{ji}
  \cr
  &\enspace\enspace
    \texttt{return } f^{C, h}(i) + x^C_i
  \cr
\cr
&\underline{
  \text{F}^{Y \in \\{A, B, C\\}, h}():
}\cr
  &\enspace
    \texttt{wait } F^{Y, m} \neq \bot \land \forall i.\ \exists j.\ \text{ready}\_{ij}
  \cr
  &\enspace
    \texttt{return } f^{Y, h} \cdot G
  \cr
\end{aligned}
}
}\cr
\otimes\cr
F[\text{Sync}]\cr
\circledcirc \cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}\\}
\end{matrix}
}
$$

**Lemma**

$$
\mathscr{P}[\text{Triple}] \leadsto \mathscr{P}[\text{IdealTriple}]
$$

**Proof**

Clearly, $\mathscr{P}[\text{Triple}]$ is simulated by the following
protocol making use of $\mathscr{P}[\text{SplitShare}]$ twice:

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
&\underline{
  (1)\text{Triple}_i():
}\cr
  &\enspace
    z^A_i, z^B_i \xleftarrow{\\$} \mathbb{F}_q
  \cr
  &\enspace
    \text{Set}^0_i(z^A_i), \text{Set}^1_i(z^B_i)
  \cr
  &\enspace
    \text{SetMask}_i()
  \cr
  \cr
  &\enspace
    \text{WaitSet}^0_i(), \text{WaitSet}^1_i()
  \cr
  &\enspace
    \text{WaitMask}_i()
  \cr
  &\enspace
    \text{Share}^0_i(), \text{Share}^1_i
  \cr
  \cr
  &\enspace
    a_i \gets \text{WaitShare}^0_i(), b_i \gets \text{WaitShare}^1_i()
  \cr
  &\enspace
    \text{Multiply}_i(z^A_i, z^B_i)
  \cr
  &\enspace
    \text{A} \gets \sum_i \text{Z}^{0}(i),
    \text{B} \gets \sum_i \text{Z}^{1}(i)
  \cr
  &\enspace
    C_i \gets z^B_i \cdot \text{F}^{0,h}(0)
  \cr
  &\enspace
    \pi^2_i \gets \text{Prove}^\psi(\text{Z}^{1}(i), A, C_i; z^B_i)
  \cr
  &\enspace
    \Rsh_i(\star, (C_i, \pi_i), 1)
  \cr
  &\enspace\cr
  &\enspace
    (C\_\bullet, \pi^2\_\bullet) \Lsh_i(\star, 1)
  \cr
  &\enspace
    \texttt{if } \exists j.\ \neg \text{Verify}^\psi(\pi^2_j, (\text{Z}^{1}(j), \text{F}^{0,h}(0)))
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
  &\enspace
    z^C_i \gets \text{WaitMultiply}_i()
  \cr
  &\enspace
    \text{Share}_i(z^C_i)
  \cr
  &\enspace
    c_i \gets \text{WaitShare}_i()
  \cr
  &\enspace
    C \gets \sum_i C_i
  \cr
  &\enspace
    \texttt{if } \sum_i \text{Z}^{2}(i) \neq C
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 2)
  \cr
  &\enspace
    \texttt{return } (a_i, b_i, c_i, A, B, C)
  \cr
\end{aligned}
}
}
\end{matrix}
}
\lhd
\begin{matrix}
\mathscr{P}[\text{SplitShare}]\cr
\otimes\cr
\mathscr{P}[\text{SplitShare}]\cr
\otimes\cr
\mathscr{P}[\text{Convert}]\cr
\otimes\cr
\mathscr{P}[\text{Multiply}]\cr
\end{matrix}
$$

This simulates the desired protocol.

TODO

$\blacksquare$