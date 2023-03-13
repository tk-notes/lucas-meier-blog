---
title: "Cait-Sith Security (2): Key Sharing"
date: 2023-03-13T16:52:30+01:00
type: note
note-tags:
  - "Cait-Sith"
  - "Cryptography"
  - "Protocols"
  - "TSS"
katex: true
---

Next, we look at key sharing.

**Definition: (Ideal Homomorphism NIZK)**

$$
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F[\text{ZK}(\varphi)]$
}\cr
\cr
&\Pi[\bullet] \gets \bot\cr
\cr
&\underline{
  (1)\text{Prove}_i(b;a):
}\cr
  &\enspace
    \texttt{assert } \varphi(a) = b
  \cr
  &\enspace
    \pi_i \xleftarrow{\\$} \texttt{01}^\lambda
  \cr
  &\enspace
    \Pi[\pi_i] \gets b
  \cr
\cr
&\underline{
  \text{Verify}(\pi, b):
}\cr
  &\enspace
    \texttt{return } \Pi[\pi] \neq \bot \land \Pi[\pi] = b
  \cr
\end{aligned}
}
}
$$

$\square$

**Definition: (Key Share Protocol)**

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{KeyShare}]$
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
  (1)\text{Run}_i(s):
}\cr
  &\enspace
    s_i \gets s
  \cr
  &\enspace
    f_i \xleftarrow{\\$} \\{ f_i \in \mathbb{F}_q[X]\_{\leq t - 1} \mid f_i(0) = s_i \\\}
  \cr
  &\enspace
    F_i \gets f_i \cdot G
  \cr
  &\enspace
    \text{SetCommit}_i(F_i)
  \cr
  &\enspace
    \text{Commit}_i()
  \cr
  \cr
  &\enspace
    \text{WaitCommit}_i()
  \cr
  &\enspace
    \pi_i \gets \text{Prove}_i(F_i(0); f(0))
  \cr
  &\enspace
    \text{Open}_i()
  \cr
  &\enspace
    \Rsh_i(\star, \pi_i, 3)
  \cr
  &\enspace
    \Rsh_i(\star, [f_i(j) \mid j \in [n]], 4)
  \cr
  \cr
  &\enspace
    F\_\bullet \gets \text{WaitOpen}_i()
  \cr
  &\enspace
    \pi\_\bullet \gets \Lsh_i(\star, 3)
  \cr
  &\enspace
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1 \lor \neg \text{Verify}(\pi_j, F_j(0)):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 3)
  \cr
  &\enspace
    x\_{\bullet i} \gets \Lsh_i(\star, 4)
  \cr
  &\enspace
    x_i \gets \sum_j x\_{ji}, \enspace F \gets \sum_j F_j(0)
  \cr
  &\enspace
    \texttt{if } x_i \cdot G \neq F(i):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 4)
  \cr
  &\enspace
    \texttt{return } (x_i, F(0))
  \cr
\end{aligned}
}
}
\quad
\begin{matrix}
F[\text{ZK}(\varphi)]\cr
\otimes\cr
F[\text{SyncComm}]\cr
\circledcirc \cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}\\}
\end{matrix}
}
$$

$\square$

**Definition: (Ideal Key Sharing Protocol)**

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{IdealKeyShare}]$
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
  (1)\text{Run}_i(s):
}\cr
  &\enspace
    \text{Set}_i(s)
  \cr
  &\enspace
    \text{Sync}_i(\star, 0)
  \cr
  &\enspace
    \text{WaitSync}_i(\star, 0)
  \cr
  &\enspace
    \text{Open}_i(\star)
  \cr
  &\enspace
    (x_i, X) \gets \text{WaitOpen}_i()
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
  $F[\text{KeyShare}]$
}\cr
\cr
&\underline{
  (1)\text{Set}_i(s):
}\cr
  &\enspace
    s_i \gets s
  \cr
\cr
&\underline{
  \text{Open}_i(S):
}\cr
  &\enspace
    \texttt{assert } s_i \neq \bot
  \cr
  &\enspace
    \text{open}\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
  \text{WaitOpen}_i():
}\cr
  &\enspace
    \texttt{wait } \forall j.\ \text{open}\_{ji}
  \cr
  &\enspace
    \texttt{if } f = \bot:
  \cr
  &\enspace\enspace
    f \xleftarrow{\\$} \\{f \in \mathbb{F}_q[X]\_{\leq t - 1} \mid f(o) = \sum_i s_i \\}
  \cr
  &\enspace
    \texttt{return } (f(i), f(0) \cdot G)
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

$\square$