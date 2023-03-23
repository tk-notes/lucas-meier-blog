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
    \texttt{return } \text{WaitOpen}_i()
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
    f \xleftarrow{\\$} \\{f \in \mathbb{F}_q[X]\_{\leq t - 1} \mid f(0) = \sum_i s_i \\}
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

**Proof:**

$\mathcal{P}[\text{KeyShare}] \lhd \mathcal{P}[\text{IdealCommit}] \overset{\epsilon}{\leadsto} \mathcal{P}[\text{IdealKeyShare}]$
for a negligible $\epsilon$, and for any number of malicious corruptions
such that $|\mathcal{M}| < t$.

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
  (1)\text{Run}_i(x):
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
    \Rsh_k
  ,\cr
    \Lsh_k
  ,\cr
    \text{Prove}_k
  ,\cr
    \text{Verify}
  ,\cr
    \text{SetCommit}_k
  ,\cr
    \text{WaitCommit}_k
  ,\cr
    \text{Open}_k
  ,\cr
    \text{WaitOpen}_k
  ,\cr
    \texttt{stop}
  ,\cr
    \texttt{Sync}_k
  ,\cr
    \texttt{WaitSync}_k
\end{pmatrix}
}
\cr
  \circ
\cr
F[\text{ZK}(\varphi)] \otimes F[\text{SyncComm}] \otimes F[\text{Commit}] \otimes F[\text{Sync}] \circledcirc F[\text{Stop}]
\end{matrix}
$$

Next, inline messages

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^1_H$
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
    \text{Commit}_i(\star)
  \cr
  \cr
  &\enspace
    \text{WaitCommit}_i()
  \cr
  &\enspace
    \text{Sync}_i(\star, 1)
  \cr
  &\enspace
    \pi_i \gets \text{Prove}_i(F_i(0); f(0))
  \cr
  &\enspace
    \text{Open}_i(\star)
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \pi\_{ij} \gets \pi_i
  $}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    x\_{ij} \gets f_i(j)
  $}
  \cr
  \cr
  &\enspace
    \text{WaitSync}_i(\star, 1)
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    F\_\bullet \gets \text{WaitOpen}_i(\star)
  $}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{wait}\_{(i, 3)} \forall j.\ \pi\_{ji} \neq \bot
  $}
  \cr
  &\enspace
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1 \lor \neg \text{Verify}(\pi_j, F_j(0)):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 3)
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{wait}\_{(i, 4)} \forall j.\ x\_{ji} \neq \bot
  $}
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
\otimes
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^1_M$
}\cr
&\ldots\cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 3):
}\cr
  &\enspace
    \forall j \in S.\ \pi\_{kj} \gets m\_j
  \cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 4):
}\cr
  &\enspace
    \forall j \in S.\ x\_{kj} \gets m\_j
  \cr
\cr
&\underline{
  \Lsh_k(S, 3):
}\cr
  &\enspace
    \texttt{wait}\_{(k, 3)} \forall j \in S.\ \pi\_{jk} \neq \bot
  \cr
  &\enspace
    \texttt{return } [\pi\_{jk} \mid j \in S]
  \cr
\cr
&\underline{
  \Lsh_k(S, 4):
}\cr
  &\enspace
    \texttt{wait}\_{(k, 3)} \forall j \in S.\ x\_{jk} \neq \bot
  \cr
  &\enspace
    \texttt{return } [x\_{jk} \mid j \in S]
  \cr
\end{aligned}
}
}\cr
\otimes\cr1(\ldots)
\end{matrix}
\cr
  \circ
\cr
\boxed{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $F_0$
}\cr
\cr
&F_i, \text{com}\_{ij}, \text{open}\_{ij} \gets \bot\cr
&\texttt{pub } \pi\_{ij}, x\_{ij} \gets \bot\cr
\cr
&\underline{
  (1)\text{SetCommit}_i(F):
}\cr
  &\enspace
    F_i \gets F
  \cr
\cr
&\underline{
  \text{Commit}_i(S):
}\cr
  &\enspace
    \texttt{assert } F_i \neq \bot
  \cr
  &\enspace
    \text{com}\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
  \text{WaitCommit}_i(S):
}\cr
  &\enspace
    \texttt{wait}\_{(i, 0)} \forall j \in S.\ \text{com}\_{ji}
  \cr
\cr
&\underline{
  \text{Open}_i(S):
}\cr
  &\enspace
    \texttt{assert } F_i \neq \bot
  \cr
  &\enspace
    \text{open}\_{ij} \gets \texttt{true} (\forall j \in S)
  \cr
\cr
&\underline{
  \text{WaitOpen}_i(S):
}\cr
  &\enspace
    \text{wait}\_{(i, 2)} \forall j \in S.\ \text{open}\_{ji}
  \cr
  &\enspace
    \texttt{return } F\_\bullet
  \cr
\end{aligned}
}
\otimes
F[\text{ZK}(\varphi)] \otimes F[\text{SyncComm}] \circledcirc F[\text{Stop}]
\end{matrix}
$$

By stopping inside $\Gamma_M$, we deliver earlier.
Any proof not coming from prove is false, with neglible probability.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^2_H$
}\cr
\cr
&\underline{
  (1)\text{Run}_i(s):
}\cr
  &\enspace
    \ldots
  \cr
  &\enspace
    \texttt{wait}\_{(i, 3)} \forall j.\ \pi\_{ji} \neq \bot
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{wait}\_{(i, 4)} \forall j.\ x\_{ji} \neq \bot
  $}
  \cr
  &\enspace
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1 \lor \neg \text{Verify}(\pi_j, F_j(0)):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 3)
  \cr
  &\enspace
    \ldots
  \cr
\end{aligned}
}
}
\otimes
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^2_M$
}\cr
&\ldots\cr
&\colorbox{bae6fd}{$
X_k, B_k, \pi\_k \gets \bot
$}\cr
\cr
&\underline{
  (1)\text{SetCommit}_k(F):
}\cr
  &\enspace
  \colorbox{bae6fd}{$
    X_k \gets F(0)
  $}
  \cr
  &\enspace
    \text{SetCommit}_k(F)
  \cr
\cr
&\underline{
  (1)\text{Prove}_k(B;b):
}\cr
  &\enspace
  \colorbox{bae6fd}{$
    B_k \gets X_k
  $}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \pi\_k \gets \text{Prove}_k(B;b)
  $}
  \cr
  &\enspace
    \texttt{return } \pi\_k
  \cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 3):
}\cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{for } j \in S \cap \mathcal{H}.\ m_j \neq \pi_k \lor B_k \neq X_k:
  $}
  \cr
  &\enspace\enspace
  \colorbox{bae6fd}{$
    \texttt{stop}(\\{j\\}, 3)
  $}
  \cr
  &\enspace
    \forall j \in S.\ \pi\_{kj} \gets m\_j
  \cr
\end{aligned}
}
}\cr
\otimes\cr1(\ldots)
\end{matrix}
\cr
  \circ
\cr
F_0
\otimes
F[\text{ZK}(\varphi)] \otimes F[\text{SyncComm}] \circledcirc F[\text{Stop}]
\end{matrix}
$$

Next, make it so that we open 3 things at once.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^4_H$
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
    \text{Commit}_i(\star)
  \cr
  \cr
  &\enspace
    \text{WaitCommit}_i()
  \cr
  &\enspace
    \pi_i \gets \text{Prove}_i(F_i(0); f(0))
  \cr
  &\enspace
    \text{Open}_i(\star, (\pi_i, f_i(\bullet)))
  \cr
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    (F\_\bullet, \pi\_{\bullet i}, x\_{\bullet i}) \gets \text{WaitOpen}_i(\star)
  $}
  \cr
  &\enspace
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1 \lor \neg \text{Verify}(\pi_j, F_j(0)):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 3)
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
\otimes
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $\Gamma^4_M$
}\cr
&\ldots\cr
\cr
&\underline{
  (1)\text{SetCommit}_k(F):
}\cr
  &\enspace
    X_k \gets F(0)
  \cr
  &\enspace
    \text{SetCommit}_k(F)
  \cr
\cr
&\underline{
  (1)\text{Prove}_k(B;b):
}\cr
  &\enspace
    B_k \gets X_k
  \cr
  &\enspace
    \pi\_k \gets \text{Prove}_k(B;b)
  \cr
  &\enspace
    \texttt{return } \pi\_k
  \cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 3):
}\cr
  &\enspace
    \texttt{for } j \in S \cap \mathcal{H}.\ m_j \neq \bot \land m_j \neq \pi_k:
  \cr
  &\enspace\enspace
    \texttt{stop}(\\{j\\}, 3)
  \cr
  &\enspace
    \forall j \in S.\ \pi\_{kj} \gets m\_j
  \cr
  &\enspace
    \text{Open?}_k()
  \cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 4):
}\cr
  &\enspace
    \forall j \in S.\ x\_{kj} \gets m\_j
  \cr
  &\enspace
    \text{Open?}_k()
  \cr
\cr
&\underline{
  \Lsh_k(S, 3):
}\cr
  &\enspace
    (\forall j \in \mathcal{H})\ (\bullet, \pi\_{j k}, \bullet) \gets \texttt{nowait } \text{WaitOpen}\_k(\\{j\\})
  \cr
  &\enspace
    \texttt{wait}\_{(k, 3)} \forall j \in S.\ \pi\_{jk} \neq \bot
  \cr
  &\enspace
    \texttt{return } [\pi\_{jk} \mid j \in S]
  \cr
\cr
&\underline{
  \Lsh_k(S, 4):
}\cr
  &\enspace
    (\forall j \in \mathcal{H})\ (\bullet, \bullet, x\_{j k}) \gets \texttt{nowait } \text{WaitOpen}\_k(\\{j\\})
  \cr
  &\enspace
    \texttt{wait}\_{(k, 3)} \forall j \in S.\ x\_{jk} \neq \bot
  \cr
  &\enspace
    \texttt{return } [x\_{jk} \mid j \in S]
  \cr
\cr
&\underline{
  \text{Sync}_k(S):
}\cr
  &\enspace
    \forall j \in S.\ \text{sync}\_{kj} \gets \texttt{true}
  \cr
  &\enspace
    \text{Open?}_k()
  \cr
\cr
&\underline{
  \text{WaitSync}_k(S):
}\cr
  &\enspace
    \text{open}\_{kj} \gets \texttt{nowait } \text{WaitOpen}_k(\\{j\\})
  \cr
  &\enspace
    \texttt{wait}\_{(k, 1)} \forall j \in S.\ \text{sync}\_{kj} \lor \text{open}\_{kj}
  \cr
\cr
&\underline{
  \text{Open?}_k():
}\cr
  &\enspace
    \texttt{for } j \in \mathcal{H}.\ \text{sync}\_{kj}, \pi\_{kj}, x\_{kj} \neq \bot:
  \cr
  &\enspace\enspace
    \text{Open}_i(\\{j\\}, \pi\_{kj}, x\_{kj})
  \cr
\end{aligned}
}
}\cr
\otimes\cr1(\ldots)
\end{matrix}
\cr
  \circ
\cr
\boxed{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F_1$
}\cr
\cr
&F_i, \text{com}\_{ij}, \text{open}\_{ij} \gets \bot\cr
&\pi\_{ij}, x\_{ij} \gets \bot\cr
\cr
&\ldots\cr
\cr
&\colorbox{bae6fd}{$
\underline{
  \text{Open}_i(S, \pi\_\bullet, x\_\bullet):
}$}\cr
  &\enspace
    \texttt{assert } F_i \neq \bot
  \cr
  &\enspace
    \text{open}\_{ij} \gets \texttt{true} (\forall j \in S)
  \cr
  &\enspace
    \pi\_{ij}, x\_{ij} \gets \pi_j, x\_j
  \cr
\cr
&\underline{
  \text{WaitOpen}_i(S):
}\cr
  &\enspace
    \text{wait}\_{(i, 2)} \forall j \in S.\ \text{open}\_{ji}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{return } (F\_\bullet, x\_{\bullet i}, \pi\_{\bullet i})
  $}
  \cr
\end{aligned}
}
\otimes
F[\text{ZK}(\varphi)] \circledcirc F[\text{Stop}]
\end{matrix}
$$

This is just a simulation of a protocol $\mathscr{P}_0$ where we need
to open along with a proof and shares.

Let's unroll again.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^5_H$
}\cr
\cr
&\underline{
  (1)\text{Run}_i(x):
}\cr
&\enspace
  \ldots
\cr
\end{aligned}
}
}
\otimes
\boxed{\colorbox{FBCFE8}{\large
  $\Gamma^5_M$
} = 1
\begin{pmatrix}
    \text{Prove}_k
  ,\cr
    \text{Verify}
  ,\cr
    \text{SetCommit}_k
  ,\cr
    \text{WaitCommit}_k
  ,\cr
    \text{Open}_k
  ,\cr
    \text{WaitOpen}_k
  ,\cr
    \texttt{stop}
\end{pmatrix}
}
\cr
  \circ
\cr
F_1 \otimes F[\text{ZK}(\varphi)] \otimes F[\text{SyncComm}] \circledcirc F[\text{Stop}]
\end{matrix}
$$

Next, except with negligible probability, we can extract
an $s_i$ value and set it.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^6_H$
}\cr
\cr
&\underline{
  (1)\text{Run}_i(x):
}\cr
&\enspace
  \ldots
\cr
\end{aligned}
}
}
\otimes
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^6_M$
}\cr
\end{aligned}
}
}
\cr
  \circ
\cr
F_1 \otimes F[\text{ZK}(\varphi)] \otimes F[\text{SyncComm}] \circledcirc F[\text{Stop}]
\end{matrix}
$$

$\blacksquare$