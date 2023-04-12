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

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{IdealConvert}]$
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
  (1)\text{SetMask}_i():
}\cr
  &\enspace
    \text{SetMask}_i(\star)
  \cr
\cr
&\underline{
  \text{WaitMask}_i():
}\cr
  &\enspace
    \text{WaitMask}_i(\star)
  \cr
\cr
&\underline{
  (1)\text{Share}_i(z):
}\cr
  &\enspace
    \text{Share}_i(\star, z)
  \cr
\cr
&\underline{
  \text{WaitShare}_i():
}\cr
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
&f^h \xleftarrow{\\$} \\{f \in \mathbb{F}_q[X]\_{\leq t - 1} \mid f(0) = 0\\}\cr
&F^m, \text{ready}\_{ij}, z\_{ij}, x_j \gets \bot\cr
\cr
&\underline{
  (1)\text{SetMask}_i(S):
}\cr
  &\enspace
    \text{ready}\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
  (1)\text{Cheat}(F):
}\cr
  &\enspace
    \texttt{assert } F(0) = 0 \land \text{deg}(F) = t - 1
  \cr
  &\enspace
    F^m \gets F
  \cr
  &\enspace
    \texttt{return } f^h \cdot G
  \cr
\cr
&\underline{
  \text{WaitMask}_i(S):
}\cr
  &\enspace
    \texttt{wait}\_{(i, 0)} \forall j \in S.\ \text{ready}\_{ji} \land F^m \neq \bot
  \cr
\cr
&\underline{
  \text{Share}_i(S, z\_\bullet):
}\cr
  &\enspace
    z\_{ij} \gets z_j\ (\forall j \in S)
  \cr
\cr
&\underline{
  \text{CheatShare}(S, x\_\bullet):
}\cr
  &\enspace
    \texttt{assert } F^m \neq \bot \land \forall j \in S.\ x_j \cdot G = F^m(j)
  \cr
  &\enspace
    x_j \gets x_j
  \cr
\cr
&\underline{
  \text{WaitShare}_i(S):
}\cr
  &\enspace
    \texttt{wait}\_{(i, 1)} \forall j \in S.\ x_j \neq \bot \land z\_{ji} \neq \bot
  \cr
  &\enspace
    \texttt{return } \sum_j z\_{ji} + f^h(i) + x_i
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
    \texttt{for } j \in S.\ \pi\_{ij}, x\_{ij} = \bot:
  \cr
  &\enspace\enspace
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
    \texttt{return } (F\_\bullet, \pi\_{\bullet i}, x\_{\bullet i})
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
F_1 \otimes F[\text{ZK}(\varphi)] \circledcirc F[\text{Stop}]
\end{matrix}
$$

Next, except with negligible probability, we can extract
an $s_i$ value.

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
&\ldots\cr
&\mu[\bullet] \gets \bot\cr
\cr
&\colorbox{bae6fd}{$
\underline{
  (1)\text{Prove}_k(B; b):
}$}\cr
  &\enspace
    \pi \gets \text{Prove}_k(B; b)
  \cr
  &\enspace
    \mu[\pi] \gets b
  \cr
  &\enspace
    \texttt{return } \pi
  \cr
\cr
&\underline{
  \text{Open}_k(S, (\pi\_\bullet, x\_\bullet)):
}\cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{for } j \in S \cap \mathcal{H}.\ \pi\_{j} \notin \mu:
  $}
  \cr
  &\enspace\enspace
  \colorbox{bae6fd}{$
    \texttt{stop }(\\{j\\}, 3)
  $}
  \cr
  &\enspace
    \text{Open}_k(S, (\pi\_\bullet, x\_\bullet))
  \cr
\end{aligned}
}
}
\cr
  \circ
\cr
F_1 \otimes F[\text{ZK}(\varphi)] \circledcirc F[\text{Stop}]
\end{matrix}
$$

Now, we can get rid of the ZK proofs entirely.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^7_H$
}\cr
\cr
&\underline{
  (1)\text{Run}_i(s):
}\cr
  &\enspace
    \ldots
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \text{Open}_i(\star, f_i(0), f_i(\bullet))
  $}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    (F\_\bullet, x\_{\bullet i}) \gets \text{WaitOpen}_i(\star)
  $}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1:
  $}
  \cr
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
  $\Gamma^7_M$
}\cr
&\ldots\cr
&\colorbox{bae6fd}{$
\pi_1, \ldots, \pi_n \xleftarrow{\\$} \texttt{01}^{\lambda}
$}\cr
&\colorbox{bae6fd}{$
F_k, m\_{ij}, \mu[\bullet] \gets \bot
$}\cr
\cr
&\colorbox{bae6fd}{$
\underline{
  (1)\text{SetCommit}_k(F):
}$}\cr
  &\enspace
    F_k \gets F
  \cr
  &\enspace
    \text{SetCommit}_k(F)
  \cr
\cr
&\colorbox{bae6fd}{$
\underline{
  (1)\text{Prove}_k(B; b):
}$}\cr
  &\enspace
    \mu[\pi_k] \gets b
  \cr
  &\enspace
    \texttt{return } \pi_k
  \cr
\cr
&\colorbox{bae6fd}{$
\underline{
  \text{Open}_k(S, (\pi\_\bullet, x\_\bullet)):
}$}\cr
  &\enspace
    \texttt{for } j \in S \cap \mathcal{H}.\ \mu[\pi\_{j}] \cdot G \notin \neq F_k(0):
  \cr
  &\enspace\enspace
    \texttt{stop }(\\{j\\}, 3)
  \cr
  &\enspace
    \forall j \in S.\ m\_{kj} \gets \pi_j
  \cr
  &\enspace
    \text{Open}_k(S, (\mu[\pi\_\bullet], x\_\bullet))
  \cr
\cr
&\colorbox{bae6fd}{$
\underline{
  \text{WaitOpen}_k(S):
}$}\cr
  &\enspace
    (F\_\bullet, x\_{\bullet k}) \gets \text{WaitOpen}_i(\star)
  \cr
  &\enspace
    \forall j \in S \cap \mathcal{H}.\ m_j \gets \pi_j
  \cr
  &\enspace
    \forall j \in S \cap \mathcal{M}.\ m_j \gets m\_{j k}
  \cr
  &\enspace
    \texttt{return } (F\_\bullet, m\_\bullet, x\_{\bullet k})
  \cr
\end{aligned}
}
}
\cr
  \circ
\cr
\boxed{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F_2$
}\cr
\cr
&F_i, \text{com}\_{ij}, \text{open}\_{ij} \gets \bot\cr
&x\_{ij} \gets \bot\cr
\cr
&\ldots\cr
\cr
&\colorbox{bae6fd}{$
\underline{
  \text{Open}_i(S, s, x\_\bullet):
}$}\cr
  &\enspace
    \texttt{assert } F_i \neq \bot \land s \cdot G = F_i(0)
  \cr
  &\enspace
    \text{open}\_{ij} \gets \texttt{true} (\forall j \in S)
  \cr
  &\enspace
    \texttt{for } j \in S.\ x\_{ij} = \bot:
  \cr
  &\enspace\enspace
    x\_{ij} \gets x\_j
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
    \texttt{return } (F\_\bullet, x\_{\bullet i})
  $}
  \cr
\end{aligned}
} \circledcirc F[\text{Stop}]
\end{matrix}
$$

At this point, we're simulating a protocol $\mathscr{P}_1$,
and so we can reset and unroll again.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^8_H$
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
  $\Gamma^8_M$
} = 1
\begin{pmatrix}
    \text{SetCommit}_k
  ,\cr
    \text{Commit}_k
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
F_2 \circledcirc F[\text{Stop}]
\end{matrix}
$$

Next we change things so that the functionality generates
polynomials for honest parties.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $\Gamma^9_H$
}\cr
\cr
&\underline{
  (1)\text{Run}_i(s):
}\cr
  &\enspace
    \text{SetCommit}_i(s)
  \cr
  &\enspace
    \text{Commit}_i(\star)
  \cr
  &\enspace
    \text{WaitCommit}_i(\star)
  \cr
  &\enspace
    \text{Open}_i(\star, s, \bot)
  \cr
  &\enspace
    (F\_\bullet, x\_{\bullet i}) \gets \text{WaitOpen}_i(\star)
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
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^9_H$
}\cr
\cr
&\ldots\cr
&\colorbox{bae6fd}{$
\text{badF}_k \gets \bot
$}\cr
&\colorbox{bae6fd}{$
\underline{
  (1)\text{SetCommit}_k(F):
}$}\cr
  &\enspace
    \texttt{if } \text{deg}(F) \neq t - 1:
  \cr
  &\enspace\enspace
    \text{badF}_k \gets \texttt{true}
  \cr
  &\enspace
    \text{SetCommit}_i(\bot, F)
  \cr
&\underline{
  \text{Commit}_k(S):
}\cr
  &\enspace
    \texttt{if } \text{badF}_k:
  \cr
  &\enspace\enspace
    \texttt{stop}(S \cap \mathcal{H}, 3)
  \cr
  &\enspace
    \text{Commit}_k(S)
  \cr
\end{aligned}
}
}
\cr
  \circ
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $F_3$
}\cr
\cr
&\text{com}\_{ij}, \text{open}\_{ij}, f_i, F_i, x\_{ij} \gets \bot\cr
\cr
&\underline{
  (1)\text{SetCommit}_i(s, F):
}\cr
  &\enspace
    \texttt{assert } s \neq \bot \lor (F \neq \bot \land \text{deg}(F) \leq t - 1)
  \cr
  &\enspace
    \texttt{if } s \neq \bot:
  \cr
  &\enspace\enspace
    f_i \xleftarrow{\\$} \\{ f \in \mathbb{F}_q[X]\_{\leq t - 1} \mid f(0) = s\\}
  \cr
  &\enspace\enspace
    F_i \gets f_i \cdot G
  \cr
  &\enspace
    \texttt{else}:
  \cr
  &\enspace\enspace
    F_i \gets F
  \cr
\cr
&\underline{
  \text{Commit}_i(S):
}\cr
  &\enspace
    \forall j \in S.\ \text{com}\_{ij} \gets \texttt{true}
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
  \text{Open}_i(S, s, x\_\bullet):
}\cr
  &\enspace
    \texttt{assert } F_i \neq \bot \land s \cdot G = F_i(0)
  \cr
  &\enspace
    \text{open}\_{ij} \gets \texttt{true} (\forall j \in S)
  \cr
  &\enspace
    \texttt{if } f_i \neq \bot:
  \cr
  &\enspace\enspace
    \forall j \in S, x\_{ij} = \bot.\ x\_{ij} \gets f_i(j)
  \cr
  &\enspace
    \texttt{else}:
  \cr
  &\enspace\enspace
    \forall j \in S, x\_{ij} = \bot.\ x\_{ij} \gets x_j
  \cr
\cr
&\underline{
  \text{WaitOpen}_i(S):
}\cr
  &\enspace
    \text{wait}\_{(i, 2)} \forall j \in S.\ \text{open}\_{ji}
  \cr
  &\enspace
    \texttt{return } (F\_\bullet, x\_{\bullet i})
  \cr
\end{aligned}
}
}
\circledcirc F[\text{Stop}]
\end{matrix}
$$

Now, $\Gamma^9_H$ acts as a simulator for a protocol $\mathscr{P}_2$,
and so we can reset again.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^{10}_H$
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
  $\Gamma^{10}_M$
} = 1
\begin{pmatrix}
    \text{SetCommit}_k
  ,\cr
    \text{Commit}_k
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
F_3 \circledcirc F[\text{Stop}]
\end{matrix}
$$

$\blacksquare$