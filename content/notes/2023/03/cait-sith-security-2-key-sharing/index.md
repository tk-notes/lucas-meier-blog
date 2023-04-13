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
    \texttt{return } \text{WaitShare}_i(\star)
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
\textcolor{ef4444}{
  (1)\text{Cheat}(F):
}
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
\textcolor{ef4444}{
  \text{CheatShare}(S, x\_\bullet):
}
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
&\underline{
\textcolor{ef4444}{
  \text{Leak}(i):
}
}\cr
  &\enspace
    \texttt{return } z_i \cdot G
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

**Lemma:**

$$
\mathscr{P}[\text{Convert}] \leadsto \mathscr{P}[\text{IdealConvert}]
$$

**Proof:**

First, $\mathscr{P}[\text{Convert}] \leadsto \mathscr{P}^0$,
where $\mathscr{P}^0$ replaces $\mathscr{P}[\text{Commit}]$
with $\mathscr{P}[\text{IdealCommit}]$.

Unrolling, we get:

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
  (1)\text{SetMask}_i():
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
    \text{Sync}_k
  ,\cr
    \text{WaitSync}_k
  ,\cr
    \texttt{stop}
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
  (1)\text{SetMask}_i():
}\cr
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
&\underline{
  \text{WaitMask}_i():
}\cr
  &\enspace
    \text{WaitCommit}_i(\star)
  \cr
  &\enspace
    \text{Sync}_i(\star, 0)
  \cr
\cr
&\underline{
  (1)\text{Share}_i(z_i):
}\cr
  &\enspace
    \text{Open}_i(\star)
  \cr
  &\enspace
    Z_i \gets z_i \cdot G
  \cr
  &\enspace
    \pi_i \gets \text{Prove}_i(Z_i; z_i)
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    Z\_{ij} \gets Z_i,\ \pi\_{ij} \gets \pi_i
  $}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    x\_{ij} \gets z_i + f_i(j)
  $}
  \cr
\cr
&\underline{
  \text{WaitShare}_i():
}\cr
  &\enspace
    \text{WaitSync}_i(\star, 0)
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    F\_\bullet \gets \text{WaitOpen}_i(\star)
  $}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{wait}\_{(i, 1)} \forall j.\ Z\_{ji}, \pi\_{ji} \neq \bot
  $}
  \cr
  &\enspace
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1 \lor F_j(0) \neq 0 \lor \neg \text{Verify}(\pi\_{ji}, Z\_{ji}):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{wait}\_{(i, 2)} \forall j.\ x\_{ji} \neq \bot
  $}
  \cr
  &\enspace
    x_i \gets \sum_j x\_{ji}, \enspace F \gets \sum_j Z\_{ji} + \sum_j F_j
  \cr
  &\enspace
    \texttt{if } x_i \cdot G \neq F(i):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 2)
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
  \Rsh_k(S, m\_\bullet, 1):
}\cr
  &\enspace
    \forall j \in S.\ (Z\_{kj}, \pi\_{kj}) \gets m\_j
  \cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 2):
}\cr
  &\enspace
    \forall j \in S.\ x\_{kj} \gets m\_j
  \cr
\cr
&\underline{
  \Lsh_k(S, 1):
}\cr
  &\enspace
    \texttt{wait}\_{(k, 1)} \forall j \in S.\ Z\_{jk}, \pi\_{jk} \neq \bot
  \cr
  &\enspace
    \texttt{return } [(Z\_{jk}, \pi\_{jk}) \mid j \in S]
  \cr
\cr
&\underline{
  \Lsh_k(S, 2):
}\cr
  &\enspace
    \texttt{wait}\_{(k, 2)} \forall j \in S.\ x\_{jk} \neq \bot
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
\small{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $F^0$
}\cr
\cr
&F_i, \text{com}\_{ij}, \text{open}\_{ij} \gets \bot\cr
&\texttt{pub } Z\_{ij}, \pi\_{ij}, x\_{ij} \gets \bot\cr
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
  \text{WaitMask}_i():
}\cr
  &\enspace
    \ldots
  \cr
  &\enspace
    \texttt{wait}\_{(i, 1)} \forall j.\ Z\_{ji}, \pi\_{ji} \neq \bot
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{wait}\_{(i, 1)} \forall j.\ x\_{ji} \neq \bot
  $}
  \cr
  &\enspace
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1 \lor F_j(0) \neq 0 \lor \neg \text{Verify}(\pi\_{ji}, Z\_{ji}):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
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
F_k, \mu[\bullet] \gets \bot
$}\cr
\cr
&\underline{
  (1)\text{SetCommit}_k(F):
}\cr
  &\enspace
  \colorbox{bae6fd}{$
    F_k \gets F
  $}
  \cr
  &\enspace
    \text{SetCommit}_k(F)
  \cr
\cr
&\underline{
  (1)\text{Prove}_k(Z;z):
}\cr
  &\enspace
  \colorbox{bae6fd}{$
    \pi \gets \text{Prove}_k(B;b)
  $}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \mu[\pi] \gets Z
  $}
  \cr
  &\enspace
    \texttt{return } \pi
  \cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 1):
}\cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{for } j \in S \cap \mathcal{H}:
  $}
  \cr
  &\enspace\enspace
  \colorbox{bae6fd}{$
    (Z_j, \pi_j) \gets m_j
  $}
  \cr
  &\enspace\enspace
  \colorbox{bae6fd}{$
    \texttt{if } \text{deg}(F_k) \neq t - 1 \lor F_k(0) \neq 0 \lor \mu[\pi] \neq Z_j
  $}
  \cr
  &\enspace\enspace\enspace
  \colorbox{bae6fd}{$
    \texttt{stop}(\\{j\\}, 1)
  $}
  \cr
  &\enspace
    \forall j \in S.\ (Z\_{kj}, \pi\_{kj}) \gets m\_j
  \cr
\end{aligned}
}
}\cr
\otimes\cr1(\ldots)
\end{matrix}
\cr
  \circ
\cr
F^0
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
  (1)\text{SetMask}_i():
}\cr
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
&\underline{
  \text{WaitMask}_i():
}\cr
  &\enspace
    \text{WaitCommit}_i(\star)
  \cr
  &\enspace
    \text{Sync}_i(\star, 0)
  \cr
\cr
&\underline{
  (1)\text{Share}_i(z_i):
}\cr
  &\enspace
    Z_i \gets z_i \cdot G
  \cr
  &\enspace
    \pi_i \gets \text{Prove}_i(Z_i; z_i)
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \text{Open}_i(\star, Z_i, \pi_i, z_i + f_i(j))
  $}
  \cr
\cr
&\underline{
  \text{WaitShare}_i():
}\cr
  &\enspace
    \text{WaitSync}_i(\star, 0)
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    (F\_\bullet, Z\_\bullet, \pi\_\bullet, x\_\bullet) \gets \text{WaitOpen}_i(\star)
  $}
  \cr
  &\enspace
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1 \lor F_j(0) \neq 0 \lor \neg \text{Verify}(\pi\_{ji}, Z\_{ji}):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
  &\enspace
    x_i \gets \sum_j x\_{ji}, \enspace F \gets \sum_j Z\_{ji} + \sum_j F_j
  \cr
  &\enspace
    \texttt{if } x_i \cdot G \neq F(i):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
  &\enspace
    \texttt{return } (x_i, F(0))
  \cr
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
\cr
&\mu[\bullet], \text{open}\_{kj}, \text{sync}\_{kj}, Z\_{kj}, \pi\_{kj}, x\_{kj} \gets \bot\cr
\cr
&\underline{
  (1)\text{SetCommit}_k(F):
}\cr
  &\enspace
    F_k \gets F
  \cr
  &\enspace
    \text{SetCommit}_k(F)
  \cr
\cr
&\underline{
  (1)\text{Prove}_k(Z;z):
}\cr
  &\enspace
    \pi \gets \text{Prove}_k(Z;z)
  \cr
  &\enspace
    \mu[Z] \gets z
  \cr
  &\enspace
    \texttt{return } \pi
  \cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 1):
}\cr
  &\enspace
    \texttt{for } j \in S \cap \mathcal{H}:
  \cr
  &\enspace\enspace
  \colorbox{bae6fd}{$
    (Z_j, \pi_j) \gets m_j
  $}
  \cr
  &\enspace\enspace
  \colorbox{bae6fd}{$
    \texttt{if } \text{deg}(F_k) \neq t - 1 \lor F_k(0) \neq 0 \lor \mu[\pi] \neq Z_j
  $}
  \cr
  &\enspace\enspace\enspace
  \colorbox{bae6fd}{$
    \texttt{stop}(\\{j\\}, 1)
  $}
  \cr
  &\enspace
    \forall j \in S.\ (Z\_{kj}, \pi\_{kj}) \gets m\_j
  \cr
  &\enspace
    \text{Open?}_k()
  \cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 2):
}\cr
  &\enspace
    \forall j \in S.\ x\_{kj} \gets m\_j
  \cr
  &\enspace
    \text{Open?}_k()
  \cr
\cr
&\underline{
  \Lsh_k(S, 1):
}\cr
  &\enspace
    (\forall j \in \mathcal{H})\ (\bullet, Z\_{jk}, \pi\_{j k}, \bullet) \gets \texttt{nowait } \text{WaitOpen}\_k(\\{j\\})
  \cr
  &\enspace
    \texttt{wait}\_{(k, 3)} \forall j \in S.\ Z\_{jk}, \pi\_{jk} \neq \bot
  \cr
  &\enspace
    \texttt{return } [(Z\_{jk}, \pi\_{jk}) \mid j \in S]
  \cr
\cr
&\underline{
  \Lsh_k(S, 2):
}\cr
  &\enspace
    (\forall j \in \mathcal{H})\ (\bullet, \bullet, \bullet, x\_{j k}) \gets \texttt{nowait } \text{WaitOpen}\_k(\\{j\\})
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
    o\_{kj} \gets \texttt{nowait } \text{WaitOpen}_k(\\{j\\})\ (j \in S \cap \mathcal{H})
  \cr
  &\enspace
    o\_{kj} \gets \text{sync}\_{kj}\ (j \in S \cap \mathcal{M})
  \cr
  &\enspace
    \texttt{wait}\_{(k, 1)} \forall j \in S.\ o\_{kj}
  \cr
\cr
&\underline{
  \text{Open}_k(S):
}\cr
  &\enspace
    \text{open}\_{kj} \gets \texttt{true}\ (\forall j \in S)
  \cr
  &\enspace
    \text{Open?}_k()
  \cr
\cr
&\underline{
  \text{WaitOpen}_k(S):
}\cr
  &\enspace
    (F_j, \bullet, \bullet, \bullet) \gets \text{WaitOpen}_k(S \cap \mathcal{H})\ (j \in S \cap \mathcal{H})
  \cr
  &\enspace
    \texttt{wait}\_{(k, 1)} \forall j \in S \cap \mathcal{M}.\ \text{open}\_{kj}
  \cr
  &\enspace
    \texttt{return } [F_j \mid j \in S]
  \cr
\cr
&\underline{
  \text{Open?}_k():
}\cr
  &\enspace
    \texttt{for } j \in \mathcal{H}.\ \text{open}\_{kj}, \text{sync}\_{kj}, Z\_{kj}, \pi\_{kj}, x\_{kj} \neq \bot:
  \cr
  &\enspace\enspace
    \text{Open}_i(\\{j\\}, Z\_{kj}, \pi\_{kj}, x\_{kj})
  \cr
\cr
\ldots
\end{aligned}
}
}\cr
\otimes\cr1(\ldots)
\end{matrix}
\cr
  \circ
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F^1$
}\cr
\cr
&F_i, \text{com}\_{ij}, \text{open}\_{ij} \gets \bot\cr
&Z\_{ij}, \pi\_{ij}, x\_{ij} \gets \bot\cr
\cr
&\ldots\cr
\cr
&\colorbox{bae6fd}{$
\underline{
  \text{Open}_i(S, Z\_\bullet, \pi\_\bullet, x\_\bullet):
}$}\cr
  &\enspace
    \texttt{assert } F_i \neq \bot
  \cr
  &\enspace
    \text{open}\_{ij} \gets \texttt{true} (\forall j \in S)
  \cr
  &\enspace
    \texttt{for } j \in S.\ Z\_{ij}, \pi\_{ij}, x\_{ij} = \bot:
  \cr
  &\enspace\enspace
    Z\_{ij}, \pi\_{ij}, x\_{ij} \gets Z_j, \pi_j, x\_j
  \cr
\cr
&\underline{
  \text{WaitOpen}_i(S):
}\cr
  &\enspace
    \text{wait}\_{(i, 1)} \forall j \in S.\ \text{open}\_{ji}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{return } (F\_\bullet, Z\_{\bullet i}, \pi\_{\bullet i}, x\_{\bullet i})
  $}
  \cr
\end{aligned}
}
}
\otimes
F[\text{ZK}(\varphi)] \circledcirc F[\text{Stop}]
\end{matrix}
$$

This is just a simulation of a protocol $\mathscr{P}_1$ where we need
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
  (1)\text{SetMask}_i():
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
F^1 \otimes F[\text{ZK}(\varphi)] \circledcirc F[\text{Stop}]
\end{matrix}
$$

Next, except with negligible probability, we can extract
a $z_i$ value.

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
  (1)\text{SetMask}_i():
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
  (1)\text{Prove}_k(Z; z):
}$}\cr
  &\enspace
    \pi \gets \text{Prove}_k(Z; z)
  \cr
  &\enspace
    \mu[\pi] \gets z
  \cr
  &\enspace
    \texttt{return } \pi
  \cr
\cr
&\underline{
  \text{Open}_k(S, Z\_\bullet, \pi\_\bullet, x\_\bullet):
}\cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{for } j \in S \cap \mathcal{H}.\ \mu[\pi_j] \cdot G \neq Z_j:
  $}
  \cr
  &\enspace\enspace
  \colorbox{bae6fd}{$
    \texttt{stop }(\\{j\\}, 1)
  $}
  \cr
  &\enspace
    \text{Open}_k(S, Z\_\bullet, \pi\_\bullet, x\_\bullet)
  \cr
\end{aligned}
}
}
\cr
  \circ
\cr
F^1 \otimes F[\text{ZK}(\varphi)] \circledcirc F[\text{Stop}]
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
  (1)\text{Share}_i(z_i):
}\cr
  &\enspace
    \ldots
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \text{Open}_i(\star, z_i, z_i + f_i(\bullet))
  $}
  \cr
\cr
&\underline{
  \text{WaitShare}_i():
}\cr
  &\enspace
  \colorbox{bae6fd}{$
    (F\_\bullet, Z\_{\bullet i}, x\_{\bullet i}) \gets \text{WaitOpen}_i(\star)
  $}
  \cr
  &\enspace
  \colorbox{bae6fd}{$
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1 \lor F_j(0) \neq 0:
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
  \text{Open}_k(S, Z\_\bullet, \pi\_\bullet, x\_\bullet):
}$}\cr
  &\enspace
    \texttt{for } j \in S \cap \mathcal{H}.\ \mu[\pi_j] \cdot G \neq Z\_j:
  \cr
  &\enspace\enspace
    \texttt{stop }(\\{j\\}, 3)
  \cr
  &\enspace
    \forall j \in S.\ m\_{kj} \gets \pi_j
  \cr
  &\enspace
    \text{Open}_k(S, \mu[\pi\_\bullet], x\_\bullet)
  \cr
\cr
&\colorbox{bae6fd}{$
\underline{
  \text{WaitOpen}_k(S):
}$}\cr
  &\enspace
    (F\_\bullet, Z\_{\bullet k}, x\_{\bullet k}) \gets \text{WaitOpen}_i(\star)
  \cr
  &\enspace
    \forall j \in S \cap \mathcal{H}.\ m_j \gets \pi_j
  \cr
  &\enspace
    \forall j \in S \cap \mathcal{M}.\ m_j \gets m\_{j k}
  \cr
  &\enspace
    \texttt{return } (F\_\bullet, Z\_{\bullet k}, m\_\bullet, x\_{\bullet k})
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
&\colorbox{FBCFE8}{\large
  $F^2$
}\cr
\cr
&F_i, \text{com}\_{ij}, \text{open}\_{ij} \gets \bot\cr
&z\_{ij}, x\_{ij} \gets \bot\cr
\cr
&\ldots\cr
\cr
&\colorbox{bae6fd}{$
\underline{
  \text{Open}_i(S, z\_\bullet, x\_\bullet):
}$}\cr
  &\enspace
    \texttt{assert } F_i \neq \bot
  \cr
  &\enspace
    \text{open}\_{ij} \gets \texttt{true} (\forall j \in S)
  \cr
  &\enspace
    \texttt{for } j \in S.\ z\_{ij}, x\_{ij} = \bot:
  \cr
  &\enspace\enspace
    z\_{ij}, x\_{ij} \gets z_j, x_j
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
    \texttt{return } (F\_\bullet, z\_{\bullet i } \cdot G, x\_{\bullet i})
  $}
  \cr
\end{aligned}
}
} \circledcirc F[\text{Stop}]
\end{matrix}
$$

At this point, we're simulating a protocol $\mathscr{P}^2$,
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
  (1)\text{SetMask}_i():
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
F^2 \circledcirc F[\text{Stop}]
\end{matrix}
$$

Now, this simulates the desired protocol:

$$
\begin{matrix}
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
    \texttt{return } \text{WaitShare}_i(\star)
  \cr
\end{aligned}
}
}
\otimes
\boxed{
\small{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $S$
}\cr
\cr
&\underline{
  (1)\text{SetCommit}_k(F):
}\cr
  &\enspace
    F_k \gets F
  \cr
  &\enspace
    \text{bad}_k \gets \text{deg}(F) \neq t - 1 \lor F(0) \neq 0
  \cr
  &\enspace
    \texttt{if } \forall k \in \mathcal{M}.\ F_k \neq \bot \land F^h = \bot:
  \cr
  &\enspace\enspace
    F^m \gets \sum_k F_k
  \cr
  &\enspace\enspace
    F^h \gets \text{Cheat}(F^m)
  \cr
\cr
&\underline{
  \text{Commit}_k(S):
}\cr
  &\enspace
    \texttt{assert } F_k \neq \bot
  \cr
  &\enspace
    \text{SetMask}_k(S)
  \cr
\cr
&\underline{
  \text{WaitCommit}_k(S):
}\cr
\cr
&\underline{
  \text{Open}_k(S, z\_\bullet, x\_\bullet):
}\cr
  &\enspace
    \texttt{if } \text{bad}_k:\ \texttt{stop}(S \cap \mathcal{H}, 1)
  \cr
  &\enspace
    \texttt{for } j \in S \cap \mathcal{H}.\ \forall k.\ x\_{kj} \neq \bot \land \sum_k x\_{kj} \cdot G \neq F^m(j):
  \cr
  &\enspace\enspace
    \texttt{stop}(\\{j\\}, 1)
  \cr
\cr
&\underline{
  \text{WaitOpen}_k(S):
}\cr
\cr
&\ldots
\end{aligned}
}
}
\cr
  \circ
\cr
F[\text{Convert}] \circledcirc F[\text{Stop}]
\end{matrix}
$$

$\blacksquare$