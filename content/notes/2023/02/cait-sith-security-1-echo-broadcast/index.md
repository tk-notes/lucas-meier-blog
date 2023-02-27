---
title: "Cait-Sith Security (1): Echo Broadcast"
date: 2023-02-27T14:03:26+01:00
type: note
note-tags:
  - "Cait-Sith"
  - "Cryptography"
  - "Protocols"
  - "TSS"
katex: true
---

One sub-component used a couple of times is a combined broadcast commitment
functionality, implemented via echo broadcast.

**Definition (Echo Broadcast Protocol):**
The echo broadcast protocol $\mathscr{P}[\text{EB}]$ is defined by the following parties,
for $i \in [n]$:

$$
\boxed{
\normalsize{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $P_i$
}\cr
\cr
&\text{con}_i, c_i, o_i \gets \bot\cr
\cr
&\begin{aligned}
&\underline{(1)\text{Commit}_i(x):}\cr
&\enspace o_i \gets x\cr
&\enspace c_i \gets H_1(x)\cr
&\enspace \Rsh_i(\star, c_i, 0)\cr
\cr
&\underline{\text{WaitCommit}_i():}\cr
&\enspace [c_j \mid j \neq i] \gets \texttt{await }\Lsh_i([n], 0)\cr
&\enspace \text{con}_i \gets H_2(c_1, \ldots, c_n)\cr
&\enspace \Rsh_i(\star, \text{con}_i, 1)\cr
&\enspace \texttt{return } [c_j \mid j \in [n]]\cr
\cr
\cr
\end{aligned}
&\begin{aligned}
&\underline{(1)\text{Open}_i():}\cr
&\enspace \texttt{assert } o_i \neq \bot\cr
&\enspace \Rsh_i(\star, o_i, 2)\cr
\cr
\cr
&\underline{\text{WaitOpen}_i():}\cr
&\enspace \texttt{assert } \text{con}_i \neq \bot\cr
&\enspace [\text{con}_j \mid j \neq i] \gets \texttt{await }\Lsh_i([n], 1)\cr
&\enspace \texttt{abort if } \exists j \neq j'.\ \text{con}_j \neq \text{con} _{j'}\cr
&\enspace [\text{o}_j \mid j \neq i] \gets \texttt{await }\Lsh_i([n], 2)\cr
&\enspace \texttt{abort if } \exists j.\ H_1(o_j) \neq c_j\cr
&\enspace \text{con}_i \gets H_2(c_1, \ldots, c_n)\cr
&\enspace \texttt{return } [o_j \mid j \in [n]]\cr
\end{aligned}
\end{aligned}
}
}
$$

The protocol has an ideal functionality $F[\text{SyncComm}] \otimes 1(H_1, H_2)$,
where $H_1$ and $H_2$ are global random oracles.

$\square$

**Definition (Echo Broadcast Functionality):**

$$
\boxed{
\normalsize{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $F[\text{EB}]$
}\cr
\cr
&c_1, o_1, \ldots \gets \bot\cr
&\text{confirmed}_1, \ldots \gets \bot\cr
&\text{open}_1, \ldots \gets \bot\cr
\cr
&\begin{aligned}
&\underline{(1)\text{Commit}_i(x):}\cr
&\enspace o_i \gets x\cr
&\enspace c_i \gets H_1(x)\cr
\cr
&\underline{\text{WaitCommit}_i():}\cr
&\enspace\texttt{wait } \forall j.\ c_j \neq \bot\cr
&\enspace \text{confirmed}_i \gets \texttt{true}\cr
&\enspace \texttt{return } [c_j \mid j \in [n]]\cr
\cr
&\underline{\text{Stop}():}\cr
&\enspace \texttt{die}
\end{aligned}
&\begin{aligned}
&\underline{(1)\text{Open}_i():}\cr
&\enspace \texttt{assert } o_i \neq \bot\cr
&\enspace \text{open}_i \gets \texttt{true}\cr
\cr
&\underline{\text{WaitOpen}_i():}\cr
&\enspace \texttt{assert } \text{confirmed}_i \cr
&\enspace\texttt{wait } \forall j.\ o_j \neq \bot\cr
&\enspace \texttt{return } [o_j \mid j \in [n]]\cr
\cr
\cr
\cr
\end{aligned}
\end{aligned}
}
}
$$

$\square$

**Lemma:** $\mathscr{P}[\text{EB}] \leadsto_{\mathcal{M}} F[\text{EB}]$,
where $\mathcal{M}$ is the corruption class allowing up to $n - 1$
parties to be maliciously corrupt,
relative to global random oracles $H_1$, $H_2$, allowing $Q_1$
and $Q_2$ queries, respectively.

{{<note>}}
Strictly speaking, we mean that $\mathscr{P} \leadsto \mathscr{F}$,
where $\mathscr{F}$ is a trivial protocol over $F$, providing access
to each function directly in the protocol.
{{</note>}}

**Proof:**

First, we consider the protocol $\mathscr{P}^0$, which is the same
as $\mathscr{P}[\text{EB}]$, except that the confirmation consists
of all commitments, rather than their hash:

$$
\boxed{
\normalsize{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $P'_i$
}\cr
\cr
&\ldots\cr
\cr
&\underline{\text{WaitCommit}_i():}\cr
&\enspace [c_j \mid j \neq i] \gets \texttt{await }\Lsh_i([n], 0)\cr
&\enspace \text{con}_i \gets \colorbox{#bae6fd}{$(c_1, \ldots, c_n)$}\cr
&\enspace \Rsh_i(\star, \text{con}_i, 1)\cr
&\enspace \texttt{return } [c_j \mid j \in [n]]\cr
\cr
&\ldots\cr
\end{aligned}
}
}
$$

Our next goal is to show that $\mathscr{P}[\text{EB}] \leadsto \mathscr{P}^0$.
The basic idea is that it's difficult to find two confirmation
vectors $(c_1, \ldots), (c'_1, \ldots)$ that yield the same hash,
so checking a hash is as good as checking the entire vector.

**Sub-Proof:**

Let's assume that indices $i$ are honest, and indices $k$ are malicious,
and unroll $\text{Inst}^{H_1, H_2}(\mathscr{P}[\text{EB}])$, which gives us
$\Gamma_0$:

$$
\begin{matrix}
\boxed{
\normalsize{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^0_H$
}\cr
\cr
&\underline{(1)\text{Commit}_i(x):}\cr
&\enspace o_i \gets x\cr
&\enspace c_i \gets H_1(x)\cr
&\enspace \Rsh_i(\star, c_i, 0)\cr
\cr
&\underline{\text{WaitCommit}_i():}\cr
&\enspace [c_j \mid j \neq i] \gets \texttt{await }\Lsh_i([n], 0)\cr
&\enspace \text{con}_i \gets H_2(c_1, \ldots, c_n)\cr
&\enspace \Rsh_i(\star, \text{con}_i, 1)\cr
&\enspace \texttt{return } [c_j \mid j \in [n]]\cr
\cr
&\underline{(1)\text{Open}_i():}\cr
&\enspace \texttt{assert } o_i \neq \bot\cr
&\enspace \Rsh_i(\star, o_i, 2)\cr
\cr
&\underline{\text{WaitOpen}_i():}\cr
&\enspace \texttt{assert } \text{con}_i \neq \bot\cr
&\enspace [\text{con}_j \mid j \neq i] \gets \texttt{await }\Lsh_i([n], 1)\cr
&\enspace \texttt{abort if } \exists j \neq j'.\ \text{con}_j \neq \text{con} _{j'}\cr
&\enspace [\text{o}_j \mid j \neq i] \gets \texttt{await }\Lsh_i([n], 2)\cr
&\enspace \texttt{abort if } \exists j.\ H_1(o_j) \neq c_j\cr
&\enspace \text{con}_i \gets H_2(c_1, \ldots, c_n)\cr
&\enspace \texttt{return } [o_j \mid j \in [n]]\cr
\end{aligned}
}
}
\otimes
\boxed{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^0_M$
}\cr
\cr
&\textcolor{#ef4444}{\underline{(1)\Rsh_k(P, [m_j], w):}}\cr
&\enspace \texttt{super}.\Rsh_k(P, [m_j], w)\cr
\cr
&\textcolor{#ef4444}{\underline{(1)\Lsh_k(P, w):}}\cr
&\enspace \texttt{super}.\Lsh_k(P, w)\cr
\cr
&\textcolor{#ef4444}{\underline{\text{Stop}():}}\cr
&\enspace \texttt{super}.\text{Stop}()\cr
\cr
\end{aligned}
}\otimes \textcolor{#a855f7}{1(H_1, H_2)}\cr
\circ\cr
\left(F[\text{SyncComm}] \otimes 1(H_1, H_2)\right)
\end{matrix}
$$


$\textcolor{#FBCFE8}{\blacksquare}$

$\blacksquare$