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
&\text{con}_1, c_1, o_1, \text{confirmed}_1, \text{open}_1, \ldots \gets \bot\cr
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
\end{aligned}
\end{aligned}
}
}
$$

$\square$
