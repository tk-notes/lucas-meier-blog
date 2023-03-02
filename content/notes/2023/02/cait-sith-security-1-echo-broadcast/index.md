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
The broadcast protocol $\mathscr{P}[\text{EB}]$ is defined by the following parties,
for $i \in [n]$:

$$
\boxed{
\small{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $P_i$
}\cr
\cr
&\underline{
  (1)\text{Broadcast}_i(x):
}\cr
  &\enspace
    \Rsh_i(\star, x, 0)
  \cr
  &\enspace
    [\hat{x}_j] \Lsh_i(\star, 0)
  \cr
  &\enspace
    \text{con}_i \gets \text{Hash}(\hat{x}_1, \ldots, \hat{x}_n)
  \cr
  &\enspace
    \Rsh_i(\star, \text{con}_i, 1)
  \cr
  &\enspace
    [\hat{\text{con}}_j] \Lsh_i(\star, 1)
  \cr
  &\enspace
    \texttt{if } \exists j.\enspace
    \hat{\text{con}}_j \neq \text{con}_i:
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
\end{aligned}
}
}
$$

**Lemma**
**Proof**

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^0_H$
}\cr
\cr
&\underline{
  (1)\text{Broadcast}_i(x):
}\cr
&\enspace
  \ldots
\cr
\end{aligned}
}
}
\otimes
\boxed{\colorbox{#FBCFE8}{\large
  $\Gamma^0_M$
} = 1
\begin{pmatrix}
    \Rsh_k
  ,\cr
    \Lsh_k
  ,\cr
    \text{Hash}
\end{pmatrix}
}
\cr
  \circ
\cr
F[\text{SyncComm}] \otimes F[\text{Hash}]
\end{matrix}
$$

Next, send both hash and vector.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^1_H$
}\cr
\cr
&\underline{
  (1)\text{Broadcast}_i(x):
}\cr
  &\enspace
    \Rsh_i(\star, x, 0)
  \cr
  &\enspace
    [\hat{x}_j] \Lsh_i(\star, 0)
  \cr
  &\enspace
    h_i \gets \text{Hash}(\hat{x}_1, \ldots, \hat{x}_n)
  \cr
  &\enspace
  \colorbox{#bae6fd}{$
    \Rsh_i(\star, (h_i, [\hat{x}_j]), 1)
  $}
  \cr
  &\enspace
  \colorbox{#bae6fd}{$
    [(\hat{h}_j, \vec{x}_j)] \Lsh_i(\star, 1)
  $}
  \cr
  &\enspace
    \texttt{if } \exists j.\enspace
    \hat{h}_j \neq h_i:
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
\end{aligned}
}
}
\otimes
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#bae6fd}{\large
  $\Gamma^1_M$
}\cr
\cr
&\underline{
  \Rsh_k(S, [h_j], 1):
}\cr
  &\enspace
    \Rsh_k(S, [(h_j, \bot)], 1)
  \cr
\cr
&\underline{
  \Lsh_k(S, 1):
}\cr
  &\enspace
    [(h_j, \bullet)]\Lsh_k(S, [(h_j, \bot)], 1)
  \cr
  &\enspace
    \texttt{return } [\hat{h}_j]
  \cr
\end{aligned}
}
}
\cr
  \otimes
\cr
  1(\Rsh_k, \Lsh_k, \text{Hash})
\end{matrix}
\cr
  \circ
\cr
F[\text{SyncComm}] \otimes F[\text{Hash}]
\end{matrix}
$$

Next, have honest parties omit hash.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^2_H$
}\cr
\cr
&\underline{
  (1)\text{Broadcast}_i(x):
}\cr
  &\enspace
    \Rsh_i(\star, x, 0)
  \cr
  &\enspace
    [\hat{x}_j] \Lsh_i(\star, 0)
  \cr
  &\enspace
    \Rsh_i(\star, (\bot, [\hat{x}_j]), 1)
  \cr
  &\enspace
    [(\hat{h}_j, \vec{x}_j)] \Lsh_i(\star, 1)
  \cr
  &\enspace
    \texttt{if } \neg \forall j.\enspace
    \begin{matrix}
      (\hat{h}_j \neq \bot \land \hat{h}_j = \text{Hash}(\vec{x}_i))\ \lor\cr
      (\vec{x}_j \neq \bot \land \text{Hash}(\vec{x}_j) = \text{Hash}(\vec{x}_i))
    \end{matrix}
    :
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
\end{aligned}
}
}
\otimes
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^2_M$
}\cr
\cr
&\underline{
  \Rsh_k(S, [h_j], 1):
}\cr
  &\enspace
    \Rsh_k(S, [(h_j, \bot)], 1)
  \cr
\cr
&\colorbox{#bae6fd}{$\underline{
  \Lsh_k(S, 1):
}$}\cr
  &\enspace
    [(\hat{h}_j, \vec{x}_j)]\Lsh_k(S, [(h_j, \bot)], 1)
  \cr
  &\enspace
    \texttt{if } \hat{h}_j = \bot:
  \cr
  &\enspace\enspace
    \texttt{assert } \vec{x}_j \neq \bot
  \cr
  &\enspace\enspace
    \hat{h}_j \gets \text{Hash}(\vec{x}_j)
  \cr
  &\enspace
    \texttt{return } [\hat{h}_j]
  \cr
\end{aligned}
}
}
\cr
  \otimes
\cr
  1(\Rsh_k, \Lsh_k, \text{Hash})
\end{matrix}
\cr
  \circ
\cr
F[\text{SyncComm}] \otimes F[\text{Hash}]
\end{matrix}
$$

Except with negligible probability, the hashes won't collide.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^3_H$
}\cr
\cr
&\underline{
  (1)\text{Broadcast}_i(x):
}\cr
  &\enspace
    \ldots
  \cr
  &\enspace
    \texttt{if } \neg \forall j.\enspace
    \begin{matrix}
      (\hat{h}_j \neq \bot \land \hat{h}_j = \text{Hash}(\vec{x}_i))\ \lor\cr
      \colorbox{#bae6fd}{$
      (\vec{x}_j \neq \bot \land \vec{x}_j = \vec{x}_i)
      $}
    \end{matrix}
    :
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
\end{aligned}
}
}
\otimes
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^3_M$
} = \Gamma^2_M
\end{aligned}
}
}
\cr
  \otimes
\cr
  1(\Rsh_k, \Lsh_k, \text{Hash})
\end{matrix}
\cr
  \circ
\cr
F[\text{SyncComm}] \otimes F[\text{Hash}]
\end{matrix}
$$

Now, get rid of communication.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#bae6fd}{\large
  $\Gamma^4_H$
}\cr
\cr
&\underline{
  (1)\text{Broadcast}_i(x):
}\cr
  &\enspace
    \hat{x}\_{ij} \gets x
  \cr
  &\enspace
    \texttt{wait}\_{(i, 0)}\ \forall j.\ \hat{x}\_{ji} \neq \bot
  \cr
  &\enspace
    e\_{ij} \gets (\bot, \hat{x}\_{\bullet i})
  \cr
  &\enspace
    \texttt{wait}\_{(i, 1)}\ \forall j.\ e\_{ji} \neq \bot
  \cr
  &\enspace
    (\hat{h}_j, \vec{x}_j) \gets e\_{ji}
  \cr
  &\enspace
    \texttt{if } \neg \forall j.\enspace
    \begin{matrix}
      (\hat{h}_j \neq \bot \land \hat{h}_j = \text{Hash}(\vec{x}_i))\ \lor\cr
      (\vec{x}_j \neq \bot \land \vec{x}_j = \hat{x}\_{\bullet i})
    \end{matrix}
    :
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
\end{aligned}
}
}
\otimes
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#bae6fd}{\large
  $\Gamma^4_M$
}\cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 0):
}\cr
  &\enspace
    \hat{x}\_{kj} \gets m_j\ (\forall j \in S)
  \cr
\cr
&\underline{
  \Rsh_k(S, h\_\bullet, 1):
}\cr
  &\enspace
    e\_{kj} \gets (h_j, \bot)\ (\forall j \in S)
  \cr
\cr
&\underline{
  \Lsh_k(S, 0):
}\cr
  &\enspace
    \texttt{wait}\_{(k, 0)}\ \forall j \in S.\ \hat{x}\_{jk} \neq \bot
  \cr
  &\enspace
    \texttt{return } [\hat{x}\_{j k} \mid j \in S]
  \cr
\cr
&\underline{
  \Lsh_k(S, 1):
}\cr
  &\enspace
    \texttt{wait}\_{(k, 1)}\ \forall j \in S.\ e\_{jk} \neq \bot
  \cr
  &\enspace
    (\hat{h}_j, \vec{x}_j) \gets e\_{jk}
  \cr
  &\enspace
    \texttt{if } \hat{h}_j = \bot:
  \cr
  &\enspace\enspace
    \texttt{assert } \vec{x}_j \neq \bot
  \cr
  &\enspace\enspace
    \hat{h}_j \gets \text{Hash}(\vec{x}_j)
  \cr
  &\enspace
    \texttt{return } [\hat{h}_j \mid j \in S]
  \cr
\end{aligned}
}
}
\cr
  \otimes
\cr
  1(\Rsh_k, \Lsh_k, \text{Hash})
\end{matrix}
\cr
  \circ
\cr
\boxed{
\small{
\begin{aligned}
\texttt{pub } x\_{ij}, e\_{ij} \gets \bot
\end{aligned}
}
}
\otimes
F[\text{Stop}] \otimes F[\text{Hash}]
\end{matrix}
$$

Next, remove $e_{ij}$ messages.

$\blacksquare$


$\square$

**Definition (Commit Protocol):**
The echo broadcast protocol $\mathscr{P}[\text{Commit}]$ is defined by the following parties,
for $i \in [n]$:

<!-- $$ -->
\boxed{
\small{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $P_i$
}\cr
\cr
&\text{con}_i, c_i, o_i \gets \bot\cr
\cr
&\begin{aligned}
&\underline{
  (1)\text{Commit}_i(x):
}\cr
&\enspace
  o_i \gets x
\cr
&\enspace
  c_i \gets H_1(x)
\cr
&\enspace
  \Rsh_i(\star, c_i, 0)
\cr
\cr
&\underline{\text{WaitCommit}_i():}\cr
&\enspace
  [c_j \mid j \neq i] \gets \texttt{await }\Lsh_i([n], 0)
\cr
&\enspace
  \text{con}_i \gets H_2(c_1, \ldots, c_n)
\cr
&\enspace
  \Rsh_i(\star, \text{con}_i, 1)
\cr
&\enspace
  \texttt{return } [c_j \mid j \in [n]]
\cr
\cr
\cr
\end{aligned}
&\begin{aligned}
&\underline{
  (1)\text{Open}_i():
}\cr
&\enspace
  \texttt{assert } o_i \neq \bot
\cr
&\enspace
  \Rsh_i(\star, o_i, 2)
\cr
\cr
\cr
&\underline{
  \text{WaitOpen}_i():
}\cr
&\enspace
  \texttt{assert } \text{con}_i \neq \bot
\cr
&\enspace
  [\text{con}_j \mid j \neq i] \gets \texttt{await }\Lsh_i([n], 1)
\cr
&\enspace
  \texttt{abort if } \exists j \neq j'.\ \text{con}_j \neq \text{con} _{j'}
\cr
&\enspace
  [\text{o}_j \mid j \neq i] \gets \texttt{await }\Lsh_i([n], 2)
\cr
&\enspace
  \texttt{abort if } \exists j.\ H_1(o_j) \neq c_j
\cr
&\enspace
  \text{con}_i \gets H_2(c_1, \ldots, c_n)
\cr
&\enspace
  \texttt{return } [o_j \mid j \in [n]]
\cr
\end{aligned}
\end{aligned}
}
}
$$

$\square$