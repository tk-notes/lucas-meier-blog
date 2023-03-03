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
    \hat{x}\_\bullet \Lsh_i(\star, 0)
  \cr
  &\enspace
    \text{con}_i \gets \text{Hash}(\hat{x}\_\bullet)
  \cr
  &\enspace
    \Rsh_i(\star, \text{con}_i, 1)
  \cr
  &\enspace
    \hat{\text{con}}\_\bullet \Lsh_i(\star, 1)
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
    \hat{x}\_\bullet \Lsh_i(\star, 0)
  \cr
  &\enspace
    h_i \gets \text{Hash}(\hat{x}\_\bullet)
  \cr
  &\enspace
  \colorbox{#bae6fd}{$
    \Rsh_i(\star, (h_i, \hat{x}\_\bullet), 1)
  $}
  \cr
  &\enspace
  \colorbox{#bae6fd}{$
    (\hat{h}\_\bullet, \vec{x}\_\bullet) \Lsh_i(\star, 1)
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
  \Rsh_k(S, h\_\bullet, 1):
}\cr
  &\enspace
    \Rsh_k(S, (h\_\bullet, \bot), 1)
  \cr
\cr
&\underline{
  \Lsh_k(S, 1):
}\cr
  &\enspace
    (\hat{h}\_\bullet, \ldots)\Lsh_k(S, 1)
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
    \hat{x}\_\bullet \Lsh_i(\star, 0)
  \cr
  &\enspace
    \Rsh_i(\star, (\bot, \hat{x}\_\bullet), 1)
  \cr
  &\enspace
    (\hat{h}\_\bullet, \vec{x}\_\bullet) \Lsh_i(\star, 1)
  \cr
  &\enspace
    \texttt{if } \exists j.\enspace
    \begin{matrix}
      (\hat{h}_j \neq \bot \land \hat{h}_j \neq \text{Hash}(\vec{x}_i))\ \lor\cr
      (\vec{x}_j \neq \bot \land \text{Hash}(\vec{x}_j) \neq \text{Hash}(\vec{x}_i))
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
  \Rsh_k(S, h\_\bullet, 1):
}\cr
  &\enspace
    \Rsh_k(S, (h\_\bullet, \bot), 1)
  \cr
\cr
&\colorbox{#bae6fd}{$\underline{
  \Lsh_k(S, 1):
}$}\cr
  &\enspace
    (\hat{h}\_\bullet, \vec{x}\_\bullet)\Lsh_k(S, 1)
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
F[\text{SyncComm}] \otimes F[\text{Hash}]
\end{matrix}
$$

Now, except with negligible probability, we can use preimages instead.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^3_H$
}\cr
\cr
&\colorbox{#bae6fd}{$\underline{
  (1)\text{Broadcast}_i(x):
}$}\cr
  &\enspace
    \Rsh_i(\star, x, 0)
  \cr
  &\enspace
    \hat{x}\_\bullet \Lsh_i(\star, 0)
  \cr
  &\enspace
    \Rsh_i(\star, (\bot, \hat{x}\_\bullet), 1)
  \cr
  &\enspace
    (\hat{h}\_\bullet, \vec{x}\_\bullet) \Lsh_i(\star, 1)
  \cr
  &\enspace
    \texttt{if } \exists j.\enspace
    \begin{matrix}
      (\hat{h}_j \neq \bot \land \hat{h}_j \neq \text{Hash}(\vec{x}_i))\ \lor\cr
      (\vec{x}_j \neq \bot \land \text{Hash}(\vec{x}_j) \neq \text{Hash}(\vec{x}_i))
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
  $\Gamma^3_M$
}\cr
\cr
&\mu[\ldots] \gets \bot\cr
&\underline{
  \text{Hash}(x\_\bullet):
}\cr
  &\enspace
    h \gets \text{Hash}(x\_\bullet)
  \cr
  &\enspace
    \mu[h] \gets x\_\bullet
  \cr
  &\enspace
    \texttt{return } h
  \cr
\cr
&h\_{ij} \gets \bot\cr
&\underline{
  \Rsh_k(S, h\_\bullet, 1):
}\cr
  &\enspace
    \texttt{assert } \forall j \in S.\ h_j \neq \bot
  \cr
  &\enspace
    \texttt{for } j \in S \cap \mathcal{M}:
  \cr
  &\enspace\enspace
    h\_{kj} \gets h_j
  \cr
  &\enspace
    \texttt{for } j \in S \cap \mathcal{H}:
  \cr
  &\enspace\enspace
    \texttt{if } \mu[h_j] \neq \bot:
  \cr
  &\enspace\enspace\enspace
    \Rsh_k(\\{j\\}, (\bot, \mu[h_j]), 1)
  \cr
  &\enspace\enspace
    \texttt{else}:
  \cr
  &\enspace\enspace\enspace
    \Rsh_k(\\{j\\}, (h_j, \bot), 1)
  \cr
  &
\cr
&\underline{
  \Lsh_k(S, 1):
}\cr
  &\enspace
    (\ldots, \vec{x}\_\bullet)\Lsh_k(S \cap \mathcal{H}, 1)
  \cr
  &\enspace
    \texttt{wait}\_{(k, 1)}\ \forall j \in S \cap \mathcal{M}.\ h\_{jk} \neq \bot
  \cr
  &\enspace
    \texttt{return }\begin{bmatrix}
      \text{Hash}(\vec{x}\_j) &\mid& j \in S \cap \mathcal{H}\cr
      h\_{jk} &\mid& j \in S \cap \mathcal{M}\cr
    \end{bmatrix}
  \cr
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
    \texttt{if } \exists j.\enspace
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
    \vec{x}\_{ij} \gets \hat{x}\_{\bullet i}
  \cr
  &\enspace
    \text{sync}\_{ij} \gets \texttt{true}
  \cr
  &\enspace
    \texttt{wait}\_{(i, 1)}\ \forall j.\ \text{sync}\_{ji} \neq \bot
  \cr
  &\enspace
    \texttt{if } \exists j.\enspace
    \begin{matrix}
      (\hat{h}\_{ji} \neq \bot \land \hat{h}\_{ji} \neq \text{Hash}(\hat{x}\_{\bullet i}))\ \lor\cr
      (\vec{x}\_{ji} \neq \bot \land \vec{x}\_{ji} \neq \hat{x}\_{\bullet i})
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
    \hat{h}\_{kj} \gets h_j\ (\forall j \in S)
  \cr
  &\enspace
    \text{sync}\_{kj} \gets \texttt{true}\ (\forall j \in S)
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
    \texttt{wait}\_{(k, 1)}\ \forall j \in S.\ \text{sync}\_{jk} \neq \bot
  \cr
  &\enspace
    \texttt{if } \exists j \in S.\ \hat{h}\_{jk} = \bot:
  \cr
  &\enspace\enspace
    \texttt{assert } \vec{x}\_{jk} \neq \bot
  \cr
  &\enspace\enspace
    \hat{h}\_{jk} \gets \text{Hash}(\vec{x}\_{jk})
  \cr
  &\enspace
    \texttt{return } [\hat{h}\_{jk} \mid j \in S]
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
\texttt{pub } \hat{x}\_{ij}, \hat{h}\_{ij}, \vec{x}\_{ij}, \text{sync}\_{ij} \gets \bot
\end{aligned}
}
}
\otimes
F[\text{Stop}] \otimes F[\text{Hash}]
\end{matrix}
$$

Next, remove hash checks.

$$
\begin{matrix}
\boxed{
\small{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $\Gamma^5_H$
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
    \texttt{wait}\_{(i, 1)}\ \forall j.\ \text{sync}\_{ji} \neq \bot
  \cr
  &\enspace
    \colorbox{#bae6fd}{$
      \texttt{if } \exists j.\enspace
        (\vec{x}_j \neq \bot \land \vec{x}_j = \hat{x}\_{\bullet i})
      :
    $}
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
  $\Gamma^5_M$
}\cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 0):
}\cr
  &\enspace
    \hat{x}\_{kj} \gets m_j\ (\forall j \in S)
  \cr
  &\enspace
    \colorbox{#bae6fd}{$
      \text{Bad} \gets \\{i \in S \cap \mathcal{H} \mid \hat{x}\_{\bullet i} \neq \bot \land \text{Hash}(\hat{x}\_{\bullet i}) \neq h_i\\}
    $}
  \cr
  &\enspace
    \colorbox{#bae6fd}{$
      \text{Stop}(\text{Bad}, 1)
    $}
  \cr
\cr
&\underline{
  \Rsh_k(S, h\_\bullet, 1):
}\cr
  &\enspace
    \colorbox{#bae6fd}{$
      \text{Bad} \gets \\{i \in S \cap \mathcal{H} \mid \hat{x}\_{\bullet i} \neq \bot \land \text{Hash}(\hat{x}\_{\bullet i}) \neq h_i\\}
    $}
  \cr
  &\enspace
    \colorbox{#bae6fd}{$
      \text{Stop}(\text{Bad}, 1)
    $}
  \cr
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
\texttt{pub } \hat{x}\_{ij}, \hat{h}\_{ij}, \vec{x}\_{ij}, \text{sync}\_{ij} \gets \bot
\end{aligned}
}
}
\otimes
F[\text{Stop}] \otimes F[\text{Hash}]
\end{matrix}
$$

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