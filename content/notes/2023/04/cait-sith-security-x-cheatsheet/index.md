---
title: "Cait-Sith Security (X): Cheat Sheet"
date: 2023-04-02T19:51:20+02:00
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
  $\mathscr{P}[\text{EchoBroadcast}]$
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
  (1)\text{StartBroadcast}_i(x):
}\cr
  &\enspace
    \Rsh_i(\star, x, 0)
  \cr
\cr
&\underline{
  \text{WaitBroadcast}_i(x):
}\cr
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
    \texttt{return } x\_{\bullet}
  \cr
\cr
&\underline{
  \text{EndBroadcast}_i():
}\cr
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
\quad
\begin{matrix}
F[\text{SyncComm}]\cr
\otimes\cr
F[\text{Hash}]\cr
\end{matrix}\cr
\cr
\text{Leakage} := \\{\text{Hash}, \texttt{stop}\\}
\end{matrix}
}
$$
$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{Commit}]$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&x_i, r_i \gets \bot\cr
\cr
&\underline{
  (1)\text{SetCommit}_i(x):
}\cr
  &\enspace
    x_i \gets x, \quad r_i \xleftarrow{\\$} \texttt{01}^{2 \lambda}
  \cr
  &\enspace
    \text{SetBroadcast}_i(\text{Hash}(x_i, r_i))
  \cr
\cr
&\underline{
  \text{Commit}_i():
}\cr
  &\enspace
    \text{SendBroadcast}_i(\star)
  \cr
\cr
&\underline{
  \text{WaitCommit}_i():
}\cr
  &\enspace
    \texttt{return } \text{WaitBroadcast}_i()
  \cr
\cr
&\underline{
  \text{Open}_i():
}\cr
  &\enspace
    \texttt{assert } x_i \neq \bot
  \cr
  &\enspace
    \Rsh_i(\star, (x_i, r_i), 2)
  \cr
\cr
&\underline{
  \text{WaitOpen}_i():
}\cr
  &\enspace
    c\_\bullet \gets \text{WaitCommit}_i()
  \cr
  &\enspace
    \text{EndBroadcast}_i()
  \cr
  &\enspace
    (\hat{x}\_{\bullet}, \hat{r}\_{\bullet}) \Lsh_i(\star, 2)
  \cr
  &\enspace
    \texttt{if } \exists j.\ \text{Hash}(\hat{x}_j, \hat{r}_j) \neq c_j:
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 2)
  \cr
  &\enspace
    \texttt{return } \hat{x}\_{\bullet}
  \cr
\cr
\end{aligned}
}
}
\quad
\begin{matrix}
F[\text{Stop}]\cr
\circledcirc\cr
F[\text{SyncComm}]\cr
\otimes\cr
F[\text{Hash}]\cr
\end{matrix}\cr
\cr
\text{Leakage} := \\{\text{Hash}, \texttt{stop}\\}
\end{matrix}
}
\lhd \mathscr{P}[\text{EchoBroadcast}]
$$
$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{Convert}]$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&f_i \gets \bot\cr
\cr
&\underline{
  (1)\text{SetMask}_i():
}\cr
  &\enspace
    f_i \xleftarrow{\\$} \\{ f_i \in \mathbb{F}_q[X]\_{\leq t - 1} \mid f_i(0) = 0 \\\}
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
&\underline{
  \text{WaitMask}_i():
}\cr
  &\enspace
    \text{WaitCommit}_i()
  \cr
\cr
&\underline{
  \text{Share}_i(z_i):
}\cr
  &\enspace
    \text{Open}_i()
  \cr
  &\enspace
    \Rsh_i(\star, [z_i + f_i(j) \mid j \in [n]], 0)
  \cr
\cr
&\underline{
  \text{WaitShare}_i(Z):
}\cr
  &\enspace
    F\_\bullet \gets \text{WaitOpen}_i()
  \cr
  &\enspace
    x\_{\bullet i} \gets \Lsh_i(\star, 0)
  \cr
  &\enspace
    x_i \gets \sum_j x\_{ji}, \enspace F \gets Z + \sum_j F_j(0)
  \cr
  &\enspace
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1 \lor x_i \cdot G \neq F(i) \lor F(0) \neq Z:
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 3)
  \cr
  &\enspace
    \texttt{return } x_i
  \cr
\end{aligned}
}
}
\quad
\begin{matrix}
F[\text{Commit}]\cr
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
  (1)\text{Share}_i(s):
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
    \text{Open}_i()
  \cr
  &\enspace
    \Rsh_i(\star, [f_i(j) \mid j \in [n]], 3)
  \cr
  &\enspace\cr
  &\enspace
    F\_\bullet \gets \text{WaitOpen}_i()
  \cr
  &\enspace
    x\_{\bullet i} \gets \Lsh_i(\star, 3)
  \cr
  &\enspace
    x_i \gets \sum_j x\_{ji}, \enspace F \gets \sum_j F_j(0)
  \cr
  &\enspace
    \texttt{if } \exists j.\ \text{deg}(F_j) \neq t - 1 \lor x_i \cdot G \neq F(i):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 3)
  \cr
  &\enspace
    \texttt{return } (x_i, F(0))
  \cr
\end{aligned}
}
}
\quad
\begin{matrix}
F[\text{SyncComm}]\cr
\circledcirc \cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}\\}
\end{matrix}
}
\lhd \mathscr{P}[\text{Commit}]
$$

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{KeyGen}]$
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
  (1)\text{Gen}_i():
}\cr
  &\enspace
    s \xleftarrow{\\$} \mathbb{F}_q
  \cr
  &\enspace
    \texttt{return } \text{Share}_i(s)
  \cr
\end{aligned}
}
}
\end{matrix}
}
\lhd \mathscr{P}[\text{KeyShare}]
$$


$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{Presign}]$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&\texttt{setup}_i \gets \texttt{false}\cr
&x_i, X \gets \bot\cr
\cr
&\underline{
  (1)\text{Setup}_i():
}\cr
  &\enspace
    (x_i, X) \gets \text{Gen}_i()
  \cr
  &\enspace
    \texttt{setup}_i \gets \texttt{true}
  \cr
\cr
&\underline{
  (1)\text{Presign}_i^\tau():
}\cr
  &\enspace
    \texttt{assert } \texttt{setup}_i
  \cr
  &\enspace
    (a_i, b_i, c_i, A, B, C) \gets \text{Triple}_i^{(\tau, 0)}()
  \cr
  &\enspace
    (k_i, d_i, \text{kd}_i, K, D, \text{KD}) \gets \text{Triple}_i^{(\tau, 1)}()
  \cr
  &\enspace
    \Rsh_i(\star, \lambda(\mathcal{P}) \cdot \text{kd}_i, 1)
  \cr
  &\enspace
    \Rsh_i(\star, \lambda(\mathcal{P}) \cdot (k_i + a_i), 2)
  \cr
  &\enspace
    \Rsh_i(\star, \lambda(\mathcal{P}) \cdot (x_i + b_i), 3)
  \cr
  &\enspace\cr
  &\enspace
    \text{kd}\_\bullet \Lsh_i(\star, 1)
  \cr
  &\enspace
    \text{ka}\_\bullet \Lsh_i(\star, 2)
  \cr
  &\enspace
    \text{xb}\_\bullet \Lsh_i(\star, 3)
  \cr
  &\enspace
    \text{kd} \gets \sum_j \text{kd}_j
  \cr
  &\enspace
    \texttt{if } \text{kd} \cdot G \neq \text{KD}:\enspace\texttt{stop}(\star, 1)
  \cr
  &\enspace
    \text{ka} \gets \sum_j \text{ka}_j
  \cr
  &\enspace
    \texttt{if } \text{ka} \cdot G \neq K + A:\enspace\texttt{stop}(\star, 2)
  \cr
  &\enspace
    \text{xb} \gets \sum_j \text{xb}_j
  \cr
  &\enspace
    \texttt{if } \text{xb} \cdot G \neq X + B:\enspace\texttt{stop}(\star, 3)
  \cr
  &\enspace\cr
  &\enspace
    R \gets \frac{1}{\text{kd}} \cdot D
  \cr
  &\enspace
    \sigma_i \gets \text{ka} \cdot x_i - \text{xb} \cdot a_i + c_i
  \cr
  &\enspace
    \texttt{return } (X, R, k_i, \sigma_i)
  \cr
\end{aligned}
}
}
\quad
\begin{matrix}
F[\text{SyncComm}]\cr
\circledcirc \cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}\\}
\end{matrix}
}
\lhd
\begin{matrix}
\mathscr{P}[\text{KeyGen}]\cr
\otimes\cr
\mathscr{P}[\text{Triple}]
\end{matrix}
$$

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{Sign}]$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&\texttt{setup}_i \gets \texttt{false}\cr
\cr
&\underline{
  (1)\text{Setup}_i():
}\cr
  &\enspace
    \texttt{super}.\text{Setup}_i()
  \cr
  &\enspace
    \texttt{setup}_i \gets \texttt{true}
  \cr
\cr
&\underline{
  (1)\text{Sign}_i^\tau(m):
}\cr
  &\enspace
    \texttt{assert } \texttt{setup}_i
  \cr
  &\enspace
    (X, R, k_i, \sigma_i) \gets \text{Presign}_i^\tau()
  \cr
  &\enspace
    s_i \gets \text{Hash}(m) \cdot k_i + x(R) \cdot \sigma_i
  \cr
  &\enspace
    \Rsh_i(\star, s_i, 4)
  \cr
  &\enspace
    s\_\bullet \gets \Lsh_i(\star, 4)
  \cr
  &\enspace
    s \gets \sum_j s_j
  \cr
  &\enspace
    \texttt{if } \neg \text{ECDSA}.\text{Verify}(X, m, (R, s)):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 4)
  \cr
  &\enspace
    \texttt{return } s
  \cr
\end{aligned}
}
}
\quad
\begin{matrix}
F[\text{SyncComm}]\cr
\circledcirc \cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}\\}
\end{matrix}
}
\lhd \mathscr{P}[\text{Presign}]
$$

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{Multiply}]$
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
    \forall j \neq i.\ \text{StartMTA}_i^{(0, ij)}(\text{Flip}_i(a, b))
  \cr
  &\enspace
    \forall j \neq i.\ \text{StartMTA}_i^{(1, ij)}(\text{Flip}_i(b, a))
  \cr
\cr
&\underline{
  (1)\text{EndMultiply}_i():
}\cr
  &\enspace
    \texttt{assert } \text{start}_i
  \cr
  &\enspace
    \texttt{wait}\_{(i, 0)} \forall j. (\gamma^0\_j, \gamma^1\_j) \gets (\text{EndMTA}_i^{(0, ij)}(), \text{EndMTA}_i^{(1, ij)}())
  \cr
  &\enspace
    \texttt{return } a \cdot b + \sum_j (\gamma^0_j + \gamma^1\_j)
  \cr
\end{aligned}
}
}
\end{matrix}
}
\lhd
\begin{matrix}
F[\text{MTA}]^{2n^2}\cr
\end{matrix}
$$

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{Triple}]$
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
    f_i, e_i \xleftarrow{\\$} \mathbb{F}_q[X]\_{\leq t - 1}
  \cr
  &\enspace
    F_i, E_i, L_i \gets f_i \cdot G, e_i \cdot G, l_i \cdot G
  \cr
  &\enspace
    \text{SetCommit}_i((F_i, E_i, L_i))
  \cr
  &\enspace
    \text{Commit}_i()
  \cr
  &\enspace
    \text{SetMask}_i()
  \cr
  \cr
  &\enspace
    \text{WaitCommit}_i()
  \cr
  &\enspace
    \text{WaitMask}_i()
  \cr
  &\enspace
    \text{Open}_i()
  \cr
  &\enspace
    \Rsh_i(\star, [(f_i(j), e_i(j)) \mid j \in [n]], 0)
  \cr
  &\enspace\cr
  &\enspace
    (F\_\bullet, E\_\bullet) \gets \text{WaitOpen}_i()
  \cr
  &\enspace
    (a\_{\bullet i}, b\_{\bullet i}) \gets \Lsh_i(\star, 0)
  \cr
  &\enspace
    a_i \gets \sum_j a\_{ji}, \enspace F \gets \sum_j F_j(0)
  \cr
  &\enspace
    b_i \gets \sum_j a\_{ji}, \enspace E \gets \sum_j E_j(0)
  \cr
  &\enspace
    \texttt{if } a_i \cdot G \neq E(i) \lor b_i \cdot G \neq F(i):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 0)
  \cr
  &\enspace
    \text{Multiply}_i(f_i(0), e_i(0))
  \cr
  &\enspace
    C_i \gets e_i(0) \cdot F(0)
  \cr
  &\enspace
    \pi_i \gets \text{Prove}^\psi(E_i(0), F(0), C_i; e_i(0))
  \cr
  &\enspace
    \Rsh_i(\star, (C_i, \pi_i), 1)
  \cr
  &\enspace\cr
  &\enspace
    (C\_\bullet, \pi\_\bullet) \Lsh_i(\star, 1)
  \cr
  &\enspace
    \texttt{if } \exists j.\ \neg \text{Verify}^\psi(E_j(0), F(0), C_j)
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
  &\enspace
    z_i \gets \text{WaitMultiply}_i()
  \cr
  &\enspace
    \text{Share}_i(z_i)
  \cr
  &\enspace
    c_i \gets \text{WaitShare}_i(C)
  \cr
  &\enspace
    \texttt{return } (a_i, b_i, c_i, E(0), F(0), C)
  \cr
\end{aligned}
}
}
\quad
\begin{matrix}
F[\text{ZK}(\psi)]\cr
\otimes\cr
F[\text{SyncComm}]\cr
\circledcirc \cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}\\}
\end{matrix}
}
\lhd
\begin{matrix}
\mathscr{P}[\text{Commit}]\cr
\otimes\cr
\mathscr{P}[\text{Convert}]\cr
\otimes\cr
\mathscr{P}[\text{Multiply}]\cr
\otimes\cr
\mathscr{P}[\text{Convert}]\cr
\end{matrix}
$$

# Ideal Protocols

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{IdealBroadcast}]$
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
  (1)\text{StartBroadcast}_i(x):
}\cr
  &\enspace
    \text{SetBroadcast}_i(x)
  \cr
  &\enspace
    \text{SendBroadcast}_i(\star)
  \cr
\cr
&\underline{
  \text{WaitBroadcast}_i():
}\cr
  &\enspace
    x\_{\bullet} \gets \text{GetBroadcast}_i(\star)
  \cr
  &\enspace
    \text{Sync}_i(\star)
  \cr
  &\enspace
    \texttt{return } x\_{\bullet}
  \cr
\cr
&\underline{
  \text{EndBroadcast}_i():
}\cr
  &\enspace
    \text{WaitSync}_i(\star)
  \cr
  &\enspace
    \texttt{if } \text{BadBroadcast}_i():
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
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
  $F[\text{Broadcast}]$
}\cr
\cr
&x_i, \text{sent}\_{ij}, \text{trap}\_{ij} \gets \bot\cr
\cr
&\underline{
  (1)\text{SetBroadcast}_i(x):
}\cr
  &\enspace
    x_i \gets x
  \cr
\cr
&\underline{
  \text{SendBroadcast}_i(S):
}\cr
  &\enspace
    \texttt{assert } x_i \neq \bot
  \cr
  &\enspace
    \text{sent}\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
  \text{GetBroadcast}_i(S):
}\cr
  &\enspace
    \texttt{wait}\_{(i, 0)}\ \text{sent}\_{ji}\ (\forall j \in S)
  \cr
  &\enspace
    \texttt{return } [x_j \mid j \in S]
  \cr
\cr
&\underline{
  \textcolor{ef4444}{\text{Trap}(j, m\_\bullet)}:
}\cr
  &\enspace
    \texttt{assert } \forall i.\ m_i = \bot \lor (\text{trap}\_{i j} = \bot \land x_i = \bot)
  \cr
  &\enspace
    \text{trap}\_{i j} \gets m_i
  \cr
\cr
&\underline{
  \text{BadBroadcast}_i():
}\cr
  &\enspace
    \texttt{return } \exists j.\ \text{trap}\_{j i} \neq \bot \land \text{trap}\_{j i} \neq x\_j
  \cr
\end{aligned}
}
}\cr
\otimes\cr
F[\text{Sync}(1)]\cr
\otimes\cr
F[\text{Stop}]\cr
\end{matrix}\cr
\cr
\text{Leakage} := \\{\text{Trap}, \texttt{stop}\\}
\end{matrix}
}
$$
$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{IdealCommit}]$
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
  (1)\text{SetCommit}_i(x):
}\cr
  &\enspace
    \text{SetCommit}_i(x)
  \cr
\cr
&\underline{
  \text{Commit}_i():
}\cr
  &\enspace
    \text{Commit}_i(\star)
  \cr
\cr
&\underline{
  \text{WaitCommit}_i():
}\cr
  &\enspace
    \text{WaitCommit}_i(\star)
  \cr
  &\enspace
    \text{Sync}_i(\star)
  \cr
\cr
&\underline{
  \text{Open}_i():
}\cr
  &\enspace
    \text{Open}_i(\star)
  \cr
\cr
&\underline{
  \text{WaitOpen}_i():
}\cr
  &\enspace
    \text{WaitCommit}_i()
  \cr
  &\enspace
    \text{WaitSync}_i(\star)
  \cr
  &\enspace
    \texttt{return } \text{WaitOpen}_i(\star)
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
  $F[\text{Commit}]$
}\cr
\cr
&x_i, \text{com}\_{ij}, \text{open}\_{ij} \gets \bot\cr
\cr
&\underline{
  (1)\text{SetCommit}_i(x):
}\cr
  &\enspace
    x_i \gets x
  \cr
\cr
&\underline{
  \text{Commit}_i(S):
}\cr
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
    \texttt{assert } x_i \neq \bot
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
    \texttt{return } x\_\bullet
  \cr
\end{aligned}
}
}\cr
\otimes\cr
F[\text{Sync}(1)]\cr
\circledcirc\cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}\\}
\end{matrix}
}
$$

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $F[\text{MTA}]$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&a_1, a_2, \beta_1, \beta_2 \gets \bot\cr
&\Delta \gets \bot\cr
\cr
&\underline{
  (1)\text{StartMTA}_i(a):
}\cr
  &\enspace
    a_i \gets a
  \cr
\cr
&\underline{
  \text{Sample}():
}\cr
  &\enspace
    \texttt{assert } a_1, a_2, \Delta \neq \bot
  \cr
  &\enspace
    \texttt{if } \beta_1, \beta_2 = \bot:
  \cr
  &\enspace\enspace
    (\beta_1, \beta_2) \xleftarrow{\\$} \\{(\beta_1, \beta_2) \in \mathbb{F}_q^2 \mid \beta_1 + \beta_2 = a_1 \cdot a_2 + \Delta \\}
  \cr
\cr
&\underline{
  (1)\text{EndMTA}_i():
}\cr
  &\enspace
    \texttt{wait}\_{(i, 0)} a_1, a_2, \Delta \neq \bot
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
\end{aligned}
}
}
\end{matrix}
}
$$

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $F[\text{Multiply}]$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&a\_{ij}, b\_{ij}, \beta_i, \Delta \gets \bot\cr
\cr
&\underline{
  (1)\text{StartMultiply}_i(a\_\bullet, b\_\bullet):
}\cr
  &\enspace
    a\_{i\bullet} \gets a\_\bullet,\ b\_{i \bullet} \gets b\_{\bullet}
  \cr
\cr
&\underline{
  (1)\text{EndMultiply}_i():
}\cr
  &\enspace
    \texttt{wait}\_{(i, 0)} \forall ij.\ a\_{ij}, b\_{ij} \neq \bot \land \Delta \neq \bot
  \cr
  &\enspace
    \text{Sample}()
  \cr
  &\enspace
    \texttt{return } \beta_i
  \cr
\cr
&\underline{
  \text{Sample}():
}\cr
  &\enspace
    \texttt{assert } \forall ij.\ a\_{ij}, b\_{ij} \neq \bot \land \Delta \neq \bot
  \cr
  &\enspace
    \texttt{if } \forall i.\ \beta_i = \bot:
  \cr
  &\enspace\enspace
    c \gets \sum\_{ij} a\_{ij} \cdot b\_{ij}
  \cr
  &\enspace\enspace
    (\beta_1, \ldots, \beta_n) \gets \\{\beta_i \xleftarrow{\\$} \mathbb{F}_q^n \mid \sum_i \beta_i = c + \Delta \\}
  \cr
\cr
&\underline{
  (1)\text{Cheat}(\Delta):
}\cr
  &\enspace
    \Delta \gets \Delta
  \cr
\end{aligned}
}
}
\end{matrix}
}
$$
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

# Connections

(All for negligeable epsilon, and up to $t - 1$ malicious corruptions.)

- $\mathscr{P}[\text{EchoBroadcast}] \leadsto \mathscr{P}[\text{IdealBroadcast}]$.
- $\mathscr{P}[\text{Commit}] \leadsto \mathscr{P}[\text{IdealCommit}]$
- $\mathscr{P}[\text{KeyShare}] \leadsto \mathscr{P}[\text{IdealKeyShare}]$
- $\mathscr{P}[\text{KeyGen}] \leadsto \mathscr{P}[\text{IdealKeyGen}]$
- $\mathscr{P}[\text{Convert}] \leadsto \mathscr{P}[\text{IdealConvert}]$
- $\mathscr{P}[\text{Presign}] \leadsto \mathscr{P}[\text{IdealPresign}]$
- $\mathscr{P}[\text{Sign}] \leadsto \mathscr{P}[\text{IdealSign}]$
- $\mathscr{P}[\text{Multiply}] \leadsto F[\text{Multiply}]$
- $\mathscr{P}[\text{Triple}] \leadsto \mathscr{P}[\text{IdealTriple}]$
