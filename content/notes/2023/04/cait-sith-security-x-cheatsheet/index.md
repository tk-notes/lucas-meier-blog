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
\lhd \mathscr{P}[\text{KeyGen}]
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