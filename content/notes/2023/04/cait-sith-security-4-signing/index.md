---
title: "Cait-Sith Security (4): Signing"
date: 2023-04-16T21:50:00+02:00
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
&\underline{
  (1)\text{Presign}_i^\tau():
}\cr
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
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F[\text{Setup}]$
}\cr
\cr
&f \xleftarrow{\\$} \mathbb{F}_q[X]\_{\leq t - 1}\cr
\cr
&\underline{
  \text{Key}_i():
}\cr
  &\enspace
    \texttt{return } (f(0) \cdot G, f(i))
  \cr
\end{aligned}
}
}\cr
\otimes\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F[\text{Triple}]$
}\cr
\cr
&f^{A, \tau}, f^{B, \tau} \xleftarrow{\\$} \mathbb{F}_q[X]\_{\leq t - 1}\cr
&f^{C, \tau} \xleftarrow{\\$} \\{ f \in \mathbb{F}_q[X]\_{\leq t - 1} \mid f(0) = f^{A, \tau}(0) \cdot f^{B, \tau} \\}
\cr
\cr
&\underline{
  \text{Triple}_i^\tau():
}\cr
  &\enspace
    (a_i, b_i, c_i) \gets
    (f^{A, \tau}(i), f^{B, \tau}(i), f^{C, \tau}(i))
  \cr
  &\enspace
    (A, B, C) \gets (f^{A, \tau}(0) \cdot G, f^{B, \tau}(0) \cdot G, f^{C, \tau}(0) \cdot G)
  \cr
  &\enspace
    \texttt{return } (a_i, b_i, c_i, A, B, C)
  \cr
\end{aligned}
}
}\cr
\otimes\cr
F[\text{Triple}]\cr
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
&\underline{
  (1)\text{Sign}_i^\tau(m):
}\cr
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