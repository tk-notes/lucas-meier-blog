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
    \Rsh^\tau_i(\star, \lambda(\mathcal{P}) \cdot \text{kd}_i, 1)
  \cr
  &\enspace
    \Rsh^\tau_i(\star, \lambda(\mathcal{P}) \cdot (k_i + a_i), 2)
  \cr
  &\enspace
    \Rsh^\tau_i(\star, \lambda(\mathcal{P}) \cdot (x_i + b_i), 3)
  \cr
  &\enspace\cr
  &\enspace
    \text{kd}\_\bullet \Lsh^\tau_i(\star, 1)
  \cr
  &\enspace
    \text{ka}\_\bullet \Lsh^\tau_i(\star, 2)
  \cr
  &\enspace
    \text{xb}\_\bullet \Lsh^\tau_i(\star, 3)
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
    \texttt{return } (X, x_i, R, k_i, \sigma_i)
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
F[\text{SyncComm}]^{\mathbb{N}}\cr
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
  $\mathscr{P}[\text{IdealPresign}]$
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
    \text{Sync}^{\tau}_i(\star, 0)
  \cr
  &\enspace
    \text{WaitSync}^\tau_i(\star, 0)
  \cr
  &\enspace
    \texttt{return } \text{Presign}_i^\tau()
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
  $F[\text{Presign}]$
}\cr
\cr
&f^X, f^{K, \tau}, \xleftarrow{\\$} \mathbb{F}_q[X]\_{\leq t - 1}\cr
&f^{\Sigma, \tau}, \xleftarrow{\\$} \\{ f \in \mathbb{F}_q[X]\_{\leq t - 1} \mid f(0) = 0 \\}\cr
\cr
&\underline{
  \text{Presign}_i^\tau():
}\cr
  &\enspace
    X \gets f^X(0) \cdot G
  \cr
  &\enspace
    x_i \gets f^X(i)
  \cr
  &\enspace
    R \gets \frac{1}{f^{K, \tau}(0)} \cdot G
  \cr
  &\enspace
    k_i \gets f^{K, \tau}(i)
  \cr
  &\enspace
    \sigma_i \gets f^X(0) \cdot f^{K, \tau}(0) + f^{\Sigma, \tau}(i)
  \cr
  &\enspace
    \texttt{return } (X, x_i, R, k_i, \sigma_i)
  \cr
\cr
&\underline{
  \text{K}^\tau():
}\cr
  &\enspace
    \texttt{return } f^{K, \tau}(0) \cdot G
  \cr
\end{aligned}
}
}\cr
\otimes\cr
F[\text{Sync}]^{\mathbb{N}}\cr
\circledcirc \cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}\\}
\end{matrix}
}
$$

**Lemma:**
$$
\mathscr{P}[\text{Presign}] \leadsto \mathscr{P}[\text{IdealPresign}]
$$
**Proof:**

First, we note that $\mathscr{P}[\text{Presign}] \leadsto \mathscr{P}^0$,
which modifies $P_i$ to consolidate message sending:

$$
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
    \Rsh^\tau_i(\star, (\lambda(\mathcal{P}) \cdot \text{kd}_i, \lambda(\mathcal{P}) \cdot (k_i + a_i), \lambda(\mathcal{P}) \cdot (x_i + b_i)), 0)
  \cr
  \cr
  &\enspace
    (\text{kd}\_\bullet, \text{ka}\_\bullet, \text{xb}\_\bullet) \Lsh^\tau_i(\star, 0)
  \cr
  &\enspace
    \text{kd} \gets \sum_j \text{kd}_j
  \cr
  &\enspace
    \texttt{if } \text{kd} \cdot G \neq \text{KD}:\enspace\texttt{stop}(\star, 0)
  \cr
  &\enspace
    \text{ka} \gets \sum_j \text{ka}_j
  \cr
  &\enspace
    \texttt{if } \text{ka} \cdot G \neq K + A:\enspace\texttt{stop}(\star, 0)
  \cr
  &\enspace
    \text{xb} \gets \sum_j \text{xb}_j
  \cr
  &\enspace
    \texttt{if } \text{xb} \cdot G \neq X + B:\enspace\texttt{stop}(\star, 0)
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
$$

A sketch of the simulator here would be to delay sending messages
from malicious to honest parties until all three bundles have been sent,
and to detect failures early to make all aborts look the same.

From $\mathcal{P}^0$, we can jump to $\mathcal{P}[\text{IdealPresign}]$ directly.

$$
\begin{matrix}
\boxed{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $\Gamma^0_H$
}\cr
&\ldots
\end{aligned}
}
\otimes
\boxed{
\small{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $S$
}\cr
&\alpha^\tau_j, \beta^\tau_j, d^\tau_j, \delta^\tau_j \xleftarrow{\\$} \mathbb{F}_q\cr
&\alpha^\tau \gets \sum_j \alpha^\tau_j, \beta^\tau \gets \sum_j \beta^\tau_j, \delta^\tau \gets \sum_j \delta^\tau_j\cr
&\gamma^\tau_j \xleftarrow{\\$} \\{(\gamma_1, \ldots, \gamma_n) \mid \sum_j \gamma_j = \alpha \cdot \beta \\}\cr
&(X, x^\tau_k, R^\tau, k^\tau_k, \sigma^\tau_k) \gets \text{Presign}^\tau_k()\cr
&K^\tau \gets \text{K}^\tau(), \text{XK}^\tau \gets \text{XK}^\tau()\cr
\cr
&A^\tau \gets \alpha^\tau \cdot G - K^\tau\cr
&B^\tau \gets \beta^\tau \cdot G - X\cr
&C^\tau \gets \alpha^\tau \beta^\tau - \alpha^\tau \cdot X - \beta^\tau \cdot K^\tau + \text{XK}^\tau\cr
&D^\tau \gets \delta^\tau \cdot R^\tau, \text{KD}^\tau \gets \delta^\tau \cdot G\cr
\cr
&\underline{
  \text{Triple}^{\tau, 0}_k():
}\cr
  &\enspace
    a_k \gets \alpha^\tau_k - k^\tau_k
  \cr
  &\enspace
    b_k \gets \beta^\tau_k - k^\tau_k
  \cr
  &\enspace
    c_k \gets \gamma^\tau_k - \alpha \cdot x - \beta \cdot k + \sigma^\tau_k
  \cr
  &\enspace
    \texttt{return } (a_k, b_k, c_k, A^\tau, B^\tau, C^\tau)
  \cr
\cr
&\underline{
  \text{Triple}^{\tau, 1}_k():
}\cr
  &\enspace
    \texttt{return } (k^\tau_k, d^\tau_k, \delta^\tau_k, K^\tau, D^\tau, \text{KD}^\tau)
  \cr
\cr
\cr
&m^\tau\_{ij} \gets \bot\cr
\cr
&\underline{
  \Rsh^{\tau}_k(S, \hat{m}\_\bullet = (\text{kd}\_\bullet, \text{ka}\_\bullet, \text{xb}\_\bullet), 0):
}\cr
  &\enspace
    m^\tau\_{kj} \gets \hat{m}\_{kj}\ (\forall j \in S)
  \cr
  &\enspace
    \text{Sync}_k(S \cap \mathcal{H}, 0)
  \cr
  &\enspace
    \texttt{for } j \in \mathcal{H}.\ \forall k \in \mathcal{M}.\ m^\tau\_{kj} \neq \bot:
  \cr
  &\enspace\enspace
    (\text{kd}_k, \text{ka}_k, \text{xb}_k) \gets m\_{k j}
  \cr
  &\enspace
    \hat{\text{kd}} = \sum\_{j \in \mathcal{H}} \delta^\tau_j + \sum\_{k \in \mathcal{M}} \text{kd}_k
  \cr
  &\enspace\enspace
    \hat{\text{ka}} = \sum\_{j \in \mathcal{H}} \alpha^\tau_j + \sum\_{k \in \mathcal{M}} \text{ka}_k
  \cr
  &\enspace\enspace
    \hat{\text{xb}} = \sum\_{j \in \mathcal{H}} \beta^\tau_j + \sum\_{k \in \mathcal{M}} \text{xb}_k
  \cr
  &\enspace\enspace
    \texttt{if } \hat{\text{kd}} \cdot G \neq \text{KD}^\tau \lor \hat{\text{ka}} \cdot G \neq K^\tau + A^\tau \lor \hat{\text{xb}} \cdot G \neq X + B^\tau:
  \cr
  &\enspace\enspace\enspace
    \texttt{stop}(\\{j\\}, 0)
  \cr
\cr
&\underline{
  \Lsh^{\tau}_k(S, 0):
}\cr
  &\enspace
    \text{WaitSync}_k(S \cap \mathcal{H}, 0)
  \cr
  &\enspace
    \texttt{wait}\_{(k, 0)} \forall j \in S.\ m^\tau\_{jk} \neq \bot
  \cr
  &\enspace
    r_j \gets m^\tau\_{jk}\ (j \in S \cap \mathcal{M})
  \cr
  &\enspace
    r_j \gets (\delta^\tau_j, \alpha^\tau_j, \beta^\tau_j)\ (j \in S \cap \mathcal{H})
  \cr
  &\enspace
    \texttt{return } r\_\bullet
  \cr
\cr
&\ldots\cr
\end{aligned}
}
}
\cr
\circ\cr
F[\text{Presign}]
\end{matrix}
$$

$\blacksquare$

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
    (X, \bullet, R, k_i, \sigma_i) \gets \text{Presign}_i^\tau()
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
F[\text{Sync}(0)]\cr
\circledcirc \cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}\\}
\end{matrix}
}
\lhd \mathscr{P}[\text{Presign}]
$$