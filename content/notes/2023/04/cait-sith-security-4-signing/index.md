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

For the sake of simplicity, we use a simpler key generation
and triple protocol as ideal functionalities,
to isolate the analysis of our presignature and signature protocols.

# Presigning

**Definition (Presignatures):**

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

$\square$

The functionalities we use here are perfect, in essence.
The idea behind the presignature functionality
is relatively simple.
One triple is used to help multiply $k$ and $x$,
and the other contains $k$ itself, which we use to help invert $k$
for the signature formula.

For convenience, we make it so that the presignature gives us $X$ and $x$ (secret shared).

Another convention is that the same key $x$ is used for an arbitrary
number of signatures, hence $\mathbb{N}$ instances.
We use $\tau$ as index to denote this instances.

**Definition (Ideal Presignatures):**

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
&\underline{
  \text{XK}^\tau():
}\cr
  &\enspace
    \texttt{return } f^{X}(0) \cdot f^{K, \tau}(0) \cdot G
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

$\square$

The ideal functionality basically spits out presignatures at will,
all under the same key.
We also get access to $k \cdot G$ and $kx \cdot G$, in addition to $k^{-1} \cdot G$.
$kx \cdot G$ is actually something you learn from a signature anyhow,
once it's completed.

**Lemma:**
For a negligible $\epsilon$, and up to $t - 1$ malicious corruptions, we have:

$$
\mathscr{P}[\text{Presign}] \overset{\epsilon}{\leadsto} \mathscr{P}[\text{IdealPresign}]
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
F[\text{Presign}] \circledcirc F[\text{Stop}]
\end{matrix}
$$

The idea behind the simulator is that you generate random values
for the messages you're going to receive,
and then use those to reverse engineer what the large values
like $A, B$ should be.

$\blacksquare$

# Signing

Signatures are pretty straightforward once you have presignatures.

**Definition (Signing):**

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
  (1)\text{Sign}_i^\tau():
}\cr
  &\enspace
    (X, \bullet, R, k_i, \sigma_i) \gets \text{Presign}_i^\tau()
  \cr
  &\enspace
    m \gets \text{GetMessage}^\tau()
  \cr
  &\enspace
    s_i \gets \text{Hash}(m) \cdot k_i + x(R) \cdot \sigma_i
  \cr
  &\enspace
    \Rsh_i(\star, s_i, 1)
  \cr
  &\enspace
    s\_\bullet \gets \Lsh_i(\star, 1)
  \cr
  &\enspace
    s \gets \sum_j s_j
  \cr
  &\enspace
    \texttt{if } \neg \text{ECDSA}.\text{Verify}(X, m, (R, s)):
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
  &\enspace
    \texttt{return } s
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
  $F[\text{Messages}]$
}\cr
\cr
&m^\tau \gets \bot
\cr
&\underline{
  \text{SetMessage}^\tau(m):
}\cr
  &\enspace
    \texttt{if } m^\tau = \bot: m^\tau \gets m
  \cr
\cr
&\underline{
  \text{GetMessage}^\tau():
}\cr
  &\enspace
    \texttt{wait } m^\tau \neq \bot
  \cr
  &\enspace
    \texttt{return } m^\tau
  \cr
\end{aligned}
}
}\cr
\otimes\cr
F[\text{SyncComm}]^\mathbb{N}\cr
\circledcirc \cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}, \text{SetMessage}^\tau\\}
\end{matrix}
}
\lhd \mathscr{P}[\text{Presign}]
$$

$\square$

We assume that there's a separate functionality which provides consensus
on the message to sign in each instance.

**Definition (Ideal Signing):**

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{IdealSign}]$
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
  (1)\text{Sign}_i^\tau():
}\cr
  &\enspace
    \text{Sync}^\tau_i(\star, 0)
  \cr
  &\enspace
    \text{WaitSync}^\tau_i(\star, 0)
  \cr
  &\enspace
    \text{Ready}^\tau_i(\star)
  \cr
  &\enspace
    \texttt{return } \text{Sig}^\tau_i()
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
  $F[\text{Sign}]$
}\cr
\cr
&\text{ready}\_{ij}^\tau \gets \texttt{false}\cr
&x, k^\tau \xleftarrow{\\$} \mathbb{F}_q\cr
\cr
&\underline{
  \text{Ready}^\tau_i(S):
}\cr
  &\enspace
    \text{ready}^\tau\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
  \text{WaitReady}^\tau_i(S):
}\cr
  &\enspace
    \texttt{wait}\_{(i, 1)} \forall j \in \mathcal{S}.\ \text{ready}^\tau\_{ji}
  \cr
\cr
&\underline{
  \text{Sig}^\tau_i():
}\cr
  &\enspace
    \texttt{wait}\_{(i, 1)} \forall j \in \mathcal{P}. \exists i.\ \text{ready}^\tau\_{ji}
  \cr
  &\enspace
    m \gets \text{GetMessage}^\tau()
  \cr
  &\enspace
    R \gets \frac{1}{k^\tau} \cdot G
  \cr
  &\enspace
    s \gets k \cdot (\text{Hash}(m) + x(R) \cdot x)
  \cr
  &\enspace
    \texttt{return } (R, s)
  \cr
\cr
&\underline{
  \text{Leak}^\tau():
}\cr
  &\enspace
    \texttt{return } (x \cdot G, xk^\tau \cdot G, k^\tau \cdot G, \frac{1}{k^\tau} \cdot G)
  \cr
\end{aligned}
}
}\cr
\circledcirc\cr
F[\text{Messages}]\cr
\otimes\cr
F[\text{Sync}]^\mathbb{N}\cr
\circledcirc \cr
F[\text{Stop}]
\end{matrix}\cr
\cr
\text{Leakage} := \\{\texttt{stop}, \text{SetMessage}^\tau, \text{GetMessage}^\tau, \text{Leak}^\tau\\}
\end{matrix}
}
\lhd \mathscr{P}[\text{Presign}]
$$

The ideal functionality unfortunately has to reflect
the round timing of the protocol itself.

**Lemma:**

For a negligible $\epsilon$, and up to $t - 1$ malicious corruptions, we have:

$$
\mathscr{P}[\text{Sign}] \overset{\epsilon}{\leadsto} \mathscr{P}[\text{IdealSign}]
$$

**Proof:**

First, we can replace $\mathscr{P}[\text{Presign}]$ with $\mathscr{P}[\text{IdealPresign}]$.

From there, we can use a similar simulator as last time:

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
&x_i, k^\tau\_i, \sigma^\tau\_i \xleftarrow{\\$} \mathbb{F}_q\ (i \in \mathcal{M})\cr
&k^\tau\_i, \sigma^\tau\_i \gets \bot\ (i \in \mathcal{H})\cr
&s\_{ij} \gets \bot\cr
&(X, \text{XK}^\tau, K^\tau, R^\tau) \gets \text{Leak}^\tau()\cr
\cr
&\underline{
  \text{Presign}^\tau_k():
}\cr
  &\enspace
    \text{Ready}^\tau_k(\mathcal{M})
  \cr
  &\enspace
    \texttt{return } (X, x_i, K^\tau, R^\tau, k^\tau_k, \sigma^\tau_k)
  \cr
\cr
&\underline{
  \text{K}^\tau():
}\cr
  &\enspace
    \texttt{return } K^\tau
  \cr
\cr
\cr
&\underline{
  \text{XK}^\tau():
}\cr
  &\enspace
    \texttt{return } \text{XK}^\tau
  \cr
\cr
&\underline{
  \Lsh^\tau_k(S, m\_\bullet, 1):
}\cr
  &\enspace
    \texttt{wait}\_{(k, 1)} \forall j \in S \cap \mathcal{M}.\ s\_{jk} \neq \bot
  \cr
  &\enspace
    r\_\bullet \gets \bot
  \cr
  &\enspace
    r_j \gets s\_{jk}\ (j \in S \cap \mathcal{M})
  \cr
  &\enspace
    \text{WaitReady}^\tau_k(S \cap \mathcal{H})
  \cr
  &\enspace
    \texttt{for } j \in S \cap \mathcal{H}:
  \cr
  &\enspace\enspace
    m \gets \text{GetMessage}^\tau()
  \cr
  &\enspace\enspace
    \texttt{if } |\\{ i \in \mathcal{H} \mid k^\tau_i = \bot \\}| = 1:
  \cr
  &\enspace\enspace\enspace
    k^\tau_j \xleftarrow{\\$} \mathbb{F}_q
  \cr
  &\enspace\enspace\enspace
    s \gets \text{Sig}^\tau_k()
  \cr
  &\enspace\enspace\enspace
    \sigma^\tau_j \gets \frac{1}{x(R)}\cdot \left(
      s - \text{Hash}(m) \cdot \sum_i k^\tau_i - x(R) \cdot \sum\_{i \neq j} \sigma^\tau_j
    \right)
  \cr
  &\enspace\enspace
    \texttt{else}:
  \cr
  &\enspace\enspace\enspace
    k^\tau_j, \sigma^\tau_j \xleftarrow{\\$} \mathbb{F}_q
  \cr
  &\enspace\enspace
    r_j \gets \text{Hash}(m) \cdot k^\tau_j + x(R) \cdot \sigma^\tau_j
  \cr
  &\enspace
    \texttt{return } r\_\bullet
  \cr
\cr
&\underline{
  \Rsh^\tau_k(S, m\_\bullet, 1):
}\cr
  &\enspace
    \text{Ready}^\tau_k(S)
  \cr
  &\enspace
    \texttt{for } j \in S.\ s\_{kj} = \bot:\ s\_{kj} \gets m_j
  \cr
  &\enspace
    \texttt{for } j \in \mathcal{H}. \forall k \in \mathcal{M}. s\_{kj} \neq \bot:
  \cr
  &\enspace\enspace
    \texttt{if } \sum\_{k \in \mathcal{M}} s\_{kj} \neq \text{Hash}(m^\tau) \cdot \sum_k k^\tau_k + x(R^\tau) \cdot \sum_k \sigma^\tau_k:
  \cr
  &\enspace\enspace\enspace
    \texttt{stop}(\\{j\\}, 1)
  \cr
&\ldots\cr
\end{aligned}
}
}
\cr
\circ\cr
F[\text{Messages}] \circledcirc F[\text{Stop}]
\end{matrix}
$$

The strategy is the same as other simulators in this section, where
we use the fact that only the sum has to verify
correctly, in order to give junk values up until the last moment.

$\blacksquare$

# The security of using presignatures

Here, we've limited ourselves to showing that our protocol
implements "ECDSA with presignatures",
as far as the security of "ECDSA with presignatures"
as a threshold signature scheme,
see [Groth & Shoup 2021](https://eprint.iacr.org/2021/1330).
