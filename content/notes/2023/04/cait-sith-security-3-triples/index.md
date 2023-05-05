---
title: "Cait-Sith Security (3): Multiplication and Triples"
date: 2023-04-05T18:22:14+02:00
type: note
note-tags:
  - "Cait-Sith"
  - "Cryptography"
  - "Protocols"
  - "TSS"
katex: true
---

We basically start with the ideal
functionality for multiplicative-to-additive conversion (MTA),
from [HMRT21](https://eprint.iacr.org/2021/1373).
We slightly simplify their functionality, by giving the adversary
more power to corrupt the result.

**Definition (MTA):**
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

$\square$

The basic idea is that we convert a secret $s = a \cdot b$ into shares
$\alpha, \beta$ such that $\alpha + \beta = s$.
But, the malicious party can cause the result
to fail, by instead being a secret sharing of $s + \Delta$.

# Multiplication

Next, we consider multiplication using this functionality.

First, we need to take a detour, to consider using two instances
of the functionality together.

Consider the following protocol:

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{MTA}^2]$
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
  (1)\text{Start}\_{ij}(a, b):
}\cr
  &\enspace
    \text{StartMTA}^0_i(\text{Flip}\_{ij}(a, b))
  \cr
  &\enspace
    \text{StartMTA}^1_i(\text{Flip}\_{ij}(b, a))
  \cr
\cr
&\underline{
  (1)\text{EndMTA}\_{ij}():
}\cr
  &\enspace
    \texttt{return } \text{EndMTA}^0_i() + \text{EndMTA}^1_i()
  \cr
\cr
&\underline{
  (1)\text{Cheat}^\tau(\Delta)
}\cr
  &\enspace
    \text{Cheat}^\tau(\Delta)
  \cr
\end{aligned}
}
}
\end{matrix}
}
\lhd \mathscr{P}[\text{MTA}]^2
$$

Well, it turns out that this implements the following functionality:

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $F[\text{MTA}^2]$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&a_1, a_2, b_1, b_2, \beta_1, \beta_2 \gets \bot\cr
&\Delta \gets \bot\cr
\cr
&\underline{
  (1)\text{Start}_i(a, b):
}\cr
  &\enspace
    a_i \gets a,\ b_i \gets b
  \cr
\cr
&\underline{
  \text{Sample}():
}\cr
  &\enspace
    \texttt{assert } a_1, a_2, b_1, b_2, \Delta \neq \bot
  \cr
  &\enspace
    \texttt{if } \beta_1, \beta_2 = \bot:
  \cr
  &\enspace\enspace
    (\beta_1, \beta_2) \xleftarrow{\\$} \\{(\beta_1, \beta_2) \in \mathbb{F}_q^2 \mid \beta_1 + \beta_2 = a_1 \cdot b_2 + a_2 \cdot b_1 + \Delta \\}
  \cr
\cr
&\underline{
  (1)\text{End}_i():
}\cr
  &\enspace
    \texttt{wait}\_{(i, 0)} a_1, a_2, b_1, b_2, \Delta \neq \bot
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
\cr
&\underline{
  \text{Leak}(\Delta)
}\cr
  &\enspace
    \texttt{return } (a_1 \neq \bot, a_2 \neq \bot, b_1 \neq \bot, b_2 \neq \bot)
  \cr
\end{aligned}
}
}
\end{matrix}
}
$$

This is a bit of a simplification, in that there's only a single $\Delta$
for the result, rather than 2.
This will make our life a bit easier later.

**Lemma:**

For a negligible $\epsilon$, and any number of malicious corruptions, we have:

$$
\mathscr{P}[\text{MTA}^2] \overset{\epsilon}{\leadsto} F[\text{MTA}^2]
$$

**Proof:**

First, the case of all corruptions is trivial, as with any protocol,
and the protocol clearly functions in the case of no corruptions,
so we consider the case where, without loss of generality, the second
party is corrupt.

We start, as usual, by unrolling things.
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
  (1)\text{Start}_1(a, b):
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
    \text{StartMTA}^\tau_2
  ,\cr
    \text{EndMTA}^\tau_2
  ,\cr
    \text{Cheat}^\tau
\end{pmatrix}
}
\cr
\circ\cr
F[\text{MTA}] \otimes F[\text{MTA}]
\end{matrix}
$$

And from here, we can directly perform a simulation.

$$
\begin{matrix}
\boxed{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $\Gamma^1_H$
} = 1
\begin{pmatrix}
    \text{Start}_1
  ,\cr
    \text{End}_1
\end{pmatrix}
\end{aligned}
}
\otimes
\boxed{
\small{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $S$
}\cr
\cr
&\Delta^1, \Delta^2, \text{first} \gets \bot\cr
&\alpha \xleftarrow{\\$} \mathbb{F}_q\cr
\cr
&\underline{
  (1)\text{StartMTA}^\tau_2(a):
}\cr
  &\enspace
    \texttt{if } a^\tau = \bot:
  \cr
  &\enspace\enspace
    a^\tau \gets a
  \cr
  &\enspace
    \texttt{if } a^1, a^2 \neq \bot:
  \cr
  &\enspace\enspace
    \text{Start}(a^1, a^2)
  \cr
\cr
&\underline{
  (1)\text{EndMTA}^\tau_2():
}\cr
  &\enspace
    (r_1, \bullet, r_2, \bullet) \gets \text{Leak}()
  \cr
  &\enspace
    \texttt{wait}\_{(2, 0)} r\_\tau \neq \bot \land a^\tau, \Delta^\tau \neq \bot
  \cr
  &\enspace
    \texttt{if } \text{first} = \bot:
  \cr
  &\enspace\enspace
    \text{first} \gets \tau
  \cr
  &\enspace
    \texttt{if } \text{first} = \tau:
  \cr
  &\enspace\enspace
    \texttt{return } \alpha
  \cr
  &\enspace
    \texttt{return } \text{End}_2()
  \cr
\cr
&\underline{
  (1)\text{Cheat}^\tau(\Delta):
}\cr
  &\enspace
    \texttt{if } \Delta^\tau = \bot:
  \cr
  &\enspace\enspace
    \Delta^\tau \gets \Delta
  \cr
  &\enspace
    \texttt{if } \Delta^1, \Delta^2 \neq \bot:
  \cr
  &\enspace\enspace
    \text{Cheat}(\Delta^1 + \Delta^2 - \alpha)
  \cr
\end{aligned}
}
}
\cr
\circ\cr
F[\text{MTA}^2]
\end{matrix}
$$

The basic idea is that the adversary can only tell whether or not
they've received correct shares of the MTA by summing everything
together, from both the honest and malicious parties,
because of this, we can give them junk until they get the second
share, in which case we need to make it so that the sum is preserved.
We can do this by cheating with $\Delta^1 + \Delta^2 - \alpha$,
and using the result of the "double MTA" as their second share.
When we sum everything up, we'll get the right share.

$\blacksquare$

Next, let's look at the actual multiplication protocol.

**Definition (Multiplication):**
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
F[\text{MTA}]^{n^2}\cr
\end{matrix}
$$

$\square$

The basic idea is that you need to do MTAs between each pair
of parties.
One bit of notation we use is that we use $ij$ as indices,
whereas in reality this only ranges of pairs such that $i \leq j$,
to not repeat the same pair twice.
We also use $\text{Flip}_i(a, b)$ to denote a process
whereby for each pair $i, j$, one party uses $a$ and the other $b$.
We assume some common convention for doing this flip.

This gets us to the ideal functionality for multiplication:

**Definition (Ideal Multiplication):**

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{IdealMultiply}]$
}\cr
\cr
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $P_i$
}\cr
\cr
&a, b \gets \bot\cr
\cr
&\underline{
  (1)\text{StartMultiply}_i(a, b):
}\cr
  &\enspace
    a, b \gets a, b
  \cr
  &\enspace
    a\_{\bullet} \gets a,\ b\_{\bullet} \gets b
  \cr
  &\enspace
    \text{StartMultiply}_i(a\_{\bullet}, b\_{\bullet})
  \cr
\cr
&\underline{
  (1)\text{EndMultiply}_i():
}\cr
  &\enspace
    \texttt{return } a \cdot b + \text{EndMultiply}_i()
  \cr
\end{aligned}
}
}
\quad
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F[\text{Multiply}]$
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
    \texttt{wait}\_{(i, 0)} \forall i \neq j.\ a\_{ij}, b\_{ij} \neq \bot \land \Delta \neq \bot \land a\_{ii} \neq \bot
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
    \texttt{assert } \forall i \neq j.\ a\_{ij}, b\_{ij} \neq \bot \land \Delta \neq \bot
  \cr
  &\enspace
    \texttt{if } \forall i.\ \beta_i = \bot:
  \cr
  &\enspace\enspace
    c \gets \sum\_{i \neq j} a\_{ij} \cdot b\_{ji}
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
\cr
&\underline{
  \text{Leak}(i, j):
}\cr
  &\enspace
    \texttt{return } a\_{ij}, b\_{ij} \neq \bot
  \cr
\end{aligned}
}
}
\end{matrix}
}
$$

$\square$

There are two ways the adversary can tamper with the result here.
They can cheat via $\Delta$, as one might expect,
or they can cheat by using inconsistent shares in the multiplication.
This is reflected by using an entire vector $a\_\bullet, b\_\bullet$,
rather than a single scalar.

**Lemma:**

For some negligible $\epsilon$, and any number of malicious corruptions, we have:
$$
\mathscr{P}[\text{Multiply}] \overset{\epsilon}{\leadsto} \mathscr{P}[\text{IdealMultiply}]
$$
**Proof:**

First, $\mathscr{P}[\text{Multiply}] = \mathscr{P}^0 \lhd \mathscr{P}[\text{MTA}^2]^{n^2}$,
where:

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}^0$
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
    \forall j \neq i.\ \text{Start}_i^{ij}(a, b)
  \cr
\cr
&\underline{
  (1)\text{EndMultiply}_i():
}\cr
  &\enspace
    \texttt{assert } \text{start}_i
  \cr
  &\enspace
    \texttt{return } a \cdot b + \sum\_{j \neq i} \text{End}_i^{ij}()
  \cr
\end{aligned}
}
}
\end{matrix}
}
\lhd
\begin{matrix}
\mathscr{P}[\text{MTA}^2]^{n^2}\cr
\end{matrix}
$$

Then, it holds that:
$$
\mathscr{P}^0 \lhd \mathscr{P}[\text{MTA}^2]^{n^2/2}
\leadsto \mathscr{P} \lhd F[\text{MTA}^2]^{n^2/2}
= \mathscr{P}^1
$$

Unrolling $\mathscr{P}^1$, we get the following:

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
  (1)\text{StartMultiply}_i(a, b):
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
    \text{Start}_k^{ki}
  ,\cr
    \text{End}_k^{ki}
  ,\cr
    \text{Cheat}^{ki}
\end{pmatrix}
}
\cr
\circ\cr
F[\text{MTA}^2]^{n^2/2}
\end{matrix}
$$
This is equivalent to:
$$
\begin{matrix}
\boxed{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $\Gamma^0_H$
} = 1
\begin{pmatrix}
    \text{StartMultiply}_i
  ,\cr
    \text{EndMultiply}_i
\end{pmatrix}
\end{aligned}
}
\otimes
\boxed{
\small{
\begin{aligned}
&\colorbox{bae6fd}{\large
  $S$
}\cr
\cr
&\text{left} \gets \\{(k, i)\ k \in \mathcal{M}, i \in \mathcal{H} \\}\cr
&a\_{ki}, b\_{ki}, \alpha\_{ki}, \Delta\_{ki} \gets \bot\cr
&a\_{kl}, b\_{kl} \gets 0\cr
\cr
&\underline{
  \text{Start}_k^{ki}(a, b)
}\cr
  &\enspace
    a\_{ki} \gets a, b\_{ki} \gets b
  \cr
  &\enspace
    \texttt{if } \forall i.\ a\_{ki}, b\_{ki} \neq \bot:
  \cr
  &\enspace\enspace
    \text{StartMultiply}_i(a\_\bullet, b\_\bullet)
  \cr
\cr
&\underline{
  \text{End}_k^{ki}()
}\cr
  &\enspace
    \texttt{if } (k, i) \notin \text{left}:
  \cr
  &\enspace\enspace
    \texttt{return } \alpha\_{ki}
  \cr
  &\enspace
    \texttt{wait}\_{(i, 0)} a\_{ki}, b\_{ki}, \Delta\_{ki} \neq \bot \land \text{Leak}(i, k)
  \cr
  &\enspace
    \text{left} \gets \text{left} / \\{(k, i)\\}
  \cr
  &\enspace
    \texttt{if } |\text{left}| = 0:
  \cr
  &\enspace\enspace
    \alpha_k \gets \text{EndMultiply}_k()\enspace (k \in \mathcal{M})
  \cr
  &\enspace\enspace
    \texttt{return } \sum\_{k \in \mathcal{M}} \alpha_k - \sum\_{\alpha\_{ki} \neq \bot} \alpha\_{ki}
  \cr
  &\enspace
    \texttt{else}:
  \cr
  &\enspace\enspace
    \alpha\_{ki} \xleftarrow{\\$} \mathbb{F}_q
  \cr
  &\enspace\enspace
    \texttt{return } \alpha\_{ki}
  \cr
\cr
&\underline{
  \text{Cheat}^{ki}(\Delta)
}\cr
  &\enspace
    \Delta\_{ki} \gets \Delta
  \cr
  &\enspace
    \texttt{if } \forall k, i.\ \text{Delta}\_{ki} \neq \bot
  \cr
  &\enspace\enspace
    \text{Cheat}\left(\sum\_{(k, i)} \Delta\_{ki}\right)
  \cr
\end{aligned}
}
}
\otimes
1
\begin{pmatrix}
\text{Start}^{kl}_k,\cr
\text{End}^{kl}_k,\cr
\text{Cheat}^{kl}\cr
\end{pmatrix}
\cr
\circ\cr
F[\text{Multiply}]
\end{matrix}
$$

In essence, this is just a more complicated variant of the simulator from the previous proof.
All that matters is that the sum of all shares is correct,
so we can just keep using junk up until the very last share,
at which point we need to ensure that the result is correct.
We also need to aggregate the cheating values used by all of the malicious
parties,
in order to get just a single bias in the end.

$\blacksquare$

# Triples

**Definition (Triple Generation):**

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
    F_i, E_i \gets f_i \cdot G, e_i \cdot G
  \cr
  &\enspace
    \text{SetCommit}_i((F_i, E_i))
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
    \pi^0_i \gets \text{Prove}^\varphi(F_i(0); f_i(0))
  \cr
  &\enspace
    \pi^1_i \gets \text{Prove}^\varphi(E_i(0); e_i(0))
  \cr
  &\enspace
    \Rsh_i(\star, (\pi^0_i, \pi^1_i), 0)
  \cr
  &\enspace
    \Rsh_i(\star, [(f_i(j), e_i(j)) \mid j \in [n]], 1)
  \cr
  &\enspace\cr
  &\enspace
    (F\_\bullet, E\_\bullet) \gets \text{WaitOpen}_i()
  \cr
  &\enspace
    (\pi^0\_{\bullet i}, \pi^1\_{\bullet i}) \gets \Lsh_i(\star, 1)
  \cr
  &\enspace
    (a\_{\bullet i}, b\_{\bullet i}) \gets \Lsh_i(\star, 1)
  \cr
  &\enspace
    a_i \gets \sum_j a\_{ji}, \enspace F \gets \sum_j F_j(0)
  \cr
  &\enspace
    b_i \gets \sum_j a\_{ji}, \enspace E \gets \sum_j E_j(0)
  \cr
  &\enspace
    \text{bad}^0 \gets
    \exists j. \neg \text{Verify}^\varphi(\pi^0_j, F_j(0))
  \cr
  &\enspace
    \text{bad}^1 \gets
    \exists j. \neg \text{Verify}^\varphi(\pi^1_j, E_j(0))
  \cr
  &\enspace
    \texttt{if } a_i \cdot G \neq E(i) \lor b_i \cdot G \neq F(i) \lor \text{bad}^0 \lor \text{bad}^1
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
    \pi^2_i \gets \text{Prove}^\psi(E_i(0), F(0), C_i; e_i(0))
  \cr
  &\enspace
    \Rsh_i(\star, (C_i, \pi_i), 1)
  \cr
  &\enspace\cr
  &\enspace
    (C\_\bullet, \pi^2\_\bullet) \Lsh_i(\star, 1)
  \cr
  &\enspace
    \texttt{if } \exists j.\ \neg \text{Verify}^\psi(\pi^2_j, (E_j(0), F(0), C_j))
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
F[\text{ZK}(\varphi)]\cr
\otimes\cr
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
\end{matrix}
$$

$\square$

This protocol is very long, but relatively straightforward.
The idea is that you first generate $a$ and $b$, and commit
to their sharings as polynomials.
Then you prove that you know your secret share,
and convert those to threshold shares.
Concurrently,
you also run multiplication to get a secret sharing of $a * b$,
and run a little protocol to learn $a * b \cdot G$,
using ZK proofs to make sure that this check value $C$ has been
learned correctly.
Finally, you can use this to verify your share of the multiplication,
detecting cheating there, including inconsistent shares.

**Definition (Ideal Triple Generation):**
$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}[\text{IdealTriple}]$
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
    \text{Set}_i(\star)
  \cr
  &\enspace
    \text{WaitSet}_i(\star, \texttt{true})
  \cr
  &\enspace
    \text{Share}^0_i(\star)
  \cr
  &\enspace
    (a_i, b_i) \gets \text{WaitShares}^0_i(\texttt{true})
  \cr
  &\enspace
    \text{Share}^1_i(\star)
  \cr
  &\enspace
    c_i \gets \text{WaitShares}^1_i(\texttt{true})
  \cr
  &\enspace
    (A, B, C) \gets
    (\text{F}^{A,h}()(0), \text{F}^{B,h}()(0), \text{F}^{C,h}()(0))
  \cr
  &\enspace
    \texttt{return } (a_i, b_i, c_i, A, B, C)
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
&f^{A,h}, f^{B,h}, \xleftarrow{\\$} \mathbb{F}_q[X]\_{\leq t - 1}\cr
&f^{A,h}, f^{B,h}, \xleftarrow{\\$} \\{f \in \mathbb{F}_q[X]\_{\leq t - 1} \mid f(0) = f^{A, h}(0) \cdot f^{B, h}(0)\\}\cr
&F^{A,m}, F^{B,m}, F^{C,m}, \text{ready}\_{ij}, \text{shared}^{\\{0, 1\\}}\_{ij}, a\_{i}, b\_{i}, c\_{i}, x^A_i, x^B_i, x^C_i \gets \bot\cr
\cr
&\underline{
  \text{Set}_i(S):
}\cr
  &\enspace
    \text{ready}\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
\textcolor{ef4444}{
  (1)\text{Cheat}(F^A, F^B, F^C):
}
}\cr
  &\enspace
    \texttt{assert } F(0) = 0 \land \text{deg}(F) = t - 1
  \cr
  &\enspace
    F^{A, m}, F^{B, m}, F^{C, m} \gets F^A, F^B, F^C
  \cr
\cr
&\underline{
  \text{WaitSet}_i(S, m):
}\cr
  &\enspace
    \texttt{wait}\_{(i, 0)} \forall j \in S.\ \text{ready}\_{ji} \land (m \to F^{\bullet, m} \neq \bot)
  \cr
\cr
&\underline{
  \text{Share}^0_i(S):
}\cr
  &\enspace
    \text{shared}^0\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
\textcolor{ef4444}{
  \text{CheatShare}^0(S, \hat{x}^A\_\bullet, \hat{x}^B\_\bullet):
}
}\cr
  &\enspace
    \texttt{for } Y \in \\{A, B\\}:
  \cr
  &\enspace\enspace
    \texttt{assert } F^{Y,m} \neq \bot \land \forall j \in S.\ \hat{x}^Y_j \cdot G = F^{Y, m}(j)
  \cr
  &\enspace\enspace
    \texttt{for } j.\ x^Y_j \neq \bot: x^Y_j \gets \hat{x}^Y_j
  \cr
\cr
&\underline{
  \text{WaitShares}^0_i(h):
}\cr
  &\enspace
    \texttt{if } h:
  \cr
  &\enspace\enspace
    \texttt{wait}\_{(i, 1)} x^A_i, x^B_i \neq \bot \land \forall j. \land \forall j. \text{shared}^0\_{ji}
  \cr
  &\enspace\enspace
    \texttt{return } (f^{A,h}(i) + x^A_i, f^{B,h}(i) + x^B_i)
  \cr
  &\enspace
    \texttt{else}:
  \cr
  &\enspace\enspace
    \texttt{wait}\_{(i, 1)} \forall j \in \mathcal{H}. \text{shared}^0\_{ji} \neq \bot
  \cr
  &\enspace\enspace
    \texttt{return } (f^{A,h}(i), f^{B,h}(i))
  \cr
\cr
&\underline{
  \text{Share}^1_i(S):
}\cr
  &\enspace
    \text{shared}^1\_{ij} \gets \texttt{true}\ (\forall j \in S)
  \cr
\cr
&\underline{
\textcolor{ef4444}{
  \text{CheatShare}^1(S, \hat{x}^C\_\bullet):
}
}\cr
  &\enspace
    \texttt{assert } F^{C,m} \neq \bot \land \forall j \in S.\ \hat{x}^C_j \cdot G = F^{C, m}(j)
  \cr
  &\enspace
    \texttt{for } j.\ x^C_j = \bot: x^C_j \gets \hat{x}^C_j
  \cr
\cr
&\underline{
  \text{WaitShares}^1_i():
}\cr
  &\enspace\enspace
    \texttt{wait}\_{(i, 1)} x^C_i, \neq \bot \land \forall j. \land \forall j. \text{shared}^1\_{ji}
  \cr
  &\enspace\enspace
    \texttt{return } f^{C, h}(i) + x^C_i
  \cr
\cr
&\underline{
  \text{F}^{Y \in \\{A, B, C\\}, h}():
}\cr
  &\enspace
    \texttt{wait } F^{Y, m} \neq \bot \land \forall i.\ \exists j.\ \text{ready}\_{ij}
  \cr
  &\enspace
    \texttt{return } f^{Y, h} \cdot G
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

The ideal triple generation protocol is as you might expect,
with the functionality itself doing the multiplication perfectly.
However, we once again have the various tweaks to secret sharing
induced by the commit reveal paradigm.
See the key sharing document for some discussion about
these artifacts, which naturally show up here again.

**Lemma**

For a negligible $\epsilon$, and up to $t - 1$ corrupt parties:

$$
\mathscr{P}[\text{Triple}] \overset{\epsilon}{\leadsto} \mathscr{P}[\text{IdealTriple}]
$$

**Proof**

Clearly, $\mathscr{P}[\text{Triple}]$ is simulated by the following
protocol making use of $\mathscr{P}[\text{SplitShare}]$ twice:

$$
\boxed{
\begin{matrix}
\colorbox{FBCFE8}{\large
  $\mathscr{P}^0$
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
    z^A_i, z^B_i \xleftarrow{\\$} \mathbb{F}_q
  \cr
  &\enspace
    \text{Set}^0_i(z^A_i), \text{Set}^1_i(z^B_i)
  \cr
  &\enspace
    \text{SetMask}_i()
  \cr
  \cr
  &\enspace
    \text{WaitSet}^0_i(), \text{WaitSet}^1_i()
  \cr
  &\enspace
    \text{WaitMask}_i()
  \cr
  &\enspace
    \text{Share}^0_i(), \text{Share}^1_i
  \cr
  \cr
  &\enspace
    a_i \gets \text{WaitShare}^0_i(), b_i \gets \text{WaitShare}^1_i()
  \cr
  &\enspace
    \text{StartMultiply}_i(z^A_i, z^B_i)
  \cr
  &\enspace
    \text{A} \gets \sum_i \text{Z}^{0}(i),
    \text{B} \gets \sum_i \text{Z}^{1}(i)
  \cr
  &\enspace
    C_i \gets z^B_i \cdot A
  \cr
  &\enspace
    \pi^2_i \gets \text{Prove}^\psi(\text{Z}^{1}(i), A, C_i; z^B_i)
  \cr
  &\enspace
    \Rsh_i(\star, (C_i, \pi_i), 1)
  \cr
  &\enspace\cr
  &\enspace
    (C\_\bullet, \pi^2\_\bullet) \Lsh_i(\star, 1)
  \cr
  &\enspace
    \texttt{if } \exists j.\ \neg \text{Verify}^\psi(\pi^2_j, (\text{Z}^{1}(j), A, C_j))
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 1)
  \cr
  &\enspace
    z^C_i \gets \text{EndMultiply}_i()
  \cr
  &\enspace
    \text{Share}_i(z^C_i)
  \cr
  &\enspace
    c_i \gets \text{WaitShare}_i()
  \cr
  &\enspace
    C \gets \sum_i C_i
  \cr
  &\enspace
    \texttt{if } \sum_i \text{Z}^{2}(i) \neq C
  \cr
  &\enspace\enspace
    \texttt{stop}(\star, 2)
  \cr
  &\enspace
    \texttt{return } (a_i, b_i, c_i, A, B, C)
  \cr
\end{aligned}
}
}
\end{matrix}
}
\lhd
\begin{matrix}
\mathscr{P}[\text{SplitShare}]\cr
\otimes\cr
\mathscr{P}[\text{SplitShare}]\cr
\otimes\cr
\mathscr{P}[\text{Convert}]\cr
\otimes\cr
\mathscr{P}[\text{Multiply}]\cr
\end{matrix}
$$

From here, we can jump to the final protocol with an atrocious simulator:

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
&z^A_i, z^B_i \xleftarrow{\\$} \mathbb{F}_q\ (\forall i \in \mathcal{H} / \\{1\\})\cr
&Z^A_i, Z^B_i \gets z^A_i \cdot G, z^B_i \cdot G\ (\forall i \in \mathcal{H} / \\{1\\})\cr
&z^A_i, z^B_i, Z^A_i, Z^B_i \gets \bot\ (\forall i \in \mathcal{M})\cr
&\gamma_i \xleftarrow{\\$} \mathbb{F}_q\ (\forall i \neq 1)\cr
&\hat{z}^C\_{ij}, x^A_i, x^B_i, x^C_i, F^{A, m}, F^{B, m}, F^{C, m}, \hat{m}\_{ij} \gets \bot\cr
&\text{ready}^A\_{ij}, \text{ready}^B\_{ij}, \text{ready}^C\_{ij} \gets \bot\cr
&\text{shared}^A\_{ij}, \text{shared}^B\_{ij}, \text{shared}^C\_{ij} \gets \bot\cr
\cr
&\underline{
  \text{Set}_k^0(S, z, Z):
}\cr
  &\enspace
    \texttt{assert } z \neq \bot \lor Z \neq \bot
  \cr
  &\enspace
    \texttt{if } z^A_k, Z^A_k = \bot \land z \neq \bot: z^A_k \gets z, Z^A_k \gets z \cdot G
  \cr
  &\enspace
    \texttt{elif } Z^A_k = \bot \land Z \neq \bot : Z^A_k \gets Z
  \cr
  &\enspace
    \text{ready}^A\_{kj} \gets \texttt{true }\ (\forall j \in S)
  \cr
  &\enspace
    \text{Set?}_k()
  \cr
\cr
&\underline{
  \text{Set}_k^1(S, z, Z):
}\cr
  &\enspace
    \texttt{assert } z \neq \bot \lor Z \neq \bot
  \cr
  &\enspace
    \texttt{if } z^B_k, Z^B_k = \bot \land z \neq \bot: z^B_k \gets z, Z^B_k \gets z \cdot G
  \cr
  &\enspace
    \texttt{elif } Z^B_k = \bot \land Z \neq \bot : Z^B_k \gets Z
  \cr
  &\enspace
    \text{ready}^A\_{kj} \gets \texttt{true }\ (\forall j \in S)
  \cr
  &\enspace
    \text{ready}^B\_{kj} \gets \texttt{true }\ (\forall j \in S)
  \cr
  &\enspace
    \text{Set?}_k()
  \cr
\cr
&\underline{
  \text{SetMask}_k(S):
}\cr
  &\enspace
    \text{ready}^C\_{kj} \gets \texttt{true }\ (\forall j \in S)
  \cr
  &\enspace
    \text{Set?}_k()
  \cr
\cr
&\underline{
  \text{Set?}_k():
}\cr
  &\enspace
    \texttt{for } j.\ \forall Y \in \\{A, B, C\\}. \text{ready}^Y\_{kj}:
  \cr
  &\enspace\enspace
    \texttt{super}.\text{Set}_k(\\{j\\})
  \cr
\cr
&\underline{
  \text{WaitSet}_k^0(S, m):
}\cr
  &\enspace
    \texttt{wait } \forall j \in S \cap \mathcal{M}.\ \text{ready}^A\_{jk}
  \cr
  &\enspace
    \texttt{super}.\text{WaitSet}_k(S \cap \mathcal{H})
  \cr
\cr
&\underline{
  \text{WaitSet}_k^1(S, m):
}\cr
  &\enspace
    \texttt{wait } \forall j \in S \cap \mathcal{M}.\ \text{ready}^B\_{jk}
  \cr
  &\enspace
    \texttt{super}.\text{WaitSet}_k(S \cap \mathcal{H})
  \cr
\cr
&\underline{
  \text{WaitMask}_k(S, m):
}\cr
  &\enspace
    \texttt{wait } \forall j \in S \cap \mathcal{M}.\ \text{ready}^C\_{jk}
  \cr
  &\enspace
    \texttt{super}.\text{WaitSet}_k(S \cap \mathcal{H})
  \cr
\cr
&\underline{
  \text{Share}^0_k(S, z):
}\cr
  &\enspace
    \texttt{assert } Z^A_k \neq \bot
  \cr
  &\enspace
    \texttt{if } z^A_k = \bot:
  \cr
  &\enspace\enspace
    \texttt{assert } z \cdot G = Z^A_k
  \cr
  &\enspace\enspace
    z^A_k \gets z
  \cr
  &\enspace
    \text{shared}^A\_{kj} \gets \texttt{true}\ (\forall j \in S)
  \cr
  &\enspace
    \text{Share?}^0_k()
  \cr
\cr
&\underline{
  \text{Share}^1_k(S, z):
}\cr
  &\enspace
    \texttt{assert } Z^B_k \neq \bot
  \cr
  &\enspace
    \texttt{if } z^B_k = \bot:
  \cr
  &\enspace\enspace
    \texttt{assert } z \cdot G = Z^B_k
  \cr
  &\enspace\enspace
    z^B_k \gets z
  \cr
  &\enspace
    \text{shared}^B\_{kj} \gets \texttt{true}\ (\forall j \in S)
  \cr
  &\enspace
    \text{Share?}^0_k()
  \cr
\cr
&\underline{
  \text{WaitShare}^0_k(h):
}\cr
  &\enspace
    (a_k, b_k) \gets \text{WaitShares}^0_k()
  \cr
  &\enspace
    \texttt{return } a_k
  \cr
\cr
&\underline{
  \text{WaitShare}^1_k(h):
}\cr
  &\enspace
    (a_k, b_k) \gets \text{WaitShares}^0_k()
  \cr
  &\enspace
    \texttt{return } b_k
  \cr
\cr
&\underline{
  (1)\text{StartMultiply}_k(a\_\bullet, b\_\bullet):
}\cr
  &\enspace
    a\_{kj} \gets a_j, b\_{kj} \gets b_j
  \cr
\cr
&\underline{
  (1)\text{Cheat}^{\tiny \text{F[Multiply]}}_k(\Delta):
}\cr
  &\enspace
    \Delta \gets \Delta
  \cr
\cr
&\underline{
  \text{EndMultiply}_k():
}\cr
  &\enspace
    \texttt{return } \gamma_k - z^A_k \cdot z^B_k
  \cr
\cr
&\underline{
  \text{Share}_k(S, z\_\bullet):
}\cr
  &\enspace
    \texttt{for } j \in S.\ \hat{z}^C\_{kj} = \bot:
  \cr
  &\enspace
    \texttt{for } j \in S.\ \hat{z}^C\_{kj} = \bot: \hat{z}^C\_{kj} \gets z_j
  \cr
  &\enspace
    \texttt{for } j \in \mathcal{H}.\ \forall i \in \mathcal{M}.\ \hat{z}^C\_{ij} \neq \bot:
  \cr
  &\enspace\enspace
    \texttt{if } \sum\_{i \in \mathcal{H}} \text{Z}(j) + \sum\_{i \in \mathcal{M}} \hat{z}^C\_{ij} \cdot G \neq F^{C, h}(0):
  \cr
  &\enspace\enspace\enspace
    \texttt{stop}(\\{j\\})
  \cr
  &\enspace
    \texttt{super}.\text{Share}^1_i(S)
  \cr
\cr
&\underline{
  \text{Share?}^0_k():
}\cr
  &\enspace
    \texttt{for } j.\ \text{shared}^A\_{kj} \land \text{shared}^B\_{kj}:
  \cr
  &\enspace\enspace
    \texttt{super}.\text{Share}^0_k(\\{j\\})
  \cr
\cr
&\underline{
  \text{WaitShare}_k(h):
}\cr
  &\enspace
    \texttt{if } h:
  \cr
  &\enspace\enspace
    \texttt{return } \text{WaitShare}^1_k()
  \cr
  &\enspace
    \texttt{else}:
  \cr
  &\enspace\enspace
    \texttt{wait } x^C_k \neq \bot \land \forall i \in \mathcal{M}.\ z^C\_{ik} \neq \bot
  \cr
  &\enspace\enspace
    \texttt{return } \text{WaitShare}^1_k() - \sum\_{i \in \mathcal{M}} z^C\_{ik} - x^C_k
  \cr
\cr
&\underline{
  \text{Prove}^\psi(W, P, Q; w):
}\cr
  &\enspace
    \texttt{assert } w \cdot G = W \land w \cdot P = Q
  \cr
  &\enspace
    \pi \xleftarrow{\\$} \texttt{01}^{2 \lambda}
  \cr
  &\enspace
    \mu[\pi] \gets (W, P, Q)
  \cr
\cr
&\underline{
  \text{Verify}^\psi(\pi, (W, P, Q)):
}\cr
  &\enspace
    \texttt{return } \mu[\pi] \overset{?}{=} (W, P, Q)
  \cr
\cr
&\underline{
  \Rsh_k(S, m\_\bullet, 1)
}\cr
  &\enspace
    \texttt{for } j \in S \cap \mathcal{H}:
  \cr
  &\enspace\enspace
    (C_k, \pi) \gets m_j
  \cr
  &\enspace\enspace
    \texttt{if } \mu[\pi] \neq (Z^B_k(), \text{F}^{A, h}(0), C_k):
  \cr
  &\enspace\enspace\enspace
    \texttt{stop}(\\{j\\})
  \cr
  &\enspace
    \hat{m}\_{kj} \gets m_j\ (\forall j \in S)
  \cr
\cr
&\underline{
  \Lsh_k(S, 1):
}\cr
  &\enspace
    \texttt{wait}\_{(k, 1)} \forall j \in S \cap \mathcal{M}.\ \hat{m}\_{kj} \neq \bot
  \cr
  &\enspace
    r\_\bullet \gets \bot
  \cr
  &\enspace
    \texttt{for } j \in S \cap \mathcal{H}. \nexists i.\ \hat{m}\_{ij} \neq \bot:
  \cr
  &\enspace\enspace
    \pi \xleftarrow{\\$} \texttt{01}^{2 \lambda}
  \cr
  &\enspace
    \mu[\pi] \gets (\text{Z}^1(j), F^{A, h}(0), \text{C}(j))
  \cr
  &\enspace
    r_j \gets (C_j, \pi)
  \cr
  &\enspace
    r_j \gets \hat{m}\_{kj}\ (\forall j \in S)
  \cr
  &\enspace
    \texttt{return } r\_\bullet
  \cr
\cr
&\underline{
  (1)\text{Cheat}^0(F):
}\cr
  &\enspace
    \texttt{assert } F(0) = 0 \land \text{deg}(F) = t - 1
  \cr
  &\enspace
    F^{A, m} \gets F
  \cr
  &\enspace
    \text{Cheat?}()
  \cr
\cr
&\underline{
  \text{CheatShare}^0(S, \hat{x}\_\bullet):
}\cr
  &\enspace
    \texttt{assert } F^{A, m} \neq \bot \land \forall j \in S.\ \hat{x}_j \cdot G = F^{A, m}(j)
  \cr
  &\enspace
    x^A_j \gets \hat{x}_j\ (\forall j \in S)
  \cr
  &\enspace
    \text{CheatShare?}()
  \cr
\cr
&\underline{
  \text{F}^{0, h}():
}\cr
  &\enspace
    \texttt{return } \text{F}^{A, h} - \text{F}^{A, h}(0)
  \cr
\cr
&\underline{
  (1)\text{Cheat}^1(F):
}\cr
  &\enspace
    \texttt{assert } F(0) = 0 \land \text{deg}(F) = t - 1
  \cr
  &\enspace
    F^{B, m} \gets F
  \cr
  &\enspace
    \text{Cheat?}()
  \cr
\cr
&\underline{
  \text{CheatShare}^1(S, \hat{x}\_\bullet):
}\cr
  &\enspace
    \texttt{assert } F^{B, m} \neq \bot \land \forall j \in S.\ \hat{x}_j \cdot G = F^{B, m}(j)
  \cr
  &\enspace
    x^B_j \gets \hat{x}_j\ (\forall j \in S)
  \cr
  &\enspace
    \text{CheatShare?}()
  \cr
\cr
&\underline{
  \text{F}^{1, h}():
}\cr
  &\enspace
    \texttt{return } \text{F}^{B, h} - \text{F}^{B, h}(0)
  \cr
\cr
&\underline{
  (1)\text{Cheat}^{\tiny \text{F[Convert]}}(F):
}\cr
  &\enspace
    \texttt{assert } F(0) = 0 \land \text{deg}(F) = t - 1
  \cr
  &\enspace
    F^{C, m} \gets F
  \cr
  &\enspace
    \text{Cheat?}()
  \cr
\cr
&\underline{
  \text{Cheat?}()
}\cr
  &\enspace
    \texttt{if } F^{A, m}, F^{B, m}, F^{C, m} \neq \bot:
  \cr
  &\enspace\enspace
    \texttt{super}.\text{Cheat}(F^{A, m}, F^{B, m}, F^{C, m})
  \cr
\cr
&\underline{
  \text{CheatShare?}():
}\cr
  &\enspace
    \texttt{for } j. \hat{x}^A_j, \hat{x}^B_j \neq \bot:
  \cr
  &\enspace\enspace
    \texttt{super}.\text{CheatShare}^0(\\{j\\}, \hat{x}^A\_\bullet, \hat{x}^B\_\bullet)
  \cr
\cr
&\underline{
  \text{CheatShare}(S, \hat{x}\_\bullet):
}\cr
  &\enspace
    \texttt{assert } F^{C, m} \neq \bot \land \forall j \in S.\ \hat{x}_j \cdot G = F^{C, m}(j)
  \cr
  &\enspace
    \hat{x}^C_j \gets \hat{x}_j\ (\forall j \in S)
  \cr
  &\enspace
    \texttt{super}.\text{CheatShare}^1(S, \hat{x}\_\bullet)
  \cr
\cr
&\underline{
  \text{F}^h():
}\cr
  &\enspace
    \texttt{return } \text{F}^{C, h} - \text{F}^{C, h}(0)
  \cr
\cr
&\underline{
  \text{Z}^0(i):
}\cr
  &\enspace
    \texttt{return } \text{F}^{A, h}(0) - \sum_j Z^A_j \texttt{ if } i = 1 \texttt{ else } Z^A_i
  \cr
\cr
&\underline{
  \text{Z}^1(i):
}\cr
  &\enspace
    \texttt{return } \text{F}^{B, h}(0) - \sum_j Z^B_j \texttt{ if } i = 1 \texttt{ else } Z^B_i
  \cr
\cr
&\underline{
  \text{C}(i):
}\cr
  &\enspace
    \texttt{return } \text{F}^{C, h}(0) - \sum_j z^A_j \cdot \text{F}^{B, h}(0) \texttt{ if } i = 1 \texttt{ else } z^A_i \cdot \text{F}^{B, h}(0)
  \cr
\cr
&\underline{
  \text{Z}(i):
}\cr
  &\enspace
    \texttt{return } \hat{C}() - \sum_i \gamma_i \cdot G \texttt{ if } i = 1 \texttt{ else } \gamma_i \cdot \text{F}^{B, h}(0)
  \cr
\cr
&\underline{
  \hat{C}():
}\cr
  &\enspace
    \texttt{wait } \text{for all variables used below}
  \cr
  &\enspace
    O\_{11} = \text{F}^{C, h}(0) - \sum\_{i \in \mathcal{H} / \\{1\\}} z^B_i \cdot \text{F}^{A, h}(0) - \sum\_{i \in \mathcal{H} / \\{1\\}} z^A_i \cdot \text{F}^{B, h}(0) - O\_{hh}
  \cr
  &\enspace
    O\_{h1} = \sum\_{i \in \mathcal{H},j \in \mathcal{H} / \\{1\\}} z^B_j \cdot Z^A_i \cdot G
  \cr
  &\enspace
    O\_{h1} = \sum\_{i \in \mathcal{H} / \\{1\\},j \in \mathcal{H}} z^A_i \cdot Z^B_j \cdot G
  \cr
  &\enspace
    O\_{hh} = \sum\_{i,j \in \mathcal{H} / \\{1\\}} z^A_i \cdot z^B_j \cdot G
  \cr
  &\enspace
    O\_{hm} = \sum\_{i \in \mathcal{H}, j \in \mathcal{M}} \text{Z}^0(i) \cdot b\_{ji}
  \cr
  &\enspace
    O\_{mh} = \sum\_{i \in \mathcal{M}, j \in \mathcal{H}} a\_{ij} \cdot \text{Z}^1(j)
  \cr
  &\enspace
    O\_{mm} = \sum\_{i, j \in \mathcal{M}} a\_{ij} \cdot b\_{ji} \cdot G
  \cr
  &\enspace
    \texttt{return } O\_{11} + O\_{1h} + O\_{h1} + O\_{hh} + O\_{hm} + O\_{mh} + O\_{mm} + \Delta \cdot G
  \cr
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

The vast majority of this simulator is just mindless bookkeeping.
The main part of interest is the $\hat{C}$ function,
which basically simulates the expected
result of the multiplication, based on the bad values used by the adversary.
In essence, this reflects the fact that the adversary learns
nothing about the other parties shares
through the combination of the faulty multiplication protocol
and share check phase,
because they can calculate the results "in the exponent"
already.

That's the only real tricky part of the simulator,
the rest is just adding in necessary checks ourselves,
and coalescing contributions from adversaries, as we've done many times.

$\blacksquare$