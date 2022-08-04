---
title: "On Doerner et al's modifications to Extended Random OT"
date: 2021-08-28T13:35:16+02:00
type: note
katex: true
note-tags:
  - "Cryptography"
  - "Math"
---

In Doerner et al's threshold ECDSA protocol {{<ref-link "1">}}, they
make use of an (extended) random oblivious transfer protocol
by Keller et al {{<ref-link "2">}}. This protocol adds a verification
check to achieve malicious security. Doerner et al. silently modified
this check in their paper, without a comment on the security
of their changes. In fact, their modifications degrade the verification
properties of this check. If this random OT were used to make
a normal oblivious transfer, by padding each message with the random
pads from the previous step, then this would lead to the vulnerabilities
outlined in {{<ref-link "3">}}. Fortunately, it seems that the way
Doerner et al. subsequently use the random OT prevents the flawed
verification check from manifesting into a concrete vulnerability.

Nonetheless, it is still a bit unsettling to have important modifications
like these presented with any comment or proof.

# Correlated OT

The starting point is the correlated OT functionality in Keller et al.

We have two parties: a sender S and a receiver R.

S has a secret correlation $\Delta \in \mathbb{F}_2^k$.

R provides as input a matrix $X \in \mathcal{M}_{l \times k}(\mathbb{F}_2)$.

R receives a random matrix $T \in \mathcal{M}_{l \times k}(\mathbb{F}_2)$

S receives a matrix $Q \in \mathcal{M}_{l \times k}(\mathbb{F}_2)$
whose rows satisfy:

$$
Q_j = T_j + X_j * \Delta
$$

(Here $*$ denotes bitwise and / multiplication, $+$ denotes bitwise xor / addition)

We can then use this to create a batched random OT functionality, where we have

$$
\begin{aligned}
&_0V_j := H(j, Q_j)\cr
&_1V_j := H(j, Q_j + \Delta)\cr
\end{aligned}
$$

as two random pads for the sender. Then, if R is honest, $X_j$ can be
written as $(x_j, \ldots, x_j)$, and they learn one of the pads:

$$
_{x_j}V_j = H(J, T_j)
$$

If R is dishonest, and $X_j$ is polychrome (i.e. isn't $(0, \ldots, 0)$
or $(1, \ldots, 1)$), then this can lead to vulnerabilities if this
random OT is then used for a standard OT to transfer one of two unrelated
messages {{<ref-link "3">}}.

To fix this, Keller et al. add a verification check to make sure
that R isn't dishonest.

R and S both agree on random $\chi_i, \ldots, \chi_l \in \mathbb{F}_{2^k}$.

R computes the check values:

$$
x := \sum_{j \in [l]} x_j \cdot \chi_j
$$
$$
t := \sum_{j \in [l]} T_j \cdot \chi_j
$$

and sends them to S.

With $\cdot$ denoting multiplication in the binary field.

S computes the check value:

$$
q := \sum_{j \in [l]} Q_j \cdot \chi_j
$$

And then verifies:

$$
q \stackrel{?}{=} t + x \cdot \Delta
$$

# Doerner's Modification

Instead of using multiplication in the field, Doerner et al. instead
use bitwise and / multiplication $*$.

So they compute:

$$
q := \sum_{j \in [l]} Q_j * \chi_j
$$
$$
q \stackrel{?}{=} t + x * \Delta
$$

The problem is that this check fails to prevent a dishonest R
from cheating by providing a polychrome $X_j$. Indeed, expanding
$q$, we have:

$$
\begin{aligned}
q &= \sum_{j \in [l]} (T_j + X_j * \Delta) * \chi_j\cr
 &=\sum_{j \in [l]} T_j * \chi_j + \sum_{j \in [l]} X_j * \Delta * \chi_j\cr
 &=\sum_{j \in [l]} T_j * \chi_j + \Delta * \sum_{j \in [l]} X_j * \chi_j\cr
\end{aligned}
$$

Crucially, in the last step, we rely on the fact that the interaction
with $\Delta$ and $\chi_j$ now commutes. This allows us to set:

$$
t := \sum_{j \in [l]} T_j * \chi_j
$$
$$
x := \sum_{j \in [l]} X_j * \chi_j
$$

And then have the verification check pass:

$$
q \stackrel{?}{=} t + x * \Delta
$$

This check will pass even if $X_j$ is polychrome. This can't
happen with the original check (except with negligible probability),
because when you have $(X_j * \Delta) \cdot \chi_j$, the operations
don't commute, unless you have $X_j$ monochrome, in which case you can
rewrite the expression as $x_j \cdot \Delta \cdot \chi_j$.

## The Fix

The obvious fix is to use field multiplication as in the original
OT protocol from Keller et al. {{<ref-link "2">}}.

# Vulnerabilities from polychrome $X_j$

If these random pads are used to transfer messages that R knows,
then using polychrome $X_j$ lets R learn the secret $\Delta$.

The usual way to turn a random OT in a transfer of two messages, is
as follows. S has two random values $r_0$, and $r_1$, and R has
one of the random values $r_w$. S has two messages $m_0$, and $m_1$,
and would like to send $m_w$ to $R$, without learning $w$, and without
$R$ learning $m_{\bar{w}}$.

S sends $m_0 \oplus r_0$, and $m_1 \oplus r_1$, and R can remove
the random pad from the message they've chosen.

The problem, in our cases, stems from when R knows what the message
$m_0$ will be. In this case, they learn $r_0$.

Recall that this pad is generated as:

$$
r_0 := H(j, Q_j) = H(j, T_j + X_j * \Delta)
$$

What R does is to set $X_j = (1, 0, \ldots)$, then they know that:

$$
r_0 = H(j, T_j + (\Delta_1, 0, \ldots))
$$

Since they know $T_j$, they can simply try both possibilities
for $\Delta_1$, and compare with $r_0$. They can repeat the same process
for different messages, and different bits of $\Delta$, until they
learn the entire secret.

The fundamental problem here is that by using a polychrome $X_j$, R
can segregate out individual bits of $\Delta$, making guessing
much easier.

# Doerner's additive transfer

Now, in Doerner et al's ECDSA protocol, they don't actually use
the random transfer to do an oblivious transfer. Instead, they use
a slightly modified version, where $R$ has a bit $w$, $S$ has
an offset $\alpha$, and $R$ learns $t + w \cdot \alpha$, for some
random vector $t$, and $S$ learns $t$.

This is accomplished by using the two random pads $r_0 = H(j, Q_j)$ and $r_1 = H(j, Q_j + \Delta)$
in a different way.

S sets $t := r_0$, and then sends the message $m := r_0 + r_1 + \alpha$
to R.

If $w = 0$, then R has $r_0$, and that becomes their result $t + 0 \cdot \alpha$.

If $w = 1$, then R has $r_1$ and their result is $m - r_1 = t + \alpha$.

Because of this change, in practice it becomes difficult for R
to learn bits of $\Delta$ by cheating, since they're not able to
see what the pad $r_0$ is.

In practice, it would seem that the verification check is actually
not even necessary in the context of Doerner et al's threshold ECDSA,
because of this subsequent usage of the random OT.

Nonetheless, it would have been better if the removal of this check were explicitly commented, instead of the check being broken by a silent modification.
Furthermore, my inability to practically exploit the flawed check
doesn't mean that it's secure.

# References

{{<ref
  "1"
  "https://eprint.iacr.org/2018/499.pdf"
  "[1] Secure Two-party Threshold ECDSA from ECDSA Assumptions - Jack Doerner, Yashvanth Kondi, Eysa Lee, Abhi Shelat">}}
{{<ref
  "2"
  "https://link.springer.com/content/pdf/10.1007%2F978-3-662-47989-6_35.pdf"
  "[2] Actively Secure OT Extension with Optimal Overhead - Marcel Keller, Emmanuela Orsini, Peter Scholl">}}
{{<ref
  "3"
  "https://link.springer.com/content/pdf/10.1007%2F978-3-540-45146-4_9.pdf"
  "[3] Extending Oblivious Transfers Efficiently - Yuval Isahi, Joe Kilian, Kobbi Nissim, Erez Petrank">}}
