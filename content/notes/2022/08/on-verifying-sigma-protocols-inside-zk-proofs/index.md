---
title: "On Verifying Sigma Protocols inside ZK Proofs"
date: 2022-08-14T20:14:21+02:00
type: note
note-tags:
  - "Cryptography"
  - "ZK Proofs"
katex: true
---

Let's say you have a public coin protocol $\Sigma$, and you've Fiat-Shamirized it
to create a non-interactive proof system.

You might want to be able to verify these proofs inside of another ZK proof system.
For example, these protocol proofs might be quick to generate, but large,
so you could use a SNARK proof to compress them.

A priori, this would require running the Fiat-Shamir process *inside*
of the circuit used for the second layer proof, which would require a hash
function invocation in circuit, which may not be ideal.

You can actually avoid this invocation by using a commitment scheme.

The idea is that before the execution of the second layer ZK proof systems,
you execute $N$ rounds, for each of the public messages in $\Sigma$.
In each round, you send the commitment $C_i = \text{Com}(M_i)$ for the message
the prover sends in round $i$ of $\Sigma$.
In each round, you receive a challenge $e_i$ generated in the same way
as in $\Sigma$.

Then, you you use your second layer ZK proof for the relation:

$$
\begin{aligned}
\\{&(x, e_0, \ldots, e_N | w, M_0, \ldots, M_N, r_0, \ldots, r_N) :\cr
 &\Sigma.\text{Verify}(x, M_0, \ldots, M_N, e_0, \ldots, e_N) \land\cr
&\forall i.\ \text{Decom}(C_i, M_i, r_i)\\}
\end{aligned}
$$

In essence, you prove that the public coin protocol validates the messages and
challenges, and that the messages correspond with the commitments you
sent initially.

# Cost

This increase communication complexity by $N$ times the size of a commitment,
which really isn't that much.

# Why?

If "ZK Proofs with Commitment" are particularly efficient, this might be
faster than doing the Fiat-Shamir hashing *inside* of the second layer proof.
I think it might be interesting to explore this notion in more depth,
because using commitments also allows combining together proofs for different statements,
tying them to the same input by using commitments.

If the commitment amounts to just hashing the inputs, this might still be advantageous if more complex sampling is needed, for example if you need
to sample trits or a sampling without repetition from a subset, etc.
