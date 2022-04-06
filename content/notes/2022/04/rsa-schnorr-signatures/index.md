---
title: "RSA Schnorr Signatures"
date: 2022-04-05T22:57:48+02:00
type: note
note-tags:
  - "Cryptography"
  - "Math"
katex: true
---

{{<note>}}
I wasn't aware when quickly writing this up, but it turns out that this is
an application of the [Guillou-Quisquater protocol](https://link.springer.com/chapter/10.1007/3-540-45961-8_11).
{{</note>}}

[Maurer's 2009 paper](https://crypto.ethz.ch/publications/files/Maurer09.pdf)
provides a generalization of schnorr signatures to a large class of situations.
Essentially, any time you have a group homomorphism $\varphi : G \to H$,
you can provide a zero-knowledge protocol to prove:

$$
\Pi(X ; x) := \varphi(x) = X
$$

(with $x$ kept secret).

In the case of a cyclic group of prime order $q$, with generator $G$,
and the following homomorphism:

$$
\begin{aligned}
&\varphi : \mathbb{F}_q \to \mathbb{G}\cr
&\varphi(x) := x \cdot G
\end{aligned}
$$

We have the usual Schnorr sigma protocol, which leads to Schnorr signatures
after a Fiat-Shamir transform.

# RSA

We can use the following group homomorphism, inspired by RSA:

$$
\begin{aligned}
&\varphi : \mathbb{Z}/(N)^* \to \mathbb{Z}/(N)^*\cr
&\varphi(m) := m^e
\end{aligned}
$$

Here, $(N, e)$ is an RSA public key.

Now, this is evidently a group homomorphism, since:

$$
(a \cdot b)^e = a^e \cdot b^e
$$

Furthermore, this homomorphism is one way, provided we don't know
the factorization of $N$, and we think RSA is hard.


# The Signature Protocol

For the memes, let's explicitly describe the signature scheme.

**Key-Generation**

Generate random primes $p, q$ of half the desired modulus size.
Let $N = p \cdot q$, and pick $e$ such that $\text{gcd}(e, (p - 1)(q - 1)) = 1$.

Pick a random $x\xleftarrow{R} \mathbb{Z}/(N)^*$,
and then set $X \gets s^e \mod N$.

$x$ is the private key.

$(N, e, X)$ is the public key.

**Signing**
$$
\begin{aligned}
k &\xleftarrow{R} \mathbb{Z}/(N)^*\cr
K &\gets k^e \mod N\cr
c &\gets H(N, e, X, K, m)\cr
r &\gets k \cdot x^c \mod N\cr
(K&, r)
\end{aligned}
$$


{{<note>}}
What type should $c$ have? Well, one approach is to take it such
that $c$ is coprime with $e$, so that we'll have extractability.
Basically, given $x^c$ and $x^e$, you can learn $x$. We also need
$e$ to be large enough to have our security parameter, i.e.
$e > 2^128$ or so.
{{</note>}}

**Verification**
$$
r^e \stackrel{?}{\equiv} K \cdot X^{H(N, e, X, K, m)}\mod N
$$

# Security

Trust me :)
