---
title: "RSA Schnorr Signatures"
date: 2022-04-05T22:57:48+02:00
type: note
note-tags:
  - "Cryptography"
  - "Math"
katex: true
---

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
Let $N = p \cdot q$, and pick $e$ such that $\text{gcd}(e, (p - 1)(q - 1)) = 1$. (For example, by choosing $e = 65537$).

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
What type should $c$ have? Well, ideally it should be taken modulo
$\varphi(N)$, but obviously we can't know that value. Instead, if we
take a uniform integer with $\text{size}(N) + \lambda$ bits, with $\lambda$
our security parameter, we'll have perfectly suitable exponent.
{{</note>}}

**Verification**
$$
r^e \stackrel{?}{\equiv} K \cdot X^{H(N, e, X, K, m)}\mod N
$$

# Security

Trust me :)
