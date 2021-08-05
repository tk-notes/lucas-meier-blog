---
title: "Avalanche Signatures"
date: 2021-08-05T07:44:59+00:00
type: note
katex: true
note-tags:
  - "Elliptic Curve"
  - "Cryptography"
---

Parties $P_1, P_2$, holding shares $x_1, x_2$ of a private key $x_1 + x_2$.
Both parties know $X_1 = x_1 \cdot G$ and $X_2 = x_2 \cdot G$, as well
as the public key $X = X_1 + X_2$.

---

## $P_1$

$k_1 \xleftarrow{R} \mathbb{Z}/(q)$

$\displaystyle K_1 = \frac{1}{k_1} \cdot G$

$K_1 \longrightarrow$

$\textcolor{blue}{\Pi^{\text{SHR}}(K_1;k_1)} \longrightarrow$

---

## $P_2$

Check $\textcolor{blue}{\Pi^{\text{SHR}}(K_1)}$.

$k_2 \xleftarrow{R} \mathbb{Z}/(q)$

$\displaystyle K_2 = \frac{1}{k_2} \cdot G$

$\displaystyle R = \frac{1}{k_2} \cdot K_1$

$r = x(R)$

$\alpha_{\bullet22} = k_2(m + rx_2)$

$K_2 \longrightarrow$

$\textcolor{blue}{\Pi^{\text{SHR}}(K_2;k_2)} \longrightarrow$

$\alpha_{\bullet22} \longrightarrow$

---

## $P_1$

Check $\textcolor{blue}{\Pi^{\text{SHR}}(K_2)}$.

$\displaystyle R = \frac{1}{k_1} \cdot K_2$

$r = x(R)$

$\textcolor{blue}{\alpha_{\bullet22} \cdot K_2 \stackrel{?}{=} m \cdot G + r \cdot X_2}$

$\alpha_{1 \bullet 1} = k_1 (m + r x_1)$

$\alpha_{1 2 2} = k_1 \alpha_{\bullet 22}$

$\alpha_{1 \bullet 1}, \alpha_{1 2 2} \longrightarrow$

---

## $P_2$

$\textcolor{blue}{\alpha_{1 \bullet 1} \cdot K_1 \stackrel{?}{=} m \cdot G + r \cdot X_1}$

$\textcolor{blue}{\alpha_{1 2 2} \cdot R \stackrel{?}{=} m \cdot G + r \cdot X_2}$

$\alpha_{1 2 1} = k_2 \alpha_{1 \bullet 1}$

$\alpha_{1 2 1} \longrightarrow$

---

## $P_1$

$\textcolor{blue}{\alpha_{1 2 1} \cdot R \stackrel{?}{=} m \cdot G + r \cdot X_1}$

---

Then $\alpha_{121} + \alpha_{122} = k_1k_2(2m + r(x_1 + x_2))$. If you set
$m = 2^{-1} H(M)$, then this works out. This requires $2$ to have an inverse modulo
the order of the subgroup, which is always the case.
