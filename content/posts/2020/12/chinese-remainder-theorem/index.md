---
title: "Chinese Remainder Theorem for Programmers"
date: 2020-12-13
draft: false
katex: true
tags:
  - Math
  - Algebra
  - Programming
---

This is a quick post about the
[Chinese Remainder Theorem](https://www.wikiwand.com/en/Chinese_remainder_theorem).
Specifically, how to use it to solve a system of system of simple modular equations.

<!--more-->

This came up today in an Advent of Code problem, and enough people were asking
questions about the theorem to warrant a little blog post, in my opinion.

In brief, the CRT let's us solve a system of equations:

$$
\begin{aligned}
x &\equiv a_1 \mod n_1 \cr
\vdots \cr
x &\equiv a_k \mod n_k
\end{aligned}
$$

by efficiently finding the $x$ that satisfies all of these constraints, provided
that each of the $n_i$ are *coprime*, i.e. share no common divisors.

# The Modulo Operation

The behavior of `mod` should be pretty well understood, at least for positive numbers.
`mod x m` means dividing `x` with `m`, and looking at what's left, so we have:

```haskell
mod 220 2 == 0
mod 221 2 == 1
mod 67 16 == 3
```

etc.

Another way of defining this is to say that $mod(x, m)$ is the difference between
$x$, and the largest multiple of $m$ less than $x$, i.e.

$$
\text{mod}(x, m) := x - k \cdot m
$$

where $k \cdot m \leq x$, and $\forall k' \cdot m \leq x. \quad k' \leq k$.

The behavior of `mod` for negative numbers varies depending on the programming language.
In haskell, for example, `mod (-x) m` is `-(mod x m)`. But according to the mathematical
definition we've just given, this wouldn't be the case.

For example $\text{mod}(-1, 3) = 2$, because the closest multiple of $3$, below $-1$, would be $-3$,
and there's a difference of $2$ in order to reach $-1$ from there. Similarly,
we would have $\text{mod}(-5, 3) = 1$, because the closest multiple of $3$ here would be $-6$.

We can define a function to emulate this behavior, in languages where `mod (-x) m` = `-(mod x m)`:

```haskell
fullMod :: Integer -> Integer -> Integer
fullMod x m = mod (mod x m + m) m
```

If `x` is positive, then we have `mod x m + m`, and we take that modulo `m`. But, adding a multiple of `m`,
doesn't change the modulus. This is because $\text{mod}(a + b, m) = \text{mod}(\text{mod}(a, m) + \text{mod}(b, m), m)$,
and $\text{mod}(m, m) = 0$.

If `x` is negative, then we end up with `-(mod x m) + m`, i.e. `m - mod x m` after the first step. This is the correct behavior,
since if we have something like $\text{mod}(-5, 3)$, we want to end up with $1$, but instead we
get $-2$ first. By adding this to $3$, we end up with the $1$ we wanted. We don't need to do another reduction
at this point, so the extra `mod _ m` changes nothing.

# Modular Arithmetic

If we look at all of the integers, modulo some `m`, we end up with the set of numbers $\\{0, 1, \ldots, m - 1\\}$.
Since these are still integers, we can carry out addition and multiplication as usual, but remembering
to take the modulus with `m` afterwards. This gives us a *ring* $\mathbb{Z}/m\mathbb{Z}$, or $C_m$, which is quite a bit shorter.

For example, in $C_3$, we have $1 + 2 = 0$, $2 \cdot 2 = 1$, and other possibilities.

{{<note>}}
To be super pedantic, the elements of $C_m$ are not $\\{0, \ldots, m -1 \\}$, but rather
$\\{[0], [1], \ldots\\}$, where $[x]$ denotes the set of integers with a given modulus $x$.
In practice, these work out the same, and we won't need this level of detail.
{{</note>}}

For each integer $x \in \mathbb{Z}$, we have a corresponding modulus in $C_m$. Many integers
will have the same modulus. To express that two integers have the same modulus, we say:

$$
x \equiv y \mod m
$$

Usually, $y$ will be reduced completely, and so we this means $\text{mod}(x, m) = y$. The subtle difference
here is that $\text{mod}$ outputs an integer in the range $\\{0, \ldots, m - 1\\}$, but in the equivalence
equation, $x$ and $y$ don't necessarily have to be in this range.

As further examples, we have:

$$
\begin{aligned}
6 &\equiv 0 &\mod 3 \cr
5 &\equiv -1 &\mod 3 \cr
-5 &\equiv 2 &\mod 3
\end{aligned}
$$

One useful property is that if:

$$
x \equiv y \mod m
$$

Then:

$$
x - y \equiv 0 \mod m
$$

i.e. $x - y$ is a multiple of $m$.

# Bézout's Algorithm

We just need one more tool before we can tackle the CRT: Bézout's Algorithm. This algorithm
states that if we have two integers $a, b \in \mathbb{Z}$, and their greatest common divisor
is $\gcd(a, b)$, then we can find two integers $x, y \in \mathbb{Z}$, such that:

$$
x \cdot a + y \cdot b = \gcd(a, b)
$$

This is basically a suped up version of Euclid's algorithm, finding not only
the greatest common divisor, but also the necessary integer factors to get the equation
above.

## Euclid's Algorithm

Remember that the $gcd$ of two numbers should return the largest number dividing both of them.

Let's go over the idea behind Euclid's algorithm first.

Note that $\text{gcd}(a, 0) = a$. Anything divides $0$, so we just need a divisor of $a$.
The largest one is just $a$ itself.

Now, let's say we can decompose $a$:

$$
a = qb + r
$$

This uses euclidean division to find a quotient and a remainder, with $r < b$.

We can show that any divisor of $a$ and $b$ is a divisor of $b$ and $r$, and vice versa.

First, let's show $d | b, d | r \implies d | a$.

Note that if $d | b$, then $b = k_b d$ for some factor $k_b$.

With this in hand,
it's clear that if $d | b, d | r$, then we can write $a = qb + r$ as

$$
a = q k_b d + k_r d = (q k_b + k_r) d
$$

This means that $d | a$.

Now for $d | a, d | b \implies d | r$.

We can write $a - qb = r$, and get
$$
k_a d - q k_b d = r
$$

and thus $d | r$.

Since the common divisors of $a$ and $b$ are nothing more than the common divisors of $b$ and $r$,
$\gcd(a, b)$ must be $\gcd(b, r)$.

This gives us a recursive algorithm to calculate the $\gcd$:

```haskell
gcd :: Integer -> Integer -> Integer
gcd a b | a < b = gcd b a
gcd a 0 = a
gcd a b =
  let r = mod a b
  in gcd b r
```

The first clause is the *swapping* rule, the second the base case, with $\text{gcd}(a, 0)$,
and the third implements the recursive rule we just went over.

## Extending the Algorithm

Bézout's algorithm extends this to find not only the $\gcd$, but also two factors
$x, y$ such that $xa + yb = \gcd(a, b)$

We have a similar base case:

With $\gcd(a, 0) = a$, it's obvious that:

$$
1 \cdot a + 0 \cdot 0 = a
$$

Now, for the general case, we decompose $a$ into $qb + r$, like before.

Let's say we've found $x, y$ such that:

$$
xb + yr = \gcd(b, r) = \gcd(a, b)
$$

But, we can write $r$ as:

$$
r = a - qb
$$

Which gives us:

$$
\gcd(a, b) = xb + yr = xb + y(a - qb) = ya + (x - q)b
$$

So $(x, y)$ becomes $(y, (x - q))$

We can write out a similar algorithm pretty easily:

```haskell
bezout :: Integer -> Integer -> (Integer, Integer)
bezout a b | a < b = bezout b a
bezout a 0 = (1, 0)
bezout a b =
  let q = div a b
      r = mod a b
      (x, y) = bezout b r
  in (y, x - q)
```

# The Chinese Remainder Theorem

We now have all the tools we need to tackle the CRT itself!

To set the stage again, our starting point is a system of $k$ equations:

$$
\begin{aligned}
x &\equiv a_1 \mod n_1 \cr
x &\equiv a_2 \mod n_2 \cr
\vdots \cr
x &\equiv a_k \mod n_k
\end{aligned}
$$

and our goal here, is to find some integer $x \in \mathbb{Z}$ satisfying
each of these equations.

The CRT applies in the case where $n_1, n_2, \ldots, n_k$ are all coprime.

This means that:

$$
\gcd(n_i, n_j) = 1
$$

for any pair $i, j$.

## Two Equations

Let's first look at the case where we have just two equations:

$$
\begin{aligned}
x &\equiv a_1 \mod n_1 \cr
x &\equiv b_2 \mod n_2
\end{aligned}
$$

At first this might seem tricky, but remember that we assumed
that $n_1$ and $n_2$ are coprime. This means that $\gcd(n_1, n_2) = 1$.
Let's try using Bézout's algorithm!

This gives us $m_1, m_2$ such that:

$$
m_1 n_1 + m_2 n_2 = 1
$$

Notice if that if we look at this equation modulo $n_1$, we get:

$$
m_2 n_2 \equiv 1 \mod n_1
$$

This is because modulo  $n_1$, the extra multiple $m_1 n_1$ simply disappears

But multiplying anything by $1$ gives us back that same number, even modulo $n_1$.

This means that:

$$
a_1 m_2 n_2 \equiv a_1 \mod n_1
$$

But hey, that's one of the things we wanted to be true!

We can apply a similar argument to show that:

$$
a_2 m_1 n_1 \equiv a_2 \mod n_2
$$

So, we have a solution for each part, but how can we combine them to get
a solution for both parts at once?

We just add them together!

$$
a_1 m_2 n_2 + a_2 m_1 n_1
$$

This works, because modulo $n_1$, the right part disappears, and we
get $a_1 m_2 n_2$, which is $a_1$, as we saw before.

Similarly, modulo $n_2$, the left part disappears, and we get $a_2 m_1 n_1$,
which *also* works out to $a_2$, as we saw before.

This gives us a solution for the case of two equations.

## Generalizing


Let's say we have $k$ equations, instead of just $2$:

$$
\begin{aligned}
x &\equiv a_1 \mod n_1 \cr
x &\equiv a_2 \mod n_2 \cr
\vdots \cr
x &\equiv a_N \mod n_N
\end{aligned}
$$

What we're going to be doing is solving the first two equations, and then
using that to produce a new system of $N - 1$ equations.

Let $a_{1,2}$ be a solution to the first $2$ equations, i.e.

$$
\begin{aligned}
a_{1,2} &\equiv a_1 \mod n_1 \cr
a_{1,2} &\equiv a_2 \mod n_2 \cr
\end{aligned}
$$

What we can prove is:

$$
x \equiv a_{1,2} \mod n_1 n_2
$$

if and only if

$$
\begin{aligned}
x &\equiv a_1 \mod n_1 \cr
x &\equiv a_2 \mod n_2 \cr
\end{aligned}
$$

The first direction is simple. If $x \equiv a_{1, 2} \mod n_1 n_2$,
then $x - a_{1, 2}$ is a multiple of $n_1 n_2$, i.e. it is $k n_1 n_2$ for
some $k$. It's then clear that $x - a_{1, 2}$ is also a multiple of
$n_1$, as well as $n_2$.

This means that:

$$
\begin{aligned}
x &\equiv a_{1, 2} \mod n_1 \cr
x &\equiv a_{1, 2} \mod n_2 \cr
\end{aligned}
$$

But, remember that $a_{1, 2}$ is a solution to the first two equations, giving us:

$$
\begin{aligned}
x &\equiv a_1 \mod n_1 \cr
x &\equiv a_2 \mod n_2 \cr
\end{aligned}
$$

$\square$

For the other direction, we assume that:

$$
\begin{aligned}
x &\equiv a_1 \mod n_1 \cr
x &\equiv a_2 \mod n_2 \cr
\end{aligned}
$$

Using the fact that $a_{1, 2}$ is a solution, once more we get:

$$
\begin{aligned}
x &\equiv a_{1, 2} \mod n_1 \cr
x &\equiv a_{1, 2} \mod n_2 \cr
\end{aligned}
$$

This means that $(x - a_{1, 2})$ is a multiple of both $n_1$ and $n_2$,i.e.

$$
x - a_{1, 2} = k_1 n_1 = k_2 n_2
$$

But, since $\gcd(n_1, n_2) = 1$ we can show that $k_1 = k n_2$:

We can write $k_1 n_1$ as

$$
k_1 (1 - m_2 n_2) = k_2 n_2
$$

using Bézout, but then we get:

$$
k_1 = k_1 m_2 n_2 + k_2 n_2 = (k_1 m_2 n_2 + k_2) n_2
$$

showing that $k_1$ is indeed a multiple of $n_2$.

Thus we have:

$$
x - a_{1, 2} = k n_2 n_1
$$

but this means that:

$$
x \equiv a_{1, 2} \mod n_1 n_2
$$

as desired $\square$.

Since $x \equiv a_{1, 2} \mod n_1 n_2$ is equivalent to solving both of the
first two equations, we can simply replace both of those equations, to get:

$$
\begin{aligned}
x &\equiv a_{1, 2} \mod n_1 n_2 \cr
x &\equiv a_3 \mod n_3 \cr
\vdots \cr
x &\equiv a_N \mod n_N
\end{aligned}
$$

We now have one less equation, so this works as a recursive rule.

As a base case, if we end up with a single equation:

$$
x \equiv a \mod n
$$

we simply choose $a$ as our solution.

## Concretely

Alrighty, that was a bit of a mouthful, but hopefully it'll be a bit more understandable
if we write this out in code.

We have:

```haskell
solve :: [(Integer, Integer)] -> Integer
```

as our signature. This function will return the smallest positive integer
solving a list of $(a, n)$ congruence pairs.

For zero equations, anything is a solution. So we just return $0$

```haskell
solve [] = 0
```

For a single equation, the solution is obvious:

```haskell
solve [(a, n)] = fullMod a n
```

Note that we use the `fullMod` function we defined earlier, so that we get the smallest positive
solution. `a` itself would be a solution, but we normalize it here to get the "nicest" version
of it.

Now for two or more equations:

```haskell
solve ((a1, n2) : (a2, n2) : rest) =
  let (m1, m2) = bezout n1 n2
      a12 = a1 * m2 * n2 + a2 * m1 * n2
  in solve ((a12, n1 * n2) : rest)
```

So, we solve the first two equations, using Bézout, to get:

$$
a_{1,2} = a_1 m_2 n_2 + a_2 m_1 n_1
$$

And then we produce an equivalent equation $x \equiv a_{1, 2} \mod n_1 n_2$, and continue solving,
having replaced those two equations with this single one.

# Conclusion

Hopefully this was a clear enough explanation. The
[wikipedia article](https://www.wikiwand.com/en/Chinese_remainder_theorem)
is actually pretty good for this subject, so I'd recommend that as a good reference.

If you like this kind of thing I'd recommend picking up an Algebra book, like
"Contemporary Abstract Algebra", or "Algebra Chapter 0".

