---
title: "Let's Describe Algorithms Better"
date: 2021-01-16T20:18:19+01:00
draft: true
katex: true
tags:
  - Algorithms
  - Programming
---

I was reading a cryptography textbook, and I came across an algorithm
that reminded me why I often dislike descriptions of algorithms
in textbooks. This is a short post going over this example,
and thinking about how to improve these descriptions.

<!--more-->

Specifically, I often dislike the imperative descriptions of
algorithms, which do a great job of explaining the operational
semantics, but don't give any insight into *why*
an algorithm actually works.

# The Example

{{<note>}}
This example comes from:
[Hoffstein, Pipher, and Silverman, An Introduction to Mathematical Cryptography.](https://www.springer.com/gp/book/9781493917105)
This book is very good otherwise, the math is described quite nicely,
with plenty of detail. This kind of imperative presentation of
algorithm is ubiquitous, as mentioned earlier. Furthermore,
this algorithm is in an exercise, where you're supposed to show
its equivalence with a more clearly explained version, so I can't
fault the authors all that much.
{{</note>}}


This algorithm takes integers $N$, $g$, and $A$, and calculates:

$$
g^A \mod N
$$

As some background, the idea behind algorithms for doing this fast
is to use the fact that if you write $A$ as powers of $2$,
say $A = 2 + 4 + 16$ for the sake of example, then we have:

$$
g^A = g^{(2 + 4 + 16)} = g^2 \cdot g^4 \cdot g^{16}
$$

More generally, we can multiply all of the power of 2 exponents
together.

An algorithm applying this technique is given an imperative description
as:

1. Set $a = g$ and $b = 1$
2. Loop while $A > 0$
3. If $A \equiv 1 \mod 2$, set $b = b \cdot a \mod N$
4. Set $a = a^2 \mod N$, and $A = \lfloor A / 2 \rfloor$
5. End Loop
6. Return $b$, which is equal to $g^A \mod N$

At least for me, it's not immediately obvious that this does the right
thing, and I have an awfully hard time trying to remember this algorithm.

The advantage of an algorithm described imperatively like this,
is that it translates immediately to an *actual* programming language.
Let's use Python, to illustrate this:

```python
def pow(N, g, A):
  a = g
  b = 1
  while A > 0:
    if A % 2 == 1:
      b = (b * a) % N
    a = (a * a) % N
    A = A // 2
  return b
```

Even now, though the actual code makes it easier to trace through,
it doesn't immediately give you an understanding of what's really
going on.

## A better description

There's a lot of essence in this algorithm to be distilled.
One first insight is that our iteration is controlled entirely
by `A`, so we should think about what values we're using.

At each iteration, we care about `A % 2`, and then use `A // 2`
at the next iteration. What we're really doing is iterating over
all the bits of `A`, from least significant to most significant.

So, if `A` is `22`, for example, the bits we'd see would be:

```python
[0, 1, 1, 0, 1]
```

At each iteration, the value of `a` is the square of the previous
iteration.

What we're doing with `b` is multiplying it with some of these values,
but only selectively. We only multiply it by the latest value
of `a` *if* the bit for this iteration is `1`.

A better description, in pseudo code, would be:

1. Let $G = [g, g^2, g^4, g^8, \ldots]$ be a list of successive squares (taken modulo $N$)
2. Let $B$ be a list of bits in $A$, from least to most significant
3. Pair up $G$ and $B$
4. Keep only those elements where the bit is $1$
5. Multiply all the remaining exponents, modulo $N$
6. This value is $g^A \mod N$

For me, this strategy is much easier to remember.
Instead of describing things in the nitty-gritty details,
we use a higher level description. This description
does a better job, in my view, of distilling the essential
details of the algorithm, and removing out less important details.

One goal of the original algorithm was efficiency, specificallly
not storing values, and working only with a few accumulators.
This declarative description talks about lists, albeit infinite ones,
so it doesn't do as good a job at describing the operational semantics.

Thankfully, in a language like Haskell, where you have a lazy lists,
you can effectively implement this description with the described
performance characteristics:

```haskell
pow :: Int -> Int -> Int -> Int
pow n g a =
  zip (bits a) (squares g)
  |> filter (fst >>> (== 1))
  |> map snd
  |> prod
  where
    bits :: Int -> [Int]
    bits 0 = []
    bits n = mod n 2 : bits (div n 2)

    squares :: Int -> [Int]
    squares = iterate (\s -> s * s `mod` n)

    prod :: [Int] -> Int
    prod = foldr (\x acc -> x * acc `mod` n) 1
```

I find this algorithm much more satisfying, and much easier to
remember:

"The fast squaring algorithm? Oh, it's the one where
you zip the bits of the exponent with the successive squares,
and then multiply them!"

# Broader lessons?

Now, this was a contrived example, in that I literally
started this post just because I disliked the original presentation
of the algorithm. Perhaps I'm biased, in that the true understanding
comes from translating an imperative description into
higher level insight.

That being said, maybe we should try and provide this kind
of higher level insight *alongside* a lower level operational semantics.

One downside of the declarative description is that while the
time complexity is still somewhat clear, the space complexity,
and other performance characteristics are less clear.

It's much easier to see why a given algorithm might have better
performance characteristics when it's described in terms of
low level imperative operational details. This is because
the performance ultimately does depend on all of these details.

It's kind of hard to "forget" understanding the higher
level insights though. Once you've understood *why* an imperative
algorithm works, you conflate the higher intuition with
the low level details, and it becomes hard to not read
these details grouped with the lens of your intuition.

It's because of this, I suspect, that so many algorithm descriptions
in textbooks don't that good a job of connecting imperative
details to deeper understanding of the algorithm.

While it is ultimately good to develop your own framing and
and intuition when working through a new piece of knowledge,
it's also great to provide some directions and insight
as an author.

We can definitely do a lot better :)
