---
title: "Some Thoughts on Numeric Classes"
date: 2021-02-28T11:11:09+01:00
draft: true
katex: true
tags:
  - Math
  - Haskell
---

<!--more-->

# Haskell's Hierarchy

Here's an overview of Haskell's hierarchy of typeclasses
{{<img "1.png">}}

You can see the different typeclasses involved, and the dependencies
linking all of them together.

## Eq

The first class is [`Eq`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Eq.html),
giving us decideable equality for a type:

```haskell
class Eq a where
  (==) :: a -> a -> a
```

Naturally, this defines the `==` operator we all know and love. This
must be an equivalence relation, satisfying:

```haskell
x == x
x == y -> y == x
x == y, y = z -> x = z
```

## Ord

Then you have [`Ord`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Ord.html),
which augments `Eq` with comparisons:

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
```

(I've simplified things a bit here)

And the `Ordering` type is a simple enumeration:

```haskell
data Ordering = LT | EQ | GT
```

It's clear that `Ord` should entail `Eq`, since we can just check whether
compare returns `EQ` for equality.

Using `compare`, the standard comparison operators `<, >, <=, >=` can all be provided.
While `Ord` doesn't specify any laws itself, it's common to assume that `<=` is
a partial order, satisfying:

```haskell
x <= x
x <= y, y <= z -> x <= z
```

## Semigroup

Now we get to the "algebraic" classes.

The first of these is [`Semigroup`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Semigroup.html):

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

The only thing we assume about this operator is its associtivity:

```haskell
a <> (b <> c) = (a <> b) <> c
```

## Monoid

[`Monoid`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html)
augments the `Semigroup` class with an identity element:

```haskell
class Semigroup  a => Monoid a where
  mempty :: a
```

This element satisfies:

```haskell
x <> mempty = x
mempty <> x = x
```

## Enum

Next, we have the [`Enum`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Enum) class:

```haskell
class Enum a where
  toEnum :: Int -> a
  fromEnum :: a -> Int
```

The main purpose of this class is to provide semantics for the
`[a..b]` syntax, otherwise this is more of a pragmatic class, instead of
one with some underlying algebraic idea.

## Num

And now we have the infamous [`Num`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Num) class:

```haskell
class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```

This is a big amalgamation of everything you'd expect numbers to be, and
then some more. The main purpose of this is to interpret the operators
mentioned here, including `-` as well, and to interpret numeric literals,
like `3`, or `420`, using the `fromInteger` method.

In my opinion, this class is pretty ugly, and perhaps my central motivation
in wanting to think about some ways to reform this hierarchy.

## Real

Now, you have another ad-hoc class, called [`Real`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Num):

```haskell
class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
```

I confess that I've never used this class all that much. Ostensibly,
its purpose is just to provide a means to convert some type into
rational numbers, which provides a direct means for comparision,
hence the `Ord` instance.

If we can think of elements of a type as being rational numbers, it'd make
sense to also stipulate that they're numbers, using the inferred operations.
The `Num` requirement makes sense, under this interpretation.

## Integral

Finally, we have the [`Integral`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Integral)
class:

```haskell
class (Real a, Enum a) => Integral a where
  quotRem :: a -> a -> (a, a)
  toInteger :: a -> Integer
```

(Simplifying a bit here, but these form a complete definition).

Basically, this is for types that are equivalent to integers, and we have the
corresponding division operations we expect to have for integers:
`mod, div` et alii.

# Interlude: Two Worlds

## The Mathematical Classes

## The Pragmatic Classes

# A Proposed Hierarchy

{{<img "2.png">}}

## Semigroup

## Monoid

## Additive

## Ring

### Choosing Instances

## Num

## Enum

## Integral

### Roundtripping

## Real

# Conclusion
