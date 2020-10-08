---
title: "Recursive Types as Initial Algebras"
date: 2020-09-09
draft: false
description: "Exploring how the fixed point of a functor is an initial algebra"
path: "/posts/2020/09/recursive-types-as-initial-algebras"
type: post
image: "/posts/2020/09/recursive-types-as-initial-algebras/cover.jpg"
tags:
  - Math
  - Category Theory
  - Haskell
---

Recently (well, more like a month ago), I came across this interesting observation:

![](/posts/2020/09/recursive-types-as-initial-algebras/1.png)

In my head, I immediately jumped to the notion of *Algebras* in Category Theory.
I had recently studied that notion, found it quite interesting, and was very happy to
see this observation, because it was actually quite obvious to me thanks to what I'd recently learned.

The goal of this post is to unpack that tweet, and then explain why that observation is true.

# Recursive Types and Functors

Let's start with an example of a recursive type in Haskell. The usual example is that of some kind of toy
programming language:

```haskell
data Lang
    = Add Lang Lang
    | Mul Lang Lang
    | I Int
```

This is just a simple programming language in which we have integers, via `I 3, I 34, ...` and tiny arithmetic expressions
involving those integers: `Add (I 3) (I 4), Mul (Add (I 4) (I 3)) (I 4)`.

Because of the recursive nature of this definition, we can nest expressions arbitrarily far:

```haskell
Add (I 1) (Add (I 2) (I 3))
Add (I 1) (Add (I 2) (Add (I 3) (I 4)))
...
```

Now if we look at a different kind of recursive structure, say a Rose Tree:

```haskell
data RoseTree = Branch Int [RoseTree]
```

then we have similar recursive expressions possible:

```haskell
Empty
Branch 0 [Branch 1 [], Branch 2 [], Branch 3 [Branch 4]]
```

The specifics of these two data types is not as interesting as what they have in common.
What they have in common is their recursive nature of course. With `Lang`, we have a few points where
the data structure recurses back into itself, and with `RoseTree` we do the same.

One question we can ask ourselves, is whether we might be able to write these two data types as a combination
of a specific "template" for how the type "folds" back into itself, and then a type
doing the nesting.

To illustrate this, let's consider just the shape of these two data types:

```haskell
data LangF r
    = Add r r
    | Mul r r
    | I Int

data RoseTreeF r = Branch Int [r]
```

We've made these types generic over the type `r`, which is used as the placeholder for where
the recursive type folds back into itself.

Now we can define a recursive type, which does the folding for any kind of template like this:

```haskell
newtype Fix f = Fix (f (Fix f))
```

All this data type does is take another data type `f`, representing the shape of the recursion,
and actually pipes the recursion through that shape. We can see that `Fix LangF = Lang`, and
`Fix RoseTreeF = RoseTree`. With this trick, we can write down any recursive type as a shape, and then use
`Fix` to do the plumbing for us.

Now, I've been talking about a "shape", but really, what we need is a so-called *Functor*. This
is nothing more than a data type we can map over:

```hs
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)
```

It's pretty clear how `LangF` and `RoseTreeF` are functors; so clear, in fact, that we can do:

```haskell
{-# LANGUAGE DeriveFunctor #-}

data RoseTreeF r = Branch Int [r] deriving (Functor)
```

to have the compiler do all of that work for us.

If you want to understand a bit more about this, encoding, see 
[[1]](/posts/2020/09/recursive-types-as-initial-algebras/#ref-1).

## What the Tweet says

The `Fix` data type is the more common name for the `RecR f` type mentioned in that tweet,
so we understand how `Fix / RecR` allows us to turn a template into a recursive type,
by simply plumbing the shape through recursively.

The second part of the tweet says that the type `Fix` that we've defined is in fact equivalent to:

```hs
newtype FixA f = FixA (forall a. (f a -> a) -> a)
```

Now, we could provide an isomorphism between these types in Haskell right now, and be done with it,
but we wouldn't understand *why* this is possible.

# Algebras

Let's take what seems like a detour for now, and explore the concept of an *Algebra*. If you've
studied abstract algebra, you might be familiar with the notion of a Group, or a Monoid, or a Ring.
These are all the kinds of objects that we can generalize to something called an *Algebra over a Functor*.

Let's look at a specific example of such an object. Personally, I like Groups. A Group, traditionally,
is a set $G$ equipped with operations:

$$
e : G \\
\bullet : G \times G \to G \\
(-)^{-1} : G \to G \\
$$

Satisfying some additional axioms, which actually aren't important for this presentation of algebras.

So, given some elements $a, b, c \in G$, we have $a \bullet b \bullet c^{-1} \in G$, as another example
of an element we can form this way.

Another way of looking at this that if $G$ is a group, then we can take "abstract operations" like $a \bullet b$,
and interpret them as specific elements of $G$. So if $G$ is $(\mathbb{Z}, +)$, then we interpret the operations as:

$$
e \mapsto 0 \\
a \bullet b \mapsto a + b \\
a^{-1} \mapsto -a \\
$$

This is a bit of a subtle difference, but the key is that the elements on the left are not in $\mathbb{Z}$ itself,
rather they denote abstract combinations of operations using integers, and the elements on the right are actually complete
integers.

## In Haskell

If we move down to Haskell, this point might be a bit clearer.

The structure of a Group can be given by a data type:

```haskell
data GroupF a
    = E
    | Op a a
    | Inv a
```

Then, we can say that the integers form a group via:

```haskell
intIsGroup :: GroupF Int -> Int
intIsGroup E = 0
intIsGroup (Op a b) = a + b
intIsGroup (Inv a) = -a
```

With this presentation, it's clearer how `GroupF a` provides us with a single layer of group operations, and
that if `a` is actually a group, then we can interpret these operations as actual elements of the group.
We can see this as being able to evaluate some kind of algebraic expression involving the operations.
We can evaluate something like $a \bullet b \bullet c \bullet d^{-1} \bullet e$ into a single integer,
as if punching the expression into a calculator.

This motives a general definition of a `Group` as nothing more than a type `a`, and a function `GroupF a -> a`.

## Back to Category Theory

Back in Category Theory heaven, instead of `GroupF`, we have a functor: $G : \bold{Set} \to \bold{Set}$, from
the category of sets to itself, given by mapping some set of elements $\{a, b\}$ to the set of abstract group operations over that set:

$$
\{e, a \bullet a, b \bullet b, a \bullet b, a^{-1}, b^{-1}\}
$$

Then, if some set $S$ can be equipped with a Group structure, we could then provide a function $G S \to S$.

In general, we can define an algebra over a functor $F : \mathcal{C} \to \mathcal{C}$ over some category $\mathcal{C}$
to be a morphism $F O \to O$ in that same category.

# The Category of Algebras

In the same way that we can have homomorphisms between Groups, maps that preserve the operations, we
can have morphisms between algebras over a given functor $F$.

Given two algebras $(A, \epsilon_A : F A \to A)$ and $(B, \epsilon_B)$, a morphism $(A, \epsilon_A) \to (B, \epsilon_B)$ is a morphism $\varphi \in \mathcal{C}(A, B)$,
satisfying:

![](/posts/2020/09/recursive-types-as-initial-algebras/2.png)

So evaluating the operations and then mapping the result should be the same as replacing the elements inside of the structure, and then evaluating.

A bit more concretely:

$$
a \bullet b \mapsto \varphi(a) \bullet \varphi(b) \mapsto \varphi(a)\varphi(b)
$$

Should yield the same result as:

$$
a \bullet b \mapsto ab \mapsto \varphi(ab)
$$

This is in line with common notions of a group homomorphism.

Having defined morphisms, we now have a notion of a *Category* of algebras over a given functor. If we go back to our
example of a functor for Groups, this category would consist of groups and homomorphisms between them.

## Initial Algebras

An initial object in a category $\mathcal{C}$ is an object $X$ such that for any other object $A$, we
have a unique morphism $X \to A$.

In the category of algebras over $F$, this would mean some object $(X, \epsilon_X : F X \to X)$, along with a homomorphism
$X \to A$ respecting the evaluation for any other algebra $A$. In other words, if some object $A$ has the evaluation map
$F A \to A$, then we have a morphism $X \to A$.

Now it turns out that we can show ([[2]](/posts/2020/09/recursive-types-as-initial-algebras/#ref-2)) that for this object
$X \cong F X$. This means that adding another layer of operations does not change our object at all. Thus,
in some sense, our object looks like $F F F F \ldots$, and adding another layer of $F$ changes nothing.

But, back in Haskell land, this shows that `Fix F` is an initial object in the category of `F` algebras!

# Putting together the pieces

Back in Haskell land, an `F` algebra would be a type `A`, along with a function `F A -> A`. To say that
`X` is initial, would mean that we have a function `X -> A`, provided that `F A -> A` exists. We could expand
this further and say that we have:

```haskell
(F A -> A) -> (X -> A)
```

i.e. if you show me that `A` is an algebra, then I can provide you with a morphism `X -> A`.

We can reorder this to get:

```haskell
X -> (F A -> A) -> A
```

for any type `A`.

But, since `X` is *defined* by the existence of these morphisms, we might as well define:

```haskell
newtype Init f = Init (forall a. (f a -> a) -> a)
```

And by the theorem mentioned before, we have that this type is equivalent to:

```haskell
newtype Fix f = Fix (f (Fix f))
```

This happens precisely because the fixed point of a functor satisfies the properties of an initial object
in the category of algebras over that functor!


Now, let's provide a concrete isomorphism:

```haskell
newtype Init f = Init { unInit : forall a. (f a -> a) -> a }

newtype Fix f = Fix { unFix :: f (Fix f) }

unwrap :: Functor f => f (Init f) -> Init f
unwrap x = Init (\ev -> ev (fmap (\i -> unInit i ev) x))

iso1 :: Functor f => Fix f -> Init f
iso1 = unwrap . fmap iso1 . unFix

iso2 :: Functor f => Init f -> Fix f
iso2 (Init g) = g Fix
```

The key workhorse here is `unwrap`, which is nothing more than the evaluation map we much have for any algebra,
but which we can define just by the initiality of `Init f`.

# For the Natural Numbers

In [my previous post](/posts/2020/08/encoding-the-naturals), I went over three encodings of the natural numbers.
This technique provides us with a fourth.

First we have the standard recursive definition:

```haskell
data Nat = Z | S Nat
```

Then, applying our technique, we have:

```haskell
data NatF a = ZF | SF a

data NatAlg = NatAlg (forall a. (NatF a -> a) -> a)
```

We can show concretely that this is isomorphic to the standard encoding of the natural numbers:

```haskell
zero :: NatAlg
zero = NatAlg (\ev -> ev ZF)

succ :: NatAlg -> NatAlg
succ (NatAlg g) = NatAlg (\ev -> ev (SF (g ev)))

iso1 :: Nat -> NatAlg
iso1 Z = zero
iso1 (S n) = succ (iso1 n)

iso2 :: NatAlg -> Nat
iso2 (Nat g) = g (\case
    ZF -> Z
    (SF n) -> S n
)
```

Notice how `zero` along with `succ` constitute a definition of `unwrap`:

```haskell
unwrap :: NatF NatAlg -> NatAlg
unwrap Z = zero
unwrap (S n) = succ n
```

we defined `zero` and `succ` using the more general isomorphism.

# Summary

Briefly speaking, in Haskell, the following types are isomorphic (provided `f` is a functor)

```haskell
data Fix f = Fix (f (Fix f))

data Init f = Init (forall a. (f a -> a) -> a)
```

# References

<p>
  <a id="ref-1"  href="https://www.schoolofhaskell.com/user/bartosz/understanding-algebras">
    [1] Bartosz Milewski, Understanding F-Algebras
  </a>
</p>
<p>
  <a id="ref-2"  href="https://ncatlab.org/nlab/show/initial+algebra+of+an+endofunctor">
    [2] nLab, Initial Algebra of An Endofunctor
  </a>
</p>