---
title: "Programming Problem: Run-Length Encoding"
date: 2021-02-03T17:08:09+01:00
draft: false
katex: true
tags:
  - Haskell
  - Programming
---

This is just a quick post about a programming problem that's
been circulating around [on twitter recently](https://twitter.com/Al_Grigor/status/1357028887209902088):
<!--more-->

{{<img "1.png">}}

Essentially, this problem asks us to convert from the standard encoding of a list:

```haskell
[a, a, a, a, b, b, b, c, c, a]
```

to the run-length encoding of that same list:

```haskell
[(a, 4), (b, 3), (c, 2), (a, 1)]
```

Mathematically, this is just the conversion between two different representations
of the free monoid on some set. This allows us to combine different elements
of a set $\\{a, b, c\\}$ by putting them one after the other in a list:

$$
a \bullet b \bullet c = abc
$$

Using the terser notation $a \bullet a = aa = a^2$, a list like:

$$
aaaabbbcca
$$

becomes:

$$
a^4 b^3 c^2 a
$$

which is the same principle behind run-length encoding. If you think about it,
in Haskell, a list like:

```haskell
[a, a, a, a, b, b, b, c, c, a]
```

is equivalent to the concatenation of a list for each element:

```haskell
[a] <> [a] <> [a] <> [a] <> [b] <> [b] <> [b] <> [c] <> [c] <> [a]
```

This is exactly why in which a list lets us turn any type into a Monoid.
It gives us a free way to concatenate elements of our type.

This equation can be exploited through the function `foldMap`:

```haskell
foldMap :: Monoid m => (a -> m) -> [a] -> m
```

Essentially, given a list:

```haskell
[a, a, a, b, b]
```

and a function `f :: A -> M`, we first convert the list like we did previously:

```haskell
[a] <> [a] <> [a] <> [b] <> [b]
```

And now we replace each use of `[]` with `f`:

```haskell
f a <> f a <> f a <> f b <> f b
```

Since `M` is a monoid, we know what to do with all of these `<>` operations.

This decomposition makes it clear that `foldMap (\x -> [x])` is going to
leave our list untouched.

In order to convert to the run-length encoding, all we need to do is
- Make our run-length encoding a Monoid
- Create a function `a -> RunLength a`, analogous to `\x -> [x]`

# The code

First, let's define our `RunLength a`, representing a list
`[a]`, but with less redundancy:

```haskell
import Data.Sequence (Seq (Empty, (:<|), (:|>)))
import qualified Data.Sequence as Seq

newtype RunLength a = RunLength (Seq.Seq (Int, a))
  deriving (Eq, Show)
```

The run length encoding is just a sequence of `(element, count)` pairs.

{{<note>}}
I've used the `Seq` data structure instead of `[]` here, because we'll be needing
to access both ends of this collection efficiently.

The patterns `first :<| rest`, and `rest :|> last` can be used to do this.
With lists, only the first pattern is efficient.
{{</note>}}

This is just an alternate representation of `[a]`, and it's easy to convert back:

```haskell
runLengthToList :: RunLength a -> [a]
runLengthToList (RunLength runs) = foldMap (uncurry replicate) runs
```

The idea is to convert each `(3, a)` pair into a list `[a, a, a]`,
with the right amount of elements. Thankfully, we have:

```haskell
replicate :: Int -> a -> [a]
```

which does exactly what we want, and `uncurry` lets us change its signature
to `uncurry replicate :: (Int, a) -> [a]`

Finally, `foldMap` is the function we saw earlier, and will concatenate
each of these runs together.

We can also convert a single element to a run-length list:

```haskell
singleton :: a -> RunLength a
singleton a = RunLength (Seq.singleton (1, a))
```

A single element is just a run of length 1.

Now, let's create a monoid instance for `RunLength a`:

```haskell
instance Eq a => Semigroup (RunLength a) where
  RunLength (as :|> (countA, a)) <> RunLength ((countB, b) :<| bs)
    | a == b = RunLength ((as :|> (countA + countB, a)) <> bs)
  RunLength as <> RunLength bs = RunLength (as <> bs)

instance Eq a => Monoid (RunLength a) where
  mempty = RunLength Empty
``` 

Now, the empty list acts as the identity for concatenation. Usually,
given something like:

$$
b^4a^3 \bullet b^2c^3
$$

we can simply concatenate both run lists:

$$
b^4a^3b^2c^3
$$

This is reflected in our fallback case for `<>`. The only special case is
when we see the same element on both sides:

$$
b^3a^2 \bullet a^3c^2
$$

In which case we need to create a single element joining both sides:

$$
b^3 a^5 c^2
$$

This is reflected in our first rule, which checks if the last
element of one list matches the first of the other, in which case
it replaces both elements with a single one, with the correct
count.

Having defined a Monoid instance, we can now finish
off the problem with our coup-de-grâce:

```haskell
runLengthFromList :: Eq a => [a] -> RunLength a
runLengthFromList = foldMap singleton
```

As promised, all the work is done by the Monoid instance for `RunLength a`,
and our `singleton :: a -> RunLength a` function.

# Conclusion

The observation that lists are essentially the free monoid on some type
is key to eeking out the elegance of our solution. Of course,
this solution is longer than some one-liners you might come up with;
it makes me feel a lot cooler though :)

The full code is available [here](https://gist.github.com/cronokirby/dfd58d1c7f345a23265846d657335957).