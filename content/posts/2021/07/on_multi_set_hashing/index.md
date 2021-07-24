---
title: "On Multi-Set Hashing"
date: 2021-07-24T09:49:36+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Math"
---

Designing a hash function where the order of inputs doesn't matter
is surprisingly easy.

<!--more-->

# The Problem

Recently, I curiously asked a question on Twitter:

{{<img "1.png">}}

A typical hash function takes some data, usually in the form of
a sequence of bytes, and outputs a succint digest. Any slight
change to the data causes a large change in the digest.

Here, I'm asking if you can design a hash function for a *set*
of objects. Such that the order you hash in doesn't matter. For example,

$$
H(\\{a, b, c\\})
$$

should return the same digest as:

$$
H(\\{c, a, b\\})
$$

One simple way to accomplish this is to sort your objects before
passing them to the hash function. For example,
you'd sort $\\{c, a, b\\}$ to get the ordering $\\{a, b, c\\}$,
and then pass that to the hash function. This would make the ordering
of objects not matter, since you normalize everything before
using the hash function.

Normal hash functions accept incremental input. If you want to hash
a 2GB file, you don't need to have the whole file in memory.
Instead, you can feed the hash function small chunks of the file,
and end up with the same result as feeding in the entire 2GB of
data at once.

The problem with sorting the objects before feeding them into the
hash function is that we lose this incremental property. We
need to keep a large sorted collection until we've seen all
of the objects.

What I was looking for in this tweet was a hash function that
could accept objects directly as they arrived, but where
the final result wouldn't depend on the order of arrival,
only which objects had arrived.

We could also extend this function to *multi-sets*, where each
object can appear multiple times. We'd want the same incremental
property, where the result would now depend only on the objects
we've seen, and how many times we've seen them.

Thankfully, [Samir](https://twitter.com/SamirTalwar) raised my
awareness to a paper {{<ref-link "1">}} addressing this exact problem!

{{<img "2.png">}}

# Some Approaches

# A Concrete Implementation

# Conclusion

# References

{{<ref
  "1"
  "http://people.csail.mit.edu/devadas/pubs/mhashes.pdf"
  "Incremental Multiset Hash Functions - Dwaine Clarke, Srinivas Devadas, Marten Van Dijk, Blaise Gassend, G. Edward Suh">}}