---
title: "Encoding Traits with Go Generics"
date: 2022-03-19T21:27:53+01:00
draft: true
katex: false
tags:
  - "Generics"
  - "Go"
---

It turns out that [Go v1.18](https://tip.golang.org/doc/go1.18#generics)'s
generics functionality is enough to encode traits, which has many
applications. In particular, we could write a generic library for Elliptic
Curves, among other things.

<!--more-->

# Background

Before version 1.18, the main way of achieving polymorphism in Go was via
interfaces. For example, we could define an interface for objects which
are serializable:

```go
type Serializable interface {
    Serialize() []byte
}
```

Go has *implicit* interface implementation. To implement this
interface, we just need to have a type which happens to have
a method with the right name:


```go
type Pony struct {
    name string
}

func (p *Pony) Serialize() []byte {
    return []byte(p.name)
}
```

We can write a method which takes in an arbitrary element
of this interface:


```go
func serialize(s Serializable) []byte {
    s.Serialize()
}
```

Note that which method is called depends on what object
gets passed to this function at *runtime*. We use dynamic
dispatch to look through virtual tables in order to call
the right function.

The other approach we can contrast this with is the
*trait* or *typeclass* approach, popularized by languages
like Rust and Haskell.

In Haskell's case, we would implement `Serializable` like this:

```haskell
class Serializable a where
    serialize :: a -> ByteString
```

We would then use this typeclass in a similar way to Go:

```haskell
actuallySerialize :: Serializable s => s -> ByteString
actuallySerialize s = serialize s
```

Note that unlike Go, the method call is resolved at *compile time*.
In practice, a different version of `actuallySerialize` will
be created for each type it ends up being called with.
Go takes a similar approach with its generics, as we'll
see soon.

I actually wrote [about the differences](/posts/2019/08/from-interfaces-to-traits/) between these approaches 3 years ago,
so I'll refer you to that post for a more detailed discussion.

The most important difference is that the typeclass approach
lets us refer to the type implementing the typeclass. For
example, in our `Serializable` class in Haskell, we had
a reference to `a`, which is whatever type was implementing
the class.

This self type can appear at any position, and even multiple
times. For example, we could define a class for groups as:

```haskell
class Group a where
    one :: a
    inv :: a -> a
    mul :: a -> a -> a
```
