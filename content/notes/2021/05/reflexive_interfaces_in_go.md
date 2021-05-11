---
title: "Reflexive Interfaces in Go"
date: 2021-05-11T20:13:08+02:00
type: note
note-tags:
  - "Go"
  - "Programming Languages"
---

In languages with typeclasses, like `Rust`, or `Haskell`, you can have
an interface that uses its type in different positions, like:

```haskell
class Group a where
  unit :: a
  combine :: a -> a -> a
  invert :: a -> a
```

In `Go`, on the other hand, you can only really enforce some type
as the "first argument", e.g.:

```go
type Consumer interface {
  Consume(string)
}
```

There are many situations where you'd want to be able to reference the
type implementing an interface, for example when representing concepts
from algebra.

One not very type-safe way of doing this is casting:

```go
type Group interface {
  Combine(that Group) Group
  Invert() Group
}
```

Then, when implementing this for a concrete group, you'd cast to the
right type:

```go
func (x *mygroup) Combine(that Group) {
  casted := that.(*mygroup)
  ...
}
```

The idea is that you never mix different instances of `Group` together.

With generics, you'd be able to do something more type-safe:

```go
type Group[T] interface {
  Combine(that T) T
  Invert() T 
}
```

And then consistently doing:

```go
func foo[T Group[T]](x T) {
  ...
}
```

To ensure that the types lign up as desired.

One problem with both of these approaches is that there's no way
to model nullary operations, like:

```haskell
unit :: a
```

I still don't really know of a good way to model something like this
in Go.
