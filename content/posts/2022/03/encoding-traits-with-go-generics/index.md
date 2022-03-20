---
title: "Encoding Traits with Go Generics"
date: 2022-03-20T16:42:38+01:00
draft: false
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

Whereas interfaces can only define methods, that is,
functions that look like:

```text
a -> X -> Y
```

traits can place the `a` anywhere in the function type.
With Go's interfaces, there's no good way to reference
the type implementing the interface, or to require that that
the return type of the method is the same as the receiver.

## A Workaround

One little workaround is that you can make the contract
about returning an element of the same type *implicit*.
For example, we could define a group interface as:

```go
type Group interface {
    Inv() Group
    Mul(Group) Group
}
```

This interface says that we return and accept any type implementing
this interface. But, we can implicitly allow implementors
to only work with their own type, casting whenever necessary:

```go
type F13 int

func (x F13) Inv() Group {
    // x ** 12 mod 13
    var out F13
    return out
}

func (x F13) Mul(generic Group) Group {
    y := generic.(F13)
    return x * y % 13
}
```

It's understood that implementors won't actually accept any
object satisfying the interface, but only objects with the
same type as them. Similarly, they will only ever return
objects of the same type.

Naturally, this casting comes at a runtime cost, and is also
very brittle. If you don't conform to this implicit
contract, you'll get panics at runtime, which isn't
exactly pleasant.

We used an approach like this to abstract over curves
in the [Threshold Signature library](https://github.com/taurusgroup/multi-party-sig/blob/main/pkg/math/curve/curve.go) I worked on at Taurus.

One problem with this approach is that there's no good way
to represent the methods which create an element of the group
out of thin air, like:

```haskell
class Group a where
    one :: a
    ...
```

For this, one approach is to define a secondary interface,
related to the group, which knows how to create elements of that
group:

```go
type GroupDomain interface {
    One() Group
}

type Group interface {
    Domain() GroupDomain
    Inv() Group
    Mul(Group) Group
}
```

For Elliptic Curves, the natural thing to do is to define
an interface for a `Curve`, which knows how to create scalars
and points.

This approach is still somewhat artificial, but is actually
what we'll be basing ourselves upon when using generics,
albeit with more type safety.

# Enter Generics

I find Go's generics to be pretty complicated. In my opinion,
most of this is non-essential complexity, comparing
to a green-field implementation of generics. But, since Go
had to find a way to implement generics in a backwards
compatible way and with minimal changes to the language,
I can't really comment on whether or not the solution was more
complicated than necessary in that context.

All of this to say that I won't be explaining how they work
completely here, so I refer you to [the Go reference](https://go.googlesource.com/proposal/+/refs/heads/master/design/43651-type-parameters.md).

The core of this proposal is that functions can now
accept type parameters, like:

```go
func id[T any](t T) T {
    return t
}

func main() {
    id[int](3)
    id[string]("foo")
}
```

In theory, one version of `id` will get created for each
type it gets used with. What matters is that the dispatch
doesn't have to be done at runtime, unlike with interfaces.

This function doesn't really do anything interesting with
the generic type. In fact, with `T any`, we can't really
do anything with the type itself, beyond moving it
around between containers.

To do interesting things, we need some kind of *constraint*
on `T`, so that we can assume certain behaviors, and then
make use of those behaviors.

Go decided to base its constraint system off of its existing
interface system. So, we can take a normal interface:

```go
type Serializable interface {
    Serialize() []byte
}
```

and then use it as a constraint in a generic function:

```go
func actuallySerialize[T Serializable](t T) []byte {
    return t.Serialize()
}
```

Now, we could have written this as:

```go
func actuallySerializeI(t Serializable) []byte {
    return t.Serialize()
}
```

The difference is that one of these uses static dispatch,
and the other uses dynamic dispatch. Otherwise, they're pretty
similar.

Where things really get interesting is that types can
also be generic. So you can have:

```go
type Array[T any] struct {
    data []T
}
```

We can also have generic interfaces:

```go
type ConvertibleTo[T any] interface {
    Convert() T
}
```

And `ConvertibleTo[T]` can also be used as a type constraint!

```go
func convert[T any, C ConvertibleTo[T]](c C) X {
    return c.Convert()
}
```

This is the fundamental construct we'll use to create a
trait for groups, along with the "domain" idea we saw earlier.

# Representing Groups

The basic idea is that instead of making an interface for
a group, we make an interface parameterized by a group,
which knows how to perform operations over that group.

To illustrate the idea, we have:

```go
type Group[E any] interface {
    One() E
    Inv(E) E
    Mul(E, E) E
}
```

This mimics the `Group` typeclass we had in Haskell earlier.

Now, the type implementing this interface will just be
a dummy type:

```go
type F13Element int

type F13 struct{}

func (F13) One() F13Element {
    return 1
}

func (F13) Inv(e F13Element) F13Element {
    // return e ** 12 % 13
}

func (F13) Mul(a, b F13Element) F13Element {
    return a * b % 13
}
```

We can then create and use a generic function over groups like this:

```go
func square[E any, G GroupDomain[E]](group G, e E) E {
	return group.Mul(e, e)
}

func main() {
	square[F13Element, F13](F13{}, 2)
}
```

The object for the group itself doesn't really contain any
information, what matters is that the methods on that object
implement the operations for that group. All we care about
is the dictionary of methods the object carries around.
The advantage of generics is that this dictionary only
exists at compile time, and we have a larger amount
of type-safety compared to the runtime casting approach.

{{<note>}}
This is actually very similar to the Haskell approach,
in some sense. In Haskell, a class is analogous to a generic
data
type containing the functions of that class as fields.
A function with a type constraint is really a function taking
an element of this generic data type as an argument. This
is exactly what we're doing in Go.
{{</note>}}

## Associated Types

In most uses of a group, we also want to access the associated
class of scalars for that group. We can do this pretty
easily as well:

```go
type Scalars[S any] interface {
    Zero() S
    One() S
    Add(S, S) S
    Neg(S) S
    Mul(S, S) S
    Inv(S) S
}

type Group[S any, E any, SD Scalars[S]] interface {
    Scalars() SD
    Identity() E
    Generator() E
    Mul(E, E) E
    Inv(E) E
    Scale(S, E) E
}
```

A group is parameterized by three things now. First, we
have the type of elements in the group, then we have the
type of scalars in the group, and then we need some
type which can manipulate the scalars.

Like with groups, the type implementing `Scalars[S]` will
be an empty struct, acting only as a vehicle for the
dictionary of methods.

We can then implement a generic method using these types:

```go
func twoTimesG[E, S any, SD Scalars[S], G Group[S, E, SD]](group G) E {
    scalars := group.Scalars()
    one := scalars.One()
    two := scalars.Add(one, one)
    return group.Scale(two, group.Generator())
}
```

I think this scales to a more complete description
of a Cryptographic group, with methods for deserialization,
conversion from ints, etc, but I've yet to actually
try writing a full package with this technique.

# Conclusion

This method is really a straightforward application of
some ideas from Haskell, but I expect this kind of technique
to show up in some shape or form, because the ability
to abstract over things like groups, and in general
algebraic structures, is quite useful. Even if you're not
doing math, there are many situations where something
of one type can be combined with itself to make
another thing of that same type.

I think the method I've described here should also be directly
useful to create a generic Elliptic Curve library for Cryptography,
which would be a worthwhile addition to the Go ecosystem.
When writing our Threshold Signature library, we wanted
to abstract over curves, instead of doing everything over
`secp256k1`, and I think this approach would be better than
the dynamic dispatch version we came up with. Having a standardized
solution to interoperate across different libraries would
be great as well.

I think this technique isn't the ultimate solution to this
problem though, and it's possible that people will find
more elegant ways of expressing the same concept in Go.
Maybe Go's generics will evolve more functionality over time;
who knows?
