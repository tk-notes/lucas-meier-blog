---
title: "(Haskell in Haskell) 4. Simplification"
date: 2021-01-11T11:04:03+01:00
draft: true
tags:
  - Haskell
  - Programming Languages
  - Compiler
---

In this post we'll be writing a **simplifier** for our subset of Haskell.
We'll be transforming and trimming down our AST to make it more suitable for type-checking,
and easier to convert to lower-level representations.

<!--more-->

So far, we've mainly been working on what's called the _frontend_ of our compiler.
This goes from the source code that the user deals with, and gives us a representation
of that code we can work with. Now we're starting work on the middle part of our compiler,
which analyzes and transforms this representation, preparing it for the _backend_,
which will generate the C so that our program can be executed.

We'll be going over a handful topics:

- What does our simplifier need to do anyways?
- What kind of information do we need for the types defined in our program?
- How do we resolve the type synonyms that the user created?
- What redundant parts of the syntax tree can be rewritten?
- How do we unify multiple function heads into simple case expressions?

By the end of this post, we'll have seen approaches to these questions and problems,
and created a compiler stage putting these ideas into practice.

# What our simplifier needs to do

Our main goal for this part is creating a _simplifier_. But what is
this simplifier trying to accomplish in the first place? There's no single
answer, because the simplifier is ultimately a stage that exists to tie a few loose
knots and tidy up a few things we've left open after the parsing stage.

Right now, after parsing, our representation of the code is very similar
to the source code we've just extracted it from. What we aim to do in this stage
is to transform this source level representation into something that's much easier
to work with for the stages that come after us.

We'll need to untangle type and value information, which are currently mixed in
our representation, in the same way that Haskell lets you define both types and values
in the same file. We'll also be streamlining our representation of values, simplifying
the various constructs in our language, and desugaring a lot of the syntax that
our parser previously left untouched.

We'll also be taking this opportunity to do some basic checks on the integrity
of our code, reporting different errors as we transform our syntax tree. These errors
are natural extensions of the kind of transformations we want to do.

## Type Information

As we've just gone over, Haskell mixes type information and value information,
and one goal of this stage is going to be to untangle the two parts.

Take a snippet of Haskell like this:

```haskell
type X = Int

data MyData = MyData X

x :: MyData
x = MyData 3
```

Here we have 4 items of interest:

1. The type synonym `X = Int`
2. A data definition creating a new type `MyData`
3. A type annotation for `x`
4. A value definition for `x`

Items 1 and 2 are squarely in the realm of types, and 4 in
the realm of values. Item 3 gives us a type for this value.

One inconvenient aspect of our current representation is that all of these kinds
of things are scattered throughout our source code, without any
clean separation or organization.

The first problem is that the potential type annotation for a value is separate from
that value. In fact, values can be declared multiple times, to allow us to provide pattern
matching, like:

```haskell
add 0 0 = 0
add n m = n + m
```

We'll want to unify all the definitions of `add` into one part of our syntax tree,
and bring it together with whatever type annotation it might have had.

We also don't really need to lug around the syntax for all the type information
the user is providing us. What we really want to do is to _distill_ all of this information
and only keep track of the parts we need for later.

### Type Synonyms

Type synonyms don't actually need to be kept around, if you think about it.
When we create a type synonym and use it:

```haskell
type X = Int

add :: X -> X -> X
```

what we're really saying is that this program is equivalent to:

```haskell
add :: Int -> Int -> Int
```

and the type synonym can be eliminated completely for later stages.

This applies equally even if that synonym is used inside of a data type:

```haskell
type X = Int

data MyData = MyData X
```

This program can be replaced with:

```haskell
data MyData = MyData Int
```

without changing its meaning at all.

So, we can really discard all of the type synonyms, but only _after_ we've replaced
all of their occurrences with a fully resolved type. We need to be careful here, since type synonyms
can reference other synonyms:

```haskell
type X = Y
type Y = Z
type Z = Int
```

We can't simply make a map `Synonym -> ResolvedType` by looking at each synonym in the file.
Instead we'll need to do a bit more work to figure out what this map looks like. Regardless,
we'll be using a map like this to eliminate all of the synonyms eventually, and we can discard
that map before the next stage, since every type will have been fully resolved.

{{<note>}}
If you want to provide nicer errors to the user, then you actually _do_ want to keep this map around,
so that you can reference the type synonms that a user provided when talking about errors.

For example, if you have:

```haskell
type X = Int

x :: X
x = "foo"
```

It'd be nice to say something like "couldn't match type `X` with `String`" than
"couldn't match type `Int` with `String`", since it's closer to the code the user provided.
{{</note>}}

### Constructor Information

The remaining bit of type information we need to keep track of are the
new data types defined by the user. When we have some data type like:

```haskell
data List a = Cons a (List a) | Nil
```

we'll then need to keep track of the two constructors:

```haskell
Nil :: List a
Cons :: a -> List a -> List a
```

We can't get rid of these just yet, since we need to use them for type checking in
the next stage. After all, we need to recognize bad usages, like:

```haskell
Cons 3 3
```

for example.

## Redundancy

### Let vs Where

### Lambdas

### Applications

### Builtins

## Multiple Function Heads

## Simplifying Cases

# Creating a Stub Simplifier

# Gathering Type Information

## Constructor Information

## Resolving Synonyms

## Using This

# Gathering Synonym Information in Theory

## Type Synonyms as a Graph

## Topological Sort

### Depth First Search

# Gathering Synonyms in practice

## Depth First Search in practice

# Gathering Constructors

# Finishing Type Information

# Simplifier changes to the AST

### Removing redundant forms

### Single Application

### Builtin functions

### No nested patterns

## Defining our new AST

# Simplifying patterns in theory

## The decision game

## Pattern matrices

## Explaining the basic algorithm

## Named patterns

# Simplifying Patterns in practice

## Basic types

## Basic Utilities

## Core algorithm

# Converting the AST

## Simple Changes

## Definitions

# Gluing things together

## Examples

# Conclusion
