---
title: "(Haskell in Haskell) 4. Simplification"
date: 2021-02-03T17:08:09+01:00
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

So, we want to extract out the type information strewn across
our program, but what about the values? We're not going to leave them untouched either.

As mentioned a few times in the last post, our syntax tree after parsing is very
close to the syntax of the language, and has quite a few redundant constructs. These constructs
exist because there is syntax for them, and not because they represent truly distinct
programs.

Another major goal in our simplifier is going to be trimming the fat on our syntax tree,
and simplifying away all of this redundancy.

### Let vs Where

The first bit of redundant syntax are `let` expressions and `where` expressions. Syntactically,
these are different, but they really express the same thing. We can represent
a `where` expression like:

```haskell
x + y
  where
    x = 2
    y = 3
```

using `let` instead:

```haskell
let x = 2
    y = 3
in x + y
```

By doing this throughout our syntax tree, we get rid of one superfluous constructor in
our tree, which simplifies our work throughout the remaining stages of our compiler.

### Lambdas

Syntactically, we allow lambdas with multiple parameters at once:

```haskell
\x y -> x + y
```

Our syntax tree represents this verbatim, as:

```haskell
LambdaExpr ["x", "y"] (BinExpr Add (NameExpr "x") (NameExpr "y"))
```

It's common knowledge that this snippet of Haskell is equivalent to:

```haskell
\x -> \y -> x + y
```

And we could have parsed our original expression as:

```haskell
LambdaExpr ["x"] (LambdaExpr ["y"] ...)
```

matching the second version. We've chosen to stick closer to the syntax in our parser,
but our simplifier will get rid of these variants, having lambda expressions only
take a _single_ parameter, using multiple nested lambdas for multiple parameters.

### Applications

In the same way that lambda expressions with multiple parameters can be represented
as a sequence of curried lambdas, function application with multiple parameters is
also sugar for curried application. For example:

```haskell
add 1 2
```

is syntax sugar for:

```haskell
(add 1) 2
```

Our simplifier is going to take care of this as well. In our parser, applications take
multiple parameters, but now we're going to always represent things as:

```haskell
Apply f e
```

where `f` is our function, and `e` our expression. For multiple parameters, we just use currying:

```haskell
Apply (Apply add 1) 2
```

{{<note>}}
I'm using a bit of pseudo-syntax here, but we'll precisely define the structure of our new AST
soon enough.
{{</note>}}

### Builtins

In real Haskell, you have custom operators, and this entails making operators actually functions.
So something like:

```haskell
1 + 2 + 3
```

is actually lingo for:

```haskell
(+) (((+) 1) 2) 3
```

(with the specific ordering dependent on the fixity of the operator, of course)

We've opted to instead hardcode the various operators we're going to be using. We'd represent
this same expression as:

```haskell
BinExpr Add (BinExpr Add 1 2) 3
```

having simplified the tree a bit.

Instead of using function application, we have a specific syntactical form
for operators. What we want to do in our simplifier is to shift this slightly,
to reuse function application:

```haskell
Apply (Builtin Add) (Apply (Builtin Add) 1 2) 3
```

Instead of having a special form for using operators, we now use function application,
and our operators become expression in their own right. We also need to apply currying here,
as mentioned in the previous step.

Doing things this way simplifies the next stages quite a bit. We don't have
this extra kind of expression to deal with. For example, in the typechecker, we just
treat `Builtin Add` as an expression of type `Int -> Int -> Int`, as if it were a named
function that happened to be defined.

This is in some sense arriving at the same point Haskell does, where after parsing, operators
become the application of certain functions. The difference is that we've hardcoded certain
functions as existing, and these functions are now _builtin_ to the compiler.

## Multiple Function Heads

So, there are quite a few things we can simplify about _expressions_, but we also
have a lot on our plate when it comes to definitions. As mentioned before, the definition
for a given value can be spread out over multiple places:

```haskell
add :: Int -> Int -> Int
add 0 0 = 0
add n m = n + m
```

In this example, the definition of `add` is separated across 3 different elements.
We have one type annotation:

```haskell
add :: Int -> Int -> Int
```

one definition using pattern matching:

```haskell
add 0 0 = 0
```

and a second definition using pattern matching, but with simple name patterns:

```haskell
add n m = n + m
```

Our first order of duty is going to be to somehow unify these three different items
into a _single_ definition. We want each value definition in our program to correspond
to a single item in our simplified tree.

To accomplish this we need to first gather all of the definitions of a value, along with
its type annotation. We can do a few integrity checks here, like making sure that each
definition has the same number of arguments, that multiple type annotations exist, etc.
Then we simplify this collection into a single definition, which requires a couple
of things.

### Creating Functions

There are multiple ways of defining a function like `add` above:

```haskell
add n m = n + m

add n = \n -> n + m

add = \n -> \m -> n + m
```

All of these are equivalent. The first definition is really syntax sugar for the last
definition. What we want to do is transform functions declaring parameters
like this to bindings of a name to a simple lambda instead.

### Multi Cases

The last change was pretty simple, but this one is a bit more complicated.
The idea is that in the same way that we transform simple names on the left side
of `=` into lambdas on the right side, we also need to transform patterns like this:

```haskell
inc 0 = 1
inc x = x + 1
```

into lambdas and case expressions:

```haskell
inc = \$0 -> case $0 of
  0 -> 1
  x -> x = 1
```

This is straightforward so far, and easy to see. The problem comes with how
functions can match on _multiple values_. This isn't possible with case expressions.

{{<note>}}
One solution to this problem is to simply create a case construction allowing
us to match on multiple values at once, punting the problem this poses until
after the type-checker.

I opted against doing this, because by tackling this complexity _now_ it makes the type-checker
simpler as well.
{{</note>}}

For example, take our ongoing example:

```haskell
add 0 0 = 0
add n m = n + m
```

If we try and desugar this as we did previously, we have a problem:

```haskell
add = \$0 -> \$1 -> case ? of
```

We can't simply move each of the patterns we had previously, since we have no way
matching against both values at the same time. What we need to do is figure
out a way of nesting our case expressions. In this case, there's a pretty
simple solution:

```haskell
add = \$0 -> \$1 ->
  case $0 of
    0 -> case $1 of
      0 -> 0
      m -> $0 + m
    n -> n + $1
```

It's not too hard to translate things like this manually. The difficult
comes from doing this _automatically_. We want to write
an algorithm to perform conversions like this.

## Simplifying Cases

Another detail we'll be tackling at the same time is removing _nested_
cases completely. Right now, we can do things like this:

```haskell
data List a = Cons a (List a) | Nil

drop3 :: List a -> List a
drop3 = \xs -> case xs of
  Cons _ (Cons _ (Cons _ rest)) -> rest
  _ -> Nil
```

This function drops off 3 elements from the front of a list.
To do this, we need to use _multiple_ nested patterns, in order to
match against the tail of different lists.

In fact, we allow _arbitrary_ patterns to be used inside of a constructor.

We're going to be simplifying case expressions like this, to only
use simple patterns inside of constructors. One way of simplifying
this would be:

```haskell
drop3 = \xs -> case xs of
  Nil -> Nil
  Cons _ $0 -> case $0 of
    Nil -> Nil
    Cons _ $1 -> case $1 of
      Nil -> Nil
      Cons _ rest -> rest
```

Once again, we can do this by hand without all that much effort,
but the trickiness comes from being able to do this *automatically*.

In fact, we'll be felling two birds with one stone here. We'll write an algorithm
that converts multiple nested pattern matching, into single pattern matching
without any nesting. This way, we can tackle the problems of patterns in functions,
and simplifying normal case expressions with the same tool.

{{<note>}}
We do this not out of necessity, but because we need to do
this _eventually_, and getting this out of the way now is convenient,
since we already need to apply a simplification algorithm to our cases,
to remove multiple pattern matching in functions.

Doing this now allows us to simplify the type-checker. Otherwise, we'd
have to do all of this afterwards.
{{</note>}}

# Creating a Stub Simplifier

As usual for this series, we're going to start by creating a dummy module for our Simplifier,
and integrating it with the rest of the compiler, before coming back and slowly
filling out its various components.

Let's go ahead and add a `Simplifier` module to our `.cabal` file:

```txt
  exposed-modules:     Ourlude
                     , Lexer
                     , Parser
                     , Simplifier
                     , Types
```

Next, let's create our actual module in `src/Simplifier.hs`


```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Simplifier
  ( AST (..),
    SimplifierError (..),
    simplifier,
  )
where

import Ourlude
import qualified Parser as P
```

We have the standard import for our Prelude, of course,
and a qualified import to be able to easily access everything
in our parser stage. We'll be expanding this with other
imports as necessary.

Except for `FlexibleContexts`, we've seen all of these extensions before.

We'll be using `FlexibleContexts` to work with some typeclasses that take
multiple types, namely `MonadError`. This extension allows us to write constraints like:

```haskell
doSomething :: MonadError Concrete generic => ...
```

In this example, we use a concrete type inside of a constraint, which is enabled
by `FlexibleContexts`.

`AST` is going to be the simplified syntax tree eventually, but we can just create
a stub value for now:

```haskell
data AST t = AST deriving (Show)
```

The parameter `t` will eventually be used to hold the kind
of type stored in the AST. This isn't used right now,
but will be soon enough.

We'll be needing a type for the errors emitted by this stage,
so let's go ahead and create the `SimplifierError` type as well:

```haskell
data SimplifierError = NotImplementedYet deriving (Eq, Show)
```

This is just a stub type for now, indicating that we haven't
implemented this stage yet. We'll be coming back to this
type and actually adding useful errors later.

The last thing we'll be adding for now is our stage function:

```haskell
simplifier :: P.AST -> Either SimplifierError (AST ())
simplifier _ = Left NotImplementedYet
```

For now, we just report an error, since we haven't implemented
anything yet. Note how we use `AST ()` here. This is because
after the simplification stage, we haven't done any type inference,
so we use the type `()` for the types in our AST. The type-checker
will be filling these slots with something useful, but that's
for the next part.

## Adding a new Stage

With this stub stage defined, we can go ahead and integrate
it with the rest of our CLI.

Inside of `Main.hs`, let's import our newly created module:

```haskell
import qualified Simplifier
```

Let's now create a stage object for the Simplifier:

```haskell
simplifierStage :: Stage Parser.AST (Simplifier.AST ())
simplifierStage = makeStage "Simplifier" Simplifier.simplifier
```

Finally, let's modify `readStage` to include this stage as
an additional option:

```haskell
readStage :: String -> Maybe String -> Maybe (String -> IO ())
readStage "lex" _ =
  lexerStage |> printStage |> Just
readStage "parse" _ =
  lexerStage >-> parserStage |> printStage |> Just
readStage "simplify" _ =
  lexerStage >-> parserStage >-> simplifierStage |> printStage |> Just
```

We can now run this new command in the terminal:

```txt
$ cabal run haskell-in-haskell -- simplify foo.hs
Up to date
Simplifier Error:
NotImplementedYet
```

As expected, we immediately report an error in our simplifier stage.
To fix this, we need to actually implement our simplifier stage!

# Defining Type Information

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
