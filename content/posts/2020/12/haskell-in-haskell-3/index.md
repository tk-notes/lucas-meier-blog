---
title: "(Haskell in Haskell) 3. Parsing"
date: 2020-12-16
draft: true
tags:
  - Haskell
  - Programming Languages
  - Compiler
---

This is the third post in the [Haskell in Haskell](/series/haskell-in-haskell) series.

In this post, we'll go over creating a _parser_ for our subset of Haskell.
This stage of the compiler is responsible for taking the tokens that our
lexer produced in the [previous part](/posts/haskell-in-haskell-3).

<!--more-->

Because of all the work we did in our last post, writing this parser will actually
be *simpler* than the lexer. We've already broken up our source code into
bite-size tokens, and used the whitespace to infer semicolons and braces. Because
of this, our parser's job is much simpler.

# Parsing in theory

Before we start writing out our parser in code, let's try and figure out
what a parser is supposed to do, and why we need a parser anyways.

The goal of parsing is to go from our source code, to a representation that we
can actually work with. We want to transform Haskell code like:

```haskell
foo = 2 + 3 * 4
```

into some kind of data structure that we can meaningfully. We'll be using this
data structure to make sure that the usage of types makes sense, and then
eventually turning the data structure into C code.

We've already written one chunk of this transformation: the lexer. The lexer
groups up this source code into small tokens. Each token the lexer produces wouldn't
make sense as separate chunks. Our parser needs to take these tokens, and then
analyze their structure to produce a data structure representing our code.

## Tokens are not enough

After the lexer, we already have a data structure: a list of tokens. Why not just use
this as our representation of source code?

This won't be very useful, because the tokens don't really reflect the structure of
our source code. Take this example:

```haskell
x = 2 + 2

y = 2
```

As a flat stream of tokens, we have:

```haskell
{ x = 2 + 2 ; y = 2 }
```

But, there's actually a nested nature to this code. A better way of looking at this would be:

```haskell
(
(x = (2 + 2))
(y = 2)
)
```

The expression `2 + 2` "belongs" to the definition of `x`, and the expression
`2` "belongs" to the definition of `y`. This kind of nested structure
is a bad fit for a *flat* data structure like a list of tokens.

## Trees

On the other hand, this kind of nested data structure is perfect for a tree.

This example:

```haskell
x = 2 + 2

y = 2
```

Can be represented by a tree that looks something like this:

{{<img "1.png">}}

Each node in the tree has some kind of label, and can contain other subtrees as its children.
We also have a node at the top level, which contains all the top level definitions. Definitions
nodes use an `=` here, and have the name as one child, and the expression as the other child.
Addition uses a `+` node, with the expression arguments as children.

In fact, this approach scales nice to expressions with even more nesting:

```haskell
x =
  let y = 2 + 2
  in y 
```

Here we have a two nested expressions for `x`, giving us a tree that looks like:

{{<img "2.png">}}

It's easy to imagine how this might work for even more complicated expressions, since
nodes can contain arbitrary trees as their children.

## Valid Syntax vs Valid Semantics

This tree of nodes built up from source code is called an **Abstract Syntax Tree**, or
an **AST**, for short. It's a syntax tree, since it represents the structure of
our programming language, i.e. what syntax is valid or not. I don't actually know why
it's called an *abstract* tree. I like to think that it's abstract, because it provides
a representation of trees that are *syntactically* valid, but that don't necessarily make any sense.

For example take something like:

```haskell
x =
  let y = "34" + 3
  in "x" foo
```

This bit of code is *syntactically valid*, and can be parsed just fine, but it has a few notable issues.

- You can't add strings and numbers
- You can't use a string like `"x"` as a function
- The name `foo` is not defined in that scope

Because of these issues, our program is *semantically invalid*, even though it manages to parse.
The goal of the *type checker* that we'll write soon enough is to find all of these issues.

But, it's not our parser's job to figure these things out yet. Our parser is just concerned
with producing this *Abstract* Syntax Tree, by transforming the list of tokens produced
by our lexer.

## Grammars

One common tool used to describe these syntax rules is a [Context Free Grammar](https://www.wikiwand.com/en/Context-free_grammar),
or just a *Grammar* for short. Grammars allow us to think about the structure
of a programming language, at least in terms of its surface syntax.

As a motivating example, let's take a very simplified version of our Haskell subset.

Our example language consists of a single expression, which might be one of:

A string literal:

```haskell
"foo"
```

An int literal:

```haskell
345
```

A variable name:

```haskell
x3
```

Or, a let expression, with explicit braces and semicolons:

```haskell
let {
  x = 3;
  y = 2;
} in x
```

(For simplicitly, we require a semicolon after each definition, instead of between them)

A Grammar for this language would like something like this:

```txt
<expr> ::= <int> | <string> | <name> | <letexpr>

<letexpr> ::= let { <definitions> } in <expr>

<definitions> ::= <singleDefinition> ; <definitions> | <empty>

<singleDefinition> ::= <name> = <expr>
```

Each `<rule>` references some type of syntactic element. So `<expr>` corresponds to
an *expression* in our little language. After the `::=`, we declare each
rule that lets us build up that element. Everything not between angle brackets
is an actual token we expect to see.

So, for `<expr>` this matches the overview we gave earlier: an expression is either
an integer literal, a string literal, a name, or a let expression.

And then `<letexpr>` consists of the token `let`, followed by some definitions, inside
braces, and the finally the token `in`, and an expression.

`<definitions>` is either empty, or a single definition, followed by a `;`,
and some more definitions.

Finally, `<singleDefinition>`, for things like `x = 3`, is just a name, the token `=`,
and then an expression.

Intuitively, this grammar allows us to describe in detail the rules that define
what structure our toy language actually has. These kinds of rules should actually
be familiar for us, since our lexer used a similar approach. For example, we had things like:

```haskell
token :: Lexer (Token, String)
token = keyword <|> operator <|> literal <|> name
```

in order to define what makes up a valid token. This matches up naturally with a grammar like:

```txt
<token> ::= <keyword> | <operator> | <literal> | <name>

<keyword> ::= let | do | ...

...
```

The big difference is that our tokens were completely *flat*. Tokens couldn't end up containing
other tokens. Grammars, on the other hand, are expressive enough to expressive recursive
structures. Our toy language itself has a recursive structure: expressions can contain let
expressions, which also contain expressions. Because of this, you have a kind of recursive nesting:

```haskell
let {
  x = 2
} in let {
  y = x
} in let {
  z = y
} in z
```

Grammars should also seem eerily familiar to you as Haskeller as well. In Haskell, we
have *algebraic data types*. For example, let's say we want a type for colors,
which are going to be either red, blue, green, or an arbitrary hex string like "#ABFE00".

We would represent this with an ADT like this:

```haskell
data Color = Red | Blue | Green | Hex String
```

But, if you squint a bit, this looks kind of like a grammar:

```txt
<color> ::= red | blue | green | # <string>
```

If we revisit the grammar for our toy language:

```txt
<expr> ::= <int> | <string> | <name> | <letexpr>

<letexpr> ::= let { <definitions> } in <expr>

<definitions> ::= <singleDefinition> ; <definitions> | <empty>

<singleDefinition> ::= <name> = <expr>
```

We can go in the other direction, and model this structure using Haskell types:

```haskell
data Expr
  = IntExpr Int
  | StringExpr String
  | NameExpr Name
  | LetExpr [Definition] Expr

data Definition = Definition Name Expr
```

There's not a one-to-one correspondance here, since the grammar is also concerned
with syntactic elements, like semicolons, and braces, and we don't care about those
here. But the rough structure is the same, especially in terms of how the data type
references itself recursively.

What's cool with Haskell is that because it has these nice recursive ADTs, we can represent
our syntax tree in a way that's very similar to the grammar that defines it. We won't be using
grammars directly, but what we'll be doing will be awfully similar. I also think it's important
to touch on the concept of grammars, at least briefly, since most of the other resources
you'll read about parsing will use them heavily.

# Parser Combinators

## From Lexer Combinator to Parser Combinator

**TODO**

Talk about why we need to upgrade from the previous part

## Functor and Applicative

**TODO**
Go over the functor and applicative instances

## Alternative

**TODO**

explain how the alternative instance works

# Adding a new stage

**TODO**

Add a toy stage / parser and AST

# Structure of the AST

## Haskell Definitions

## Custom Types

## Haskell Expressions

## Pattern Matching

# Parsing in Practice

## Basic Utilities

## Types

## Primitives

## The Operator Hierarchy

## Expressions

## Definitions

## Glueing it all together

# Examples

