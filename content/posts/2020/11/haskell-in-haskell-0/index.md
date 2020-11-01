---
title: "(Haskell in Haskell) 0. Introduction"
date: 2020-11-1
draft: false
tags:
  - Haskell
  - Programming Languages
  - Compiler
---

This is an introduction to a series I'm calling **Haskell in Haskell**.
The goal of this series of posts is to go through the implementation of
a subset of the Haskell language, from parsing, to typechecking, to code
generation, using Haskell itself.
<!--more-->

We'll be walking through an implementation of a compiler written in Haskell,
starting with an empty project, all the way up to generating `C` code
that we can compile into a functional executable. We'll be going over
*all* of the code that goes into the compiler, leaving no stone unturned.

The goal of the series is not to make
a reference compiler for the Haskell language, nor try to rival with GHC
in terms of compilation speed our output quality.
Instead, I want to provide a good introduction to the concepts that go into
writing a compiler, and provide an understandable bridge the world of
programming language implementations.

One great joy in programming is learning that *you too* can implement
some application that seems impenatrable. At first writing a compiler might
seem like a daunting task, but it's actually more approachable than you'd think.

A simple compiler is not going to rival the engineering effort behind
a production compiler like GHC. However, going from understanding
nothing about some domain to understanding how to implement the basics
is often the hardest part. From that point on, you're building on
solid foundations, and there's not a lot of mystery left.

I hope that this series can clear up some mysteries, and provide a clear
stepping stone towards the wonderful world of compilers.

# Why implement Haskell?

If you pick a compiler book off the shelf, you're going to be starting from an imperative language.
You'll go from something like C, and then end up with some kind of machine code. Ironically,
C will be the ending point of our journey, where we'll hand off our work to some other compiler.

## High Level languages

This book will focus on a much higher level of abstraction, instead of worrying about the intricacies
of code generation. Don't get me wrong, this *is* an interesting topic, but turning
imperative languages into good machine code is a much better studied topic, and has much
better books written about it than I ever could.

Our main focus will instead be on working with a higher level language, slowly peeling
off the functional bits until we can get to an imperative language, and make use of
the existing infrastructure for getting down to the machine level.

This allows us to spend more time on practical issues of transforming a realistic language,
although in very simple ways, as opposed to working with an example language, transformed in
difficult ways. The problems you encounter when transforming imperative code to machine code
can be quite tricky, and are quite a bit different from what you encounter when working
with a high level language.

## Lazy Languages

Laziness is also not referenced at all in traditional literature on compilers. Haskell is
basically the only language in use that's *lazy* by default. This is an interesting choice,
but leads to quite a few tricky problems in code generation.

We'll go over what laziness means exactly at a later point in this series, but the core idea
is that when you call a function, you don't evaluate all of the arguments first. Instead
Haskell will only end up evaluating those arguments when and if they actually
need to be inspected inside of the function.

Haskell will also not take the simple but incredibly inefficient call-by-value. This means
essentially passing the unevaluated AST as an argument, instead of an evaluated value. This
works, and is lazy, but will evaluate some arguments multiple times in certain situations.

Instead, Haskell implements a variant of this where we have *thunks* which only end up
being evaluated once. This system can make laziness surprisingly efficient. The implementation
of this system is not straightforward though.

Without laziness, Haskell would not be Haskell, so it's important to cover this often
forgotten topic in a series about a Haskell compiler.

# Why using Haskell?

Now, why go about using Haskell code to explain how to write the compiler? We could
use another dialect of ML, a more common language like Java, something new like Rust, etc.

## Recursivity

The first motivation is that it's fun to implement a language using itself! We can
look at the kind of code we've written in our compiler, and then imagine what our
compiler might do looking at itself! Unfortunately, our compiler *will not* be
able to compile itself at the end of this series. This would've been far too complicated
an undertaking, and even limiting the subset we use to *write* the compiler wouldn't have helped.

I think Haskell would be an excellent choice for any other compiler though. There are
a handful of features that make Haskell simply excellent in this domain.

## Algebraic Data Types

Haskell excels at representing syntax trees, which are basically the main kind of data structure
we're going to be working with, at every single stage of the compiler.

For example, here's a little DSL of sorts for some arithmetic expressions:

```haskell
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | IntLitt Int
```

Haskell allows us to plainly represent the different variants that make up our syntax tree,
and we can easily support a recursive data structure without even breaking a sweat.
Combine this with great support for recursion, and you almost have a programming language built
for working with syntax trees!

## Purity

Haskell encourages you to avoid side-effects in most of the functions you write, although
they still exist. We'll be writing side-effects to read the file we want to compile, of course!

A compiler is great domain for Haskell though, or any other pure language, because the vast
majority of it is internal logic! You do a bit of IO at the start to read a file, but then you're
just working with data structures all the way down. This makes compilers an area where Haskell
can excel!

# What subset of Haskell?

We're not going to be writing a compiler to rival GHC, as mentioned earlier. In fact,
we won't even be arriving at a complete subset of Haskell. The glaring omissions are:

- Modules
- Type Classes
- All the necessary primitives to make the Prelude work

Type Classes could easily have been added to the subset we're supporting. On the other hand,
I don't think they add that much complexity to the type checker, and the approach we would've
used to compile them would have removed them before we get to code generation, so it's in effect
orthogonal to the rest of the compiler.

As for Modules, these aren't very Haskellish, but add a very large amount of complexity, that's
well covered by other languages. Haskell's module system isn't very difficult in *depth* but it
does have quite a lot of *breadth*, and that breadth is not novel compared to how other languages
do things. I don't think spending a lot of time about cross-module name resolution and things like
that would've explained the things that make Haskell different to compile compared to other languages.

Finally, if you want to make a subset that conforms to the a Haskell specification, you need to add
a *lot* of things. I mean a **lot**. This would've been a lot of boilerplate implementations of
primitive types and functions, for not much pedagogical again. Once again, my goal here is to explain
the things that seem a bit mysterious about implementing Haskell. I think that implementing a full subset
is, for the most part, a straightforward extension of the foundation this series tries to cover.

One regret is that we're missing `IO`, which is definitely in the "mysterious" category, but it seems
difficult to do the concept justice without implementing a bunch of primitive things.

Anyways, here's an overview of things that we *will* be going over:

- Let expressions
- Where expressions
- Arithmetic operations (only on Int)
- Strings, and concatenation
- Conditional expressions
- if then else
- case expressions
- Multiple function heads with multiple patterns
- Algebraic Data Types
- Type Synonyms

# What does a Compiler do?

We've seen the langauge we'll want to be writing a compiler for, but what does
a compiler actually do?

I remember looking at compilers when I first started programming, and they seemed
like completely unapproachable black boxes! I had no idea where to even start to understanding
how they worked!

The goal of a compiler is take your source code, and output code at a lower level of abstraction.
Our compiler will take a Haskell file, and produce a C file. We can then use a standard C
compiler to combine this file, with the C code for our runtime, and get an executable. This executable
will implement the functionality described in our source code. We'll read in the source code
as a simple string, and will have to do all the work to go from that opaque data to our output.

## Stages

You could try iterating over the characters in your source code, and see if you can generate code
from that. This might even work for an extremely simple language, like arithmetic expressions
in postfix notation (something like `2 3 + 4 *`), but will not work for a language of the complexity
we're dealing with.

Our approach will instead work in stages. Each stage takes some input from the previous stage, and produces
some output. Every stage will also potentially produce *errors* representing some problem
in the *source code*, not the compiler. For example, the code might include some characters we don't know
how to process, or the code might fail to type check.

Combined together, these stages form a complete function from a string source code to our
final target code output.

Let's see what these stages are:

### Lexing

The goal of this stage is go from a raw source code string, to a series of tokens. So, we might
take some source code like `{ f :: Int; f = 3 }`, and produce something like:

```txt
{
f
::
Int
;
f
=
3
}
```

We correctly identify that `Int` is a single token, marking a primitive type, and `::` is an operator,
etc. We will accept programs that make very no sense syntactically however, like `f = = = = 4 = 4`.
It's not that this program fails to type check, it's that it doesn't even represent something we
know as a Haskell program. Our lexer is perfectly happen with these tokens though.

Another aspect of Haskell is *whitespace*. Haskell uses whitespace to automatically insert braces and semicolons.
In fact, many people don't even realize that Haskell accepts braces and semicolons at all! It's the lexer's
job to look at the tokens, along with where they appear wrt indentation and whitespace and insert the
appropriate braces and semicolons.

### Parsing

The parser's job is to take the tokens produced by the lexer, and try and get a *syntax tree* from it.
We'll have something like:

```haskell
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | IntLitt Int
```

except about 20 times more complicated. The parser will recognize that `2 + 2 * 4` corresponds to:

```haskell
Add (IntLitt 2) (Mul (IntLitt 2) (IntLitt 4))
```

This will be our first representation of the source code we've been given, and is where the fun *really* starts.

### Simplifying

Our parser accepts all the syntax we want, including redundancies. For example, `let` and `where` serve
exactly the same purpose. The goal of the simplifier is to desugar these different variants and niceties,
and give us a more slimmed down version of our syntax tree. This will make the next steps much easier.

One very complicated aspect of Haskell that we'll handle is multiple function heads. Basically, you can write
something like this in Haskell:

```haskell
f _ 4 = 7
f 3 _ = 4
f _ _ = 0
```

This should desugar to something like:

```haskell
f = \a -> \b ->
  case a of
    3 -> case b of
      _ -> 4
    _ -> case b of
      4 -> 7
      _ -> 0
```

which is quite different than the source code we started with!
This is both the trickiest, and most interesting part of the simplifier!
We could've eliminated the redundancy a bit more, but
that's a job for optimization, really.

### Type Checking

At this point we have a simplified representation of our source code, but we might still
have programs with errors. For example `3 + "foo"` is not valid in Haskell, because you cannot
add numbers to strings. The type checker's job is to find errors like this.

Haskell also has *type inference* this means that the type of arguments and variables aren't
explicitly declared, and can be inferred by the type-checker instead. The type-checker
will try to guess what the right type for some declaration should be, based on how that
declaration is used in different code.

For example, if I see `f 3`, I know that `f` must have a type that looks like `Int -> ?`,
and if I see `f 3 + 4`, I know that `f` must in fact have type `Int -> Int`, since it needs
to produce an `Int` after just a single `Int` argument.

The subset of Haskell we're going for is simple enough that we should be able to compile
programs with *no* type annotations whatsoever!

Our type-checker will make sure to liberally sprinkle in the type annotations that it has inferred,
to make subsequent passes easier.

Another job the type-checker has is to look at the custom data types declared in the program, and
build up a table of their properties, so that we can understand the code using those
data types. We'll need to make judicious use of this information when generating code
using these types, of course!

### Intermediate Representation

We now have an AST annotated with types, and information about the custom types
used in the program. We could try and generate code from this immediately, but
it's easier to first slim down the AST much more, down to the bare minimal usage of
different constructs.

We'll be essentially "compiling" our AST down to a language called "STG". This is essentially
a very very simple functional language. For example, patterns can only go one layer deep,
we need to explicitly declarations for function parameters, etc.

The core idea of STG is that the language is so simple that each part of it maps straightforwardly
to a piece of C code, making our code generation much simpler.

### Code Generation

And finally, given the ultra simple STG representation, we just need to convert the
various constructs we encounter into snippets of C. Most of the work
has already been done for us, by getting the source code down to this super simple representation.

One big chunk of work, not really in this phase per se, is the runtime. Haskell is going to need some
runtime support, namely for garbage collection, and other related things. Because of this, we'll also be
writing a little bit of C that we can use when generating code, and that we'll compile with if
we want to generate an actual executable.

# Conclusion

And that's about it! Stay tuned for updates! I hope you'll enjoy this series (I'm certainly having fun
writing it, so far), and I'll post the link to the first part here once I finish writing it. Right now
I have 4 parts queued up, in terms of written code, and I plan to try and release one part a week,
so expect cool stuff over the coming month.