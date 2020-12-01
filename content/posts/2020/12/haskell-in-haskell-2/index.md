---
title: "(Haskell in Haskell) 2. Lexing"
date: 2020-11-24
draft: true
tags:
  - Haskell
  - Programming Languages
  - Compiler
---

This is the second post in the [Haskell in Haskell](/series/haskell-in-haskell) series.

In this post, we'll go over creating a *lexer* for our subset of Haskell. This stage
of the compiler goes from the raw characters in our a source file, to a more
structured stream of *tokens*.

We'll really be diving into some code this time, so get your editor ready!
<!--more-->

# 1 mile overview

Before we go into the code for this part, let's first get an idea of
what it is we're going to be doing in this part. What job
is the lexing phase trying to accomplish? What is it responsible for?

## Making Tokens

The first goal of the lexer is to take a our raw source code,
which is nothing more than a blob of characters (A `String`, in Haskell),
and figure out how to cluster these characters to make the Parser's job
more useful.

As a reminder, the Parser is going to take our tokens, and then produce
a complete representation of the source code, in a tree-like structure.
The Parser *could* work directly on characters, but this would make
it more complicated than necessary. Having a separate lexing phase
gives us a kind of separation of concerns here.

The Parser will be needing quite a bit of context to do its job.
For example, when parsing an expression, it knows not to try
and read out things related to types. On the other hand,
the lexer (at least the part that spits out tokens) can
be made to work *without* context.

The lexer just reads in characters, and spits out tokens. After spitting
out a token, the lexer is in the exact same "state" that it started with.
The lexer doesn't, or at least *shouldn't* have any knowledge about context.

## What tokens?

So far we've talked about "tokens", or "clusters". A good analogy is that
our source code is like a sentence in English: the sentence is composed
of raw characters, ditto for source code. The words in the sentence make
up the *tokens* in our source code. And finally, the parse tree gives us
the meaning of the sentence, or the source code.

So, we want to split up our source code into the tokens that compose it.
What kind of tokens will we end up using?

As an example, consider this simple definition:

```haskell
myDefinition = 232 + y2
```

In terms of raw characters, we have:

```txt
m y D e f i n i t i o n  =  2 3 2  +  y  2
```

In terms of tokens, we'd like to end up with:

```txt
myDefinition
=
232
+
y2
```

Here, we've first separated out an *identifier*, that must be taken as a whole.
It makes no sense to process `myDefinition` as anything but a single unit
in the parser. We also have two operators, `=`, and `+`. Naming these helps
make our parser a bit more abstract. We also have a *literal*, `232`. It
also doesn't make sense to treat a literal as anything but a unit.

We'll be going over the details of what tokens we'd like to define, and how
exactly we're going to go about lexing them out of our source code later.
But keep this idea in mind: we're trying to separate out our source
code into little *units*, all to make our Parser much simpler.

## Generating Whitespace

Our lexer will also have a job much more specific to Haskell.
Haskell has one critical feature that makes parsing it a bit tricky:
whitespace.

We're quite used to the whitespace sensitivity as Haskell programmers. Most
programming languages nowadays don't even require semicolons, even
if they do still use braces for layout. On the other hand, most people are
oblivious, at least at first, to how Haskell actually uses braces and semicolons out of the hood.

For example

```haskell
let { x = 2; y = 3 } in x + y
```

Is a perfectly valid expression.
However, most people would write this as:

```haskell
let x = 2
    y = 3
in x + y
```

and they may not even be *aware* that you could use the braces and semicolons instead.
In fact, not only is the braced version an *option*, but the whitespaced
version is nothing more than *syntax sugar* for this way of doing things.

When Haskell sees:

```haskell
let x = 2
    y = 3
in x + y
```

It understands that you *really* mean:

```haskell
let {
  x = 2;
  y = 3
}
in x + y
```

and infers those pesky braces and semicolons accordingly.

In fact, when you have multiple definitions at the top level, like:

```haskell
x = 2
y = 3
z = 4
```

This is also *lingo* for a braced version:

```haskell
{
x = 2;
y = 3;
z = 4
}
```

As hinted by this way of explaining things, what we're going
to be doing in our compiler is adding an extra *mini-stage* that
is going to look at the whitespace in our program, and insert
the correct semicolons and braces to reflect the meaning of the programmer's
indentation.

We could do this as part of the parser, but this would make the parser *much more complicated*
than it otherwise needs to be. Doing this as an extra step between the more traditional
lexer and the parser makes things much cleaner, and much more maintainable.

If you don't understand exactly what constructs end up using
indentation to insert braces and semicolons, that's normal: I haven't explained it yet :).
We'll be going over this whole business in much more detail when it comes time
to *implement* this system, so that it's fresh in our heads.

# Our First Stage

So, that's the overview of what we're going to be accomplishing
in this part, let's actually start the scaffolding for what we want to do.

## A trivial Lexer

The first thing we want to do is to create a trivial lexer. This will
just serve as a stubholder so that we can create all of the code
to run the lexer when our program is called with the right arguments.
We're going to do that now, that way we can just focus on the lexer in the rest
of this post.

Anyhow, we need a `Lexer` module, so let's add that to our `exposed-modules`
in our `.cabal` file:

```txt
library
  ...
  exposed-modules: Ourlude
                 , Lexer
  ...
```

And then, let's create a file for this module, in `src/Lexer.hs`:

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Lexer (Token(..), lexer) where

import Ourlude
```

We're using two *extensions* here to add some extra features to the code in this module.

The first [LambdaCase](https://typeclasses.com/ghc/lambda-case), allows us to
replace expressions like:

```haskell
\x -> case x of
  Foo -> ...
  Bar -> ...
```

with:

```haskell
\case
  Foo -> ...
  Bar -> ...
```

which explains the naming of this feature.

The second, [TupleSections](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#tuplesections)
allows us to use partially apply *tuples*, in the same way you can do operators.
For example, you can do:

```haskell
map (,3) ["A", "B"]
[("A", 3), ("B", 3)]
```

Without the extension, you'd have to write the less terse: `\x -> (x, 3)` instead.

Both of these extensions are included in this module because they simplify some code here
and there. `LambdaCase`, in particular, will even be used pretty heavily throughout
the project.

We also have `import Ourlude`, in order to import the custom prelude
we defined [last time](/posts/2020/11/haskell-in-haskell-1/).

Now, let's create a "stub" implementation of our lexer:

```haskell
data LexerError
  = UnimplementedError
  deriving (Eq, Show)

data Token = NoTokensYet

lexer :: String -> Either LexerError [Token]
lexer _ = Left UnimplementedError
```

These are the types that our lexer will eventually end up producing. Given a string,
containing the source code we need to lex, we should either produce an
error, of type `LexerError`, or the list of tokens we managed to lex.

Right now, we just have a dummy token type, and an error indicating that our lexer
has not been implemented yet.

## A Stage Abstraction

Before we actually implement these functions, we're first going to integrate the
lexer into our command line program, so that the user can run it on an actual file.
We're going to go ahead and write some boilerplate we can reuse for the other stages
of our compiler as well. We can get this out of the way now, to focus on the guts
of the compiler as we move forward.

What we'd like is a bit of an abstraction over the concept of a *stage*.
Our compiler will be composed of various stages, such as the lexer,
the parser, etc. Each stage takes in some input, and produces some output,
potentially failing with an error. Our lexer takes in a `String` is input,
produces `[Token]` as output, and fails with `LexerError`. Our stage
abstraction should represent this concept of input, output, and errors.

Another thing is that we'd like to be able to produce some kind
of function, or `IO` procedure to execute, knowing up to which stage we want
to proceed. So if the user does `haskell-in-haskell lex foo.hs`, we know that
they want to stop and see the output of the lexing stage, whereas `haskell-in-haskell type foo.hs`
would mean stopping after the type-checking phase, etc.
We need to use our staging abstraction to construct some kind of `IO` action we can run,
and figure out which of these actions to run based on this first argument. So if we see `lex`,
we run `lexAction`, if we see `type` we run `typeAction`, etc.

The first thing we need is some kind of opaque error type. We want this so that our
stage abstraction doesn't have to keep track of the error type, for one, and also
so that we can annotate the errors produced by that stage with some metadata, like
the name of that stage.

So, in `Main.hs`, let's create:

```haskell
data StagedError = StagedError String String
```

The two string arguments are a bit of a shame, and easy to be confused, but
the first will be the name of the stage, and the second will be a string representation
of the error that the stage produced.

We want to be able to transform `Either ThisStageError ThisStageInput` to use this
error convention, so let's create:

```haskell
stageEither :: Show e => String -> Either e a -> Either StagedError a
stageEither name m = case m of
  Left e -> Left (StagedError name (show e))
  Right a -> Right a
```

Given a name (which is presumably that of a given stage), we can massage the
error into a `StagedError` by showing the error, and then putting that
along with the name together.

We also need to print this `StagedError`. First, we need to add an imports for pretty printing:
```haskell
import Text.Pretty.Simple (pPrint, pPrintString)
```

This uses the library we mentioned
[last time](/posts/2020/11/haskell-in-haskell-1/)
, which can format any given string in a nice way.

We can then use it like so:

```haskell
printStagedError :: IO ()
printStagedError (StagedError name err) = do
  putStr name
  putStrLn " Error:"
  pPrintString err
```

So we print `<Stage> Error:`, and then the nice pretty printed error.

With this in hand, we can create our stage abstraction:

```haskell
data Stage i o = Stage
  { name :: String,
    runStage :: i -> Either StagedError o
  }
```

So, `Stage` is parameterized over an input type `i`, and an output type `o`.
We use this to create a function taking input `i`, and producing output `o`,
potentially failing with a `StagedError`. We also have the name of the stage
as an extra bit of metadata.

# How do you make a Lexer?

## Character Classes

## Longest Match

# Lexer Combinators

## The Basic Definition

### Functor

### Applicative

## Alternative

# Implementing the Lexer

- Go over the basic structure

# Handling Whitespace

## Haskell's layout structure

## Position Information

## An Imperative Layout algorithm

# Some Examples

# Conclusion

# References
