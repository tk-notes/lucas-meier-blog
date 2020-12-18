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

Alright, now it's time to start writing some code. Before we start definiting the structure
of our syntax tree, or writing the parser itself, we first need to write our tool
of choice for making parsers: the *Parser Combinator*! This is going to be a suped up
version of the *Lexer Combinator* tool we made last time, extended to handle
recursive structures!

Let's first create a module for our parser in our cabal file:

```haskell
  exposed-modules:     Ourlude
                     , Lexer
                     , Parser
```

Now let's go setup the standard module and imports in `src/Parser.hs`:

```haskell
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative (..), liftA2)
import Data.List (foldl')
import Lexer (Token (..))
import Ourlude
```

We have a `LambdaCase` as a convenient extension in this module. We also import
`Alternative`, like for Lexer Combinators, as well as some other utilities.

## From Lexer Combinator to Parser Combinator

With Lexer Combinators, we had something like `input -> Maybe (a, input)`. So we take in some input,
and then either fail, or return a value, and the remaining input. This works well for lexing, since
a lexer can only produce result, and it's also obvious which of two options we need to choose
at any point: use the longest match rule.

For parsing, on the other hand, our structure is much more complicated, so it's not always
obvious that the longest match rule will be correct. What we do instead is that in the presence of
ambiguity, we just let it happen. Instead of either failing, or returning a single way of consuming
the input, we return all possible ways to proceed forward. So, we have:

```haskell
input -> [(a, input)]
```

Now, by the time we've parsed the entire source code, there should be only a single element
in this list. Otherwise, it means there's multiple ways of parsing some code. For example,
a parser that doesn't handle operator precedence might see `2 + 3 * 4` and produce both:

```haskell
(2 + 3) * 4
2 + (3 * 4)
```

Of course, the second option is correct, because `*` has *higher precedence* than `+`, as we'll
see later.

Ambiguity in the parsing process is fine, as long as we resolve it later. We might need more than
one token to resolve this ambiguity, which is ok. As a chief example, take *where expressions*.
These are expressions with the definitions coming *after* the body they're used in:

```haskell
x + y
  where
    x = 2
    y = 3
```

If in our parser for expressions, we have some rule like:

```haskell
expr = notWhereExpr <|> whereExpr
```

Then we'll have some ambiguity here. This is because we can't tell which of these parsers is correct
until we see the `where` token. Because of this, for the first 3 tokens,
we'll have multiple options floating around.

{{<note>}}
It's possible to rearrange how you decompose your parser to avoid unbounded ambiguity
like this. For example, you could always parse an expression, and then changing the meaning
of what you parsed previously based on whether or not you see a where. We won't be needing
these kinds of tricks, but they can make parsing more efficient.
{{</note>}}

By the time we finish parsing the file, we shouldn't have any ambiguity left. If we *do*,
then that means that there are multiple ways of interpreting some code. This is basically
never a problem in the code itself, but a problem in the implementation of the language.
There are certain languages that are inherently ambiguous, like C++, in which case
the parser needs to make an arbitrary choice, but Haskell is not ambiguous. If we
fail to parse something because of ambiguity, that's probably a *bug* in our parser.

### The Concrete Type

With all of this in mind, we can go ahead and define a concrete type for `Parser`:

```haskell
newtype Parser a = Parser {runParser :: [Token] -> [(a, [Token])]}
```

This is very similar to our `Lexer` type from the last part. Instead of working over `String`,
we know work over `[Token]`, a list of tokens, instead. And instead of either failing,
or returning a result, we instead return multiple possible ways of parsing.

### Example Building Blocks

To get a bit more familiar with how this works, let's go ahead and make a couple basic
helpers.

First, a little helper that accepts a single token:

```haskell
satisifies :: (Token -> Bool) -> Parser Token
satisifies p =
  Parser <| \case
    t : ts | p t -> [(t, ts)]
    _ -> []
```

This is analogous to `Lexer.satisfies`, which accepted a single *character*.
We look at the remaining list of tokens, and if we can pull out a token matching
our predicate, we return that token, along with the remaining input. Otherwise
we return an empty list, since there's now way for this parser to succeed.

A similar function is going to be `pluck`:

```haskell
pluck :: (Token -> Maybe a) -> Parser a
```

The behavior is going to be the same as `satisfies`, except now we return a value
if the predicate matches. This is useful, because we have a few tokens like
`IntLit i` and `StringLit s` which also have an argument, and this helper
lets us match against that token, extracting the argument. For example, we can do:

```haskell
pluck <| \case
  IntLit i -> Just i
  _ -> Nothing
```

To get a parser that accepts an int literal, but produces an `Int`.

The implementation of `pluck` is similar to `satisfies`:

```haskell
pluck :: (Token -> Maybe a) -> Parser a
pluck f =
  Parser <| \case
    t : ts -> case f t of
      Just res -> [(res, ts)]
      _ -> []
    _ -> []
```

`Just` replaces the `True` case we had previously, but also provides
us with the value we want to return.

## Functor and Applicative

Like with `Lexer`, we'll be providing instances of `Functor` and `Applicative`
for `Parser`.

For `Functor`, we have:

```haskell
instance Functor Parser where
  fmap f (Parser p) = Parser (p >>> fmap (first f))
```

This just maps over the whatever results we might have produced. In fact,
this definitions is *exactly* the same as for `Lexer`, since `Maybe`
and `[]` are both `Functor`s!

For `Applicative`, the operations will mean the same thing as
for `Lexer`. `pure :: a -> Parser a` will give us a parser that always succeeds,
consuming no input, and `(<*>) :: Parser (a -> b) -> Parser a -> Parser b` will
give us a parser that runs the first parser, and then runs the second parser
using the function produced by the first with the argument produced by the second.
The difference with `Lexer` is that now a parser may return *multiple* results.
What we do here is to simply combine *all the combinations* of first and second results:

```haskell
instance Applicative Parser where
  pure a = Parser (\input -> [(a, input)])
  Parser pF <*> Parser pA =
    Parser <| \input -> do
      (f, rest) <- lF input
      (a, s) <- lA rest
      return (f a, s) 
```

`pure` does exactly what we said it does before, so it's not too surprising.

What's neat with `<*>` is that to implement the "combine all possible functions and arguments",
we just the fact that `[]` implements the `Monad` class in the same way that `Maybe` does,
allowing us to use `do` notation. For `[]`, instead of allowing us to handle failure,
we instead are able to handle "non-determinism". What ends up happening is
that for each possible pair `(f, rest)` of a function, and remaining input,
we return each possible way of using the second parser on that input, and then
applying the function on the argument produced.

In practice, most of the time there won't be any ambiguity, so:

```haskell
parser1 *> parser2
```

will simply mean "run `parser1`, and then run `parser2`"

On the other hand, if we have:

```haskell
(parser1A <|> parser1B) *> (parser2A <|> parser2B)
```

then we also have `2 * 2 = 4` paths available to us. We can parse `1A` and then `2A`,
`1A` and then `2B`, etc.

## Alternative

As you might expect, we'll also be implementing `Alternative` for `Parser`,
to get that sweet sweet `<|>` operator, and all the goodies that come along with it!
We also need an `empty :: Parser a` function, which returns a `Parser` that always fails.

```haskell
instance Alternative Parser where
  empty = Parser (const [])
  Parser pA <|> Parser pB =
    Parser <| \input -> pA input ++ pB input
```

So, to get a parser that always fails, we simply return no valid ways that we
can continue parsing.

For `<|>`, we run both parsers, and then combine both of their results using `++`.
Unlike `Lexer`, we don't try to resolve any ambiguity if they both succeed.
As explained earlier, we let the ambiguity exist, by returning all possible
ways of parsing. Our final parser should only end up returning one result though,
even if sub parsers might temporarily have some abiguity.

Like last time, the `Alternative` instance gives us plenty of goodies for free.
For example, we now have `many :: Parser a -> Parser [a]`, which allows
to turn parser for one item, into a parser for many items, placed one after the other.
There's also `some`, which does the same thing, but doesn't requires at least one
item.

So if `intParser` accepts `3`, then `some intParser` would accept `3 4 5 6`, producing
`[3, 4, 5, 6]` as our output.

# Integrating a Parser

We now have our combinator framework we need to build our parser. Before we
actually start working on parsing our subset of Haskell, let's go ahead and
get the boilerplate of adding a new stage to our compiler out of the way,
so we can focus on parsing in the rest of this post.

Let's go ahead and define a stub type for our syntax tree, and for our errors:

```haskell
data ParseError = UnimplementedError deriving (Show)

data AST = AST deriving (Show)
```

Like last time, we just have a single error: the error telling
users that are parser is not implemented yet!

Our `AST` is just a stub data type as well.

We'll be completing both of these soon enough, but for now, we just want
to get this stage of the compiler integrated into the command line program.

We also need to write a simple `parser` function that we can expose
as our implementation of this stage:

```haskell
parser :: [Token] -> Either ParseError AST
parser _ = Left UnimplementedError
```

This function will take in the list of tokens produced by the lexer,
and return either the parsed syntax tree, or an error, indicating
that parsing failed.

Finally, we can add the `AST` type and the `parser` function to the the
exports for this module:

```haskell
module Parser (AST(..), parser)
```

{{<note>}}
Right now, we don't need to export the constructors of `AST` in order
to make the stage. The stages we'll add in the next parts will be working
with the details of the syntax tree, so we will be needing to export
its constructors, as well as the other data types we'll add in this part.
{{</note>}}

## Adding a new Stage

We now need to go into `Main.hs`, and add a `Stage` for the parser. First,
let's add the `Parser` module to our list of imports.

```haskell
-- ...
import qualified Parser
-- ...
```

Right next to `lexerStage`, we can go ahead and add a new `Stage` for our parser:

```haskell
parserStage :: Stage [Lexer.Token] Parser.AST
parserStage = makeStage "Parser" Parser.parser
```

The definition of this is essentially the same as we had for `lexerStage`.
Stages were introduced in great detail
[last time](/posts/2020/12/haskell-in-haskell-2/#stages-the-idea),
so feel free to take a quick refresher if you'd like.

We also need to modify our argument parsing to accept "parse" as a stage.
This way, users can do:

```txt
haskell-in-haskell parse file.hs
```

in order to print out the AST they get from parsing `file.hs`.

Of course, they'll still be able to do:

```txt
haskell-in-haskell lex file.hs
```

to just print out the tokens they get after lexing `file.hs`.

All we need to do is modify `readStage`, giving us:

```haskell
readStage :: String -> Maybe (String -> IO ())
readStage "lex" =
  lexerStage |> printStage |> Just
readStage "parse" =
  lexerStage >-> parserStage |> printStage |> Just
readStage _ = Nothing
```

Here we get to use the fancy `>->` operator we defined last time, in order
to compose the lexing stage with the parsing stage. This works out nicely,
since the parsing stage consumes the tokens that the lexing stage produced.
In the end, we end up with a stage taking in a `String` for the source code,
and producing an `AST`, that we want to print out.

We can now run all of this, and see something like:

```haskell
> cabal run haskell-in-haskell -- parse foo.hs
Parser Error:
UnimplementedError
```

which makes sense, since we haven't implemented the parser yet. Let's
fix that!

# Structure of the AST

## Custom Types

## Haskell Definitions

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

