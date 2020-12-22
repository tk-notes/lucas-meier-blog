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

An immediate application of this `satisfies` function is going to be:

```haskell
token :: Token -> Parser Token
token = (==) >>> satisfies
```

This just matches against a specific token that we provide.

A similar function to `satisfies` is going to be `pluck`:

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

Before we can write a parser to convert our tokens into a syntax
tree, we need to *define* the shape of that syntax tree. This is the part
where we explicitly define different structures representing everything
in our language. We're going to be describing the exact subset of Haskell
that we implement, by creating various data types for each construct
in the language.

For example, we're going to have a data type for *expressions*. One of
the variants will be a binary expression, for things like `1 + 2`,
or `4 * 33`. Another variant might be function application, for things like
`f 1 2 x`. There will, of course, be many other variants.

## A Rough Idea

Our subset of Haskell is cleaved into two very distinct parts:

1. The part of the language dealing with defining types
2. The part of the language dealing with defining values

These 2 parts interact through *declarations*, which allow
us to declare that a given value has a certain type.

For example, if you have:

```haskell
x :: Int -> Int
x = \x -> x + 1
```

The `Int -> Int` is squarely in the domain of types, and we'll need some data structures
to describe function types like `Int -> Int`. The lamdba expression `\x -> x + 1` is clearly
in the domain of values. And the declaration `x :: ...` is a point of interaction, of sorts. 

Because of this, we can think of our language as being roughly groupable
into 3 sections:

1. Types: describing them, definining them, creating synonyms
2. Expressions
3. Definitions: assigning types and expressions to names

There's a lot of interaction between these categories of course. For example,
`let x = 3 in x` is an expression that also uses a definition, inside
of the `let`. With that said, it's still useful to have a bit of categorization
in our head before we start defining everything we need.

## Types

So, the first thing we can do is define how we can work with types in this subset
of Haskell. We need ways to refer to certain types, like `Int`, `String`,
or more complicated things, like `(Int -> String) -> String`. We'll
also need ways to define new types, either through custom types, like
`data Color = Red | Blue | Green`, or type synoyms, like `type MyInt = Int`.

### The Fundamental types

Before we get to definitions of new types, let's first define our data structures
representing the basic ways of using types that exist. For example,
when you have `x :: Int -> Int`, you need some way of representing whatever
type comes after the `::`. This is what we'll be defining now.

In effect, what we're doing is defining what a type "is" in our subset of Haskell.
In fact, this definition will be useful *beyond* just parsing. We can also use
this representation of types of in *type checker*, for example. Because of this,
we won't be creating this definition of what at type is in the `Parser` module,
but instead creating a new module, in `src/Types.hs`, to hold fundamental data
structures for working with types. We'll expand on this module in further parts,
as we need to do more things with types.

{{<note>}}
There's an argument to be made for never sharing things like this between different
stages. It might be more maintainable to distinguish between the *syntax* of what
constitutes a type, and the *semantics* of what a type really is later on. Essentially,
the difference is that the former talks about what things the user can type in their
source code to tell your compiler about types, but the latter defines how the compiler
itself manipulates types.

For example, this approach allows you to add *syntax sugar* for different types:
maybe `A <-> B` is syntax sugar for `(A -> B, B -> A)` in your language. Your representation
of types later on doesn't have to deal with this extra construction if you separate
the syntactic types from the "actual" types.

We won't be using a separation like this for our types, but we will have
a *simplifier* phase for expressions. We'll be going over this phase next time.
It just turns out that for types, there's not really anything we can simplify,
so it's a bit more convenient to not bother with the extra boilerplate of having
two isomorphic representations.
{{</note>}}

As usual, let's edit our `.cabal` file to add the `Types` module:

```txt
  exposed-modules:     Ourlude
                     , ...
                     , Types
```

Now, we can go ahead and define this module in `src/Types.hs`:

```haskell
module Types where

import Ourlude
```

For now, we just export everything in the module, but we'll come back and make the
imports explicit once we've defined our representation of types.

First, let's think about what kind of types you should be able to work with in
our subset of Haskell. As mentioned already, we're going to have a primitive
`Int` type, which will be the standard integer type we're used to.
Integer literals will have that type, so `3 :: Int`, for example. We've already
lost the element of surprise by defining boolean and string literals in the lexer,
so we're going to also have *primitive* boolean and string types in this
subset. So we have `String` and `Bool` as builtin types as well.

{{<note>}}
Our subset of Haskell would be expressive enough to define these in the language itself:

```haskell
data Bool = True | False

data String = StringCons Char String | EmptyString
```

The reason I've opted to make these primitive instead is that to do this thing well,
you need some kind of module system, which is out of the scope for the subset
we're trying to make with this series.
{{</note>}}

In addition to builtin types, we need a way for users to define new types,
and then reference those types. so if a use has `type X = Int`, then
they should be able to say:

```haskell
y :: X
y = 3
```

So we need to reference custom types by *name*, as well.

Our subset will also have support for polymorphism. You can define functions like:

```haskell
id x = x
```

The type of this function is of course:

```haskell
id :: a -> a
```

where `a` is not a type *per se*, but rather a *type variable*. We need a way
to reference type variables by name as well.

This also means that custom data types can be polymorphic, and have variables of their
own. For example, you might define a list data type as:

```haskell
data List a = Cons a (List a) | Nil
```

This will be valid in our language, and introduces the type `List _`, where the underscore
needs to be filled in. So you might have `List Int` as one type,
`List String` as another, and `List a`, if `a` happens to be a type variable in scope.
In brief, custom types are referenced by name, and also have multiple variables.

Finally, we need a way to talk about the function type `A -> B`, which is also
built into the language.

With all of this in mind, let's just define our representation of types:

```haskell
type TypeVar = String

type TypeName = String

infixr 2 :->

data Type
  = StringT
  | IntT
  | BoolT
  | CustomType TypeName [Type]
  | TVar TypeVar
  | Type :-> Type
  deriving (Eq, Show)
```

So, we have the basic primitive types as hardcoded options:

- `Int` corresponds to `IntT` in this representation
- `String` corresponds to `StringT`
- `Bool` corresponds to `BoolT`

We have 2 type synonyms, to clarify the difference between `TypeVar`, which is a lower
case name, like `a` used for the name of a type variable, and `TypeName`, which
is an upper case name, like `List` or `Foo`, used to reference the name
of a newly defined type.

Type variables have the variant `TVar` and take a name as argument. So `a` in Haskell
would be `TVar "a"` in our representation.

For newly defined types, we have the name of that type, as well as a list of arguments,
which are also types, so `List Int` would be `CustomType "List" [IntT]` in this representation.

You can also nest things, so `List (List Int)` would become

```haskell
CustomType "List" [CustomType "List" [IntT]]
```

Note that type synonyms are also under this category. If you have `type X = Int`,
then you'd have `CustomType "X" []` to refer to this type synonym.

Finally, we need a way to represent function types. We create a new operator
`:->` for this. (We need the `:` so that it can be used in types). With this in place
`Int -> Int` becomes `IntT :-> IntT`, as expected. Note that we declare
this operator `infixr`, which means it's right associative, like `->` is in Haskell.
This means that:

```haskell
Int :-> StringT :-> BoolT
```

is parsed by GHC as:

```haskell
Int :-> (StringT :-> BoolT)
```

which is exactly the behavior that we want.

We'll be using this representation of types throughout the compiler. Let's go ahead and
explicitly export it as well:

```haskell
module Types
  ( Type (..),
    TypeVar,
    TypeName
  )
where
```

Now, let's use these types to start working on our syntax tree, in `src/Parser.hs`.

### Defining new types

The next thing we need to do is add the data structures representing the definitions
of new types in our language. There are going to be two ways to define new types:

1. Type Synonyms, things like `type X = Int`
2. Data Definitions, things like `data Animal = Cat | Dog`

In our language, these are both considered as 2 of the kinds of definitions
that can appear at the top level. They'll both be variants of a common
`Definition` type.

We'll be needing the items we just defined in the `Types` module inside of the parser,
so let's add an import in `src/Parser.hs`:

```haskell
...
import Ourlude
import Types (Type (..), TypeName, TypeVar)
```

First, let's define type synonyms.
Note that we don't allow polymorphic things like `type List2 a = List a`,
only `type ListInt = List Int`. Because of this, we can define type synonyms
as a simple variant of `Definition`:

```haskell
data Definition
  = TypeSynonym TypeName Type
```

A type synonym has a name, which is an upper case name like `X`, and a type that
it's equal to.

And then, we need to define the structure of data definitions. The basic idea
is that we have a name and some list of constructors:

```haskell
data MyType = VariantA | VariantB | VariantC
```

We also need the ability to have arguments in each of the constructors:

```haskell
data MyType = VariantA String | VariantB Int Int | VariantC
```

We can also make the data type *polymorphic*, by specifying some type variables
next to the name:

```haskell
data MyType a b = VariantA String | VariantB Int Int | VariantC
```

Of course, we can use these type variables inside of the variants:

```haskell
data MyType a b = VariantA String | VariantB Int Int | VariantC a b
```

And that's about it for new data types.

{{<note>}}
Syntactically speaking, nothing is stopping us from using type variables that aren't
even in scope, so something like `data Foo = Foo a b c` will parse just fine.

Of course, this makes no sense *semantically*, and will be caught by later stages.
{{</note>}}

Let's first define a structure for each of the constructors / variants:

```haskell
type ConstructorName = String

data ConstructorDefinition
  = ConstructorDefinition ConstructorName [Type]
  deriving (Eq, Show)
```

A constructor has a given name, like `VariantA`, or `Cons`, as well as a list
of types, which are the arguments for the constructor. For clarity,
we've created a separate synonym for this kind of name. For example,
the `VariantB` constructor in our `MyType` example would be represented as:

```haskell
ConstructorDefinition "VariantB" [IntT, IntT]
```

Now we can add an extra variant in `Definition` for data definitions:

```haskell
data Definition
  = DataDefinition TypeName [TypeVar] [ConstructorDefinition]
  | TypeSynonym TypeName Type
  deriving (Eq, Show)
```

A data definition has a name, a list of type variables, and then a list of constructors.
Our `MyType` example becomes:

```haskell
DataDefinition "MyType" ["a", "b"]
  [ ConstructorDefinition "VariantA" [StringT],
    ConstructorDefinition "VariantB" [IntT, IntT],
    ConstructorDefinition "VariantC" [TVar "a", TVar "b"]
  ]
```

in this representation.

And now we have a complete way of representing the syntax for defining new types!
We've already started getting a feel for the tree like structure we're using
to represent our language, and we're just getting started!

## Expressions

In this section, we'll be defining most of the different kinds of expressions in our language.
For `let` and `where` expressions, which use *definitions*, which we'll define
a bit later.

### Literals

This first kind of expression we have are *literals*. We have int literals,
like `2` and `245`, string literals, like `"foo"` and `"bar"`, and finally,
boolean literals, namely, `True` and `False`. We can define a type `Literal`
representing these:

```haskell
data Literal
  = IntLiteral Int
  | StringLiteral String
  | BoolLiteral Bool
  deriving (Eq, Ord, Show)
```

We have one type of literal for each of the primitive types in our language.

Let's also add a type of expression for literals:

```haskell
data Expr
  = LitExpr Literal
  deriving (Eq, Show)
```

As an example, the literal `3` becomes the much more verbose:

```haskell
LitExpr (IntLiteral 3)
```

### Names

We can also reference names inside of expressions. For example, if
we have some value `x :: Int`, we have `x + x` as an expression. We can also reference
the constructors for different data types, like `Nil`, `Red`, etc.

We'll be representing these names with strings, but for clarity, let's create a
type synonym:

```haskell
type Name = String
```

We can then use it to define a kind of expression that just use a name:

```haskell
data Expr
  ...
  | NameExpr Name
```

Semantically, the meaning of `NameExpr "foo"` is whatever expression
`"foo"` is bound to in a given scope. So if we have `foo = 3`, then
`NamExpr "foo"` should evaluate to `IntLiteral 3`, using our expression syntax.

### If Expressions

The next kind of expression are *if* expressions, things like:

```haskell
if y then 3 else 4

if 2 + 3 > 4 then 4 else x + x
```

These are straightforwardly defined as:

```haskell
data Expr
  ...
  | IfExpr Expr Expr Expr
```

We can have arbitrary expressions inside of the condition, if-branch,
and else-branch.

So, `if y then 3 else 4` becomes:

```haskell
IfExpr
  (NameExpr "y")
  (LitExpr (IntLiteral 3))
  (LitExpr (IntLiteral 4))
```

### Lambda Expressions

The next kind of expression are *lambda* expressions, allowing us
to define anonymous functions. These look like:

```haskell
\x -> x + 1

\x -> \y -> x + y
```

Note that in Haskell, you can define multiple names at once in a lambda. For example:

```haskell
\x y -> x + y
```

is valid, and syntax sugar for:

```haskell
\x -> y -> x + y
```

The simplifier will deal with this syntax sugar, our parser will accept this
verbatim:

```haskell
data Expr
  ...
  | LambdaExpr [ValName] Expr
```

We have a list of named parameters, and then an arbitrary expression using them.
So `\x -> x` becomes:

```haskell
LambdaExpr "x" (NameExpr "x")
```

This is the first time we use the `ValName` synonym, which is defined as:

```haskell
type ValName = String
```

This will be used in many places for names that exclusively refer to values.
So `x` is a `ValName`, but something like `Nil` would rather be typed
as `ConstructorName`.

{{<note>}}
Since we're using type synonyms here, there's actually no difference between `ValName`
and `ConstructorName`. We could've used `newtype` instead, to make it impossible
to confuse them. The problem is that there are situations, like `NameExpr`,
where both might be valid.

For simplicity I've opted to not add the boilerplate of creating explicit differences,
the synonyms are mainly there for documentation, not correctness.
{{</note>}}

### Function Application

What would our functional language be without *function application*? In Haskell,
function application just takes arguments separated by whitespace:

```haskell
f 1 2 3
```

We can also have some arbitrary expression as the function as well:

```haskell
(f . g) 1 2
```

This gives us:

```haskell
data Expr
  ...
  | ApplyExpr Expr [Expr]
```

Because of currying, `f 1 2` is really sugar for `(f 1) 2`, but once
again, we'll be removing syntax sugar in the *simplifier*; the parser
needs to accept all of this. So, for now, `f 1 2` would be represented as:

```haskell
ApplyExpr
  (NameExpr "f")
  [LitExpr (IntLiteral 1), LitExpr (IntLiteral 2)]
```

### Negation

We have a single unary operator in our language: negation. Instead of
having negative numbers, like `-3`, this is instead the negation operator
`-` applied to the expression `3`. We can also do more complicated things like:

```haskell
-((\x -> x + 1) 3)
```

So, we add negation expressions:

```haskell
data Expr
  ...
  | NegateExpr Expr
```

This makes the simple `-3` into the much more verbose:

```haskell
NegateExpr (LitExpr (IntLiteral 3))
```

### Binary Operators

With the only unary operator in the language out of the way, let's add the
many *binary* operators we need. This will cover arithmetic like `a + b + c`,
to function composition `f . g . h`, to comparisons with `x > y`, etc.

We have:

```haskell
data Expr
  ...
  | BinExpr BinOp Expr Expr
```

If we have some complex arithmetic expression, like `a + a + b * (x + y)`, this can
still be written as a tree, with each node only having two children:

{{<img "3.png">}}

Or, using the structure we've just defined, as:

```haskell
BinExpr
  Add
  (BinExpr
    Add
    (NameExpr "a")
    (NameExpr "a")
  )
  (BinExpr
    Mul
    (NameExpr "b")
    (BinExpr
      Add
      (NameExpr "x")
      (NameExpr "y")
    )
  )
```

Of course, we haven't defined `Add` or `Mul`, or any of the other members
of `BinOp` yet. Let's briefly go over all of the operators:

```haskell
data BinOp
  = ...
  deriving (Eq, Show)
```

The `+` operator is used to add two numbers:

```haskell
  = Add
```

The `-` operator subtracts the right number from the left number:

```haskell
  | Sub
```

The `*` operator is for multiplication:

```haskell
  | Mul
```

The `/` operator is for division:

```haskell
  | Div
```

We have the comparision operators `<`, `<=`, `>`, `>=`, which become:

```haskell
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
```

And the comparison operators `==` and `/=` become:

```haskell
  | EqualTo
  | NotEqualTo
```

We also have the two boolean operators `||`, and `&&`, which are:

```haskell
  | Or
  | And
```

Finally, we have the function "operators". Namely, `$` and `.`. These are defined
as standard functions in normal Haskell, but since we don't have custom operators,
these are done here as well:

```haskell
  | Cash
  | Compose
```

{{<note>}}
I have no idea how widespread calling the `$` operator "cash" is. I've always
called it that since I first learned Haskell ~ 4 years ago. I've heard `f $ x`
read as "f dollar x", but that doesn't roll off the tongue like "f cash x" does.
{{</note>}}

## Pattern Matching

The next kind of expression we're going to deal with are `case` expressions, things
like:

```haskell
case x of
  2 -> x + 2
  3 -> x + 3
  y -> x + y
```

or

```haskell
case x of
  Nil -> 0
  Cons A (Cons B Nil) -> 1
  Cons B Nil -> 2
  Cons y Nil -> y
```

We have some kind of expression that we're scrutinizing, followed by
a series of branches. Each branch has some kind of *pattern* that we're matching
against, followed by an expression. The value of the case expression is the value
of whatever branch happens to match.

So, let's first define what kind of patterns we have in our language:

```haskell
data Pattern
  = ...
  deriving (Eq, Show)
```

The first kind of pattern is the wildcard: `_`. This pattern matches against everything,
but creates no new name:

```haskell
data Pattern
  = WildcardPattern
```

The next kind of pattern is similar. We can also have a name as a pattern, like:

```haskell
case 3 of
  x -> x
```

This is like the wildcard, but also assigns the value of whatever it managed to
match to the given name. So here, `x` would have the value `3` inside of the branch.
We have:

```haskell
data Pattern
  ...
  | NamePattern ValName
```

We use `ValName`, since only lower case names like `x` are valid here.

Another kind of pattern is for *literals*. We can match against literal ints, like
`3`, literal strings, like `"foo"`, or literal bools, like `True`, or `False`:

```haskell
data Pattern
  ...
  | LiteralPattern Literal
```


We also want to be able to match against custom types. If we define a custom
type like `data Color = Red | Blue | Green`, we need to be able to do things like:

```haskell
case color of
  Red -> ...
  Blue -> ...
  Green -> ...
```

If our data type has constructors, like `data Pair = Pair Int String`,
we want to be able to match against the arguments as well, giving us constructions like:

```haskell
case pair of
  Pair 3 "foo" -> ...
  Pair x "bar" -> ...
  Pair _ _ -> ...
```

With this in mind, we need to add a variant for these constructor patterns:

```haskell
data Pattern
  ...
  | ConstructorPattern ConstructorName [Pattern]
```

We have the name of the constructor, followed by a sequence of arbitrarily complex
patterns. So, given a data type like `data List a = Cons a (List a) | Nil`,
and a pattern like:

```haskell
Cons _ (Cons _ (Cons _ Nil)
```

we would represent this as:

```haskell
ConstructorPattern
  "Cons"
  [  WildcardPattern,
     ConstructorPattern "Cons"
       [ WildcardPattern,
         ConstructorPattern "Cons"
           [ WildcardPattern,
             ConstructorPattern "Nil" []
           ]
       ]
  ]
```

And now that we've defiend patterns, we can immediately define case expressions!

```haskell
data Expr
  ...
  | CaseExpr Expr [(Pattern, Expr)] 
```

So, we first have the expression we're scrutinizing, followed by a list of mappings
from patterns to expressions to use when that pattern matches. So,
some expression like:

```haskell
case scrut of
  0 -> 0
  1 -> 1
  _ -> 2
```

we would represent this as:

```haskell
CaseExpr (NameExpr "scrut")
  [ (LiteralPattern (IntLiteral 0), LitExpr (IntLiteral 0)),
    (LiteralPattern (IntLiteral 1), LitExpr (IntLiteral 1)),
    (WildcardPattern, LitExpr (IntLiteral 2))
  ]
```

## Definitions

Now that we've defined *expressions* as well as the structure of *type definitions*,
we can combine the two, in order to define the structure of value definitions.
This is about the structure of defining new values. In Haskell, there are two parts
to a value definition:

1. The type annotation, like `x :: Int`
2. The name definition itself, like `x = 3`

The type annotation and name definition don't have to appear together, and the
type annotation isn't necessary at all. Regardless, our parser will need to represent
both.

So, let's create a structure for both of these:

```haskell
data Definition
  = ValueDefinition ValueDefinition
  | DataDefinition TypeName [TypeVar] [ConstructorDefinition]
  | TypeSynonym TypeName Type
  deriving (Eq, Show)
  
data ValueDefinition
  = ...
  deriving (Eq, Show)
```

We've just introduced the notion of a `ValueDefinition`, which will contain both
the type annotations, and the name definitions we've just touched upon. We've also
added this to our concept of `Definition`. A definition now includes the two
constructs related to types, namely, data definitions, and type synoyms,
as well as the definitions for values.

For type annotations, the definition is pretty simple:

```haskell
data ValueDefinition
  = TypeAnnotation ValName Type
```

A type annotation like `x :: Int` is represented as the name of the value being annotated,
and the type we're providing after the `::`.

For name definitions, our first try might be something like:

```haskell
NameDefinition ValName Expr
```

The idea is that something like `x = 3` would become:

```haskell
NameDefinition "x" (LitExpr (IntLiteral 3))
```

This *would* work for a large class of Haskell definitions, but Haskell
has some more syntax sugar around definitions. The first piece of sugar is that
instead of having to write:

```haskell
id = \x -> x
```

You can instead move the parameter to this function to the left side of the `=`,
to get:

```haskell
id x = x
```

Of course, you can have multiple parameters as well:

```haskell
f a b = a + b
```

So, our structure for name definitions will need to accomodate these named parameters.

Another bit of sugar is that you can *pattern match* inside of these definitions:

```haskell
f 0 1 = 1
f 0 _ = 2
f a b = a + b
```

A function can be defined with multiple "heads", each of which has a pattern for
each of its arguments. Because of this, our full structure for name definitions
becomes:

```haskell
data ValueDefinition
  ...
  | NameDefinition ValName [Pattern] Expr
```

We have the name used for this value, a list of patterns for each of the arguments,
and an expression for the body of this definition.

As an example, this function:

```haskell
f :: Int -> Int
f 0 = 0
f a = a
```

Would become 3 separate definitions:

```haskell
TypeAnnotation "f" (IntT :-> IntT :-> IntT)

NameDefinition "f" [LiteralPattern (IntLiteral 0)]
                   (LiteralPattern (IntLiteral 0))

NameDefinition "f" [NamePattern "a"] (NameExpr "a")
```

We have one definition for the type annotation, and 2 more for each of the
2 heads of the function.

{{<note>}}
Note that we do no enforcing of some basic integrity checks for these heads,
like enforcing that they have the same number of patterns. This kind of thing is going
to be handled in the simplifier.
{{</note>}}

Now that we have a structure for definitions, we can add the two missing
types of expressions: `let` and `where` expressions. These are two ways to express
the same thing: local definitions inside an expression. So:

```haskell
let x :: Int
    x = 2
in x
```

and

```haskell
x
where
  x :: Int
  x = 2
```

Have exactly the same meaning. For clarity, we'll just create two different
types of expression for each of these:

```haskell
data Expr
  ...
  | LetExpr [ValueDefinition] Expr
  | WhereExpr Expr [ValueDefinition]
```

The structure is slightly different, to reflect the different order of the two
variants. `let` has the definitions before the expression, and `where` has
the definitions after the expression.

{{<note>}}
We could've choosed to parse `where` as a `let` expression, or vice-versa,
but I like the clarity of representing the syntax *exactly*. As you might imagine,
our simplifier will also remove this redundancy.
{{</note>}}

Finally, now that we have *definitions* finished, we can actually
define a realy syntax tree:

```haskell
data AST = AST [Definition] deriving (Eq, Show)
```

This just says that a Haskell program consists of a sequence of top level definitions.
So, something like:

```haskell
data Color = Red | Blue | Green

type C = Color

x :: C
x = Red
```

would be a series of 4 definitions, represented as:

```haskell
AST
  [ DataDefinition "Color"
      [ ConstructorDefinition "Red" [],
        ConstructorDefinition "Blue" [],
        ConstructorDefinition "Green" []
      ],
    TypeSynonym "C" (CustomType "Color" []),
    ValueDefinition (TypeAnnotation (CustomType "C" [])),
    ValueDefinition (NameDefinition "x" [] (NameExpr "Color"))
  ]
```

Whew! We now have a complete AST representing our subset of Haskell! The next step
in this post is to write a *parser* for it, using the little combinator
framework we created earlier.

# Parsing in Practice

So, now it's time to use all of the little tools we've made
in order to build up a parser for our AST. We're going to be building up a parser
in a bottom up fashion, starting with simple parsers for simple parts
of the language, and combining them piece by piece to make up a parser for the whole language.

## Names and Literals

We're going to be starting with the simplest form of expression: names,
and literals. There were two types of names in the lexer: `LowerName`,
for names starting with a lower case letter, and `UpperName` for names
starting with an upper case letter. We can make a parser for `LowerName`
pretty easily:

```haskell
lowerName :: Parser ValName
lowerName =
  pluck <| \case
    LowerName n -> Just n
    _ -> Nothing
```

Here we use the `pluck :: (Token -> Maybe a) -> Parser a` function we defined previously.
`pluck` lets us match against a certain type of token, and also extract some information
about that token. Here this is useful, since we want to extract
the string contained in a `LowerName` token.

We can do the same trick for `UpperName`:

```haskell
upperName :: Parser TypeName
upperName =
  pluck <| \case
    UpperName n -> Just n
    _ -> Nothing
```

The only difference here is that we're extracting from `UpperName` instead
of extracting from `LowerName`.

We've created a few different synonyms for `String`, to clarify the different
kinds of names we use throughout our AST: things like
`TypeVar`, `TypeName`, `ValName`, etc. We can immediately put the two parsers
we've just defined to use, by creating a parser for each of these syntactic
classes:

```haskell
typeVar :: Parser TypeVar
typeVar = lowerName

typeName :: Parser TypeName
typeName = upperName

valName :: Parser ValName
valName = lowerName

constructorName :: Parser ConstructorName
constructorName = upperName
```

Each of these kinds of name is either a lower case name, or an upper case name. In some contexts,
for example, function application, we might need to accept *both*,
which is why we created the `Name` synonym. We can add a parser for that as well:

```haskell
name :: Parser Name
name = valName <|> constructorName
```

Here we use the `<|>` for the first time, to indicate that a `name`,
is either a `valName`, or a `constructorName`, i.e. that we should accept either
the `LowerName` token, or the `UpperName` token here.

The next thing we can add in this section is literals. We want to be able
to parse ints, strings and bools, which make up the builtin values in our
subset of Haskell:

```haskell
literal :: Parser Literal
literal = intLit <|> stringLit <|> boolLit
  where
```

Once again, we use the handy `<|>` operator, to say that a literal is either
an integer literal, a string literal, or a bool literal. The lexer has already
done the job of creating a token for each of these, which
makes defining these alternatives very easy:

```haskell
  where
    intLit =
      pluck <| \case
        IntLit i -> Just (IntLiteral i)
        _ -> Nothing
    stringLit =
      pluck <| \case
        StringLit s -> Just (StringLiteral s)
        _ -> Nothing
    boolLit =
      pluck <| \case
        BoolLit b -> Just (BoolLiteral b)
        _ -> Nothing
```

We use the very handy `pluck` once again, in order to both match against
the right kind of literal token, and pick out the data alongside it. We then
make sure to wrap this data in the right kind of literal for our AST.
Int literals become `IntLiteral i`, strings `StringLiteral s`, bools `BoolLiteral b`,
as we went over before.

## The Operator Hierarchy

## Expressions

## Definitions

## Types

## Gluing it all together

# Examples

