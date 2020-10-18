---
title: "Lexer Combinators"
date: 2020-10-18
katex: true
tags:
  - Haskell
  - Programming Languages
---

When you write a parser in Haskell, you want to use parser combinators.
When you write a lexer, you should use lexer combinators! We'll see
what these are, and how to use them to write a simple lexer.
<!--more-->

# Parser Combinators

Our starting point is the ubiquitous parser combinator. The idea behind parser
combinators is that instead of writing one monolithic parser for some language,
we should instead write parsers for very simple languages, and then combine
those parsers together into a larger one.

To be able to do this combining, we need some set of operations we can apply
to parsers. Let's go over the fundamental ones:

First, we have some kind of basic parsers we can use as building blocks:

```haskell
char :: Char -> Parser Char
string :: String -> Parser String
```

The first parser only accepts a single string, and then returns that as its
output.
If we wanted our parsers to operate on tokens instead of strings, we would have
slightly different building blocks, but the idea is the same.

Then, we have the classic `fmap` / `<$>`:

```haskell
fmap :: (a -> b) -> Parser a -> Parser b
```

The idea is that we can take a `Parser` that accepts some language, producing
some element of type `a`, and then get a parser accepting the *same* language,
but with the output going through a transformation first.

This is extremely useful and common. For example, you might take a parser parsing
an expression surrounded by `()`, and then remove the parentheses using `fmap`.

Then we have an operation for sequencing:

```haskell
(<*>) :: Parser (a -> b) -> Parser a -> Parser b
```

This parser only accepts some string if the first parser accepts one part of it,
and the second parser accepts the rest of it. The element we return will be
the function returned by the first parser, applied to the element returned
by the second parser.

As an example, we can do:

```haskell
term :: Parser (A, B)
term = (,) <$> parserA <*> parserB
```

This parser will run the first parser, and the second, returning both results.

Right now we can build large parsers, but all they do is run things in sequence.
What we need is to have a parser that accepts the words that work in one parser,
or that work in another parser:

```haskell
(<|>) :: Parser a -> Parser a -> Parser a
```

The idea is that the language accepted by this new parser consists of strings
that are in either the first parser, or the second parser. But what should
the result be? If only one parser succeeds, then we should obviously
use what the other parser returns. What if both parsers succeed? This is
*ambiguity* in our language. With parser combinators, the idea is to move
ambiguity towards the consumer of a combinator. Instead of providing:

```haskell
runParser :: Parser a -> String -> Maybe a
```

which would be unambiguous, we instead provide:

```haskell
runParser :: Parser a -> String -> [a]
```

which returns all the possible results for a given string. Ideally, you should
design your language in a way that it can be parsed without ambiguity, but
the combinator approach doesn't really provide any canonical way to deal
with it should it arise. It just gives you all the results, and lets you try
to sort through them if you'd like.

# Lexer Combinators

We've talked about the fundamental operations, which will be the same for
*lexer combinators* as well (with the exception of `runParser`, which will be
a bit different). We have omitted any discussion of an actual *implementation*
of these combinators though. While the exact implementation depends on the
specific structure we choose for representing combinators, the fundamental
operators, and the way you use combinators stays the same, so it's important
to focus on those first.

With that said, here's the simplest way to represent a parser combinator:

```haskell
type Parser a = String -> [(a, String)]
```

The idea is that given a string, we return all the possible ways to produce
a result of type `a`, leaving the rest of the string that we've yet to consume.

As an example:

```haskell
char :: Parser Char
char = \case
  [] -> []
  (c : cs) -> [(c, cs)]
```

Now, this is for *parser* combinators, where we want to be able to deal with
ambiguity, or at least, not impose any opinion on how we should handle ambiguous
languages. On the other hand, when writing a lexer there's a *canonical* way
to deal with ambiguity: the **longest match rule**.

Let's say we have some tokens like this:

```haskell
data Token = Keyword String | Identifier String
```

Our keywords might be `Keyword "data", Keyword "if"` etc. If we see `if`, we know
that that's a keyword. On the other hand, if we see `if32`, we know that that
refers to an *identifier*. The idea here is that a lexer accepting the string
`if` would leave the `32` part alone, whereas a lexer for identifiers would
accept the entire string. When faced with an ambiguity between these two lexers,
we go with the identifier, because it consumed *more* input. If two lexers
consume the same amount of input, then we can go with the first alternative.
We'd have to make sure to define things as:

```haskell
keywords <|> identifiers
```

to avoid giving identifiers precedence over keywords.

Anyways, because we have an unambiguous way of choosing between alternatives,
we don't need to use `[]` to represent ambiguity. A lexer will either fail,
or produce a *single* result. With this in mind, we can massage the definition
of `Parser` to get:

```haskell
type Lexer a = String -> Maybe (a, String)
```

So a lexer is a function taking some input, and possible returning a result,
along with the rest of the string that it hasn't consumed.

## Implementation

Let's go through an implementation of the fundamental operations we mentioned
earlier.

First, the venerable `fmap`:

```haskell
fmap :: (a -> b) -> Lexer a -> Lexer b
fmap f (Lexer l) = Lexer (l >>> fmap (\(a, s) -> (f a, s))
```

So, we run the lexer on our input, and then `fmap` over the resulting `Maybe`.
We leave the remaining input alone, and just modify the result.

Next we have the applicative methods:

```haskell
pure :: a -> Lexer a
pure a = Lexer (\input -> Just (a, input))
```

I didn't mention this function earlier, but it's also an important one. The idea
is that we simply produce some output, without consuming any input at all.

And then to sequence two lexers together:

```haskell
(<*>) :: Lexer (a -> b) -> Lexer a -> Lexer b
Lexer lF <*> Lexer lA = Lexer <| \input -> do
  (f, rest) <- lF input
  (a, s) <- lA rest
  return (f a, s)
```

We're working in the context of `Maybe`, so that if any parser fails, then
so does the resulting parser we're constructing. First we pull out the
function produced by the first lexer, and then we feed the remaining input
to the second parser. Then we have some remaining input, a function, and
an element to apply the function to, so the result is just putting things together.

And now, for alternatives.

First, we have a lexer that accepts no input at all:

```haskell
empty :: Lexer a
empty = Lexer (const Nothing)
```

Now, let's look at choosing between multiple lexers. Remember the
**longest match rule** here. When faced with multiple choices, we pick
the element that consumed more input, or the first element if both lexers
consumed the same amount:

```haskell
(<|>) :: Lexer a -> Lexer a -> Lexer a
Lexer lA <|> Lexer lB =
  Lexer <| \input -> case (lA input, lB input) of
    (Left _, res) -> res
    (res, Left _) -> res
    (a@(Just (_, restA), b@(Just (_, restB))) ->
      if length restA <= length restB then a else b
```

We look at the results of both lexers, and pick the succeeding one if
only one succeeded. If both lexers work, we pick the one with less input remaining.
The less input remaining, the more the lexer consumed. By using `<=` with the first
parser on the left, we're biased towards the left in case the amount of input
is equal. This enables `keywords <|> identifiers` to lex as expected.

{{<note>}}
At a first glance, it might seem that `<|>` ends up running both lexers. But,
because Haskell is *lazy*, the effective behavior is to run the first lexer,
and then inspect the output to decide whether to run the second lexer.
{{</note>}}

## Basic Lexers

The fundamental ways of combining operators are here, but what about
the *building blocks*? Let's go over a few of these:

```haskell
satisfies :: (Char -> Bool) -> Lexer Char
satisfies p = Lexer <| \input -> case input of
  c : cs | p c -> Just (c, cs)
  rest -> Nothing
```

This function lets us match a single character matching a predicate. We
can use this to implement many small lexers, such as
`satisfies isUpper, satisfies isAlpha` etc.

An immediate application of this function would be:

```haskell
char :: Char -> Lexer Char
char target = satisfies (== target)
```

We can then combine this to get a lexer for an entire string:

```haskell
string :: String -> Lexer String
string = traverse char
```

Now, `traverse :: Applicative f => (a -> f b) -> [a] -> f [b]`, and
works using the `pure` and `<*>` functions we defined earlier.

Two other useful combinators are:

```haskell
many :: Lexer a -> Lexer [a]
some :: Lexer a -> Lexer [a]
```

The first one takes a lexer, and then returns a lexer that runs *zero* or more times,
and the second one does something similar, but always runs at least once. I don't
give a definition, because these two functions are actually defined for
any `Alternative`, i.e. something implementing `empty` and `<|>`:

```haskell
some l = (:) <$> l <*> many l
many l = some l <|> pure []
```

# An example lexer

At this point, we've defined all of the operations we need to define a full
lexer for a simple language. This language will just have let expressions:
```haskell
let
  x = 3;
  y = 5;
in x
```

All whitespace is filtered out, so our tokens look like this:

```haskell
data Token = Let | In | Equal | Semicolon | Name String | IntLitt Int
```

Then we can have a lexer getting out a single token:

```haskell
token :: Lexer Token
token = keywords <|> operators <|> name <|> intLitt
  where
    keywords = (Let <$ string "let") <|> (In <$ string "in")
    operators = (Equal <$ char '=') <|> (Semicolon <$ char ';')
    name = ((:) <$> startsName <*> (many continuesName))
    	 |> fmap Name
      where
       startsName = satisfies isAlpha
       continuesName = satisfies isAlphaNum
    intLitt = some (satisfies isDigit) |> fmap (read >>> IntLitt)
```

To assemble this into a full lexer pulling out many tokens, and ignoring whitespace,
we first need to have a lexer producing either a token, or a bit of whitespace:

```haskell
data RawToken = Whitespace | RawToken Token

rawToken :: Lexer RawToken
rawToken = (Whitespace <$ whitespace) <|> (RawToken <$> token)
  where
    whitespace = some (satisfies isSpace)
```

Now, we need to get a lexer that runs this lexer many times, and filters out
the whitespace:

```haskell
lexer :: Lexer [Token]
lexer = many rawToken |> fmap removeWhitespace
  where
    removeWhitespace [] = []
    removeWhitespace (Whitespace : rest) = removeWhitespace rest
    removeWhitespace (Token t : rest) = t : removeWhitespace rest
```

And then we can define a simple function that hides the `Lexer` type from end
users:

```haskell
lex :: String -> Maybe [Token]
lex input =
  let Lexer l = lexer
  in l input |> fmap fst
```

This will run our lexer on some input, and then pull out the tokens we've produced,
ignoring the remaining input.

# More efficient representations

{{<note>}}
The main discussion about lexer combinators is complete now, and this is a section
about some ideas towards improving performance. Feel free to skip to the ending
if you'd like.
{{</note>}}

At this point, we've gone over the essentials of what you can do with lexer
combinators, but out implementation is a bit slow. For example, in alternation,
we have to compare the length of two strings each time, which is a bit
slow with Haskell's default string type. Furthermore, we build up a large closure
doing all of the work, when we could use a more efficient representation. For
example, building up a finite automaton would give us similar expressive power,
but be much faster to execute. There are other strategies that could improve
performance as well.

The key to getting more performance while keeping the same ergonomics is to
keep the fundamental operations, but instead build up a symbolic representation,
that we can then transform into an efficient lexing function:

```haskell
data Lexer a where
  Satisfies :: (Char -> Bool) -> Lexer Char
  Fmap :: (a -> b) -> Lexer a -> Lexer b
  Pure :: a -> Lexer a
  Empty :: Lexer a
  Ap :: Lexer (a -> b) -> Lexer a -> Lexer b
  Alt :: Lexer a -> Lexer a -> Lexer a
```

Here we've defined a GADT representing the domain specific language we've been
making use of. We can still build up lexers in the same way, but now
the representation of a lexer is purely symbolic. We can then implement:

```haskell
runLexer :: Lexer a -> String -> Maybe (a, String)
```

by inspecting this representation, and producing a much more efficient function.

Basically, the idea is that if we had some kind of operation `<>` for combining
parsers before, then we have:

```haskell
newToOld (new1 <> new2) == newToOld new1 <> newToOld new2
```

So, if we first combine things symbolically, and then interpret things into
a concrete-style lexer, then that should be the same thing as combining both
concrete style lexers. The difference is that by having everything be symbolic,
we can inspect the lexers to implement a much more efficient function,
whereas before we were completely trapped, because our representation *was*
the function.

# Conclusion

I hope this was a somewhat instructive post about an alternative to parser
combinators for doing lexical analysis. The classic reference on parser
combinators would be
[Monadic Parsing in Haskell, Graham Hutton, Erik Meijer](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf)
. Another library that piqued my interest, although I went with a much
simpler exposition here, would be
[regex-applicative](https://hackage.haskell.org/package/regex-applicative).

I highly recommend reading the original pearl about parser combinators. It's
very readable, and quite instructive. In practice, I'd recommend using
a library like `regex-applicative`, which applies the optimization
principles I mentioned at the end of this post.

That being said, I think building things up from scratch is very instructive :)
