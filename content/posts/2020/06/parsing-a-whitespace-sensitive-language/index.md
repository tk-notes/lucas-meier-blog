---
title: "Parsing A Whitespace-Sensitive Language"
date: 2020-06-03
tags:
  - "Parsing"
  - "Programming Languages"
---

This post is about how to parse programming languages which define blocks using indentation,
instead of braces and semicolons. We'll end up learning how to infer these blocks based on the indentation,
using [Typescript](https://www.typescriptlang.org/) to parse a toy functional language.
<!--more-->

## Background Knowledge

This post stops at generating tokens we can feed into a parser. Implementing a parser
doesn't change because of whitespace sensitivity (at least, not with the technique
we'll be seeing). If you want to explore how to do parsing, [this chapter](https://www.craftinginterpreters.com/parsing-expressions.html)
of [Crafting Interpreters](https://www.craftinginterpreters.com) is very good.

The chapter on lexing from the same book might be useful to look at if you want to understand
lexing as well. We will be going over how that works here though.

Familiarity with Typescript will help in reading the code, but shouldn't be necessary
to understand what's going on.

There's no need to understand Haskell, or functional languages. We will be parsing a very small
subset of a Haskell-like language, and referencing some of the syntax of Haskell, but
nothing beyond that.

With that said, let's get on to the meat of this post.

# The Language

Here's an example program in the language we'll be working with in this post:

```txt
f = x => x * x

y =
  let
    z = 4
  in z + f z x = x * x

y =
  let
    z = 4
  in z + f z
```

We have the usual arithmetic operators: `+`, `-`, `*`, and `/`. Function application doesn't require parentheses,
so `f x` means `f(x)` in more standard notation. We define functions with `arg => expression` syntax.
Programs are just a sequence of definitions `name = expression`. One new aspect that might be unfamiliar
is the `let ... in` construct. This is what we'll call a "let expression", and is what
is sensitive to whitespace.

What the let expression allows us to do is define intermediate values that we use in another
expression. We can also nest multiple let expressions:

```txt
let
  z =
    let
      x = 2
      y = 3
    in x + y
in z
```

The way we tell which definitions belong to which block is through the indentation. The
definitions inside of the same let block all have the same indentation, which lets us
tell that they belong there.

# Parsing

Parsing is about going from the text / source code of our program, and getting to
a representation of the program that we can use for things like iinterpreting, compiling,
or manipulating the program in other programmatic ways.

This representation usually takes the form of a [Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
For example, if we take the first program we took as an example, we have:

```txt
f = x => x * x

y =
  let
    z = 4
  in z + f z
```

After parsing, we end up with a tree like structure representing the program:

{{<img "1.png">}}

We start with the top level definitions, with one branch for each of them. Then we
have a node for the definition / `=` construct, with a branch for the name, and a
branch for the expression.

The expression breaks down into a subtree, whose subexpressions also have trees, etc.

This tree like structure is much more ready for manipulation than a string of characters is.
It's much easier to use a tree of arithmetic operations to implement a calculator than it is to try and
and interpret the stream of characters as they arrive.

## Parsing as stages

Without going into the details too much, parsing usually works by using a set of rules
(called a *Grammar*) which tell us how to combine certain strings into a small part of our
tree.

For example, we might define expressions like:

```txt
expr := expr + expr | expr - expr | expr * expr | expr / expr | number
```

This says that an expression is formed by adding one of `+`, `-`, `*`, or `/` between
two expressions, or simply having a number. This would give us:

```txt
1+1+3-4

1*3-4
```

As valid expressions according to these rules.

Now there are a few problems with defining arithmetic expressions this way, but we
don't really care about that in this post. We just need to know that parsing involves
defining these kinds of rules.

Here we defined parsing rules as operating strictly on characters. So:

```txt
1 + 2
```

Wouldn't technically be valid, because the two whitespace characters ` ` aren't accounted
for in the rules. Having to add extra rules to handle things like allowing spacing things
on the same line, or ignoring comments would get very annoying, and make our grammar
more complicated.

### Tokenizing

One solution to this problem is instead of feeding each character to our parser, we
instead feed it larger blocks of meaningful "words". Defining a grammar in terms
of "words" instead of individual characters makes sense in the analogy with human
language as well. We usually call these "words" **Tokens** or **Lexemes**.

Going back to the same example program:

```txt
f = x => x * x

y =
  let
    z = 4
  in z + f z
```

One way to tokenize this would be:

```txt
f
=
x
=>
x
*
x
y
=
let
z
=
4
in
z
+
f
z
```

Notice that some concepts become a single token, such as `=>` or `let` this means that
our grammar can refer to these directly, instead of worrying about each character. We also
ignore all whitespace and comments when outputting tokens, which makes the grammar's work
much simpler as well.

# What's hard about whitespace

To us humans, it doesn't seem hard to understand what

```txt
let
  x = f
  y = f
in x + y
```

means. It's difficult for us to *not* notice the indentation structure, and align
things together. Visually, we basically see:

{{<img "2.png">}}

without even thinking about it. A parser might not see this so easily without making it
aware of indentation. If we look at the token stream, we have:

```txt
let x = f y = f in x + y
```

The problem might start to appear at this stage. You see, what about that `f y` in the
middle? What stops our parser from seeing that as the function `f` applied to the argument
`y`? After it does that, it will run into the `=` and not understand what to do, but
by then it's too late. The problem is that our tokenizer outputs a stream of tokens that
ignores all whitespace, so we've removed the visual cues that tells us when certain expressions
end.

Our goal is to find a way to output this information in a way that makes the parser's
job easy.

# An easier language

Languages like C and Typescript have braces to denote blocks, instead of indentation. C
even requires explicit semicolons between statements, instead of relying on newlines to tell it when a new statement
begins. Typescript will try and infer semicolons for you. This is a hint towards what we might
try to do later.

For now let's remove all whitespace sensitivity from our language, and actually write a lexer
for it.

The very first program we saw now becomes:

```txt
{
f = x => x * x;

y =
  let {
    z = 4
  } in z + f z
}
```

A bit ugly (in my view), but it explicitly marks when statements end, and what the
block structure looks like. Because of the semicolons, there's no issue with parsing

```txt
x = f;
y = 2
```
as `x = f y; = 2`, since we put the semicolon at the right place.

With the language being defined, let's actually write a lexer!

It's time to put on our programming socks and write some code in an actual language
(Typescript) instead of a made up one :)

## Tokens

We need a type to represent the different tokens our lexer will output. Thankfully
Typescript comes equipped has [enums](https://www.typescriptlang.org/docs/handbook/enums.html)
which fit the bill perfectly.

```typescript
enum TokenType {
  Equal = '=',
  Plus = '+',
  LeftBrace = '{',
  RightBrace = '}',
  SemiColon = ';',
  // Keywords
  Let = 'let',
  In = 'in',
  // These will need data attached
  Number = 'Number',
  Name,
}
```

This enumerates the different types of tokens we'll be outputting. For most of the tokens,
just knowing the type is knowing everything we need to. For the last two, we also want
to know what exactly the token contains, to distinguish `Number(1)` from `Number(2)`.

Our full token structure will thus be:

```typescript
interface Token {
  readonly type: TokenType;
  readonly data?: string;
}
```

For the last two types of token, we'll make sure to include `data` along with the type,
so our parser can have that information as well.

## Lexer

Now let's actually write a lexer for this simple language. Our goal, once again, will
be to take in our input string, and start spitting out tokens. Our focus here will
be to make this concrete, through an actual function.

What is a "stream" anyways? How do we represent this in Typescript? One choice would be
to have a function like this

```typescript
function lex(input: string): Token[]
```

One thing I don't like about this is that we have to explicitly construct the entire
list of tokens. This can be inefficient in later stages, since we're keeping the entire
token stream in memory, instead of "streaming" them one by one.

### Generators

[Generators](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Generator)
are a better way of providing a "stream" of data from a function.

Here's a simple generator returning integers:

```typescript
function* numbers() {
  for (let i = 0; i < 100; ++i) {
    yield i;
  }
}
```

Instead of just returning a single value, we can instead yield many values. This function
will yield all the integers up to 100. We can consume this generator like this:

```typescript
for (const n of numbers()) {
  console.log(n);
}
``` 

And we'll end up printing all the numbers that this generator yields.

One function that's very useful, is a generator over all the characters of a string:

```typescript
function *iterString(input: string) {
  for (const c of input) {
    yield c;
  }
}
```

This actually yields *strings* since we don't have a character type in TS.

### Peeking

Generators can also be seen as iterators. Given a generator, we can call it's `next` method,
and get either:
```typescript
{ done: false, value: v }
```

when there's a value that can be yielded, or:

```typesecript
{ done: true }
```

when we've reached the end of the stream.

Another primitive we'd like to have is an iterator where we can look at the next item
without consuming it. We need this for lexing, since we might want to do different things
based on the input without consuming it. For example, we need to be able to look at:
```txt
abc;
```
And parse out the name `abc` as well as the semicolon `;`. To do this we need to be able
to *peek* at the upcoming input, to decide whether to add it to the name or not.

Let's make a class that allows us to do that (feel free to gloss over it):

```typescript
type Peeked<T> = { ready: true; value: T } | { ready: false };

class Peekable<T> implements IterableIterator<T> {
  private _peeked: Peeked<T> = { ready: false };
  private _iter: Iterator<T>;

  constructor(iter: Iterable<T>) {
    this._iter = iter[Symbol.iterator]();
  }

  peek() {
    if (this._peeked.ready) {
      return { done: false, value: this._peeked.value };
    } else {
      const result = this._iter.next();
      if (!result.done) {
        this._peeked = { ready: true, value: result.value };
      }
      return result;
    }
  }

  next() {
    if (this._peeked.ready) {
      const value = this._peeked.value;
      this._peeked = { ready: false };
      return { done: false, value };
    } else {
      return this._iter.next();
    }
  }

  [Symbol.iterator]() {
    return this;
  }
}
```

This is a big blob of code, and you can skim over it if you want. The key thing is that
in addition to `next` we also define `peek` which returns what `next` would return
if we called, but doesn't advance the input stream.

We define this as an `IterableIterator` because an instance has a `next` method we can
call, making it an `Iterator`, but also a way to get an `Iterator` by calling `[Symbol.iterator]`,
making it an `Iterable`. Making something an `Iterable` is nice, because then we can do

```typescript
for (const thing of new Peekable(...))
```

### Back to lexing

Let's create a Lexer class that will contain the state we need when lexing, along
with a few different methods.

```typescript
class Lexer implements Iterable<string> {
  private _iter: Peekable<string>;

  constructor(input: string) {
    this._iter = new Peekable(iterString(input))
  }
}
```

We construct the lexer by taking an input string, and creating a new peekable iterator
over it. This will allow us to look at the next character in our input without consuming it

The next method we'll write is:

```typescript
*[Symbol.iterator]() {
  yield { type: Token.Equal };
}
```

This allows us to write things like:

```typescript
for (const token of new Lexer('a + b')) {
}
```

### Simple Tokens

The one character operators are very easy to lex:

```typescript
*[Symbol.iterator]() {
  for (let peek = this._iter.peek(); !peek.done; peek = this._iter.peek()) {
    switch (peek.value) {
      case '=':
        self.advance();
        yield { type: Token.Equal };
        break;
      case '+':
        self.advance();
        yield { type: Token.Plus };
        break;
      case '{':
        self.advance();
        yield { type: Token.LeftBrace };
        break;
      case '}':
        self.advance();
        yield { type: Token.RightBrace };
        break;
      case ';':
        self.advance();
        yield { type: Token.SemiColon };
        break;
  }
}
```

We look at the next character, and as long as it's valid, we look at which single operator
it matches, accept the character by advancing, and then yield the corresponding token.
Everything is simple, because we only need to see one character to match it with a token,
so far.

Let's add support for numbers now:

```typescript
*[Symbol.iterator]() {
  for (let peek = this._iter.peek(); !peek.done; peek = this._iter.peek()) {
    switch (peek.value) {
      case '=':
        // Single tokens ...
      default:
        if (isNumber(peek.value)) {
          const data = self.number();
          yield { type: Token.Number, data }
        }
        break;
  }
}

number() {
    let acc = '';
    for (
      let peek = this._iter.peek();
      !peek.done && isNumber(peek.value);
      peek = this._iter.peek()
    ) {
      acc += peek.value;
      this._iter.next();
    }
    return acc;
  }

function isNumber(char: string): boolean {
  return /[0-9]/.test(char);
}
```

After matching all the single tokens, we then need to check whether or not the character
is numeric. If it is, then delegate to the `number` method. The `number` method keeps
feeding numeric characters into a big string, and returns once it sees a non-numeric
character. It *doesn't* consume that character, so it's available for parsing.

### Keywords / Names

We'll be doing names similarly to numbers:

```typescript
*[Symbol.iterator]() {
  for (let peek = this._iter.peek(); !peek.done; peek = this._iter.peek()) {
    switch (peek.value) {
    case '=':
      // Single tokens ...
    default:
      if (isNumber(pee.value)) {
        const data = this.number();
        yield { type: TokenType.Number, data }
      } else if (isLowerAlpha(peek.value)) {
        const data = this.string();
        yield { type: TokenType.Name, data }
      }
      break;
  }
}

string() {
  let acc = '';
  for (
    let peek = this._iter.peek();
    !peek.done && isLowerAlpha(peek.value);
    peek = this._iter.peek()
  ) {
    acc += peek.value;
    this._iter.next();
  }
  return acc;
}

function isLowerAlpha(char: string): boolean {
  return /[a-z]/.test(char)
}
```

We have the same thing as we do for numbers, except that we're accepting `abc...` instead
of `012..`. For the sake of simplicity, I haven't included the full character range. In
practice you'd want to tweak this code slightly so that things like:

```txt
a22
aA3
snake_case
```

are also allowed by the function.

The only tokens we have left are `let` and `in`. One problem we didn't have previously
is that these tokens are currently recognized, but as names:

```typescript
{ type: TokenType.Name, data: 'let' }
{ type: TokenType.Name, data: 'in' }
```

We just need to check that the name we've just parsed doesn't correspond to one
of the keywords, in which case we return the specialized token:

```typescript
const data = this.string();
if (data === 'let') {
  yield { type: TokenType.Let };
} else if (data === 'in') {
  yield { type: TokenType.In };
} else {
  yield { type: TokenType.Name, data };
}
```

Finally, let's throw an error if we don't recognize a character, instead of just stalling:

```typescript
*[Symbol.iterator]() {
  for (let peek = this._iter.peek(); !peek.done; peek = this._iter.peek()) {
    switch (peek.value) {
      default:
        if (isNumber(peek.value)) {
          // handling number
        } else if (isLowerAlpha(peek.value)) {
          // handling strings
        } else {
          throw new Error(`Unrecognized character: '${peek.value}'`);
        }
        break;
    }
  }
}
```

### Skipping Whitespace

We can lex `a+3`, but we can't lex `a + 3` yet. This is because we're not handling
any of the whitespace characters yet. We just need to add another case to our
matching:

```typescript
function isWhitespace(char: string): boolean {
  return /[\n\r\s]/.char(string);
}

*[Symbol.iterator]() {
  for (let peek = this._iter.peek(); !peek.done; peek = this._iter.peek()) {
    switch (peek.value) {
      case '=':
      // Single tokens...
      default:
        if (isWhitespace(char)) {
          this._iter.next();
        } else if // ...
        break;
    }
  }
}
```

At this point, we have a complete lexer:

```typescript
function isNumber(char: string): boolean {
  return /[0-9]/.test(char);
}

function isLowerAlpha(char: string): boolean {
  return /[a-z]/.test(char);
}

function isWhitespace(char: string): boolean {
  return /[\n\r\s]/.test(char);
}

class Lexer implements Iterable<Token> {
  private _iter: Peekable<string>;

  constructor(input: string) {
    this._iter = new Peekable(iterString(input));
  }

  *[Symbol.iterator]() {
    for (let peek = this._iter.peek(); !peek.done; peek = this._iter.peek()) {
      switch (peek.value) {
        case '=':
          this._iter.next();
          yield { type: TokenType.Equal };
          break;
        case '+':
          this._iter.next();
          yield { type: TokenType.Plus };
          break;
        case '{':
          this._iter.next();
          yield { type: TokenType.LeftBrace };
          break;
        case '}':
          this._iter.next();
          yield { type: TokenType.RightBrace };
          break;
        case ';':
          this._iter.next();
          yield { type: TokenType.SemiColon };
          break;
        default:
          if (isNumber(peek.value)) {
            const data = this.number();
            yield { type: TokenType.Number, data };
          } else if (isLowerAlpha(peek.value)) {
            const data = this.string();
            if (data === 'let') {
              yield { type: TokenType.Let };
            } else if (data === 'in') {
              yield { type: TokenType.In };
            } else {
              yield { type: TokenType.Name, data };
            }
          } else if (isWhitespace(peek.value)) {
            this._iter.next();
          } else {
            throw new Error(`Unrecognized character: '${peek.value}'`);
          }
          break;
      }
    }
  }

  number() {
    let acc = '';
    for (
      let peek = this._iter.peek();
      !peek.done && isNumber(peek.value);
      peek = this._iter.peek()
    ) {
      acc += peek.value;
      this._iter.next();
    }
    return acc;
  }

  string() {
    let acc = '';
    for (
      let peek = this._iter.peek();
      !peek.done && isLowerAlpha(peek.value);
      peek = this._iter.peek()
    ) {
      acc += peek.value;
      this._iter.next();
    }
    return acc;
  }
}
```

We can test the program like, this:

```typescript
for (const token of new Lexer('a + 45')) {
  console.log(token);
}
```

This should print out:

```json
{ type: "Name", data: "a" }
{ type: "+" }
{ type: "Number", data: "45" }
```

# Becoming Whitespace Sensitive

Right now our language ignores all whitespace betwen characters:

```txt
let x = a + b in x + x
```

Lexes the exact same way as

```txt
let x
= a +
b     in x +
  x
```

Additionally, we require explicit semicolons and braces to be able to handle the structure
of our program. If we had written the parser, it would be working on semicolon tokens
and brace tokens:

```txt
{
f = x => x * x;

y =
  let {
    z = 4
  } in z + f z
}
```

I mentioned that we won't go into parsing in this post. The reason is that a parser
working on explicit semicolons and braces will be just fine. Our goal is to modify
how the lexer works to become whitespace sensitive, while keeping the parser the same.

Our parser will still munch on semicolons and braces. It's up to the lexer to look
at the indentation, and emit the corresponding semicolons and braces.

For example, the following program:

```txt
y =
  let
    z = 4
  in z
```

Will give us the following token stream (right now):

```txt
y = let z = 4 in z
```

What we want is for our lexer to see that `z = 4` is indented to the right of `let`, and
insert the missing braces, to get:

```txt
y = let { z = 4 } in z
```

## Annotated Tokens

More concretely, what we want is to take our existing lexer, and have it output tokens
annotated with indentation information. So instead of

```typescript
function lex(): Stream<Token> {}
```

we'll have:

```typescript
function lex(): Stream<Annotated<Token>> {}
``` 

And then we'll end up making another iterator that uses these tokens to produce semicolons
and braces as well, but let's not get ahead of ourselves here.

There are two pieces of information that we want to keep track of with our tokens:

- Whether or not the token is at the start of a line
- Which column a token appears at

For example, if we have:

```txt
  a b
```

then `a` appears at the start of a line, and `b` doesn't. `a` appears at column 2 (indexing from 0),
and `b` appears at index 4. It's clear that to be indentation sensitive, we need to keep track
of the column. We also need to look at newlines, since we want to be able to look at:

```txt
let
  x = 2
  y = 3
in x + y
```

and insert a semicolon between `x = 2` and `y = 3` because of that newline. It turns out that
encoding whether or not a token occurs at the start of a line or not is sufficient information
to do that.

Let's create a new type to wrap our tokens:

```typescript
enum LinePos {
  Start,
  Middle,
}

interface Annotated<T> {
  item: T;
  linePos: LinePos;
  col: number;
}
```

This adds the extra information we wanted to have from earlier.

### Annotating characters

Now let's write a little class that will annotate characters with this information. We'll
use this stream to feed into our old lexer.

```typescript
function* annotate(input: Iterable<string>) {
  let linePos = LinePos.Start;
  let col = 0;
  for (const c of input) {
    yield { item: c, linePos, col };
    if (c === '\n' || c === '\r') {
      linePos = LinePos.Start;
      col = 0;
    } else {
      col += 1;
      if (linePos === LinePos.Start) {
        linePos = LinePos.Middle;
      }
    }
  }
}
```

We change the current line position to be in the middle of a line as seen
as we advance the column. We move to a newline whenever we encounter
a newline character: `\n`.

Now we just need to update the lexer class:

```typescript
class Lexer implements Iterable<Annotated<Token>> {
  private _iter: Peekable<Annotated<string>>;

  constructor(input: Iterable<Annotated<string>>) {
    this._iter = new Peekable(input);
  }
}
```

We also need to update the resut of the class to yield the additional
metadata as well. For example, we have:

```typescript
        case '=':
          this._iter.next();
          yield { ...peek.value, item: { type: TokenType.Equal } };
          break;
```

We just have to do this everywhere else. You can check the full code
at the end of this post if you want all of the details.

This way of implementing things means that the position of a token
is based on its first character. So:

```txt
let
```

is at column 0, because `l` is.

## Implementing layout

Now that we have a stream of tokens along with their positions, we can move
on to the most interesting part of this post, which is actually implementing
the layout algorithm based on this information.

Our idea will be to go over this stream of annotated tokens, and insert
semicolons and braces where appropriate. We want to take the layout
expressed as whitespace, and return the explicit braces and semicolons
that maps to.

### Keeping track of layouts

One core idea is to keep track of the current indentation level.
For example, if we've just seen:

```txt
let
  x = 3
```

Then currently we're in a block indented by 2 columns, and we expect
tokens that continue the block to appear at the same indentation:

```txt
let
  x = 3
  y = 4
```

We can also have an explicit indentation, via an explicit `{`:

```txt
let { x = 3
```

In this case, we won't try and and infer any indentation at all, it's up
to the user to add the semicolons. We know that this block is closed once
we see an explicit `}`.

To keep track of what kind of layout we're currently in, we need
to create a type representing a layout context:

```typescript
type Layout = { type: 'Explicit' } | { type: 'IndentedBy'; amount: number }
```

A layout is either explicit, when the user has supplied `{` themselves,
or implicit, when the amount of whitespace tells us the indentation
we're currently observing.

We also need a couple of useful functions:

```typescript
function indentedMore(layout: Layout, than: Layout) {
  if (than.type === 'Explicit') {
    return layout.type !== 'Explicit';
  } else if (layout.type === 'Explicit') {
    return indentedMore(than, layout);
  } else {
    return layout.amount > than.amount;
  }
}
```

This function tells us when one layout is strictly greater (`>`) than
another. This will be useful later. We consider implicit indentation
to always be greater than explicit indentation, otherwise we look
at how much things are indented.

The second useful function is:

```typescript
function sameIndentation(layout: Layout, comparedTo: Layout) {
  if (layout.type !== comparedTo.type) {
    return false;
  }
  if (layout.type === 'Explicit' || comparedTo.type === 'Explicit') {
    return true;
  }
  return layout.amount === comparedTo.amount;
}
```

This checks if two layouts are identical.

### Tokens that start layouts

With the way our language works, only certain tokens can start layouts.
Specifically, after `let`, we expect a layout to start, either explicitly:

```txt
let { x = 2; y = 3 } in x + y`
```

or implicitly, via indentation and newlines

```txt
let
  x = 2
  y = 3
in x + y
```

So we create a function:

```typescript
function startsLayout(typ: TokenType) {
  return typ === TokenType.Let;
}
```

Depending on our language, there might be more tokens here. For example
if we added a `where` keyword, that worked similarly to let:

```txt
f = x
  where
    x = 3
```

which could also be written:

```txt
f = x
  where { x = 3 }
```

We'd add that to the tokens that start layouts as well.

### Layout function and rules

What we want to do is to iterate over our stream of annotated tokens,
and insert `{`, `;` and `}` at the right moments:

```typescript
function* layout(input: Iterable<Annotated<Token>>) {
  let layouts: Layout[] = [];
  const topLayout = () => (layouts.length > 0 ? layouts[0] : null);
  let expectingLayout = true;

  // ...
}
```

We have `layouts`, which is a stack of the current layout contexts. Whenever
we enter a new layout, by seeing a `{`, or through some other means, as we'll
see soon.

`expectingLayout` is something we'll use soon enough. The idea is to keep
track of whether or not the last token started a layout. For example
when we see `let` we need to be ready to start a layout based on the next
token. At the start of a file, we're expecting a layout.

Now we go through the items tokens in the input stream:

```typescript
  for (const { col, linePos, item } of input) {
    yield item;
  }
```

Right now all we're doing is taking out the token from the structure
that holds it along with its position, and just yielding that. What
we want to do is to add additional logic to insert semicolons and
braces at the right spots.

#### Indented tokens

Let's say we see an indented token `z`:

```txt
let
  y = let
    x = 3
  z
```

What we want to do in this case is end all the layouts that are further
indented than this current token. If we're continuing the current layout,
we want to make sure to insert a semicolon as well.

```typescript
    let shouldHandleIndent = linePos === LinePos.Start;

    if (shouldHandleIndent) {
      const newIndentation: Layout = { type: 'IndentedBy', amount: col };
      for (
        let layout = topLayout();
        layout && indentedMore(layout, newIndentation);
        layout = topLayout()
      ) {
        yield { type: TokenType.RightBrace };
      }
      const current = topLayout();
      if (current && sameIndentation(current, newIndentation)) {
        yield { type: TokenType.SemiColon };
      }
    }
```

We say that we should handle the indent whenever the token is at the start
of a line. As we'll see soon, this doesn't cover all cases. For example,
`}` shouldn't work like this.

Once there, we remove the current layout, and yield a `}`. This can be
seen as "closing" the current layout. Note that we don't close explicit
layouts implicitly like this, since an explicit layout is never considered
as indented more than an implicit layout.

Finally, if the top layout is the same as this layout, then we insert
a semicolon, because there were other tokens that appeared in this same
layout, and we need to seperate them by semicolons.

This final rule lets us insert semicolons whenever we observe newlines.

#### Handling }

If we encounter a `}`. This means that the user is trying to close an
explicit layout with `{`. We don't allow `}` to close implicit layouts.
What we want to do is pop the current layout off, if it's explicit,
otherwise throw an error.

We have:

```typescript
    let shouldHandleIndent = linePos === LinePos.Start;

    if (item.type === TokenType.RightBrace) {
      shouldHandleIndent = false;
      if (topLayout()?.type === 'Explicit') {
        layouts.pop();
      } else {
        throw Error('unmatched }');
      }
    }
```

We don't need to handle the indentation of `}` afterwards, as we mentioned
before. We throw an error whenever the top layout is not explicit.

#### Handling a starter token

When we encounter a token like `let`, we need to expect a layout, and not
handle that token as continuing an implicit layout.

```typescript
    } else if (startsLayout(item.type)) {
      shouldHandleIndent = false;
      expectingLayout = true;
    }
```

#### Starting layouts

Now, if we're expecting to see a layout, we need to look at the current
token's indentation (regardless of if it's at the start of a line or not),
and start an explicit or implicit layout based on that:

```typescript
    } else if (expectingLayout) {
      expectingLayout = false;
      shouldHandleIndent = false;
```

If we see a `{`, then that matches the expected layout, and starts
an explicit layout:

```typescript
      if (item.type === TokenType.LeftBrace) {
        layouts.push({ type: 'Explicit' });
      }
``` 

Otherwise, it depends on whether or not the token is indented further
than the current context. If it is, then it can start a new layout,
otherwise the token is continuing some layout, which might be further
down the stack, and involves closing implicit layouts. In that case,
we'll need to handle the token again later.

So we have:

```typescript
      } else {
        const newIndentation: Layout = { type: 'IndentedBy', amount: col };
        const currentIndentation = topLayout() ?? { type: 'Explicit' };
        if (indentedMore(newIndentation, currentIndentation)) {
          layouts.push(newIndentation);
          yield { type: TokenType.LeftBrace };
        } else {
          yield { type: TokenType.LeftBrace };
          yield { type: TokenType.RightBrace };
          shouldHandleIndent = true;
        }
      }
```

Inserting a `{` when we start an implicit layout makes sense, but what's
with inserting an empty layout `{ }`. This makes sense because we still
need to have these characters in the layout, it's just that it's empty,
because this token belongs to the layouts surround it. It's like seeing:

```txt
let
  x = let
  y = 2
```
When we see `y`, we insert `{}` after the second let, because it's empty.
Now this program isn't syntactically valid, but our lexer doesn't care
about that. That's for our parser to worry about.

We make sure to set `shouldHandleIndent` to true, because this token continues
a layout somewhere.

# Conclusion

So that was a pretty lengthy post, and got into the details quite a bit.
I had trouble going from haskell's description of their algorithm to
an actual implementation, and hopefully this post can be useful to those
who want a very verbose implementation of things.

The final code can be found [here](https://gist.github.com/cronokirby/aad5db650df406ceec76e35ea0c40ae2).
