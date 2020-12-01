---
title: "(Haskell in Haskell) 1. Setup"
date: 2020-11-23
tags:
  - Haskell
  - Programming Languages
  - Compiler
---

This is the first "real" post in the [Haskell in Haskell](/series/haskell-in-haskell/)
series.

In this post we'll go over setting up a basic project in Haskell.
We'll be building our compiler on top of this foundation in the next posts.

<!--more-->

(The code for this part is available [here](https://github.com/cronokirby/haskell-in-haskell/tree/part-1))

By the end of this series, we'll have written a Haskell program that reads in Haskell source code,
and then spits out C code.

We could write all of our code in a big Haskell file, compile that, and then call it a day, but
this isn't a manageable solution. This is why we want to make a _project_,
which will allow us to easily combine multiple source files into a single program.

# Haskell Tools

{{<note>}}
If you're already familiar with setting up Haskell projects, and already have a development
environment set up, feel free to skim over this section, or skip it entirely.
{{</note>}}

Haskell, unlike other languages, doesn't come with "out of the box" support for building projects;
at least, not adequate support for what we'd like to do. We're going to need
some additional tooling to help us.

## Compiler

I've been saying "Haskell", when talking about the compiler, but what I really mean is
[GHC](https://www.haskell.org/ghc/). In theory, there's a Haskell specification, so you
could use a different compiler. In practice, GHC is so good, and so entrenched,
that nobody bothers to use anything different. Almost every single project depends
on details specific to how GHC works, including its numerous extensions, so you couldn't even use
another compiler if you wanted to.

We could've limited ourselves to a small subset of Haskell, so that our compiler
could process _itself_, but this is too much of a limitation.
We'll be making use of GHC's features to make our work easier.

## Build Tools

Next, we need a build tool, outside of the compiler itself, that allows
us to manage the source files in our project. This tool will also be responsible
for collecting the libraries that our project depends on, making sure the versions are compatible.
We won't be relying on very many libraries. We just need a handful, belonging to the "standard
canon" of Haskell anyway.

At the time of writing, there are basically two build tools for Haskell,
[Cabal](https://www.haskell.org/cabal/),
and [Stack](https://docs.haskellstack.org/en/stable/README/).

{{<note>}}
[Nix](https://nixos.org/) is also a possibility for managing dependencies and doing builds,
but goes far beyond the simple needs we have for this project.
{{</note>}}

The difference between Cabal and Stack is mainly historical. In practice, they're more
or less the same nowadays. The main difference between the two is that Cabal
will use [Hackage](https://hackage.haskell.org/) as its source of packages, while
Stack will use [Stackage](https://www.stackage.org/). Stackage pulls its packages
from Hackage, but a group of maintainers make sure that the packages build together.
This delays the accessibility of packages, but provides more stability for programmers.

If you're familiar with Linux distributions, Hackage is a bit like _Arch_, with community
provided packages, and minimal vetting, whereas Stackage is a bit like _Ubuntu_, where you
have maintainers curating the versions of packages in the repository, and providing
stable LTS releases.

Back in the day, Cabal would install packages globally, unless you explicitly created
a Sandbox for each project. Stack was created to address this concern, as well as to use
_Stackage_ which was seen as providing advantages over using Hackage directly.
Stack only installed packages *locally* for each project, allowing different projects you have
to use different versions of packages. Eventually, Cabal created the `new-build`, `new-run`, etc.
commands to provide this behavior. Since these commands are now the default, Cabal essentially
acts like Stack nowadays.

All this to say that these tools are basically equivalent, but if you like having the
LTS snapshots that Stackage provides, you may want to use Stack.

For our purposes, we'll be using Cabal. In practice involving Stack involves installing
Cabal _anyways_, and ultimately Cabal works very well nowadays, and I _personally_ find
Stack to be somewhat superfluous.

## ghcup

We know that we'll be needing a compiler (GHC), and a build tool (Cabal), but
what's the easiest way to install them? On Linux and macOS, at least, the
answer is probably [ghcup](https://www.haskell.org/ghcup/).

This isn't just an installer for GHC and Cabal, but rather a tool that you install
once, and that then lets you _update_ your version of GHC and Cabal as often as you want.
This way you can just run this setup process a single time, and then have
an easy way to follow the new releases of GHC as they come out.

The website for ghcup has very simple installation instructions for Linux and macOS,
which just involve fetching and running a bash script. Unfortunately, for Windows
this tool doesn't seem to be available, but the website also provides
instructions towards setting up the [Haskell Platform](https://www.haskell.org/platform/),
which seems to be the preferred method of installation here.

Regardless, if you'd like to install GHC and Cabal through other means, go right ahead.
As long as you have recent versions of both of these tools, there shouldn't be any
problems following along with this project.

# Cabal Project

All right, now that we have the tools of our craft, it's time to start putting
them to good use!

The first thing we want to do is to create a project using Cabal.
A project will allow us to easily work across multiple source files,
and use a handful of useful libraries in our program. Furthermore,
by providing this series through a project, it should hopefully work
in the future, because the dependencies will be pinned to specific versions
known to work. Even if the packages release new versions, the ones we use for
this series will still be there.

First thing's first, create a directory for your project. Inside of this directory,
run:

```bash
cabal init
```

This will create a project in that directory, sgenerating some files automatically.
You can go ahead and delete `CHANGELOG.md`, if you'd like. The `.cabal`
file contains information about our project. This includes where our source
files are, what libraries we depend on, etc.

After removing some of the superfluous comments, and other irrelevant stuff, you should
have something like:

```txt
cabal-version:       >=1.10

name:                haskell-in-haskell
version:             0.1.0.0
author:              <Your Name>
maintainer:          <Your Email>
build-type:          Simple

executable haskell-in-haskell
  main-is:             Main.hs
  build-depends:       base >=4.13 && <4.14
  default-language:    Haskell2010
```

Most of this is pretty straightforward metadata. We then have an _executable_ named
`haskell-in-haskell`. When we run `cabal build` this builds that executable, with the same name.
Running `cabal install` will build the executable, and then put it in your path, so you can
run `haskell-in-haskell` in your terminal to access the compiler you've written.

Finally, we can do `cabal run`, or `cabal run haskell-in-haskell` to run this program.
After running this command, you should see a bunch of stuff as Cabal builds it for the first time, and
then, finally:

```txt
Hello, Haskell!
```

Hello Haskell indeed.

In the _executable_ section. You can see that we've specified the libraries
we're depending on. In this case, just the standard library, and a main source file for
our program: `Main.hs`.

Looking at `Main.hs`, we see:

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"
```

The `main` function in this file is what gets run by our executable, which explains
why we got that nice message earlier. If you modify the message printed here,
and run the command again, you'll see the output changed to match.

# Library

So, right now we do have a _program_, but it doesn't do much besides print a
little message. We're going to eventually turn this program into a full fledged
compiler. We could do this by adding more and more code to `Main.hs`, but this
isn't exactly very pretty nor modular. We could add more source files
to our `executable` section, telling Cabal to use them to build our program as well.
This would work, but a more common practice is to create a separate `library` section,
exporting a package containing the main functions your program needs,
and then using that library inside of a small `Main.hs` to create an actual program.

The idea behind this separation is that it makes it easier to expose the functionality
behind a program to other Haskell users, since you can provide a library doing the
same thing as your command line program. Furthermore, by forcing yourself to write
code in the form of a library, you have to think a bit more about modularity and
good code structure.

Let's go ahead and add a library section to our Cabal file:

```txt
library
  build-depends:       base >=4.13 && <5
                     , containers >=0.6 && <0.7
                     , mtl >=2.2 && <2.3
  default-language:    Haskell2010
  exposed-modules:     Example
  ghc-options:         -Wall
  hs-source-dirs:      src
```

We've included some other libraries besides the standard library this time.
`containers` will provide us with a `Map`, and a `Set` data structure, which are going
to be very useful at different points in the compiler. `mtl`, or "Monad Transformer Library"
provides us with, well, **Monad Transformers**. You don't need to understand
Transformers in all of their glory to follow this project. What we'll be doing is setting
up little "computational contexts" at different points in the compiler, mainly to throw
exceptions, but also to read from some store of information we've gathered previously,
or to keep track of a little bit of state as we do our compiler work.

`hs-source-dirs` says that we'll have our source files in the `src` directory. We
also have a single module exported by our library: `Example`. Because `src` is our source
directory, Cabal expects to find this module in `src/Example.hs`. Let's go ahead and
add some simple code there:

```haskell
module Example (example) where

example :: String
example = "This is an example"
```

This module does nothing more than export the string `example`. With this, our
library is "complete", even thought it doesn't really do anything yet. Later we'll
expand this library section until it has a complete compiler!

## Connecting Library to Executable

Now we want to connect our library with our executable, so that our main function
can use this `example` value. Later on, our main function will be calling
the functionality of our compiler, so it makes sense to get started on this
right away.

We need to modify our Cabal file's executable section to depend on our library:

```txt
executable haskell-in-haskell
  main-is:             Main.hs
  build-depends:       base >=4.13 && <4.14
                     , haskell-in-haskell
                     , pretty-simple >=4.0 && <4.1
  default-language:    Haskell2010
```

We've added a dependency on `haskell-in-haskell`, the library we've just
created, as well as a dependency on `pretty-simple`. This is a library
that allows us to pretty print data structures without
any extra code! This is very useful, because we want to allow
users of our compiler to stop it midway, and print out the intermediate results.
This library allows us to print those data structures in a nice way,
without having to add the boilerplate for formatting them.

We don't need this dependency right away, but we might as well set this up, so
that we won't even need to touch the Cabal file in the next parts of this series.

We can now use the library in `Main.hs`:

```haskell
module Main where

import Example (example)

main :: IO ()
main = putStrLn example
```

Running the whole project with `cabal run haskell-in-haskell`, you should now see:

```txt
This is an example
```

# Custom Prelude

So far, we've set a basic project up, but none of the code we have is actually
_useful_. We're going to scrap all of this toy code with actual compiler
stuff soon enough.

We are going to add one piece of _useful_ code in this part: a **Custom Prelude**!

The Haskell Prelude consists of a subset of the standard library that
is imported into every file, and available by default in the REPL. So
if you fire up `ghci`, and type out `sum [1, 2, 3]`, this works because `sum`
is in the module `Prelude`.

A custom Prelude allows us to provide our _own module_
to replace the standard one. This is a piece of code that we can write now, at the beginning
of the project, and never really touch again.

## Why a Custom Prelude

"Why are we even bothering to replace the standard Prelude?", you might ask.
Ultimately, this is a subjective concern, but I personally feel like
some of the advantages of using a custom Prelude are very useful for this project.

Haskell's Prelude hasn't changed much over the years. Because
of this, it has accumulated some cruft to it. One of the biggest problems
is the pervasive use of `String` over `Text`. `String` provides a linked list
of characters (literally `type String = [Char]`), but `Text` provides a much
more efficient byte-array based representation. Basically, you should always
use `Text` if you can.

Unfortunately, because Haskell's Prelude makes this choice a bit of a pain, in practice
you end up having to go through `String` to do a lot of things.
In fact, for this project, we'll be using `String` throughout, rather than `Text`,
because this is ultimately simpler, and avoids a good amount of boilerplate. Furthermore,
we're not working on a production compiler, and we're cutting corners in plenty
of places already. This project is about learning a simple, but complete approach
to implementing a reasonable subset of Haskell, so I feel ok papering over things like this.

Many commonly used functions are also missing from the standard Prelude.
For example, `foldl` has the wrong strictness, and performs
much worse compared to the strict `foldl'`. Unfortunately `foldl` is in the Prelude,
while you need to import `foldl'` from `Data.List`. This kind of thing is pervasive,
and the line between what does and doesn't get included seems quite arbitrary at times.

There are plenty of libraries that provide alternative Preludes:
[Protolude](https://hackage.haskell.org/package/protolude),
[Relude](https://hackage.haskell.org/package/relude),
among many others.
For this project, I've decided to just write our own, with a handful of extra functions.

In practice, I'd recommend you find an alternative Prelude you like, as a library,
and then just stick with that (`Relude` would be my personal choice). For this project,
I don't want to depends on the idiosyncracies of any particular Prelude though.
Choosing one would work, but it might alienate people not familiar with how it works.

Instead, I'm just introducing a few simple operators that make code much more readable, and consistent,
in my opinion, and a few utility functions that we use
often enough to warrant being in our Prelude.

# Creating Ourlude

So, we'll be creating a file `src/Ourlude.hs` to contain our custom Prelude. Let's
go ahead and add this to our Cabal file:

```txt
library
  ...
  exposed-modules:     Ourlude
                     , Example
```

And now let's create the `src/Ourlude.hs` file:

```haskell
module Ourlude
  ( module Prelude,
    (|>),
    (<|),
    (>>>),
    (<<<),
    foldMapM,
    mapLeft,
    first, second
  )
where

import Data.Bifunctor (first, second)
```

The first thing of note is that we're exporting the entire standard Prelude!
As much as I've ragged on about it, ultimately, all our custom Prelude does is add
a few things to the standard Prelude.

## What we're including

`first` and `second` come from `Data.Bifunctor`. What they do is map
a function over part of a tuple:

```haskell
first f (a, b) = (f a, b)
second f (a, b) = (a, f b)
```

These are just used often enough throughout the compiler to warrant
including.

We'll be implementing the rest of the functions ourselves, so make sure to add
them to `Ourlude.hs` if you're following along.

First we have `mapLeft`:

```haskell
mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = either (f >>> Left) Right
```

This does what it says on the tin. It modifies the error part of an `Either` while
leaving the other side intact. This is in the "why isn't this in the Prelude already" bin.

Next we have: `foldMapM`:

```haskell
foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
foldMapM f = mapM f >>> fmap mconcat
```

This might seem a bit weird to include, but it's actually surprisingly useful.
First, remember `foldMap`:

```haskell
foldMap :: Monoid b => (a -> b) -> [a] -> b
```

This function takes a list `[x1, x2, ..., xN]` and gives us `f x1 <> f x2 <> ... <> f xN`,
where `<>` is the monoidal operation. For example, you could use this to
show a bunch of integers, and then concatenate the digits:

```haskell
foldMap show [1, 2, 3]
```

This is useful in a compiler, because very often you're traversing lists of stuff, and producing
some kind of output that you want to merge together. For example, if you want to find
the set of names used in an list of expressions, you'd use `foldMap`, given a function
that finds the set of names used in a _single_ expression.

`foldMapM` is useful, because you're doing this kind of traversal _inside of a monadic context_.
For example, while squashing this output, you might need to throw an error, or lookup
some information that you have access to in this context.

## Forward Composition

The remaining functions come as an ensemble really:

```haskell
infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x
{-# INLINE (|>) #-}

infixr 0 <|
(<|) :: (a -> b) -> a -> b
(<|) = ($)
{-# INLINE (<|) #-}

infixr 9 <<<
(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
g <<< f = g . f
{-# INLINE (<<<) #-}

infixl 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f
{-# INLINE (>>>) #-}
```

Two of these operations should be well known to you. `<<<` is actually `.`
in disguise, and `<|` is `$` as well. The reason I include these is strictly
for consistenty with the other two.

The other two provide _forward composition_. So `f >>> g` is nothing other than
`g <<< f`, which is `g . f`, in more familiar terms. Similarly
`x |> f` is just `f $ x`, and just `f x`, really. (The advantage of `$` comes with multiple functions,
or complicated expressions like `f $ 2 + 2`).

The reason I include forward composition operators is because I find forward composition
_better in most cases_, compared to backwards composition. What I mean by "forward", is
that if we take an expression like `f (g (x))`, i.e. `f $ g $ x`, or `f <| g <| x`, we 
have `x`
first "entering" `g`, and then "entering" `f`. So the data moves
from right to left, which is _backwards_ relative to the left to right order
you're reading this text in.

By contrast `x |> g |> f` has the data moving _forwards_, or left to right.

Similarly `\x -> f (g x)` becomes `f <<< g`, or `g >>> f`, and the same remarks apply.

I think forward composition is better in most cases, simply because it matches the standard reading
order. For example, take this expression:

```haskell
Matrix . map stripHead . filter isDefault $ rows
```

Now, when I read this, I have to first move my attention all the way to the end of the line,
and then crane going backwards from right to left, in order to see how `rows` is transformed
into the final result.

This wouldn't look all that strange to many Haskellers,
since we're so used to reading things in this order. But if you use `<|`, as
we will, this *does* look a bit odd:

```haskell
Matrix <| map stripHead <| filter isDefault <| rows
```

It looks odd, because **it is**. You're taking a piece of data, and transforming it,
but you're doing so by describing transformations in the opposite order in
which they happen. It's much more natural (at least in English) to say

"take the rows, then filter using isDefault, then map stripHead, then wrap that in Matrix"

versus

"wrap in Matrix before mapping with striphead, before filtering using isDefault, over the rows"

Let's have some catharsis:

```haskell
rows |> filter isDefault |> map stripHead |> Matrix
```

This looks much more straightforward to me, and matches the natural reading order.

That's not to say that the other direction isn't useful. It is, just not for longer
chains like this. Where the other direction is useful is when it reminds us of "normal"
function application.

For example,
a large expression like this clearly benefits from using `|>`

```haskell
transpose (swap index (transpose pats))

pats |> transpose |> swap index |> transpose
```

On the other hand:

```haskell
\stuff -> map fst (filter okay stuff)
```

doesn't have enough nested computation to warranted using `|>`, it's readable as is.

If wanted to eta-reduce this, i.e. remove the lambda, it makes sense to do it like this:

```haskell
map fst <<< filter okay
```

Here `f <<< g` is supposed to be immediately evocative of `f (g x)`. In general,
I prefer using `<<<` when I'm trying to draw a parallel with direct application.

`<|`, is used in the same way as `$`, as a kind of syntactic trick:

```haskell
Matrix <| rows1 ++ rows2

return <| case x of

map <| \case

runMonadM <| do

...
```

What we're doing here is making use of the low precedence of `<|` to neatly avoid wrapping
things in parentheses. This is especially important in front of block expressions, because
wrapping a block in parentheses is a bit awkward:

```hs
f (case x of
  Nothing -> 0
  Just y -> y
)
```

# Making Ourlude Default

So, now we have a module `Ourlude`, which contains the functions we want in
our custom Prelude, but we haven't actually made it _the Prelude_. In fact,
`Prelude` is still imported by default in every file, and this will conflict
with `Ourlude` when we try to import it as well.

To amend this, we need to set things up so that `Prelude` is _not imported_
by default. Then we can manually import `Ourlude` in every file, and things
will work out.

{{<note>}}
You can also make it so that `Ourlude` gets imported by default in every file,
but it's quite a bit simpler to just import it manually. The extra line of boilerplate
in every file really doesn't amount to much. The explicitness makes the build
much simpler.
{{</note>}}

Thankfully, Haskell provides an extension to make the Prelude not implicitly
imported, called, well `NoImplicitPrelude`. We could add:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

at the top of every file, but it's easier to just add this as a default extension
in our Cabal file. This makes it so that it's included as an extension in every file.

```txt
executable haskell-in-haskell
  ...
  default-extensions:  NoImplicitPrelude

library
  ...
  default-extensions:  NoImplicitPrelude
  ...
```

{{<note>}}
While you're at it, you can also delete `Example.hs`, and then remove `Example`
from `exposed-modules` in the library section. We won't be using it anymore.
{{</note>}}

Now, this breaks all of our code, because we assumed the Prelude was imported before.

We need to import the `Prelude` in `Ourlude.hs`, to be able to reexport things:

```haskell
-- ...
import Prelude
-- ...
```

Let's change `Main.hs` to import `Ourlude` now:

```haskell
module Main where

import Ourlude

main :: IO ()
main = putStrLn <| "Hello Again!"
```

Here we use the `<|` operator from our custom Prelude, just as a little
check that everything's working right.

Let's go ahead and fire `cabal run haskell-in-haskell` again. You should see:

```txt
Hello Again!
```

# Conclusion

And that's all for this part!

Even though we're not really doing any compiler stuff yet, we've gotten a project
set up, and a lot of the boilerplate necessary for the project out of the way.

The code for this part is available [here](https://github.com/cronokirby/haskell-in-haskell/tree/part-1).

Stay tuned for the next part of this series, where we'll finally get into compiler goodness,
by writing our own lexer for Haskell, and all of its whitespace trickery!
