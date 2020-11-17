---
title: "(Haskell in Haskell) 1. Setup"
date: 2020-11-17
draft: true
tags:
  - Haskell
  - Programing
  - Compiler
---

This is the first "real" post in the [Haskell in Haskell](https://cronokirby.com/series/haskell-in-haskell/)
that I've been working on.

In this post we'll go over setting up a basic project in Haskell.
We'll build on this foundation in subsequent posts in order to create our compiler.
<!--more-->

Our final goal, by the end of this series, is to have an executable program which reads in Haskell
files, and spits out C files. As the name of this series indicates, we'll be writing this program
in Haskell.

We could write all of our code in a big Haskell file, compile that, and then call it a day, but
we want a more manageable solution for our code. This is why we want to make a *project*,
which will allow us to easily combine multiple source files into a single program.

# Haskell Tools

{{<note>}}
If you're already familiar with setting up Haskell projects, and already have a development
environment set up, feel free to skim over this section, or skip it entirely.
{{</note>}}

Haskell, unlike other languages, doesn't come with "out of the box" support for building projects.
At least, not adequate support for what we'd like to do. This is why we're going to need
some additional tooling to help us.

## Compiler

I've been saying "Haskell" when referring to the compiler, but what I really mean is
[GHC](https://www.haskell.org/ghc/). In theory, there's a Haskell specification, so you
could use a different compiler. In practice, GHC is so good, and so entrenched,
that nobody bothers to use anything different. Every single project basically depends
on details specific to how GHC works, like its numerous extensions, so you can't even use
another compiler if you wanted to.

We could've limited ourselves to a subset of Haskell small enough that our compiler
would be able to process ourselves, but ultimately this is too much of a limitation.
We'll be making judicious use of GHC's features to make our life easier.

## Build Tools

As mentioned before, we need a build tool, outside of the compiler itself, that allows
us to manage the many source files in our project. The build tool will also be responsible
for collecting the libraries that are project depends on, making sure the versions are compatible,
etc. We won't be relying on very many libraries, just a handful that are in the "standard
canon" of Haskell.

At the time of writing, there are basically two build tools for Haskell,
[Cabal](https://www.haskell.org/cabal/),
and [Stack](https://docs.haskellstack.org/en/stable/README/).

{{<note>}}
[Nix](https://nixos.org/) is also a possibility for managing dependencies and doing builds,
but is far more encompassing compared to these tools.
{{</note>}}



## ghcup

- What is ghcup, and why do we need it?

- Link to the website

- Describe setup process

# Cabal Project

Terminal Setup.

Inspecting the cabal file.

# Library

What the library portion is.

Creating it inside the cabal file.

Writing a toy example

# Executable

What the executable is for

Writing a toy example.

# Custom Prelude

What a custom prelude is.

## Why a Custom Prelude

- Deficiencies in the standard Prelude

- The ability to include commonly used functions

- Some of the alternatives

- Why just use a simple one

## Operators we're including

- Personal

## Editing Cabal File

- The no prelude extension
- Changing base libraries

## Fixing The other parts of the project

- Fixing library code
- Fixing executable code
- Running everything

# Conclusion

- What we've covered in this post
- The code for this section
