---
title: "Writing a Toy Shell in C"
date: 2022-03-11T19:28:10+01:00
draft: true
katex: false
tags:
  - "C"
  - "Programming Language"
  - "Systems"
---

{{<todo>}}
Write an introduction about recently writing a toy shell
{{</todo>}}

<!--more-->

{{<todo>}}
Caveat by saying this isn't a full overview of writing a shell, but
rather just a fun overview of what I've done.
{{</todo>}}

# What is a shell

At the end of the day, the purpose of a computer is to run programs. Right
now, you're reading this in your web browser, which is just a very
large and complicated programs. While nowadays many programs have graphical
interfaces, and get started by using a desktop shortcut, we can also
start programs using a command line interface, from our terminal.

Inside of a terminal, you're presented with a prompt, and can start programs
to do different things. For example, I can start my web browser from
the terminal:

```bash
>> firefox
```

I can also run simpler programs, like creating an empty file:

```bash
>> touch foo.txt
```

What's happening under the hood is that the terminal is running a single
program, called a *shell*, which processes these commands. The shell
reads in commands, parses them, and executes them. Most often, these commands
ask the shell to start other programs. Naturally, after that program
is finished executing, we want to go back to the shell. We may also
want that program to run in the background, and continue working in our shell.
The shell needs to take care of this orchestration, launching programs
in such a way that the shell sticks around to continue processing our commands.

The shell also redirects the output of the programs it launches in a certain way.
Usually, we want the shell to print out whatever the program prints out.
For example, if you list all the files in a directory, you want
to see the result:

```bash
>> ls
a.txt b.txt c.txt
```

Sometimes, you want to redirect the output of a program to a file, which
we can easily do inside of the shell as well:

```bash
>> ls > files.txt
```

So, in summary, a shell is just a program with a simple command-line
interface, and which lets us launch other programs, through a simple
command language.

# Bash

{{<todo>}}
Explain what the bash language is
{{</todo>}}

{{<todo>}}
Explain what subset of bash was implemented.
{{</todo>}}

# Architecture

{{<todo>}}
Explain the general architecture.
{{</todo>}}

## Lexer

## Parser

## Compiler

## Interpreter

# Producing Effects

# Conclusion
