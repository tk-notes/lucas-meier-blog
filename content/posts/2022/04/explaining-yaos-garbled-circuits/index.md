---
title: "Explaining Yao's Garbled Circuits"
date: 2022-04-26T21:24:02+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "MPC"
---

# MPC in a Nutshell

Garbled Circuits are a special case of a more general idea called
(secure) Multi-Party Computation (MPC).

The premise is that you have a group of parties $P_1, \ldots, P_n$,
with each party $P_i$ having their own secret input $x_i$. The 
players want to compute some function $f(x_1, \ldots, x_n)$ on their inputs,
learning the result $y$.

The simplest way to do this would be for the parties to share their
inputs with eachother. Each party would know all of the inputs $x_1, \ldots,
x_n$, and thus be able to compute $y = f(x_1, \ldots, x_n)$ on their own.

The problem with this naive approach is that each party would like
to keep their input $x_i$ *secret*. The difficulty in MPC is not
in computing the function $f$ among multiple parties, but rather in
keeping all of the inputs hidden while performing that computation.

## Example: The Millionaire Problem

{{<todo>}}
Yao's Millionaire Problem
{{</todo>}}


## Our Specific Setting

{{<todo>}}
Explain that we're working in the two-party setting, assuming
semi-honest.
{{</todo>}}

# Detour: Oblivious Transfer

{{<note>}}
I wasn't sure where to put this section, but felt it would be a simpler
read for most people if we took care of explaining the idea of Oblivious
Transfer first. You may not be among these people, so feel free to come
back to this section later if that works better for you.
{{</note>}}

{{<todo>}}
Explain how Oblivious Transfer works, at a high level, and then
link to an example with Simplest OT.
{{</todo>}}

# Functions as Lookup Tables

# Walking on Wires

# Conclusion
