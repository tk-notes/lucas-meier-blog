---
title: "Some MPS Utilities"
date: 2023-02-23T10:58:37+01:00
type: note
note-tags:
  - "Cryptography"
  - "Foundations"
  - "Protocols"
katex: true
---

Recently I've been working on a [new framework](https://eprint.iacr.org/2023/187)---named "MPS", for "Modular Protocol Security"---for proving the security of cryptographic protocols.

I intend on applying this framework to a threshold ECDSA protocol of mine,
[cait-sith](https://github.com/cronokirby/cait-sith).
In the spirit of open science, and because I think more examples of
how to use MPS could be useful, I think it's easier to just put
these proofs up on my website as I work on them.

Eventually I'll write a full paper and all that jazz about cait-sith,
but I think having some preliminary analysis out there doesn't need to
wait until I run networked benchmarks and stuff like that.

Anyhow, this first note in the "series" is just about some general
utilities for synchronous communication, and a general background
page I can link to for the other notes.

# Synchronous Communication

It's simpler to work in a synchronous communication model,
which assumes that message delivery happens immediately.
Also, this lets us focus on the ECDSA part of things, rather
than the sending messages aspect.

The two main modes of message sending we have in cait-sith
are sending a message to everybody, and sending a message privately.
We'll model these forms of communication using an ideal functionality
(which, in MPS, is just an asynchronous game).
{{<raw>}}
<div class="flex-initial">
$$
\boxed{
\normalsize{
\begin{aligned}
&\colorbox{#FBCFE8}{\large
  $F[\text{SyncComm}]$
}\cr
\cr
&\begin{aligned}
&m_{i \to j, w} \gets \bot\cr
\cr
&\underline{\Rsh_i(P, [m_j], w):}\cr
&\enspace \texttt{for } i \neq j \in P:\cr
&\quad \texttt{if } m_{i \to j, w} = \bot:\cr
&\quad\enspace m_{i \to j, w} \gets m_j\cr
\cr
&\underline{\Lsh_i(P, w):}\cr
&\enspace \texttt{wait } \forall i \neq j \in P.\ m_{j \to i, w} \neq \bot\cr
&\enspace \texttt{return } [m_{j \to i, w} \mid j \in P]\cr
\cr
&\underline{\text{Abort}():}\cr
&\enspace \texttt{die }\cr
\cr
&\underline{\textcolor{#ef4444}{\text{sent}}(i, j, w):}\cr
&\enspace \texttt{return } m_{i \to j, w} \neq \bot\cr
\end{aligned}
&\begin{aligned}
\end{aligned}
\cr
\end{aligned}
}
}
$$
</div>
<div>
<p style="text-align: left">
$m_{i \to j, w}$ means that every pair of parties $i \neq j$ has an empty message slot
for every future waitpoint $w$, when $i$ wants to send a message to $j$.
</p>
<p style="text-align: left">
The $\Rsh_i$ function lets $i$ send a bunch of messages to different
parties in the set $P$ they provide.
</p>
<p style="text-align: left">
$\Lsh_i$ is the counterpart, allowing us to receive messages.
We request to receive from a set of parties $P$ at a given waitpoint, and then receive all those messages, once they're ready.
</p>
<p style="text-align: left">
Any party can immediately stop the protocol at will, by aborting.
</p>
<p style="text-align: left">
The adversary is also able to see which messages have been sent,
but not their contents.
The red color suggest that this function is for the adversary's
eyes only.
</p>
</div>
{{</raw>}}

{{<note>}}
$i, j$ come from the same bounded set, $[n]$, the number of parties.
$w$ is, in theory, coming from an unbounded set $\mathbb{N}$.
In practice this is shorthand for creating the appropriate message
slot $m_{i \to j, w}$ on demand, as soon as a given waitpoint $w$
is actually used in $\Lsh_i$ or $\Rsh_i$.
This is just a technical detail which doesn't change the semantics.
{{</note>}}

One common short hand we use is $\Rsh_i(\star, m, w)$, shorthand for ${\Rsh_i([n], [m, m, \ldots], w)}$,
indicating that the same message should be sent to all parties.
(The functionality for sending ignores )

For receiving, often honest parties will just receive from everyone,
with ${\Lsh_i([n], w)}$.

Malicious parties, however, are free to send different
messages to different parties, even when the protocol specifies
that the party should be using ${\Rsh_i(\star, \ldots)}$ instead,
so we need to allow the more general behavior.

One thing we should mention is the $\texttt{die}$ keyword.
Basically, we augment packages with the ability to be "dead".
If you call a function in a package that's dead, it responds
with a dummy message indicating that, and then you become dead as well.
You can think of this like a kind of exception mechanism,
where as soon as some package in a larger system dies,
the entire system does.

Of course, honest parties will not abort unless something bad happens,
and if everybody is honest, nothing bad should ever happen.
It's important to note that malicious parties can cause aborts at will.
We don't attempt to analyze identifiable aborts in this model.
