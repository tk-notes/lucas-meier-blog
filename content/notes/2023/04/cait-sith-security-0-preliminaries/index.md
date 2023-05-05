---
title: "Cait-Sith Security (0): Preliminaries"
date: 2023-04-30T21:50:00+02:00
type: note
note-tags:
  - "Cait-Sith"
  - "Cryptography"
  - "Protocols"
  - "TSS"
katex: true
---

The security framework we follow is [MPS](https://eprint.iacr.org/2023/187),
which provides similar guarantees to UC security,
but in a state-separable proof flavor.

Unfortunately I don't think there's a much better introduction to the framework
than the paper itself, although the paper should be relatively approachable.

# Notation

We use a kind of vector notation pervasively.
The basics are that:
$$
x_i
$$
implicitly means that we're parametrizing a piece of code over
every value of $i$ as it ranges over some number of values.
Usually this is over $[n]$, the number of parties.

For example, a function named $F_i()$ would in fact
mean that we're writing down several functions at the same time,
parametrized by $i$.

Any index that isn't free doesn't range.

For example a value $y\_{ij}$ inside of a function $F_i$ would
denote the entries of a vector, ranging over $j$,
since $i$ is fixed.

For example:

$$
\begin{aligned}
&\underline{F_i(m)}:\cr
&\enspace
  y\_{ij} \gets m
\cr
\end{aligned}
$$
would denote several functions $F_i$, each of which modifies
an entire column of the matrix $y$.

We can also perform operations point wise, like $a\_{ij} \gets b\_{ij} + c\_{ij}$.

Finally, we use $a\_\bullet$ to denote all of the elements
of some vector.
For example $\text{Hash}(x\_{i\bullet})$ would hash an entire column
of a matrix.

Sometimes we also use this for arguments, e.g: $F_i(x\_\bullet)$
denotes several functions, each of which takes an entire vector,
whereas $F_i(x_j)$ denotes several functions, parametrized over both $i$ and $j$ (if these aren't already bound), each taking a single value,
rather than a vector.

If $x\_\bullet$ is in scope, we can of course write $x_j$ to denote individual entries.

## Conventions

$\mathcal{P}$ or $\star$ denotes all the parties, usually the set $[n]$,
$\mathcal{H}$ is the set of honest parties, and $\mathcal{M}$ the set of malicious parties.

We often use $i$ to range over honest parties, and $k$ to range over malicious
parties.

### Games

We also use $A \circledcirc B$ to denote $(A \otimes 1(B)) \circ B$,
i.e. $A$ lives alongside the game $B$, but also reexports its functions.

### Misc

$\lambda$ is the security parameter, $t$ the threshold needed to sign,
and $\texttt{01} = \\{0, 1\\}$.

# Aborts

We often assume an ambient abort functionality $F[\text{Stop}]$.
The basic operation it provides is $\texttt{stop}(S, w)$,
which "stops" all parties in a given set $S$ after a round $w$.
Each party can check whether or not it has stopped after a given
round via the functionality.

A convention we use is then via the notation $\texttt{wait}\_{(i, w)}$
this allows a party to wait for a condition, while also checking
that they're not supposed to stop at a given waitpoint $w$.
If an honest party detects that, then they'll trigger
a stop themselves, via $\texttt{stop}(\star, w)$,
to cause others to abort, and then they'll "die".

When a game is "dead", it means that it responds to all function calls
with a dummy value indicating this fact.
Furthermore, if a game calls a function that returns "dead",
by convention, it will also then die.

The precise semantics of aborts aren't particularly important,
but we just need some way of having parties signalling that something
bad happened, and we want to be able to selectively trigger aborts
to aid in simulation.

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
&\enspace \texttt{wait}_{(i, w)} \forall i \neq j \in P.\ m_{j \to i, w} \neq \bot\cr
&\enspace \texttt{return } [m_{j \to i, w} \mid j \in P]\cr
\end{aligned}
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

Note that this functionality implicitly depends on $F[\text{Stop}]$,
in that we use the notation $\texttt{wait}_(i, w)$,
which will check if we need to stop ourselves while waiting for a message,
because of an abort.

# Functionalities

Here are some additional useful ideal functionalities,
for random oracles, and for ZK proofs.

## Hashing

$$
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F[\text{Hash}$
}\cr
\cr
&h[\bullet] \gets \bot\cr
\cr
&\underline{
  \text{Hash}(x):
}\cr
  &\enspace
    \texttt{if } x \notin h:
  \cr
  &\enspace\enspace
    h[x] \xleftarrow{\\$} \texttt{01}^{2 \lambda}
  \cr
  &\enspace
    \texttt{return } h[x]
  \cr
\end{aligned}
}
}
$$

## ZK

$$
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F[\text{ZK}(\varphi)]$
}\cr
\cr
&\Pi[\bullet] \gets \bot\cr
\cr
&\underline{
  \text{Prove}_i(X;x):
}\cr
  &\enspace
    \texttt{assert } x \cdot G = X
  \cr
  &\enspace
    \pi \xleftarrow{\\$} \texttt{01}^{2\lambda}
  \cr
  &\enspace
    \Pi[\pi] \gets x
  \cr
  &\enspace
    \texttt{return } \pi
  \cr
\cr
&\underline{
  \text{Verify}(\pi, x):
}\cr
  &\enspace
    \texttt{return } \Pi[\pi] \neq \bot \land \Pi[\pi] = x
  \cr
\end{aligned}
}
}
$$

$$
\boxed{
\small{
\begin{aligned}
&\colorbox{FBCFE8}{\large
  $F[\text{ZK}(\psi)]$
}\cr
\cr
&\Pi[\bullet] \gets \bot\cr
\cr
&\underline{
  \text{Prove}_i(A, B, C; a):
}\cr
  &\enspace
    \texttt{assert } a \cdot G = A \land a \cdot B = C
  \cr
  &\enspace
    \pi \xleftarrow{\\$} \texttt{01}^{2\lambda}
  \cr
  &\enspace
    \Pi[\pi] \gets a
  \cr
  &\enspace
    \texttt{return } \pi
  \cr
\cr
&\underline{
  \text{Verify}(\pi, a):
}\cr
  &\enspace
    \texttt{return } \Pi[\pi] \neq \bot \land \Pi[\pi] = a
  \cr
\end{aligned}
}
}
$$