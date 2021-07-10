---
title: "Signatures From Identification Schemes"
date: 2021-07-10T09:33:56+02:00
draft: false
katex: true
tags:
  - "Math"
  - "Cryptography"
  - "Security"
---

It turns out that all you need to make a signature scheme
is a way to prove your identity.

<!--more-->

In fact, you can develop such a scheme from first principles,
arriving at a deterministic signature scheme, commonly
known as "Schnorr Signatures". I'll be trying
to demonstrate that in this post. I hope to show that
this signature system was too obvious to be patented.

That being said, I am not a lawyer.

# Identification

You arrive at "the club". The bouncer asks you, "what's the secret password?".
You tell him the "secret password".
He now knows you are indeed a member of "the club".
The bouncer knows this, because you know the "secret password".

What you *know* is a great way to prove *who* you are.

Like many serious topics in security,
we start with a game. The game involves a challenger,
and yourself. You want to prove who you are to this challenger. You
do this by exchanging messages with eacho ther. We call this game
an "identification protocol". At least, I do.

The simplest protocol follows the club analogy.
We know some secret $\textcolor{red}{a}$. If a challenger wants
to check who we are, we can prove our identity by showing
that we know our this secret. The simplest way to do this is to
send them the secret:

$$
\begin{aligned}
&\small \text{Setup}\cr
&\textcolor{red}{a} \stackrel{R}{:=} \\{0, 1\\}^n \cr
\cr
\hdashline
&\small \text{A} \cr
&\textcolor{red}{a} \xrightarrow{} \cr
\end{aligned}
$$

There are numerous problems with this scheme.

First, there's no simple way to verify our identity.
We could share a secret among members of some group.
Members could then present this secret amongst themselves to verify
membership. But we can't prove our identity to a non-member.

The other problem is that we leak our secret every time we perform
the protocol. We don't want to leak our secret.

One way to avoid this is to mask our secret with another random value:

$$
\begin{aligned}
&\small \text{Setup}\cr
&\textcolor{red}{a} \stackrel{R}{:=} \\{0, 1\\}^n \cr
\cr
\hdashline
&\small \text{A} \cr
&\textcolor{red}{k} \stackrel{R}{:=} \\{0, 1\\}^n \cr
&\textcolor{blue}{s} := \textcolor{red}{k} \oplus \textcolor{red}{a} \cr
&\textcolor{blue}{s} \longrightarrow \cr
\end{aligned}
$$

Our proof of identity $\textcolor{blue}{s}$ can be shared publicly,
without revealing our secret. This is because masking our secret
using the xor ($\oplus$) operation makes it look completely random.

Addition modulo some number $L$ would also work:

$$
\textcolor{blue}{s} := \textcolor{red}{k} + \textcolor{red}{a} \mod L
$$

{{<note>}}
In fact, modular *multiplication* would also work, provided
we pick a number coprime to the modulus $L$.
{{</note>}}

The problem with this scheme is that our proof can't be verified.
The challenger has no
information about $\textcolor{red}{k}$. The value $\textcolor{blue}{s}$
looks completely random to them. They can't verify that we know
$\textcolor{red}{a}$.

We can't reveal $\textcolor{red}{k}$
without allowing the challenger to compute:
$$
\textcolor{red}{a} = \textcolor{blue}{s} - \textcolor{red}{k}
$$

We're at a bit of an impass.

# A Technical Leap of Faith

This section presents a jump in our technical tools. We introduce
a cyclic group $\mathbb{G}$, of order $L$, and with generator $G$.
Our hypothesis is that if we have a scalar $x \in \mathbb{Z}/(L)$,
then calculating the element $H = x \cdot G$ is easy. But, given $H'$,
finding
what $x'$ is needed to produce $H' = x' \cdot G$ is very difficult.
This problem is called the "Discrete Logarithm Problem".
There are groups where we know the problem is very easy,
like $\mathbb{Z}/(N)$. There are also groups where we *believe* this
problem is very hard. The main kind of group we use now is
a group of points on an Elliptic Curve.

If medium sized Quantum Computers come to exist, they will be able
to efficiently solve Discrete Logarithms.

I don't believe this is
a problem we'll have to deal with for at least 20 years.

Using a group is a breakthrough. This is because we can safely
share $\textcolor{blue}{K} = \textcolor{red}{k} \cdot G$, even
if we want to keep $\textcolor{red}{k}$ secret. This allows us to
verify secret equations like $\textcolor{red}{a} = \textcolor{red}{b}$
by instead verifying public equations, like $\textcolor{blue}{A}
= \textcolor{blue}{B}$.

Let's update our scheme with this in mind.

$$
\begin{aligned}
&\small \text{Setup}\cr
&\textcolor{red}{a} \stackrel{R}{:=} \mathbb{Z}/(L) \cr
&\textcolor{blue}{A} := \textcolor{red}{a} \cdot G \cr
\cr
\hdashline
&\small \text{A} \cr
&\textcolor{red}{k} \stackrel{R}{:=} \mathbb{Z}/(L) \cr
&\textcolor{blue}{K} := \textcolor{red}{k} \cdot G\cr
&\textcolor{blue}{K} \longrightarrow \cr
&\textcolor{blue}{s} := \textcolor{red}{k} + \textcolor{red}{a} \mod L \cr
&\textcolor{blue}{s} \longrightarrow \cr
\hdashline
&\small \text{B} \cr
&\textcolor{blue}{s} \cdot G \stackrel{?}{=}
\textcolor{blue}{K} + \textcolor{blue}{A}
\end{aligned}
$$

We now have both a private key, $\textcolor{red}{a}$, and a public
key, $\textcolor{blue}{A}$. Our public key acts as our identity.
The identification scheme lets us prove that know the corresponding
private key.

We also have an equation to verify the validity of our proof.
This equation works because:

$$
\textcolor{blue}{s}\cdot G =
(\textcolor{red}{k} + \textcolor{red}{a})\cdot G =
\textcolor{red}{k} \cdot G + \textcolor{red}{a} \cdot G =
\textcolor{blue}{K} + \textcolor{blue}{A}
$$

The reason this scheme works is pretty ingenious. We
generate a mask $\textcolor{red}{k}$, which hides $\textcolor{red}{a}$
when added to it. But, we can allow the challenger to verify our
operation, by providing a public commitment $\textcolor{blue}{K}$
to this random mask. This shifts a verification of a property
of our secrets $\textcolor{red}{k}$ and $\textcolor{red}{a}$
into a verification about our public values
$\textcolor{blue}{K}$ and $\textcolor{blue}{A}$.

There is one big flaw in this scheme though. Our challenger can reuse
our proof to impersonate us.

After seeing $\textcolor{blue}{K}$ and $\textcolor{blue}{s}$,
they can play this game with another challenger. If they just reuse
these values, they will win the game.
his allows them to convince a challenger
that they know the secret associated with $\textcolor{blue}{A}$.

The problem is that challengers don't act
any differently, so the answer to one challenger's question
can be used for any other challenger.

To fix this, we can have the challenger randomize their challenge.
Now we have to use this randomness to make a proof of identity
tailored to this request:

$$
\begin{aligned}
&\small \text{Setup}\cr
&\textcolor{red}{a} \stackrel{R}{:=} \mathbb{Z}/(L) \cr
&\textcolor{blue}{A} := \textcolor{red}{a} \cdot G \cr
\cr
\hdashline
&\small \text{A} \cr
&\textcolor{red}{k} \stackrel{R}{:=} \mathbb{Z}/(L) \cr
&\textcolor{blue}{K} := \textcolor{red}{k} \cdot G\cr
&\textcolor{blue}{K} \longrightarrow \cr
\hdashline
&\small \text{B} \cr
&\textcolor{green}{u} \stackrel{R}{:=} \mathbb{Z}/(L) \cr
&\longleftarrow \textcolor{green}{u} \cr
\hdashline
&\small \text{A} \cr
&\textcolor{blue}{s} := \textcolor{red}{k} +
\textcolor{green}{u}\textcolor{red}{a} \mod L \cr
&\textcolor{blue}{s} \longrightarrow \cr
\hdashline
&\small \text{B} \cr
&\textcolor{blue}{s} \cdot G \stackrel{?}{=}
\textcolor{blue}{K} + \textcolor{green}{u} \cdot \textcolor{blue}{A}
\end{aligned}
$$

Our challenger gives us a unique bit of randomness  that
we need to use when producing our proof. This means that the result
of one game can't be reused again, since the challenger will be
supplying a different value for $\textcolor{green}{u}$.
This scheme is a secure identification protocol, but some variations
of it wouldn't be.

For example, we could try multiplying $\textcolor{red}{k}$
with $\textcolor{green}{u}$ instead, forming:

$$
\textcolor{blue}{s} :=
\textcolor{green}{u}\textcolor{red}{k} + \textcolor{red}{a}\mod L
$$

The problem is that a challenger could supply $\textcolor{green}{u} = 0$,
and then our proof would reveal $\textcolor{red}{a}$.

Another idea would be to have the challenger send us $\textcolor{green}{u}$
before we send our commitment $\textcolor{blue}{K}$.
The problem is that this would allow us to cheat by choosing
a devilish commitment.

If we choose $\textcolor{blue}{s} \stackrel{R}{:=} \mathbb{Z}/(L)$,
and then commit to
$\textcolor{blue}{K} := \textcolor{blue}{s} \cdot G - \textcolor{green}{u} \cdot \textcolor{blue}{A}$, then our proof will be accepted,
despite us not using any secrets at all. Because of this, it's very important
that $\textcolor{green}{u}$ is chosen after $\textcolor{blue}{K}$.
In fact, we shouldn't be able to figure out what $\textcolor{green}{u}$
will be until *after* we've generated $\textcolor{blue}{K}$, otherwise
this flaw would remain.

Thanks to [Adrian Hamelink](https://twitter.com/adr1anh/status/1412420596974161921) for bringing this one to my attention.

# A Conceptual Leap of Faith

At this point we have a secure identification scheme. A challenger
can verify that you are who you claim to be. At least, they can
verify that you own the corresponding secret for a public key.
This protocol is dynamic. The transcript for one challenge can't
be reused for another challenge.

This is similar to what we want for
a signature. The data of a signature should show that the owner
of a public key signed off on this specific message. This signature
should be unusable for any other message.

Here's the leap of faith: a signature scheme is just an identification protocol,
where the challenger is the message itself.

Conceptually, think of a bank teller asking you to sign a document.
First, they challenge your identity. Satisfied with that, they then
put a stamp on the document. Effectively, you've signed the document.

A digital signature is like this, except the document itself is issuing
the challenge. What's beautiful is that we can devise a scheme
in which the way you're prodded by a message is transparent, and
knowable in advance. This will allow anyone with access to the transcript
of the game you've played with the message to verify that
you successfully proved your identity to the message.

This acts as a way of signing a message.

Operationally, this means replacing choices made by a challenger,
with deterministic, but unpredictable, functions of the message
and whatever knowledge it has at that point. To do this,
we use hash functions.

A Cryptographic hash function has many useful
properties. Essentially, a hash function acts like a random function
on its inputs. You don't really know what it's going to output
until you shove some stuff into it, and see what comes out.

We can use this to replace the random generation of $\textcolor{green}{u}$,
with deterministic generation, based on a message $M$. We now calculate:

$$
\textcolor{green}{u} := H(\textcolor{blue}{A} || \textcolor{blue}{K} || M)
$$

Now the "challenge" we receive is based on our public key, the commitment
we've provided, and the message "challenging" us to prove our identity.

You can conceptualize this as follows. Each message is like a unique
challenger in time. The function $H$ allows us to provide each of
these challengers with a way of generating a random $\textcolor{green}{u}$,
based on the information they'd have access to if the game were dynamic.
Since $H$ is effectively a random function, it's like each message
is actually challenging us with a random $\textcolor{green}{u}$,
since there's no way to predict what the output of the hash function
is going to be in advance, until we see the message.

We can now construct our signature scheme with this in mind:

$$
\begin{aligned}
&\small \text{Setup}\cr
&\textcolor{red}{a} \stackrel{R}{:=} \mathbb{Z}/(L) \cr
&\textcolor{blue}{A} := \textcolor{red}{a} \cdot G \cr
\cr
\hdashline
&\small \text{A} \cr
&\textcolor{red}{k} \stackrel{R}{:=} \mathbb{Z}/(L) \cr
&\textcolor{blue}{K} := \textcolor{red}{k} \cdot G\cr
&\textcolor{green}{u} := H(\textcolor{blue}{A} || \textcolor{blue}{K} || M)\cr
&\textcolor{blue}{s} := \textcolor{red}{k} +
\textcolor{green}{u}\textcolor{red}{a} \mod L \cr
&(\textcolor{blue}{K}, \textcolor{blue}{s}) \cr
\cr
\hdashline
&\small \text{B} \cr
&\textcolor{green}{u} := H(\textcolor{blue}{A} || \textcolor{blue}{K} || M)\cr
&\textcolor{blue}{s} \cdot G \stackrel{?}{=}
\textcolor{blue}{K} + \textcolor{green}{u} \cdot \textcolor{blue}{A}
\end{aligned}
$$

This is basically the identification scheme earlier, except that
it's no longer interactive. Instead, we produce a signature
$\textcolor{blue}{s}$ alone, which
consists of the public information we would
have sent to a challenger. Others can verify
the signature by computing the challenge $\textcolor{green}{u}$,
and verifying the equation, as challengers did previously.

Some variations on the hash would compromise security. For example,
if we didn't include $\textcolor{blue}{K}$ in the hash, then we would
run into the same issue as earlier, when $\textcolor{green}{u}$
was sent before $\textcolor{blue}{K}$. We would be able to forge
signatures by choosing a fake commitment $\textcolor{blue}{K}$.
By including the commitment inside the hash, we make sure that the
random output depends on knowledge of $\textcolor{blue}{K}$,
which prevents $\textcolor{blue}{K}$ from depending on
the value of $\textcolor{green}{u}$.

We could also choose not to include $\textcolor{blue}{A}$ in
the hash. I don't think this compromises security. But, it's a
good idea to throw this in there, that way our challenge also depends
on who we're challenging, in addition to the message.

This is a secure signature scheme. In fact, it's an incredibly
natural signature scheme. All the choices we've made in designing
it were pretty straightforward, and usually the easiest one to make.
I'd say that this scheme is a natural consequence of using
groups to create commitments, and of conceiving of messages
as challengers in an identification scheme. Both of these ideas
have widespread usage in more advanced protocols.

# Determinism

The scheme we have works. There's just a tiny ugliness. That ugliness
is that we need to generate, and not leak,
a random $\textcolor{red}{k}$.  This is a bit excessive for
what we need. We don't need a random $\textcolor{red}{k}$.
It would be fine if running our protocol on the same message
again produced the same signature. That might even be desirable.

What we actually need is for $\textcolor{red}{k}$ to be unpredictable,
based on the message $M$, and also based on our control of
$\textcolor{blue}{A}$,
i.e. based on $\textcolor{red}{a}$.

One straightforward solution is to choose:

$$
\textcolor{red}{k} := H(\textcolor{red}{a} || M)
$$

Even better would be to use a specialized *keyed hash*, designed
for this specific use case:

$$
\textcolor{red}{k} := H_{\textcolor{red}{a}}(M)
$$

A keyed hash should be very difficult to calculate without
knowledge of the key, and provide the same random guarantees
as a hash function after that. Sometimes concatenating the
key and the message is sufficient to produce a secure keyed hash,
sometimes it isn't.
Using a scheme that provides this as a black box is better
than rolling your own.

One problem is that we now use our key $\textcolor{red}{a}$
in two ways. As a key for a hash, and as a scalar for generating
the signature. It's generally a bad idea to use a key for multiple
purposes. Instead, you should use a single master key,
and then use a "key derivation function" (KDF) to derive new single-purpose
keys for your different use cases.

This means having two KDFs, $\text{KDF}_1$, and $\text{KDF}_2$, and then
deriving our hashing key and scalar from a master key:

$$
\begin{aligned}
&\textcolor{red}{mk} \stackrel{R}{:=} \\{0, 1\\}^n \cr
&\textcolor{red}{a} := \text{KDF}_1(\textcolor{red}{mk}) \cr
&\textcolor{red}{hk} := \text{KDF}_2(\textcolor{red}{mk}) \cr
\end{aligned}
$$

(Note that in practice, you can use a single KDF function, and
pass a different "context string" to derive different key values
for different purposes. Note also that KDFs 
are often simple wrappers around hash functions).

At this point, we can use $\textcolor{red}{a}$ as we did before,
and then use $\textcolor{red}{hk}$ exclusively for generating
our nonce $\textcolor{red}{k}$:

$$
\textcolor{red}{k} := H(\textcolor{red}{hk}, M)
$$

# Wrapping up

Collecting all we've seen so far,
I now present to you, a fully deterministic signature scheme,
depending on a cylic group $\mathbb{G}$ of order $L$, generated
by $G$, and a hash function $H$, used to create corresponding
keyed hash variants $H_k$, and KDF functions $\text{KDF}_x$:

$$
\begin{aligned}
&\small \text{KeyGen}\cr
&\textcolor{red}{mk} \stackrel{R}{:=} \\{0, 1\\}^n \cr
&\textcolor{red}{a} := \text{KDF}_1(\textcolor{red}{mk}) \cr
&\textcolor{blue}{A} := \textcolor{red}{a} \cdot G \cr
&(\textcolor{red}{mk}, \textcolor{blue}{A}) \cr
\cr
\hdashline
&\small \text{Sign}(\textcolor{red}{mk}, M) \cr
\cr
&\textcolor{red}{hk} := \text{KDF}_2(\textcolor{red}{mk}) \cr
&\textcolor{red}{k} := H(\textcolor{red}{hk}, M) \cr
&\textcolor{blue}{K} := \textcolor{red}{k} \cdot G\cr
&\textcolor{green}{u} := H(\textcolor{blue}{A} || \textcolor{blue}{K} || M)\cr
&\textcolor{red}{a} := \text{KDF}_1(\textcolor{red}{mk}) \cr
&\textcolor{blue}{s} := \textcolor{red}{k} +
\textcolor{green}{u}\textcolor{red}{a} \mod L \cr
&\text{sig} := (\textcolor{blue}{K}, \textcolor{blue}{s}) \cr
\cr
\hdashline
&\small \text{Verify}(\textcolor{blue}{A}, \text{sig}) \cr
\cr
&\textcolor{green}{u} := H(\textcolor{blue}{A} || \textcolor{blue}{K} || M)\cr
&\textcolor{blue}{s} \cdot G \stackrel{?}{=}
\textcolor{blue}{K} + \textcolor{green}{u} \cdot \textcolor{blue}{A}
\end{aligned}
$$

It's quite stunning that there's such a natural build up from
a simple and obviously broken identification scheme to
this little intricate signature scheme. I had a lot of fun dissecting
how this kind of signature works, and why things are set up the way
they are.

Hopefully I've conveyed some of that fun in this post.

# Addendum: Patents

Unfortunately, while this signature scheme is exceedingly natural,
it was covered by
[a patent](https://patents.google.com/patent/US4995082A/en),
filed by Claus Schnorr, from 1991 until 2008. Did this have the effect
of rewarding Schnorr for his invention, through licensing? No.

Instead, it lead to the creation of a different scheme for
signatures: [DSA](https://www.wikiwand.com/en/Digital_Signature_Algorithm).
This scheme has arbitrary modifications designed to skirt existing
patents. At least, some people have intuited that.

Personally, I think DSA is an ugly scheme compared to the more natural
Schnorr signatures. It is not only aesthetics, but the shape of DSA
makes building more complicated extensions difficult. Thresholdizing
Schnorr Signatures is easy, but thresholdizing DSA is substantially
trickier.

Patents are intended to spur innovation. In this case, innovation
was necessary avoid the arbitrary roadblocks imposed by patents.

I don't think patents are the right tool for incentivizing innovation in Cryptography.
