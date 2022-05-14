---
title: "Some Cryptography Books I Like"
date: 2022-05-14T19:46:00+02:00
draft: false
katex: false
tags:
  - "Books"
  - "Cryptography"
---

This is just a brief post going over a few books on Cryptography
I've read, and would potentially recommend to people interested in the topic.

<!--more-->

In fact, I've yet to encounter a book on Cryptography that I
haven't found something likeable or useful in, so this is mainly
just a list of books I've read. I've had a few people ask
me for some pointers to get into Cryptography, so hopefully
this post can help those people out a bit. Personally,
I find learning through books very effective, because it's
easy to go at your own pace, and books are often very effective
at distilling a broad field into a very dense presentation.
And that's why my recommendations are mainly centered on
books, although I'm sure there are other great resources,
like videos or courses, outside of just books.

{{<note>}}
Throughout these reviews, I refer to "public key Cryptography"
and "symmetric Cryptography". Since some people reading
this may not be familiar with this demarcation, let me briefly
explain.

**Symmetric Cryptography** refers to a kind of Cryptography
in which a private key is required for all operations.
This concerns things like encryption, where you need a secret
key to encrypt messages, and then to later decrypt those messages.
Other topics of interest in this sub-field include hash functions,
block ciphers, etc.

**Public key Cryptography**, or asymmetric Cryptography,
refers to a kind of Cryptography in which some operations
require private information, while others don't. For example,
you need a private key to produce a valid signature,
but then anyone can use the public key to verify that signature.
This sub-field has flourished into a big tree of different
applications, and tends to lean more on mathematical tools
than symmetric Cryptography.
{{</note>}}

# Introductory Books

These books are suitable for someone completely
new to Cryptography. These are good introductions,
and may even be enough for someone who just wants to build
up some background knowledge on the subject.

### [Serious Cryptography - JP Aumasson (2017)](https://nostarch.com/seriouscrypto)

This is a really entertaining and accessible introduction
to Cryptography. Unlike the other books in this section,
this one doesn't really assume any kind of mathematical background.
In fact, this book is written with more of a programmer's
perspective in mind. For example, you see concrete examples
of which APIs to use to generate random numbers in Linux
as compared to Windows, which is a level of concrete detail
you don't really see in other books on Cryptography.

The book manages to cover quite a bit of material. I'd say
there's a larger focus on symmetric Cryptography,
with a lot of more in-depth material on ciphers, hash functions,
and things like that. There's a lot of detail
on different block cipher modes,
and on the construction of different hash functions, among other things.

There are also some great overview
chapters on public key Cryptography, including RSA
and Elliptic Curves, but these don't go as deep as the
other books in this section.

I'd recommend this book to anyone trying to get a taste
of Cryptography, and especially programmers trying to
get their background knowledge up to speed.

### [Cryptography - Simon Rubinstein-Salzedo (2018)](https://link.springer.com/book/10.1007/978-3-319-94818-8)

This book is essentially an undergraduate course in Cryptography,
like you'd see during a single semester of a CS degree.

As a disclaimer, I don't think this book should really be
read cover-to-cover, as there's a lot of material which
is very superfluous, in my view. For example, you can skip
the first 7 chapters, which covers historical ciphers,
unless you find that kind of thing interesting.

Nonetheless, this book does provide a useful introduction
to many topics in public key Cryptography. It's nice
to see an overview of Zero-Knowledge proofs, for example.
The presentation
is more formal than the previous book, and is suitable for
people who have a bit more background in math, or at least
in some kind of formal reasoning. This book also provides
a good amount of exercises, which can be very useful
for learning.

### [An Introduction to Mathematical Cryptography - Jeffrey Hoffstein, Jill Pipher, Joseph H. Silverman (2014)](https://link.springer.com/book/10.1007/978-1-4939-1711-2)

This book is a very cohesive introduction to
public key Cryptography. Like the previous book, this
is a somewhat formal presentation, similar to what you'd
find in an undergraduate course. This book also has
a greater focus on the mathematics, as the title suggests.
I don't think the background required is greater,
but it does require being a bit more comfortable with
mathematical reasoning than the other books in this section.

This book goes over the foundational assumptions
underpinning public key Cryptography, from RSA,
to discrete logarithms, to elliptic curves,
and even a small chapter on lattices. What's nice
about this book is that it provides a good overview
of the various attacks on these systems. For example,
different methods for factoring,or
for computing the discrete logarithm.
I still sometimes use this book as a reference, when I forget
the details of Pohlig-Hellman, or something like that.

I'd strongly recommend this book to anyone wanting
to get more seriously into Cryptography, although it might
get somewhat boring for some people given its particular focus.

# Intermediate Books

One way I'd describe the books in this section is
as "a good second book". These books are probably not
suitable for a complete beginner, but they don't assume
a lot of knowledge of Cryptography either.
A common trait with these books is that they bridge the
gap between higher level notions of Cryptography,
and the more formal notions, like game-based security,
which are used more often in papers.

### [The Joy of Cryptography - Mike Rosulek (2021)](https://joyofcryptography.com/)

This is a really well presented book, and probably has
some of the clearest explanations of game-based security
in this entire list. I think a lot of that clarity
is owed to the use of state separable proofs, which
would honestly make many areas of Cryptography a lot
clearer if people used them more. I'd recommend
this book even to more accomplished Cryptographers, just
as a good introduction to the proof technique.

In terms of the content, this book goes over a lot
of topics in symmetric Cryptography, along with a handful
of topics in public key Cryptography, with the focus
being on proving the security of schemes.

This focus on proofs makes the content on symmetric Cryptography
markedly different from the other books I've mentioned so far.
The focus is more so on proving that you can build large
secure constructions, like authenticated encryption,
from small assumptions, like pseudo-random functions.

One downside of this book is that there's not a lot of material
on public key Cryptography. I wish there were more material
here, because there would be tons of interesting things
to cover, even if only focusing on the provable security
aspect of composing constructions.

I'd highly recommend this book to anyone who hasn't read it,
especially people who know about Cryptography, but haven't
heard much about state separable proofs yet.

### [Cryptography Made Simple - Nigel P. Smart (2016)](https://link.springer.com/book/10.1007/978-3-319-21936-3)

This book is similar in scope to the previous one,
with less focus on provable security. There is still some
presentation of more traditional game-based security,
but this is slowly introduced, rather than being used pervasively.

Unlike the previous book, there's also a better coverage
of public key Cryptography. Many of the same topics
as in "An Introduction to Mathematical Cryptography"
are covered, but with more focus on algorithms rather
than the mathematics.

The book also has some very nice presentation on
more advanced topics, like Zero-Knowledge proofs
and secure multi-party computation, as well as concrete
examples, such as TLS certificates.

This book might even be a good introduction, but I think
the material is hard enough to make this more suitable
as a second book.

# Advanced Books

There's only one book in this section for now.
What I think would make books fit in this section is that
they require a good amount of background. What makes
these books interesting is that they help bridge the
gap between having a good amount of background knowledge,
and getting more comfortable with that knowledge
to the point of reading papers, which is what's left
after you get to this point.

### [A Graduate Course in Applied Cryptography - Dan Boneh, Victor Shoup (2020)](https://toc.cryptobook.us/)

This book provides a very thorough presentation of symmetric
and public key Cryptography, with a very present focus
on provable security, and in particular, game-based
security. This is a challenging, but also rewarding introduction
to a very rigorous kind of Cryptography. The book also
features plenty of exercises, which present a pretty wide
range of situations, and really help to flesh out
all of the nooks and crannies of the material.

The main downside of this book is that it's not finished yet.
This is a pretty big problem, actually, because one consequence
is that there are no references to the existing
literature, which is quite annoying. This book would
easily be 10 times more useful if it had those references,
because it would act as an even greater stepping stone
to get into the literature on Cryptography. Some
of the advertised future sections on Cryptographic
protocols would be great as well, since that's an area
that doesn't have as much focus throughout the rest of
these books.

Nonetheless, I think this book is such a good and challenging
read that I have to recommend it to everyone who wants
to dive deep into Cryptography, but I hope it does
get finished sometime soon.

# Specific Topics

These books are less about Cryptography as a whole,
but more so about very specific topics. These may
be interesting if you're interesting in that topic in particular.

## Elliptic Curves

These books are about Elliptic Curve Cryptography,
and mainly about algorithms for building and breaking
schemes using elliptic curves.

### [Guide to Elliptic Curve Cryptography - Darrel Hankerson, Scott Vanstone, Alfred Menezes (2004)](https://link.springer.com/book/10.1007/b97644)

If you want to implement elliptic curves, this book is a must
read. It's essentially a comprehensive guide on all of the
arithmetic needed to implement curves, from finite fields,
to various approaches to adding points, to scalar multiplication,
etc.

A lot of less popular kinds of elliptic curves are also
prominently feature in this book, such as optimal curves
over optimal extension fields. This probably reflects
the point in time where this book was written, where
we hadn't yet settled on a few curves over prime fields.

I still use this book as a reference quite a bit. Most
recently, I needed to implement a binary field,
and I referenced the algorithms in this book heavily.

One downside of this book is its age, so it doesn't
feature some of the newer constant time ladders for
scalar multiplication, among other things. There's been some interesting
work here in recent years, like
["Complete addition formulas for prime order elliptic curves"](https://eprint.iacr.org/2015/1060). Obviously, this book
couldn't have referenced a paper written 10 years in the future,
but an updated version of this book might be interesting
to incorporate these aspects. Some material on pairings
would be nice in an updated version as well.

If you want to implement elliptic curves from scratch,
this book is a valuable reference.

### [Handbook of Elliptic and Hyperelliptic Curve Cryptography - many authors (2005)](https://www.hyperelliptic.org/HEHCC/)

This is a very complete reference book on Elliptic Curve
Cryptography. I say *reference* book because this really
shouldn't be read cover-to-cover, or at least, I wouldn't
recommend doing that. This goes a lot more in-depth
than the previous book, with a much heavier focus on
mathematics.

Each of the chapters in the book is more or less independent,
although they do build on each other, with some of
the earlier chapters providing some more foundational
material. The chapters are almost like individual papers.
In fact, on a few occasions, I've stumbled upon papers
when researching a topic, only to realize that they
were actually chapters from this book.

For example, I had read the chapter on Montgomery Ladders
before realizing that it was actually an excerpt from
this book. I had read the paper multiple times too,
while implementing Curve25519, but it had never clicked.

This is a book you want to have on your shelf if you're
interested in Elliptic Curve Cryptography.

## MPC

I don't have many books on secure multi-party computation (MPC)
yet, but hopefully people write some more books!

### [Pragmatic MPC - David Evans, Vladimir Kolesnikov, Mike Rosulek (2018)](https://securecomputation.org/)

I read this one quite recently, and I'd highly recommend it.
This book is an overview of different MPC techniques,
from a very high-level. The book starts with a brief introduction
to simulation-based security, which is how we usually reason
about security when working with Cryptographic protocols,
and then jumps to describing various MPC protocols in
various threat models.

The book doesn't provide a very deep explanation of the protocols,
but it does provide enough to give you a high level overview
of the techniques involved, and how they compare with each-other.
A great thing about this book is that it's very well referenced,
so it's easy to use it as a stepping stone to dive
into the literature and learn more.

I highly recommend this book if you're interested in MPC.
It's also a short read, so if you're not interested in MPC,
you should still give it a look, and you might change that.

# Honorable Mentions

These are books that either don't fit into the list,
or that I haven't read, but have heard about.

### [Real World Cryptography - David Wong (2021)](https://www.manning.com/books/real-world-cryptography)

I have yet to read this book, although I've heard good
things about it, and have been wanting to check it out. From the little I've skimmed so far, I really like the illustrations
and the presentation style, and there's a lot of interesting
concrete applications presented here which you don't
find in other books.

This would probably be a good introductory or intermediate
book, so check this one out.

### [Cryptography Engineering - Niels Ferguson, Bruce Schneier, Tadyoshi Kohno (2010)](https://www.schneier.com/books/cryptography-engineering/)

I have read this book, on the other hand. In fact, I read
this book quite a few years ago before I was seriously
interested in Cryptography.

This book provides an interesting view on the concrete
applications of Cryptography, with a focus on what I'll
call more "traditional" Cryptography. So, a lot of symmetric
Cryptography, with a focus on various block cipher modes,
and usages of hash functions, along with some basic
uses of public key Cryptography, for encryption and signatures.

With a bit of hindsight, I think this book is great,
but perhaps a bit outdated. This book doesn't really
contain any mistakes, but I think the presentation of certain
topics would be different if the book were written now.
For example, I would like to see encryption focus more
on AEAD modes, over block cipher modes. For public key Cryptography,
I think presentations on Elliptic Curve Cryptography
would be important, if not even Post-Quantum Cryptography.
There's also more advanced applications of public key Cryptography
than just encryption and signatures, so I feel like presenting
those would be interesting as well.

Nonetheless, there's a lot of interesting material in this
book which can't be found elsewhere, like the chapter
on storing key material. The presentation of the book
is also really great, with a consistently approachable
and grounded writing style.

So, there's a lot of things to take away from this book,
but I wouldn't vehemently recommend it. On the other hand,
it is sort of a "classic" at this point, so it should
probably feature somewhere on your bookshelf.

# Addendum

There are more books than just the ones I've listed, so feel
free to tweet at me if I've made a glaring ommission :)
