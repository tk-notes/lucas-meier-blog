---
title: "The Web or The Internet?"
date: 2020-09-28
draft: false
description: "Ruminating on visions of document graphs"
path: "/posts/2020/09/the-web-or-the-internet"
type: post
image: "/posts/2020/09/the-web-or-the-internet/cover.jpg"
tags:
    - Rant
    - P2P
    - Internet
---

So this is going to be a bit of a Rant on some things I've been thinking about recently.

To set the background for this post, I've been working on a [note-taking app](https://github.com/cronokirby/bubble)
with a focus on individual blocks referencing eachother. Kind of like [Roam](https://roamresearch.com).
The idea is that this kind of system can be used for discrete *nodes* of knowledge, which
we can call blocks, for simplicity. Blocks don't contain much on their own, besides some text, and
references to other blocks. Those references can be hierarchical: you can declare one node to be a
child of this one. They can also have no hierachy at all: one node simply mentions another.

The advantage of a system like this comes from the references that *aren't hierarchical*. At least,
that's the advantage that I see in this kind of system, and what's piqued my interest
around this whole concept.

By allowing and showing references that aren't simply nesting, you're liberated from
the hierarchical approach, and can start working with a more natural and emergent
approach. You can work on discrete bits of information here and there, and see the connections
between different topics form as you gather more information into the system.

Having these bi-directional links between different blocks is very liberating, because you're
bringing all of the connections between topics to the forefront, rather than forcing
everything into a simple linear presentatio.

I was thinking of how I might extend this to something beyond a personal management system.
The representation of this system of knowledge is actually somewhat simple, precisely because
you can get away with making everything a block. Blocks have unique identifiers, and you reference
them by using those identifiers. Theoretically, you could reconstruct the entire knowledge
graph if you had access to all the blocks, by looking at the content of each block, seeing
what other blocks are referenced, forming edges in the graph. Internal representations
and storage of these representation are crucial for performance, since some types of queries
of very common in an actual application, but are an optimization, at the end of the day.

If you had some way of getting an irrefutable identifier for some block: something no
actor could disagree with, then you'd have a system for referencing information
unambiguously. My preferred solution would be to "freeze" certain blocks, sending them
out into the wild, this time with their identifier being a Cryptographic Hash of their contents.

In this way, once a node is out in the wild, in the "public graph", you could check the integrity
of nodes very easily. If I download a node by its identifier, then I can recalculate the hash
of its contents, and check that it matches the ID. I can be sure that I got the right node.

The potential of a system like this is very interesting. The promise is having a global untamperable
store of information, a system in which you could store knowledge, and navigate the
published works of the entire world.

Then I thought of how you might implement more complicated operations on this store
of knowledge. You see, one common operation would be finding the blocks associated
with some piece of text. Then I realized that this is usually provided in
the internet through search engines superimposed on the web that already exists. In a similar way,
someone could provide a search engine, and superimpose it over this. One issue here is that the document
system I've proposed here is static, so the engine wouldn't be inside of the system itself, but rather a layer
of abstraction over it.

But if you really think about it, the internet as it exists today is kind of a layer of abstraction over the
web of documents underneath. It's a bit funny to think about this as someone who's grown up with
the internet as a set of many monolithic applications, but there was a time (at least it seems there was)
where the internet was principally a set of documents, connected through hyperlinks.

In fact, The World Wide Web, as it used to be called, is quite close to the vision of a unified knowledge
base that I talked about towards the start of the post. The main difference is using
a name system with DNS instead of the cryptographic hash system. With the naming system
it's at least feasible to remember the addresses of sites you like, at least for a certain number of them.
The disadvantage of the cryptographic naming system is apparent here in that all of
the identifiers for different documents are completely opaque. Very easy for computers to work with,
but you basically need an abstraction layer like a search engine to be able to do anything useful
with these references.

On the other hand, with the original WWW, the search engine doesn't seem inevitable. In some sense,
I see the search engine as the first monolith imposing itself over what was the Web, and what became
the internet.

The way I see it, by having the search engine as a principal abstraction that everyone depends on,
you're already providing a layer of separation with the original vision of the web as a
a literal *web* of connected documents.

At first you might start occasionally searching for some entry point to a portion of the web,
but then spend most of your term navigating through the connections themselves. You could have
different search engines with different specialties, and for different purposes.

Nowadays, the web is structure much more like something an Orb spider would make, with a centralized hub,
being Google, the principal search engine. The depth of connections you end up reaching
is quite shallow, most interactions with the internet come through searching something, and then maybe
going a few pages deep. But the leisurely strolls in the backyard of the internet seem long-gone. Now it's
a lot more like walking into the help-desk of some office building.

Of course, there are still some sites that you navigate to directly. But these are much less documents
than *applications*. Now, don't get me wrong. I think that being able to run applications simply
by having a link to their code is a vast improvement over the hassle of downloading things. Universal
computing as another idea I find very intriguing, and something I tend to agree
with as somewhat of a technological maximist. That being said, it's definitely a large deparature from the vision of **The Web**
in the literal sense, that of interconnected documents. 

Another problem with the deparature from the era of linked documents is the complexity
of the browser, and thus the search engine. With the web-platform now including
everything from documents, to games, to complex layout and media-streaming, writing a web-browser
basically requires 80% or so of the knowledge we have about how computers and software work.
The complexity of a web-browser is only surpassed by an operating system, I'd wager.

The issue with that is that web-browsers themselves are becoming more and more centralized. There are a
handful of browsers left, and the idea of starting a new blank-slate browser is basically impossible.
Any viable alternatives will be forked engines reskinned in a different way.

Of course, you can start from a blank-slate and end up with a subset of the web we have nowaday,
maybe even a large enough subset to make Netscape users jealous!
Perhaps that is the way forward: someone carving out a subset of the web, declaring that
the "new web" and somehow convincing people to migrate towards this new more distributed system.

It's difficult to see any system like that not end up in the same centralized state we find ourselves in today.

With that phrasing, it sounds like I think that a centralized web is bad in and of itself.
I kind of feel that way, but I don't really think that's true. I think a centralized
web is more so of a threat than it is an *immediate* issue. You can't really deny that the internet
is very useful. But the way the internet has evolved, it seems a lot more like a few nodes
acting as applications that happen to be distributed over HTTP, without a lot of interconnection.
If the web were captured by a single actor, you might eventually end up in a similar situation
to the competitive capture and monopsony of App Stores, which hide computing away from their users,
and make it much more difficult to participate.

One beautiful thing about the old web is the idea of the personal website. Nowadays, this is
almost anathema. With the advent of social media, personal websites have been replaced
with personal sections of larger, monolithic websites. At the end of the day, you're subject
to the whims and manipulations of those platforms, which do not have your interests in mind.
When browsing and participating in the web as an individual, you can keep those interests
fron and center.

With this whole pandemic going on, it's basically impossible to imagine how society
would function without the infrastructure of the internet at this point. It's insane
how what was a novelty in the late 90s became a mainstay and pillar of Western Society.

It's precisely because of how important the internet is that it's a bit disappointing
that it's been captured and siloed into these centralized hubs. Maybe if we had more of a Web
than an Internet we'd be able to have an even more creative infrastructure?

I would love to be able to see what a true Web of independent creators would look like,
rather than the shadow of that Web that we see today...
