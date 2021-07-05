---
title: "Introducing Nuntius"
date: 2021-06-27T23:37:41+02:00
draft: true
katex: true
tags:
  - "Math"
  - "Cryptography"
  - "Security"
---

Recently, I made a toy E2E encrypted messanger, called
[nuntius](https://github.com/cronokirby/nuntius). I had fun tinkering
on it, and thought that some of the cryptography involved would be
fun to explain.

<!--more-->

# Application Overview

I've called Nuntius an E2E encrypted messenger. You connect to a server,
requesting to chat with a certain person. When that person connects
to the server in turn, you can start sending messages to eachother.
Calling this a messanger is perhaps a bit generous. It's more of
a session based chat application, like IRC. I'd expect a messanger
to support multiple users, and asynchronous messaging, but I'll
get to this point later.

For a more concrete overview, let's see what CLI commands are actually
necessary to start chatting. Using the
[kong](https://github.com/alecthomas/kong)
library for command line option parsing, I've generated useful help
screens for free, including the following splash screen:

```txt
→ ./nuntius --help
Usage: nuntius <command>

Flags:
  -h, --help               Show context-sensitive help.
      --database=STRING    Path to local database.

Commands:
  generate
    Generate a new identity pair.

  identity
    Fetch the current identity.

  add-friend <name> <pub>
    Add a new friend

  server [<port>]
    Start a server.

  chat <url> <name>
    Chat with a friend.

Run "nuntius <command> --help" for more information on a command.
```

The first step is generating a key pair, using `generate`:

```txt
→ ./nuntius generate --database .test/1.db
nuntiusの公開鍵3176cb883a9efd0e6002ed539dc6c03a504d982a47945e30fb0ac06e7dbc3b94
```

This prints out your public identity. Your public identity defines
who you are according to this system. You share this identity with
people you want to chat with, and it allows them to contact you.
This will save a corresponding private key in a local file.
This is actually an SQLite database, whose path is configurable,
as shown by the command I've used. If you don't specify a path,
then this file goes into your home directory.

If you ever forget what your identity key was, you can use
`identity` to fetch it from the database.

Since identity keys are used to identify the people you want to chat with,
you can associate identity keys with more familiar names. When your friend
John tells you what his Nuntius identity is, you can save that in
your local database, so that you can chat with John later.

This is done with the `add-friend` command:

```txt
→ ./nuntius add-friend John nuntiusの公開鍵3176cb883a9efd0e6002ed539dc6c03a504d982a47945e30fb0ac06e7dbc3b94
```

Then, you can chat with your friend John using Nuntius:

```txt
→ ./nuntius chat http://localhost:1234 John --database .test/1.db
```

You can start sending messages when John connects to the same server
on his end. This requires a server to connect to. The server forwards
messages between the two of you, but can't read their content at all.
I've used a local server path, but you can replace this with any other
path. I've simply chosen this one, because it's the default path
for the server command.

This command runs a Nuntius server:

```txt
→ ./nuntius server 1234
```

This takes a port as an argument, but will use `1234` as a default port
otherwise.

So, that's just about it. The big limitation here is that both users
need to be connected at the same time, to the same server. The CLI
interface is also a bit clunkier than a GUI interface. I wanted to
focus on the E2E encryption aspect, which is why I took these shortcuts.

To implement the encryption, I basically used bits and pieces of
how [Signal](https://signal.org/) works. They conveniently
have documentation for some of their abstracted protocols, which
I shamelessly reimplemented.

# Implementation

I used Go for the implementation this time, mainly for a change in pace.
I also wasn't implementing any primitives from scratch, just protocol
level constructions. This meant exploring Go's ecosystem of
cryptography libraries, which was pretty fun.

I might want to extend this little experiment into a more useful GUI app,
and I'm thinking of trying things out in Rust for that. The goal would
be to learn how to do things myself, not to rival with any other
system.

The rough idea of how the application works is simple. Two users
know eachother by their identity key, so they can perform an
exchange, deriving a shared secret. They can then use
that secret to encrypt their communication. Signal throws a twist
in both the exchange, and the encryption. They use a more complicated
exchange system, to reduce the security load on the long-lived
identity keys. They also use a ratcheting mechanism for encryption,
so that the encryption key is refreshed with new exchanges.

Let's get into how both of these work.

# Key Exchange

If your long term identity keys are Diffie Hellman keys, like X25519
keys, then you can both derive a shared secret, knowing
only the other's public key. There's no problem with this, per se.
This exchange is secure. One issue is that the identity key
is completely load-bearing. At each exchange, you need to rely
on it, without being able to rekey the exchange with new information.
Each exchange will also generate the same key, which isn't desirable
either.

## X3DH

Signal's solution to this is a protocol called
[X3DH](https://signal.org/docs/specifications/x3dh/). Instead
of using your friend's identity key to derive a shared secrete,
you instead use three keys of theirs. I believe this is
where the name comes from.

To perform the exchange, you use your identity key, $IK_A$,
as well as a a new ephemeral key $EK_A$. You use your friend's
long term key $\text{IK}_B$, as well as a signed prekey
$\text{SPK}_B$, and an ephemeral key $\text{OPK}_B$. 
The last two keys are fetched from the server, and only the signed
pre-key can be verifiably attributed to your friend, through
its signature.

Using these keys, you perform an exchange like this:

{{<img "1.png">}}

This gives you 4 shared secrets, which you can then shove into
a KDF to get a symmetric key. You can use this key to start encrypting
data, or to start a more complicated ratcheting process, as we'll
see later.

In all honesty, I don't fully understanding the reasoning behind
all of the keys involved here. I suspect that other variations
of the scheme can provide the same guarantees. There
are a few important ideas that
I think I do understand.

The first is that the exchange uses ephemeral information,
so that leakage of identity keys in the future doesn't allow
compromising past conversations. Using a prekey also reduces
load on the main identity key, and signing it prevents
an adversary from substituting their own one.

I think there might be something going on with deniability,
which might be way $\text{OPK}_B$ isn't signed. I'm not
entirely certain if this is the reasoning though.

## Setting up your keys

As I've just mentioned, $\text{SPK}_B$ and $\text{OPK}_B$ need
to be retrieved from the server, in order to start your exchange.
The first one is accompanied by a signature, and is intended
to be used multiple times. The second one is only intended to be used
once.

Your friend needs to have supplied these to the server in advance.
In the application, I handle these by resupplying keys as necessary
when chatting, since you have to connect to the server anyways.
I don't refresh $\text{SPK}$, although doing so periodically would
be a good idea. The main refresh is in supplying the server
with new bundles of $\text{OPK}$. The idea is that you generate,
say, 100 of these ephemeral keys, and sign the whole package
before sending it off to the server. The server can verify
that you generated these keys, by checking the signature.
This signature is of no use to attribute any of these ephemeral
keys to you. Each time an exchange is done with you, you burn
one of these one time keys, never using it again. If the supply
of one time keys on the server dwindles, you can refresh
it by sending a new bundle.

All of this management is transparent to how the application works,
but is important from an implementor's perspective.

## Starting a Session

Signal is intended to be used in an asynchronous setting.
You connect to the server and give it the necessary keys.
Then you can start and maintain new sessions with other users.
For the synchronous sessions, I had to adapt things a little bit.

In Signal's model, you start messaging someone by performing an exchange,
and then sending a message to Signal's server. The server dutifully
stores it until the receiver is ready. I wanted to avoid a store-and-forward
architecture here. Instead, I wait for to establish a synchronous
connection before forwarding messages.

This leads to a three way exchange:

{{<img "2.png">}}

First, you connect to the server, and wait. You take advantage
of this connection to send new one-time key bundles, if necessary.
Then, when your friend connects, you now have a synchronous
channel with them. The server notifies you of this, and sends
you the necessary information to perform an exchange. You do this,
and send out a message to your friend, including your ephemeral
exchange key, so they can do their side of the exchange.
From there on, you now have an encrypted channel you can use,
starting from the symmetric key you've derived.

# Ratcheting

## Single Key Limitations

## Chains

## Diffie-Hellman Ping Pong

# Limitations

## Session Based

## No Contact Discovery

## Limited Interface

# Conclusion
