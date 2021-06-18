---
title: "End-to-End Encryption in The Browser"
date: 2021-06-13T16:27:38+02:00
draft: true
katex: true
tags:
  - "Cryptography"
  - "Security"
  - "Web"
---

More and more applications are enabling end-to-end-encryption
for their users, enabling a great amount of privacy. Given the
popularity of web applications, some even try to provide
this functionality in the browsers. But what kind of guarantees
around security can be provided in the browser?

<!--more-->

In this post, we'll first go over what guarantees end-to-end encryption
can provide in the ideal case. Then we'll see how
the browser has additional security threats that can put these
guarantees into jeopardy. Finally, we'll see a few potential
ways to address the threats of the browser.

# The Promise of End-to-End Encryption

You hear a lot of applications claiming to provide "end-to-end encryption"
these days, but what does that term actually mean? In summary,
this means that the data processed by an application should
only be available to the user of that application, and whoever
they want to share it with.

For example, if you provide a budgeting application, you might
want the user to be able to sync their financial data between
devices. This synchronization should be encrypted, in such
a way that your server only sees encrypted data. Only the user's
devices will have access to the actual data. Since the data
is only available in the clear on the user's devices, the "ends",
we say that this application is end-to-end encryption.

This also extends to situations with multiple users. For example,
a common use-case for end-to-end encryption is the infamous
chat application. In this case, one user wants to send
messages to another user. To provide end-to-end encryption,
your server shouldn't learn the content of these messages,
only the two users chatting together will be able to see them.

Some applications don't involve interacting with a server at all.
For example, an application that stores all data locally, and
doesn't support any kind of synchronization between devices,
doesn't need a server. There's no need for encryption here,
because the sensitive data never leaves the user's device in the first place.

But certain applications are collaborative by nature,
and since most users have multiple computing devices nowadays,
supporting communication between multiple devices or users
is a great benefit to applications.

E2E encryption enables an application to provide this collaboration,
without compromising the privacy of the data handled by the application.
The user has complete control over their data, and are able
to keep it completely private.

## Technical Implementations

This isn't a blog post about how to implement E2E encryption.
All we really need to know for this post is that implementing
E2E encryption involves public-key cryptography, for
key-exchange and signing, as well as symmetric cryptography
for actually encrypting data. There are many libraries
providing these primitives, and they are certainly not very exotic.

# The Problem with the Browser

One misconception I've seen is that the hard part about
implementing E2E encryption in the browser is being able
to use the basic cryptographic primitives, like key exchange,
or symmetric ciphers. This isn't the case; at least not anymore.

First of all, you can "simply" implement these primitives
directly in JavaScript. Or rather, use a library that somebody
else has written, and even more people trust.

Secondly, browsers now natively support these basic primitives
through the [WebCrypto API](https://www.w3.org/TR/WebCryptoAPI/).
So you don't even need to worry about finding a good library.

Finally, now that WebAssembly is a thing, you can even convert
well known C libraries implementing these primitives
into a WASM bundle ready to be used in the browser.

So, the problem with the browser is not that it makes it difficult
to implement E2E encryption. In fact, from the application
developer's perspective, the browser is a perfectly adequate
platform on which to build an E2E encrypted application.

The problem is that users should have much more suspicion in your application.

The advantage of web applications is that they require no installation process.
You can give someone a link to your application, and then they can
navigate to that link, download the webpage on the fly, and are then
immediately using the application. It's a very frictionless
process, and ultimately very convenient.

This is also why E2E encryption in the browser is suspicious.

The problem here is that you download a new version of the application
each time you use it. This means that you need to trust that the application
isn't maliciously implemented each time you use it. Contrast this
with a native application, where once it's installed, you can choose
whether or not you want to update it, and wait until other people
have vetted a new version.

A maliciously implemented application would completely break
the security model of E2E encryption. For example, a chat application
could simply send whatever plaintext messages you type into the application
to some other server, and log all of your communication.

## Targeted Attacks

Another major flaw in browser applications is that not only
can you start serving a new compromised version of an application
on a dime, you can also be selective in who you compromise.

If a new version of an application is compromised, and sent
to all users, then it's possible that somebody will notice
the change, and alert everyone else.
With a web applicaiton, it's possible to serve a compromised
version to a specific user, leaving them none the wiser.

This is because you query a server in order to receive the code
for the web application each time you use it. A server can
look at which user is requesting the application, based
on their IP address, cookies, or other forms of fingerprinting,
and then serve them a compromised version of the application.

The fact that an application can change its code on each use,
and serve targeted versions to a specific user make the browser
not the most ideal platform to provide E2E applications.

# Native Apps are Different

Native apps don't suffer from these issues, at least not to the same
extent. Ultimately, you do need to trust the application code of a
native application. A compromised application can exfiltrate
your sensitive data, even if you downloaded it from an app store.

The main difference is that you don't have ever-changing and
targeted versions of an application. Releases are slower,
and can undergo a lot more scrutiny and collaborative vetting. 

## Signing

How signing is used to testify the origin of an app, and
in app stores.

Acknowledge that this is used in the browser as well.

## Releases

How slow releases help verify security.

## No Targeted Attacks

The app store acts as an intermediate verifier.

# Why Provide an E2E Web-App?

Why would you still want to provide an E2E web-app,
knowing these defaults

## Accepting Trust

If you are willing to trust the service more, then this
can work out.

## Reducing Liability

No need to store plaintext data at all.

Maybe mention Australia's encryption bill?

# Some Solutions

Go over some solutions to have e2e encryption

## Use Native Apps

Using native apps can regain these properties

## Local Bundles

Using a local web app.

## Pinned Versions

Some kind of browser based pinning.

# Conclusion
