---
title: "End-to-End Encryption in Web Apps"
date: 2021-06-20T12:29:22+02:00
draft: false
katex: true
tags:
  - "Cryptography"
  - "Security"
  - "Web"
---

End-to-end encryption is a very appealing guarantee of privacy,
and more applications want to provide this guarantee.
Web applications are popular, and they want to implement
this functionality in the browser.
What kind of guarantees does a user still have with a
web app, served to them dynamically?

<!--more-->

In this post, we'll first go over what guarantees end-to-end encryption
provides in the ideal case. Then we'll see how
web applications require additional trust from users.
Finally, we'll see a few
ways to address these concerns.

# The Promise of End-to-End Encryption

Many applications claim to provide "end-to-end encryption";
what does this term really mean? In summary,
this means that data transiting through the application's
servers is always encrypted, in such a way that only the endpoints
involved can decrypt it.

A heuristic test is Matthew Green's
[mud puddle test](https://blog.cryptographyengineering.com/2012/04/05/icloud-who-holds-key/).
Let's say a user slips in a mud puddle,
breaking their devices and losing their memory. They should have
no way to recover their data. Otherwise, the application
servers would have unencrypted backup data.

For example, your budgeting application
might let users synchronize their financial data between devices.
This synchronization should be encrypted.
Your servers can't read any of the data passing through them.
This data
is only available on the user's devices, the "ends",
so we say that this application is end-to-end (E2E) encrypted.

This can extend to multiple users:
E2E encrypted messaging apps are a typical example.
In this case, users want to send
messages to each other. To provide E2E encryption,
your server shouldn't know the content of these messages:
only the two users chatting together will be able to see them.

Some applications don't interact with a server at all.
If an application stores all data locally, and
doesn't support any kind of synchronization between devices,
then it wouldn't need a server. There's no need for encryption here,
because no data ever leaves the user's device in the first place.

But certain applications are collaborative by nature,
and most users have multiple computing devices nowadays.
Supporting communication between multiple devices or users
is a benefit to many applications.

E2E encryption lets an application to provide this collaboration
without compromising on privacy.
The user has complete control over their data. They
alone choose how to share it.

## Technical Implementations

This post isn't about how to implement E2E encryption.
All we really need to know is that implementing
E2E encryption involves some basic cryptographic primitives,
like symmetric encryption, signing, key exchange, etc.
These are not exotic primitives, and many libraries
implementing them exist.

# The Problem with the Browser

A common misconception is that the difficulty
in E2E encryption in the browser
lies in using
these basic cryptographic primitives.
This isn't the case; at least not anymore.

Firstly, you can "simply" implement these primitives
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
platform for an E2E encrypted application.

The problem is that users should be more suspicious of your application.

The advantage of web apps is that they require no installation process.
You give someone a link to your application, they
navigate to that link, download the webpage on the fly, and are then
immediately using the application. It's a very frictionless
and convenient process.

This is also why E2E encryption in the browser is suspicious.

The problem is that you download a new version of the application
each time you use it. This means that you need to trust that the application
isn't compromised each time you use it.
Contrast this
with a native application. Once it's installed, you can choose
whether or not you want to update it, and wait until other people
have vetted a new version.


A compromised application would completely break
the security model of E2E encryption. For example, a chat application
could simply collect all the messages you type out, logging
all of your communication.

Because of TLS,
a third-party can't pretend to serve you that application. Rather,
you need to trust the providers of the application to not
have done anything fishy.

Needing to trust the developers each time you use the application
is at odds with the typical claims around E2E encryption. Many
applications claim to not require any trust from their users,
despite serving them code dynamically.

## Targeted Attacks

Another major flaw in web applications is that they
can attack specific users.
Not only can you
easily start serving a compromised version of an application,
you can also be selective in who you compromise.

If a compromised version is sent to all users,
then it's possible that somebody will notice
the change, and alert everyone else.
With a web applicaiton, it's possible to serve a compromised
version to a specific user, leaving them none the wiser.

This is because you query a server in to receive
the application each time you use it.
A server can look at which user is requesting the application, based
on their IP address, cookies, or other forms of fingerprinting,
and then serve them a compromised version.

The dynamic and targeted nature of the browsers
makes a shaky foundation for E2E applications.

# Native Apps are Different

Native apps don't suffer from these issues, at least not to the same
extent. Ultimately, you do need to trust the code of a
native application. A compromised application can exfiltrate
your sensitive data, even if you got it from an app store.

The main difference is that you don't have dynamic and
targeted versions of an application. Releases are slower,
and can undergo a lot more scrutiny and collaborative vetting. 

## Signing

Unlike web applications, native applications are usually *signed*.
With app stores,
a developer has to submit a signed bundle to the store, which
can then be further signed by the manager of the store,
i.e. Apple, or Google, aut cetera. Operating systems can also implement
signing for executables. MacOS does this, providing users with
a nagging warning when they try to run unsigned executables. Apple
also charges $100 a year for the privilege of being able to sign apps,
but I digress.

Signing makes developers testify that they
released this particular code artifact. This makes sure
that you're downloading a version of an application
approved by the developers.
This allows new versions to be vetted, and you can make sure
that you're installing the code that has actually been reviewed
by other people.

You do have some integrity
guarantees for web applications,
because of HTTPS. You know that
you're talking with the right server when you fetch the web application.
Unfortunately, you don't know that the server is sending you the
right application. An adversary can't impersonate the company
providing this application, but the company itself might be compromised.

## Releases

This signing process encourages discrete releases,
rather than a continously changing application. This
rigidity might be a disadvantage,
since users have to install
new versions themselves.
Some users might keep old versions for a long time.
But this rigidity
is an advantage in our case.

Having discrete releases
enables vetting by other people.
Different users can try building
the app from an open source snapshot, and check that it matches
the released bundle. You can wait to install the new version until
you trust its integrity. You can also
keep using an older version if you don't like the changes in a newer one.

Contrast this with a web application, where at any point in time,
you might be served a new version of the application,
without realizing it at all.

## No Targeted Attacks

Another advantage of this release model is that it prevents targeted attacks.

With an app store, the developer first uploads a version of the app
to the store itself, which you then download from the store. This prevents
the developer from sending a compromised version to you
specifically, since the store controls the downloads, and not
the developer.

The use of signing prevents the store from sending you a bad version,
at least if you check that the signature is valid on your device.
Furthermore, the companies providing app stores, like Google or Apple,
are generally considered trustworthy. If they weren't, then
you shouldn't trust the mobile device running their operating system.

Signed releases prevent targeting, even without
an app store.
You can check that the version of an app that you downloaded
matches a known public release, based on its signature.

# Why Provide an E2E Web-App?

So, with all these disadvantages enumerated, are there still good reasons
to try and implement E2E encrypted web apps? With so many applications
claiming to do so, there must be some kind of legitimacy to this
practice, right?

## Accepting Trust

The most common situation is that developers require that users
give them extra trust around their E2E encrypted app. They
estimate that the extra trust required from users is worth
avoiding the extra effort to develop a native application,
and the added convenience of a web application.

Remember that there's no technical reason preventing developers
from implementing E2E encryption in the browsers. Rather,
the app delivery model requires users to place
greater and more enduring trust in the developers.

If the choice is between a web app without E2E encryption
a web app with it, I would prefer to have it. You should
be aware of the additional trust you place in the developers
of this application, but that doesn't mean you shouldn't eschew
the benefits completely.

## Reducing Liability

From the paranoid user's perspective, it seems difficult
to trust a web application claiming to provide E2E encryption.
That doesn't mean that there are no advantages to the developer.

E2E encryption reduces developer liability. By encrypting
the personal data of users of the application, the developers
can claim to have no knowledge of this data.
Even if a developer is asked to return logs of user activity,
the most they can provide is metadata. The actual contents
of the user's activity are always encrypted.

If an application stores no user data, as enforced by E2E encryption,
then it's easily compliant with data protection regulations.
There's no sensitive data to handle correctly if all data
is encrypted in the first place.

I am not a lawyer, so I don't know if governments are sensible
enough to recognize this fact.

# Some Solutions

So far, we've painted a pretty grim story of E2E encryption
in web apps. I don't think that they're dead in the water though.
A few slight tweaks to their distribution model would let
them provide similar guarantees to native apps.

## Use Native Apps

One obvious solution is to require less trust from your
users, and simply provide a native application. Then you get
all of the additional benefits we've enumerated previously.

There are frameworks to easily migrate web applications
to native ones, such as the infamous
[Electron](https://www.electronjs.org/). This framework
is somewhat maligned, with a reputation of being
a resource hog. Nonetheless, many web applications
have found it the simplest path towards a native application,
like [Discord](https://discord.com/) or
[Slack](https://slack.com).

## Local Bundles

You could also provide "local" web app. In this model,
you still have a web application, except that instead of serving
it dynamically, you instead use
archived releases. The user downloads a static release,
and then serves locally
to their own machine. This can take the form of an installed executable,
with sets up a personal server for the application.

This provides the aforementioned advantages of a native application,
while requiring less effort by the developer.

## Pinned Versions

Browsers support a feature called
[subresource integrity](https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity).
This allows external resources of a webpage, like JavaScript,
or CSS files, to be referenced alongside a hash of their contents.
This ensures that whatever gets loaded through these external
links matches the expected content.

The problem is that the root webpage file containing these
hashed links can still change dynamically. From the user's
perspective, the problems with web applications that
claim to be E2E encrypted are still there.

One solution would be to allow users to "pin"
specific snapshots of these root files. You could also have a convenient
way to use known, vetted versions of these root files.
You'd regain the versioned release model, all while allowing
the convenience of accessing an application via the web.

One way of doing this would be to lock in a specific
hash of the root file. Then, when you navigate to the
application link, your browser would alert you if the
hash changes. If you want to upgrade your version of the application,
you could easily do so by changing the locked in hash.

There is also
[a standard](https://github.com/WICG/webpackage)
in progress which would provide a form
of bundling for web applications.
This could include this kind of integrity check. This
would allow web apps to be distributed in a similar way to native apps,
along with similar integrity checks.

# Conclusion

If you have one takeaway from this post, it's that there's
no technical issue preventing applications that do E2E encryption
in the browser: it's just a matter of needing more trust. You should
prefer an E2E encrypted version of an application, but be careful
with vendor claims about the trustless nature of the application.
Ultimately, web applications require more enduring trust
compared to native applications.

One interesting analysis of where these claims can sometimes
fall short is by Nadim Kobeissi, who
wrote [a paper](https://eprint.iacr.org/2018/1121.pdf)
going over
some of the claims ProtonMail makes about their
web application.

Like with all vendor claims about security, I'd encourage you
to exercise your own judgement.
