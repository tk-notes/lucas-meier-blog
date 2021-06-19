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

One big difference between browser applications and native applications
is that the latter are *signed*. In this case of app stores,
a developer has to submit a signed bundle to the store, which
can then be further signed by the manager of the store,
i.e. Apple, or Google, aut cetera. Operating systems can also implement
signing for executables. MacOS does this, providing users with
a nagging warning when they try to run unsigned executables. Apple
also charges $100 a year for the privilege of being able to sign apps,
but I digress.

This signing means that developers have to testify that they
released this particular code artifact. This prevents someone
else from trying to pass their app as the app in question,
as well as the developers trying to give you a version that differs
from the one that they previously uploaded.

This allows new versions to be vetted by different people, and then you
can rest assured that the code you're installing actually matches
the version that has been reviewed by other people.

Now, if you're using TLS in the browser, you do have some integrity
guarantees for web applications. Because of PKI, you know that
you're talking with the right server when you fetch the web application.
Unfortunately, you don't know that the server is sending you the
right application. An adversary can't impersonate the company
providing this application, but the company itself might be compromised.

## Releases

This signing process further encourages a system of discrete releases,
rather than a continously changing application. This has the disadvantage
of providing a certain rigidity, since users have to install
new versions themselves, perhaps with a certain lag. This rigidity
is an advantage in our case.

Having discrete releases allows these releases to be first vetted by
other people. For example, different users can check try building
the app from an open source snapshot, and check that it matches
the released bundle. You wait to install the new version until
you're sufficiently confident in its integrity. You can also
keep using an older version if you don't like the changes in a newer one.

Contrast this with a web application, where at any point in time,
you might be served a new version of the application,
without realizing it at all.

## No Targeted Attacks

Another major advantage of native apps is that fixed signed releases
prevent targeted attacks.

With an app store, the developer first uploads a version of the app
to the store itself, and then you download from the store. This prevents
the developer from sending a compromised version targeted to you
specifically, since the store controls the downloads, and not
the developer.

The use of signing prevents the store from sending you a bad version,
at least if you check that the signature is valid on your device.
Furthermore, the companies providing app stores, like Google or Apple,
are generally considered trustworthy. If they weren't, then your mobile
device would be running an operating system of their making anyways.

Even without an app store, having signed releases prevents targeting,
since you can check that the version of an app that you downloaded
matches a known public release, based on its signature.

# Why Provide an E2E Web-App?

So, with all these disadvantages enumerated, are there still good reasons
to try and implement E2E encryption in the browser? With so many applications
claiming to do so, there must be some kind of legitimacy to this
practice, right?

## Accepting Trust

The most common situation is that developers accept users
to give them the extra trust around their E2E encrypted app. They
estimate that the extra trust required from users is worth
avoiding the extra effort to develop a native application,
and the added convenience of being able to open the application
in the browser.

Remember that there's no technical reason preventing developers
from implementing E2E encryption in the browsers. Instead,
it's just that the app delivery model requires users to place
greater and more enduring trust in the developers.

If the choice is between a web app with E2E encryption, or
a web app without it, I would choose the latter. You should
be aware of the additional trust you place in the developers
of this application, but that doesn't mean you shouldn't eschew
the benefits completely.

## Reducing Liability

From the paranoid user's perspective, it seems difficult
to trust a web application claiming to provide E2E encryption.

One of the major advantages of providing E2E encryption in the
browsers comes from the developer's perspective. By encrypting
the personal data of users of the application, the developers
can claim to have no liability over what users store.

Even if a developer is asked to return logs of user activity,
the most they can provide is metadata. The actual contents
of the user's activity are always encrypted.

Having a rule that no user data whatsoever is stored by the application,
as enforced by its E2E encryption policy, this provides an overachieving
way of achieving compliance with data protection regulations.
There's no sensitive data to handle correctly if all data
is encrypted in the first place.

I am not a lawyer, and I don't know if governments are sensible
enough to recognize this fact.

# Some Solutions

Is all hope lost? Can we provide some semblance of trust to
web applications?

## Use Native Apps

One obvious solution is to avoid asking the extra trust from your
users, and simply provide a native application. Then you get
all of the additional benefits we've enumerated previously.

Note that there are frameworks to easily migrate web applications
to native ones, such as the infamous Electron. This framework
is somewhat maligned, because of its reputation of being
a bit of a resource hog. Nonetheless, many web applications
have found it the simplest path towards a native application,
such as Discord, or Slack.

## Local Bundles

Another possibility is to provide a "local" web app. In this model,
you still provide a web application, except that instead of serving
it dynamically when a user requests a URL, you instead provide
archived releases, which the user then serves locally
to their own machine. This can take the form of an installed executable,
which sets up a small webserver serving the application,
and then directs the user to their browser, where they can use it normally.

This provides the aforementioned advantages of a native application,
while requiring minimal effort on the part of the developer to
setup this bundle.

## Pinned Versions

Browsers now support a feature called *subresource integrity*.
This allows external resources of a webpage, like JavaScript,
or CSS files, to by linked to alongside a hash of their contents.
This ensures that whatever gets loaded through these external
links matches the expected content.

The problem is that the root webpage file containing these
hashed links can still change dynamically. From the users
perspective, the problems with web applications that
claim to be E2E encrypted is still present.

One potential solution would be to allow users to "pin"
specific snapshots of these root files, or to have a convenient
way to use known, vetted versions of these root files. This way,
you'd regain the versioned release model, all while allowing
the convenience of accessing an application via the web.

One way of doing this would be to lock in a specific
hash of the root file. Then, when you navigate to the
application link, your browser would alert you if the
hash changes, and perhaps guide you into updating your pinned
version, if you want to upgrade to the new version of the application.

There is also a standard in progress which would provide a form
of bundling for web applications,
which might able to include this kind of integrity check. This
would make web applications on par with app store models
in terms of integrity checks.

# Conclusion

If you have one takeaway from this post, it's that there's
no technical issue preventing applications that do E2E encryption
in the browser: it's just a matter of needing more trust. You should
prefer an E2E encrypted version of an application, but be careful
with vendor claims about the trustless nature of the application.
Ultimately, web applications require more enduring trust
compared to native applications.

One interesting analysis of where these claims can sometimes
fall short is by Nadim Kobeissi, who looked at
some of the claims provided by ProtonMail about their
in browser web application.

Like with all vendor claims about security, it's healthy
to exercise your own judgement.
