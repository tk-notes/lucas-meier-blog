---
title: "My Quick Attempt at Bluesky's Satellite Challenge"
date: 2021-09-03T22:43:09+02:00
draft: false
katex: false
tags:
  - "Cryptography"
  - "Decentralized"
  - "Security"
---

Twitter's Bluesky initiative created a [little challenge](https://blueskyweb.org/satellite)
where the goal was to verifiably link different digital identities together. This
is my attempt at this.
<!--more-->

I've create an Ed25519 public key (which can be used with my little tool [eddo](https://github.com/cronokirby/eddo)):

```txt
エッドの公開鍵335efaf80642878279cd8466625ace0b68c8779b5429404d46e01e95d13eb980
```

I've signed the following document, verbatim:

```txt
This is Cronokirby.
My twitter is https://twitter.com/cronokirby.
My website is https://cronokirby.com.
```

resulting in the following signature:

```txt
エッドの署名63143968dfa673f7c638e2b924cbc272c54eee96ce7bd41c15fca53bf0f0e76c9859924b184bbdc6d8257338c18aa5f34555e5c39efa5bc94fe9480d4bfd9008
```

I assume that they're two potential anchor points upon which people might base my "online identity". 
The first is this website, and the second is my Twitter. With my website, I assume that people will
trust that the content published on this domain was actually written by me. Thus, my endorsement
of a public key presumably means that I have ownership of it, and that they can associate it with this
website. By linking my Twitter, and signing it, I also prove that I have the associated private key,
and that this key has been used to attest to my Twitter.

For those who base themselves of off Twitter, I posted [a tweet](https://twitter.com/cronokirby/status/1433892710977572925)
with the same message and signature. Once again, by posting the public key on Twitter, my followers there
will associate that public key with me, and will also be satisfied that I have the private key, because
of the signed message.

The linkage is further reinforced by the message being signed, which links the key to the two other identities.

## Shortcomings

One major shortcoming is that nothing ties this to my offline identity. For this, I could also include
a dated picture, which combines some personal object with the text of the public key. For example,
I could take a picture of this week's "The Economist", alongside one of my notebooks, wherein
I've written down the public key I want to associate myself with. People that know me may
recognize my hand-writing, or that I've used a particular notebook before. You could further
reinforce this by including something like a signed book someone gave me, which would
convince them in particular, or a government ID, which would convince even more people.

Another shortcoming is that I could very-well have been coerced into doing all of this,
or perhaps both my website and my Twitter were hacked at the same time. For this to happen,
both my Github account and my Twitter would need to be compromised at the same time.
This would also involve compromising my phone, which acts as a second factor for both
of these.
