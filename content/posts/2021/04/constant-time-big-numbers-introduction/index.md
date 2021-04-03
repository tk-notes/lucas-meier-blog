---
title: "Constant-Time Big Numbers: An Introduction"
date: 2021-04-03T11:32:53+02:00
draft: true
tags:
  - "Cryptography"
  - "Math"
  - "Security"
---

{{<note>}}
This work is being done as my BSc project at EPFL's [DEDIS lab](https://www.epfl.ch/labs/dedis/),
under the supervision of [Professor Bryan Ford](https://people.epfl.ch/bryan.ford), and would
not be possible without their generous help.
{{</note>}}

# What are Big Numbers?

## Big Numbers are familiar

Explain how big numbers arise in day-to-day-life, how they're the form
of number we're most familiar with.

## Limited hardware size

Computers can't process arbitrary sized numbers. They instead
have fixed integers. Some cryptosystems need large numbers.

## Schoolbook arithmetic

Working with big numbers uses pen-and-paper algorithms.

## Digits, bits, and limbs

# Timing attacks

What a timing attack is.

## A toy example

Timing attack in password comparison.

## More subtle sources

Timing leaks through caches, branch prediction.

# Incomplete mitigations

There are a few obvious mitigations, but they aren't perfect.

## Sleeping

People think of trying to sleep a random amount, but a signal is still present.

## Blinding operations

Without going into the mathematics, a certain amount of
randomness can be mixed into some number before a sensitive operation,
then mixed back out after the number.

# Timing leaks in Go

Go's big number library, and how it might not be suitable for crypto.

## Leaky algorithms

Some algorithms are fundamentally leaky.

## Unpadded numbers

Algorithms inevitably leak the length of a number, and Go doesn't allow
hiding this length.

# What does constant-time mean?

What exactly does constant-time mean, when working with arbitrarily large numbers.

## True vs announced length

The difference between the limbs needed to represent a number, and the limbs
we display a number as having.

## Better algorithms

Making sure we don't leak the values through our algorithms

## Leaking modulus sizes

Leaking the modulus's size is desirable for performance, and is
also a reasonable security assumption.

# Some basic techniques

A few of the tricks used to make this happen.

## Some rules of thumb

A basic model of what constraints the code has to operate under.

## Handling choices

How do you replace ifs with bitwise manipulation.

## Looping on announced lengths

Making sure to loop only on the announced lengths

# Why Go?

Why was Go the language of choice for this project.

## In a vacuum

What language would you choose for this project in a vacuum?

## Being useful

What integrations are necessary for this project, why is it desirable
to use Go?

## Some downsides to Go

What are some inconvenient aspects of using Go

# Conclusion

# References
