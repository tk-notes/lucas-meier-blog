---
title: "Porting Ludus to the Web"
date: 2021-09-30T17:53:29+02:00
draft: true
katex: false
tags:
  - "Emulation"
  - "Rust"
  - "WASM"
---

Recently, I ported my NES emulator, [Ludus](https://github.com/cronokirby/ludus) to
run in the browser, using WASM. You can play around with it [here](https://ludus-web.cronokirby.com).
This post is a brief overview of the interesting aspects in creating this port.

# Ludus

[Ludus](https://github.com/cronokirby/ludus) was an NES emulator that I wrote 3 years ago, back in
2018. I was starting my BSc at EPFL then, so it's a bit fitting to revisit the project now that
I'm starting my MSc. I implemented it in Rust back then, so I was always curious as to how easy
it would be to port it the browser using WASM. It turns out that Rust has a pretty mature ecosystem
around WASM, so this was easier than I expected. Additionally, I also made a few good choices
when designing my emulator, which made it easier to port.

## Interface and Implementation

Instead of writing my emulator as a single application that includes both the emulation
logic, and a GUI for playing the game, I separated it out into two packages:
[one package](https://github.com/cronokirby/ludus) provides the emulation logic, and
[another package](https://github.com/cronokirby/ludus-emu) provides a GUI using the first package.

The first package only contains the logic for emulation, but the NES generates images and sound,
so we need a way to provide these to whatever application is using our emulator.
I ended up doing this by creating two traits, for these two types of output:

```rust
trait VideoDevice {
    fn blit_pixels(&mut self, pixels: &PixelBuffer)
}

trait AudioDevice {
    fn push_sample(&mut self, sample: f32)
}
```

You create an instance of the emulator by providing an implementation for each of these traits,
and then the emulation can feed in new data to those implementations, whenever
the console generates a new frame, or a new audio sample.

It was pretty straightforward to use these interfaces to provide
a GUI using SDL2, which is what my original emulator did. In fact,
I first had a concrete GUI, and then decided to cleave everything
using this nice interface.

This turned out to be a pretty good idea, since it made it possible
to port the emulator to the web, without having to compile all of SDL2,
which would have had much worse performance.

## WASM

In summary, [WASM](https://webassembly.org/), or WebAssembly, is a portable
binary format, which browsers know how to execute. We can compile
our emulator, written in Rust, to WASM, and then execute it in the browser.
This allows us to write a GUI wrapper for the emulator running in the browser,
by compiling all of our logic into a WASM blob.

# Bundling

# Interfacing with the browser

## Video

## Sound

## Loading Files

# Further Work
