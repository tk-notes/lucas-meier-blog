---
title: "Porting Ludus to the Web"
date: 2021-10-02T16:07:54+02:00
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

There used to be a time where you would simply write out your web page
directly as an HTML file, and upload that directly to your web server.
These days are long gone, and now we have a plethora of build tools that
process raw code into optimized HTML bundles for the web. In our case,
such a build tool is desirable, since it can automatically compile
our Rust code into WASM, allowing us to easily mix in any extra emulator
wrapping logic we have in Rust with the other JavaScript and HTML
code we need for our web-page.

The first component of bundling, in our case, is a tool to compile
the Rust into WASM. I ended up using
[wasm-pack](https://github.com/rustwasm/wasm-pack). This provides
a convient wrapper around Rust's functionality for compiling into WASM.

Then, I needed a build tool for the web things, like
JavaScript and HTMl. I went with [parcel](https://parceljs.org/)
here, because I had used it in the past, and I liked it's no-frills
model of having default configurations for everything.
Unfortunately, parcel didn't support Rust and WASM out of the box,
at least not with wasm-pack, so I had to use [a plugin](https://github.com/wasm-tool/parcel-plugin-wasm.rs)
adding that functionality.

I also ended up fiddling quite a bit to get my CI pipeline working
in [Vercel](https://vercel.com/), that way I could deploy the project
whenever I pushed code to my Github repository, but that's a story
for another day.

# Interfacing with the browser

Even though we've managed to compile our emulator to run in
the browser, we still need to actually push the pictures and sound
that it produces somewhere on the web page, so that we can you know,
play games. We'll also need to gather input, but that's pretty simple,
by listening to keyboard input.

## Video

The primary output of our emulator are the frames making up the video
of our game. The adequate venue for displaying these frames are a
[canvas element](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API). 

Our emulator expects a Rust strict implementing our `VideoDevice` trait.
Because of this, it's easier to put the code for pushing pour pixels
to the canvas on the Rust side of things. Thankfully,
we can do this with [wasm-bindgen](https://github.com/rustwasm/wasm-bindgen).
This allows us to interact with browser APIs from inside of Rust,
compiling to WASM.

This allows us to push image data to the canvas from our Rust code
wrapping the emulator:

```rust
fn render_to(&mut self, ctx: &CanvasRenderingContext2d) -> Result<(), JsValue> {
    let data = ImageData::new_with_u8_clamped_array_and_sh(
        Clamped(&mut self.buf),
        NES_WIDTH as u32,
        NES_HEIGHT as u32,
    )?;
    ctx.put_image_data(&data, 0.0, 0.0)
}
```

## Sound

While video was pretty straightforward, sound is a bit trickier, because
we have to deal with latency. Our emulator is generating about
around 40'000 audio samples per second. If we can't consume them fast
enough, we'll end up building more and more latency,
and our audio ends up completely out of sync. If we consume them too fast,
then we end up with terrible audio clipping.

The tolerance for mistakes is much lower with audio as compared to video,
and the APIs for web-audio are also not that stellar either.

At first, I was trying to handle the audio on the JavaScript side,
by passing around a buffer of samples across the WASM barrier, but
I was having quite a few issues with audio latency.

I ended up resolving these issues by handling the audio channels
inside of Rust itself, and also implementing logic to deal with audio
jitter, by potentially running the emulator a bit more to generate
extra samples. I got this idea from reading
the source code of another emulator: [pinky](https://github.com/koute/pinky/).

# Further Work

I've gotten the emulator to be [playable on the web](https://ludus-web.cronokirby.com),
with the same features as with my
original [native application](https://github.com/cronokirby/ludus-emu),
but there are still features that I'd like to add to the application.

First, there are some basic quality-of-life features that would be needed,
like rebinding the controls to different keys, or allowing
Gamepads to be used instead of the keyboard.

You could also implement video effects, like a CRT filter,
or a smoothing feature.

There are also other emulator features which would be nice to have,
but would require support from the underlying emulator crate.
These include save-states, fast-forward, debugging tools,
and other similar features. 

But, for now, I'm happy to set an endpoint here, and move on
to other projects, for the time-being.
