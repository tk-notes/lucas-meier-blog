---
title: "Fractals on The Web"
date: 2021-02-21T10:37:55+01:00
draft: true
katex: true
tags:
  - Graphics
  - Fractals
  - Math
---

Last week, I made a little [web application](https://fractals.cronokirby.com/)
for visualizing some fractals, and I thought I'd write up a few thoughts
about how it works.

<!--more-->

I won't be going into [the code](https://github.com/cronokirby/fractals) very
much in depth, just highlighting a few interesting things I encountered.

# WebGL

I could've done all of the rendering logic in pure Javascript, using
the canvas API. This would be quite naive, since each pixel composing
a fractal image can be rendered in parallel. The GPU is the natural
piece of hardware for tackling this problem, and WebGL is the easiest
way of accessing that power.

Setting this all up requires a lot of boilerplate. We're not using
all of the capabilities of a typical WebGL program, since we don't have
any geometry to render at all. Instead we're just rendering two triangles
to fill up our canvas:

{{<img "1.png">}}

Our fragment shader, which has the job of coloring each pixel of these triangles,
will contain all of the logic to render our fractals instead.

There's also a bit of math, to assign to each pixel a point in the complex plane,
according to how we've dragged the current fractal, zoomed in, and the size
of the canvas.

This is all quite boring, not very fractally, and better explained by other sites,
such as [WebGL Fundamentals](https://webglfundamentals.org/).

# Fractal Formulas

The fractals that we're working with arise from the same procedure.
You have some kind of complex sequence, defined by iterating a (complex) function:

$$
z_0, \ z_1 = f(z_0), \ z_2 = f(z_1), \ \ldots
$$

The initial value $z_0$, and slight variations of $f$ depend on the point
$p$, where each pixel in our canvas ends up.

This sequence bounces around the complex plane:

{{<img "2.png">}}

If this sequence converges, then our point $p$ is part of the fractal set.
Otherwise, the sequence diverges, and the point $p$ does not belong to the set.
Usually, we have a heuristic, where if the sequence grows large enough, we assume
that it will never converge, and report the point as being outside the set.

We can also look at how many iterations it takes for the sequence to diverge,
to provide more coloring information, but we'll see that later.

## Mandelbrot

For the Mandelbrot set, we have:

$$
\begin{aligned}
z_0 &:= 0 \cr
f(z) &:= z^2 + p
\end{aligned}
$$

Coloring each point in the resulting fractal in black, we
get this image:

{{<img "3.png">}}

## Julia

Julia sets are kind of dual to the mandelbrot set. We have
a parameter $c$ characterizing the set, defining the following parameters:

$$
\begin{aligned}
z_0 &:= p \cr
f(z) &:= z^2 + c
\end{aligned}
$$

This is similar to the previous formula, except now the adjustment
inside of $f$ is static, but the initialization varies across the canvas.

Here are a few examples of Julia fractals, with different values for $c$:

{{<img "4.png">}}
{{<img "5.png">}}
{{<img "6.png">}}

## Variations

Both of these formulas depend on iterations of the form $f(z) = z^2 + c$.
We can imagine swapping $z^2$ for something else:

$$
\begin{aligned}
&z^2 + c \cr
&z^3 + c \cr
&z^4 + c \cr
&z \sin(z) + c
\end{aligned}
$$

Here's what the Mandelbrot set looks like under these variations:

{{<img "3.png">}}
{{<img "7.png">}}
{{<img "8.png">}}
{{<img "9.png">}}

And a Julia set:

{{<img "10.png">}}
{{<img "11.png">}}
{{<img "12.png">}}
{{<img "13.png">}}

# Coloring

## Simple Coloring

## Smooth Coloring

# Orbit Traps

## Lines

## Circles

## Squares

# Conclusion

# Addendum: Pretty Pictures
