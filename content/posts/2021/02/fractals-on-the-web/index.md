---
title: "Fractals on The Web"
date: 2021-02-21T16:07:08+01:00
draft: false
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
We usually limit the number of iterations we run this process for. After a certain
number of iterations, we assume convergence. This introduces false positives.
Using more iterations gives us a more precise image, but takes longer to run.

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

So far, the way we've colored our fractals is pretty simple. We iterate
our function a certain number of times. If we end early, because our
value grows too large, we assign one color (white, in our examples so far),
and if we reach the end of our iterations, and still haven't diverged,
we assign another color (black).

## Simple Coloring

Instead of just looking at the binary value "converges" or "doesn't converge",
we can look at how many iterations it takes for us to bail out,
say $i$, and then divide that by the total number of iterations:

$$
\frac{i}{N}
$$

This gives us a value between $0$ and $1$, which is a bit more interesting.

{{<img "14.jpg">}}

(Note: this comes from [Something Somewhere](https://smthngsmwhr.wordpress.com/2016/10/16/mandelbrot-set-visualization/),
since my site currently uses the smooth coloring tweak I explain later, and I wanted to demonstrate banding here)

## Smooth Coloring

You can notice a nice transition now, but there's still a kind of "banding" effect. The transitions
between different colors don't happen smoothly. There's a formula to fix this, which I admittedly
don't understand that well, so I'll refer you to
[Inigo Quilez](https://www.iquilezles.org/www/articles/mset_smooth/mset_smooth.htm) for more details.

Basically, instead of:

$$
\frac{i}{N}
$$

you do:

$$
\frac{i - \log \frac{\log \frac{\log |z|^2}{\log B}}{\log k}}{N}
$$

Where $k$ is the power you're iterating with. So $2$ for the standard mandelbrot
sequence, $3$ if you're doing $z^3 + c$ each iteration, etc. $z$ is the last value
you reached, and $B$ is the threshold after which you bail out.

This gives us a smoother coloring, as we can see here in a zoomed in
Mandelbrot plot.

{{<img "15.png">}}

## Palettes

Given a value between $0$ and $1$, we can color the fractal in a few interesting ways.
One is to simply linearly interpolate between two colors. One version I settled with
is due to Inigo Quilez, once again. Here the formula is:

$$
a + b \cdot cos(2 \pi (t \cdot c + d))
$$

Where $a, b, c, d$ are vectors, with operations done pointwise. The final result
is a vector, representing colors.

Varying $c$ lets us get more and more variation between colors:

{{<img "16.png">}}
{{<img "17.png">}}
{{<img "18.png">}}

We can also play around with the other values, and get different color gradients:

{{<img "19.png">}}
{{<img "20.png">}}
{{<img "21.png">}}
{{<img "22.png">}}

# Orbit Traps

Instead of using the iteration count, we can instead keep track
of the minimum distance to some geometric figure.

## Circles

For example, we can use the distance to a disk of radius 1, centered at some point in the plane:

{{<img "23.png">}}

Clamping the distance in the range $[0, 1]$, this gives us a nice image:

{{<img "24.png">}}

We can vary the center of the circle, giving us different images:

{{<img "25.png">}}
{{<img "26.png">}}
{{<img "27.png">}}

We can also take the distance to a circle of a certain radius, instead of the full disk:

{{<img "28.png">}}
{{<img "29.png">}}

## Lines

We can do the same with a vertical line:

{{<img "30.png">}}
{{<img "31.png">}}
{{<img "32.png">}}

## Squares

Or even a square:

{{<img "33.png">}}
{{<img "34.png">}}
{{<img "35.png">}}

# Conclusion

Hopefully this was interesting, and I'd welcome you to take a look
at [the code](https://github.com/cronokirby/fractals).
Inigo Quilez, like with many topics in graphics, has a great number
of articles on [fractals](https://www.iquilezles.org/www/index.htm).

# Addendum: Pretty Pictures

Now, here's just some nice pictures:

{{<img "36.png">}}
{{<img "37.png">}}
{{<img "38.png">}}
{{<img "39.png">}}
{{<img "40.png">}}
{{<img "41.png">}}
{{<img "42.png">}}
{{<img "43.png">}}
{{<img "44.png">}}
{{<img "45.png">}}
{{<img "46.png">}}
{{<img "47.png">}}
{{<img "48.png">}}
{{<img "49.png">}}
{{<img "50.png">}}
{{<img "51.png">}}
{{<img "52.png">}}
{{<img "53.png">}}
{{<img "54.png">}}
{{<img "55.png">}}
{{<img "56.png">}}
{{<img "57.png">}}
{{<img "58.png">}}
{{<img "59.png">}}
{{<img "60.png">}}
