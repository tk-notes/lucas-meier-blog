---
title: "My Blog: Version 4"
date: 2020-10-02
katex: true
tags:
  - Frontend
  - Meta
---

Having recently inaugurated the 4th version of this blog, I want to go over the
technologies that I used to make this, as well as some of the tricky things I had
to do to make things work.
<!--more-->

# The New Style

From time to time, I like changing up the styling on my blog. Because of this,
this is the 4th major iteration I've gone through on my blog. The last iteration
was the longest lived, I think, but went through a few minor changes as well.
I don't really know *why* I end up doing this, maybe it's some kind of restlessness.

I've been somewhat dissatisfied with the previous version of my blog stylistically.
The main problem was that it felt somewhat hard to read the actual posts. At the end
of the day, if it's hard to read the posts, the blog's style is failing at
its primary purpose.

That's why I've went with a much more minimalistic style for this new iteration. The
idea is that the blog is supposed to look much more like a book than a flashy website.
Hopefully this styling makes the website easier for people to read.

## Typography

The main fonts used in this site are
[Noto Serif](https://fonts.google.com/specimen/Noto+Serif) for most text
and [Fira Code](https://fonts.google.com/specimen/Fira+Code) for code. This is
a major departure from my last blog, which used a *sans-serif* font for all of
the text instead. Ultimately, I think I find the serif font a bit easier on the eyes
for large posts, and goes well with the more minimalistic and book-like style as well.

I think the font changes are a subtle, but ultimately very important change towards
the *feel* of the entire site. Having these fonts makes for a much more cohesive approach
than I had in the last version of my blog.

# Technology

Let's get to some of the tools used to build the site. I want to go over the tools as I explain
some of the either stylistic features, so that I can both explain why I went with
that stylistic choice, and how I implemented that feature inside the site generation
itself.

This site is completely static, so all of the blog posts, projects, etc. are all generated
at compile-time, and then simply served to the user later on. As a static-site generator,
I went with [Hugo](https://gohugo.io). I was already familiar with Hugo from the 2nd
iteration of my blog, and having that previous code as a reference was very useful
in getting this new version up to speed quickly.

I'm using my own theme for Hugo, if it wasn't obvious already, so that means adding a bunch
of templates for different things in the `layouts` folder, and then adding the CSS to style
it all.

# CSS

For CSS, I've been using [Tailwind](https://tailwindcss.com/). I really enjoy working with
this framework, and have been using it for all of my projects for over a year now. What sets
tailwind apart from other frameworks is that it focuses on providing very small and composable
utility classes, each of which does something like changing the text color, size, the height
etc. The framework's only stylistic choices imposed on you are manageable scales for the sizes
of different things. But it's not like, say, Bootstrap, where you have pre-built components to
choose from. With Tailwind, it's possible to have *your style*, but also get the benefits
of a framework.

## Setting up Tailwind

Setting up Tailwind isn't that hard, but is somewhat tedious.

You first need to create an NPM package description:

```sh
npm init
```

The reason we need this is that we're going to install some build time dependencies, namely
Tailwind itself:

```sh
npm install tailwindcss postcss-cli autoprefixer@9.8.6
```

Tailwind is the framework itself, PostCSS is used to process CSS files using Tailwind,
and autoprefixer is necessary for that to work as well.

{{<note>}}
There's a bug with autoprefixer that I've only managed to fix by using this *specific* version.
{{</note>}}

Now we need to create the configuration files for both Tailwind, and for PostCSS.

For Tailwind, we want this configuration in `tailwind.config.js`:

```javascript
module.exports = {
  future: {
    removeDeprecatedGapUtilities: true,
    purgeLayersByDefault: true,
  },
  purge: {
    content: ["./layouts/**/*.html"],
  },
  theme: {
    fontFamily: {
      mono: ["Fira Code", "monospace"],
      serif: ["Noto Serif", "Georgia", "Cambria", "serif"],
    },
    extend: {},
  },
  variants: {},
  plugins: [],
};
```

This sets the fonts we want to use, as I've mentioned before, and also has us
use purging, to remove unused CSS classes. This is very important for Tailwind, because it
provides a large number of small classes, most of which you might not end up using. By purging,
we can keep just the classes we actually use. To know which classes *are* used,
we look at the files indicated by `content`. To make sure that purging works, when we build
we need to have `NODE_ENV=production`, otherwise purging won't happen.

And then we need to configure PostCSS with `postcss.config.js`:

```javascript
module.exports = {
  plugins: [require("tailwindcss"), require("autoprefixer")],
};
```

This just adds the dependencies we mentioned earlier.

# Code Blocks

As you might've noticed, the code blocks have the language in the top-left corner. This annotation
is actually done *purely* in CSS. The way this works is that a code block in markdown ends
up becoming something like this in HTML:

```html
<code class="language-javascript" data-lang="javascript">
```

What needs to be done is to use the `data-lang` attribute to add the little flourish on top
of the code block. This is done by using a *pseudo-class*:

```css
.highlight code::before {
  content: attr(data-lang)'\a';
}
```

The `\a` is there to add a newline between the language annotation and the code block itself.
This snippet looks pretty easy, but was kind of tricky to figure out you were able to do.

## Syntax Highlighting

Another aspect of the code blocks you might have noticed is the black and white syntax highlighting.
I'm not sure if I'm *fully* committed to going with a monochrome style, but I kind of like
it so far.

I also haven't fully customized the theme to a point where I'm satisfied: I've just quickly
modified an existing theme to make things monochrome. A lot of tweaking could still be done
to make things nicer.

# Cool Notes

You might noticed the "Cool Note" feature I added earlier. This is an idea inspired by
[another blog](https://fasterthanli.me/), where they have *Cool Bear* giving *hot tips*
instead of *CK* giving *cool notes*. I really enjoyed reading their blog posts, and the
notes / tips allowed them to be able to make interjections or add optional information
that I would otherwise put in parentheses.

To add this to my blog posts, I need to use a *shortcode* in Hugo, which is kind of like a special
component. I use this (with double `{{` instead of single `{`):

```markdown
{<note>}
  To generate this
{</note>}
```

{{<note>}}
  To generate this
{{</note>}}

It works nicely, although Hugo unfortunately process shortcodes a bit too eagerly, so
if I actually put the exact code I use, I end up with a note nested inside of a code block.

# Lazy Images

Nowadays, browsers come with native support for [lazy loading images](https://web.dev/browser-level-image-lazy-loading/).
All you need to do is slap a tag `<img loading="lazy">` onto your images, and then they'll
only load when the user actually sees them on the page. This is great for blog posts
with many images, where the user can load the page and start reading before the images later on are
necessary. Unfortunately, native support isn't there yet in all browsers, so I used a polyfill: `loading-attribute-polyfill`,
which I believe makes use of this native feature when possible.

# Math

And now to rendering Math equations, or $\LaTeX$ equations.

I use [$\KaTeX$](https://katex.org/) for rendering math equations in the browser, and it works quite pleasently,
aside from a few quirks. To use it, I just include the scripts in the pages that need it:

```html
<link
  rel="stylesheet"
  href="https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.css"
  integrity="sha384-AfEj0r4/OFrOo5t7NnNe46zW/tFgW6x/bCJG8FqQCEo3+Aro6EYUG4+cU+KJWu/X"
  crossorigin="anonymous"
/>
<script
  defer
  src="https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.js"
  integrity="sha384-g7c+Jr9ZivxKLnZTDUhnkOnsh30B4H0rpLUpJ4jAIKs4fnJI+sEnkvrMWph2EDg4"
  crossorigin="anonymous"
></script>
<script
  defer
  src="https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/contrib/auto-render.min.js"
  integrity="sha384-mll67QQFJfxn0IYznZYonOWZ644AWYC+Pt2cHqMaRhXVrursRwvLnLaebdGIlYNa"
  crossorigin="anonymous"
  onload="renderMathInElement(document.body,{delimiters: [
    {left: '$$', right: '$$', display: true},
    {left: '$', right: '$', display: false}
  ]});"
></script>

```

I explicitly set which delimiters are used in equations.

One tricky thing was that some of my equations were broken in Hugo, but had previously worked with gatsby.

For example:

```txt
\begin{aligned}
x &= 2 + 3 \\
y &= 4
\end{aligned}
```

Rendered as:

$$
\begin{aligned}
x &= 2 + 3 \\
y &= 4
\end{aligned}
$$

The fix was to turn it into:

```txt
\begin{aligned}
x &= 2 + 3 \cr
y &= 4
\end{aligned}
```

$$
\begin{aligned}
x &= 2 + 3 \cr
y &= 4
\end{aligned}
$$

I initially tried `\newline` instead of `\cr`, but for some reason this broke matrices:

$$
\begin{pmatrix}
1 & 0 \cr
0 & 1
\end{pmatrix}
$$

# Conclusion

Overall, I'm quite happy with the redesign of my blog, and I'm quite excited to write new posts
for it!

The entire code for the blog is available [here](https://github.com/cronokirby/blog4), and I'd 
recommend checking it out if you're curious about all the details of how things work.
