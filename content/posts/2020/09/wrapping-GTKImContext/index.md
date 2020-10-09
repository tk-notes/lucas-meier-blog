---
title: "Wrapping GtkImContext"
date: 2020-09-20
draft: false
description: "Misadventures in writing a text editor with Gtk and C++"
path: "/posts/2020/09/wrapping-GTKImContext"
type: post
image: "/posts/2020/09/wrapping-GTKImContext/cover.png"
tags:
  - C++
  - Gtk
---

So, recently I've started working on [a text editor](https://github.com/cronokiry/jim),
using [Gtk](https://www.gtk.org/), via [gtkmm](https://www.gtkmm.org/en/) in C++.
I've decided to try my hand at writing the text area widget from scratch, just using the
text layout facilities provided by Gtk's [Pango](https://pango.gnome.org/). My rationale
for this is twofold:

1. It seems that Gtk's [TextView](https://developer.gnome.org/gtkmm-tutorial/stable/chapter-textview.html.en)
   widget is missing a few nice things, like source lines, and using the more developed SourceView
   is not doing a lot of the work myself.
2. I like the idea of working with lower level parts of the text editor, since my main goal for the project is
   to learn new things.

It's been pretty smooth sailing so far, although right now the only thing the editor can do is display a string,
and let the user tack on more characters at the end:

{{<img "1.png">}}

For the most part, the gtkmm bindings turn the awkward GObject system into a nice C++ class hierarchy.
Regardless of your opinions on class hierarchies, they do work nicely for GUIs. Having
each type of widget be a class, and using inheritance to modify the behavior of widgets
seems like an ideal case study for OOP.

On the other hand, the GObject system, a way of tacking on an object-oriented type system and runtime
onto C types, seems a bit heavy-handed and awkward, and the C++ wrapper helps alleviate this
awkwardness quite a bit.

Unfortunately, not every part of Gtk has a nice wrapper, especially the more obscure or low-level parts.
This week, I had to manually write a wrapper for one of these parts, namely:
[GtkImContext](https://developer.gnome.org/gtk3/stable/GtkIMContext.html).

# Text Input

So, the custom text area wants to process the characters the user types in.
To do that, it needs to listen to keypress events, and then react to them.

So, if a user hits the key `a`, then we need to add the character `'a'` at the current position
in the editor. If the user holds shift and hits the same character, we see `SHIFT + a`, and we can
add `'A'` to the buffer.

For English text, at least when it comes to programming, you can actually input all the characters
you want using single key combinations. Because of this, it might seem that we can simply
map each key press to either adding a single character to the buffer, doing something like deleting
a character in the buffer, or something else like a shortcut for an action.

The problem is that "one keypress -> one character" is actually not true. As a simple example,
take the character `'ä'`. I use a US-alt-international keyboard layout, so to input this accented character,
I press `SHIFT+'` (for `"`) and then `a`. To type out just the double quote, I need to type out
`SHIFT+'`, and then a space.

Because of this, it takes potentially two keypresses in order to generate a single character of input. 

It can get even more complicated then this, with some languages, like Japanese, requiring arbitrarily many
presses for a single character, and allowing the user to resolve ambiguities manually,
using additional keypresses.

We need some way of feeding in these keypresses somewhere, and then getting out completed characters
when we can form the keypresses into combined characters. `GtkImContext` is used to do precisely this.

# A C++ Wrapper

Unfortunately, `GtkImContext` doesn't have a C++ wrapper in `gtkmm`. Well, I couldn't manage to find one.
This means that I had to write my own wrapper. I'm not very well familiar with `gtkmm`, and how to wrap
gtk classes in the most idiomatic way, so I just went with the simplest wrapper that would work for my purposes.

It looks something like this:

```cpp
class ImContext {
    GtkIMContext *_ctx;

    sigc::signal<void, ustring> _signal_commit;

    friend void commit_cb(GtkIMContext *, const gchar *str, gpointer data);

  public:
    ImContext();

    ~ImContext();

    bool on_key_press(GdkEventKey *key_event);

    sigc::signal<void, ustring> signal_commit();
};
```

We have a constructor, a destructor, pretty standard. Then our main method is `on_key_press` which processes
keypresses, and then returns `true` if it's consumed the press, otherwise `false` if it couldn't use it,
and we might want to do something else with it later. Then we have a signal which produces a `ustring`, which
is just Gtk's way of doing unicode strings.

The idea is that we can subscribe to the commit signal to do something when this context manages to combine
multiple keypresses into a single character, which is then returned as a unicode string through the signal.
This is just a getter around the `_signal_commit` member.

Then we have the `commit_cb` function, which is just a hack to be able to have a Gtk C style callback reach into
the state of the class.

In terms of the implementation, the constructor and destructor are pretty simple:

```cpp
ImContext::ImContext() {
    _ctx = gtk_im_context_simple_new();
    g_signal_connect(_ctx, "commit", G_CALLBACK(commit_cb), this);
}

ImContext::~ImContext() {
    g_object_unref(_ctx);
}
```

We use the methods from the C library to create and free our wrapped object. We also connect the commit
signal on the underlying object to the callback I mentioned earlier, which has access to the
internal state of our class, and can forward the results of the C signal through the signal
we defined as a member of our class:

```cpp
void commit_cb(GtkIMContext *, const gchar *str, gpointer data) {
    auto ctx = reinterpret_cast<ImContext *>(data);
    ctx->_signal_commit.emit(str);
}
```

Note how earlier we passed `this` as the final argument to `g_signal_connect`, and this is what
shows up as the `gpointer data` argument. We can then cast this to a pointer to our class,
and work with it normally. This is kind of a hack, but was the simplest way I figured out
to be able to have a C style callback have access to class internals.


Processing key presses is just a matter of delegating to the C library:

```cpp
bool ImContext::on_key_press(GdkEventKey *key_event) {
    return gtk_im_context_filter_keypress(_ctx, key_event);
}
```

# Conclusion

Overall this was just a simple update, but I think it was worth sharing this information, because figuring
out how to tie all the loose knots in terms of writing a wrapper for Gtk objects was quite tricky,
especially figuring out how to get a class method (sort of) as a callback for a signal.

Hopefully this is useful to somebody, someday. I wish there was a post like this a few days ago when
I had 234 tabs open trying to figure this out :)

