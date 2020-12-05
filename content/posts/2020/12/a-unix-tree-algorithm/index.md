---
title: "A simple algorithm for UNIX's Tree"
date: 2020-12-05
draft: false
tags:
  - Algorithms
  - Command Line
  - Rust
---

This morning I decided to implement
[my own version](https://github.com/cronokirby/arbor)
of UNIX's classic `tree` command
in Rust. I thought the algorithm was both non-trivial, and interesting enough
to warrant a quick blog post.
<!--more-->

For reference, the `tree` command recursively traverse the file system from a certain
point, printing a nice representation of the file hierarchy:

```txt
.
├───B
│   ├───1.txt
│   └───2.txt
├───A
│   ├───1.txt
│   └───2.txt
├───E
│   └───F
│       └───5.txt
└───C
    └───D
        ├───foo.txt
        └───bar.txt
```

Reading the tree structure is pretty straightforward, and is mostly an exercise in using
your standard library's IO capabilities. The interesting part is rendering this tree.

In Rust, you might represent the tree like:

```rust
enum Tree {
  File(String),
  Dir(String, Vec<Tree>)
}

impl Tree {
  fn name(&self) -> &str {
    match self {
      Tree::File(s) => &s,
      Tree::Dir(s, _) => &s
    }
  }
}
```

All of the leaf nodes use the `Tree::File` variant, and directories use `Tree::Dir`, and also
have a vector of children, also file trees.

Printing out the tree indenting further at each level is quite simple. You just need to
keep track of the current level of indentation, and then when you encounter a directory node,
your recurse on each child, but also increase the indentation by a set amount before doing so.

You can end up printing something like:

```txt
foo
  1.txt
  2.txt
bar
  baz
    3.txt
```

pretty easily, but the cool thing about the `tree` command is that it prints nice bars.

Instead of keeping track of indentation levels, we could instead keep track of the number of previous
"columns" behind us. We can use this to print a certain number of vertical bars with blank space
following them:

```txt
│
```

and then the final connector:

```
└───2.txt
```

The problem is that we want to see things like:

```txt
└───C
    └───D
        ├───foo.txt
        └───bar.txt
```

Where we stop printing the vertical bars after the last directory, for aesthetic
reasons, and this version doesn't handle that. We also need to change which
connector character we use if we're printing the last item in a directory.

To handle this, you instead need to modify the algorithm to have a *stack*
of different paddings. You can push either a *bar* padding, or a *blank* padding,
all of these inform what you display.

Let's just go ahead and look at a complete implementation in Rust, and then explain
some of the interesting parts:

```rust
enum Padding {
  Blank,
  Bar
}

fn print_tree(tree: &Tree) {
    use Padding::*;

    fn rec(tree: &Tree, prev: &mut Vec<Padding>, last: bool) {
        if !prev.is_empty() {
            for i in 0..(prev.len() - 1) {
                match prev[i] {
                    Blank => print!("    "),
                    Bar => print!("│   "),
                }
            }
            if last {
                print!("└───");
            } else {
                print!("├───");
            }
        }


        println!("{}", tree.name());
        match tree {
            Tree::File(_) => {}
            Tree::Dir(_, children) => {
                let len = children.len();
                for (i, child) in children.iter().enumerate() {
                    let next_last = i == len - 1;
                    prev.push(if next_last { Blank } else { Bar });
                    rec(child, prev, next_last);
                    prev.pop();
                }
            }
        }
    }

    let mut prev = Vec::new();
    rec(tree, &mut prev, true);
}
```

There's two items of state we keep when recursing:

1. The stack of paddings
2. Whether or not the current tree is the last item in a directory

The idea is that if the stack is completely empty, that means that we're
handling the root of the tree, in which case we don't need to print anything.

Otherwise, the connector part "overrides" the top item on the stack, so we
only want to handle everything up to that last item. If we see blank padding,
we print spaces, otherwise we print a bar.

We also print the right connector, depending on whether or not we're the last item
in a directory.

We then proceed to print the name of the current node, and recurse on its children,
if necessary.

For each child we push the right state onto the stack, and then pop it off once we're done,
so that we have the right value for the next independent tree. If we're at the last child,
we push a blank padding, to avoid printing the extra vertical bars. We also
forward the fact that this child is the last when recursing.

# References

I read [this post](https://blog.shaynefletcher.org/2017/10/how-to-render-trees-like-unix-tree.html) this morning,
and it was actually quite useful, but I had to think of a few adjustments to make it work in
an imperative environment. Their solution involved an explicit buildup of padding strings,
and relied on a persistent implementation of strings for efficiency. I think implementing
it in a mutable way also allows for clear ways to implement the same idea
in more functional languages as well, whereas the other way around was tricky.
