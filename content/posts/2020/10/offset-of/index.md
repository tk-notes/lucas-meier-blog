---
title: "Always use offsetof"
date: 2020-10-19
tags:
  - C
---
If you want to find out how many bytes a certain portion of a struct uses,
you might be tempted to do some arithmetic with `sizeof`, but this will
yield unpredictable results!
<!--more-->

I was recently bitten by this. For context I had something like:

```c
struct metadata {
  ...
  int* next;
}
```

I needed to know how much space the start of the struct, up to the `next` member,
occupied, in order to do some pointer shenanigans. I had to 
bump a `void*` referencing what I knew to be the `next` field of the struct up
to the top of that struct. (Yeah, this is kind of evil, but I had to do it,
custom allocators require some amount of evil).

So I thought, ok, I take the size of the whole struct, `sizeof(struct metadata)`,
and then I remove the size of the last member `sizeof(int*)` now this bit
me right in the ass in the form of a segfaulting program, because this
calculation was incorrect. It doesn't take padding into account!

Because the underlying architecture really doesn't like accessing memory
that isn't aligned to specific boundaries, a C compiler will liberally add
some extra space in a struct to make sure that when it pushes it onto the stack,
or whatever, that the next thing is aligned well. Because of this, it's possible
that the location of the member you want to access is not `sizeof(t_member)`
bytes behind the end of the struct.

As an example, consider:

```c
#include <stdio.h>

typedef struct foo {
  int m1;
  char m2;
  char m3;
} foo;

int main() {
  printf("sizeof(foo): %d\n", sizeof(foo));
  printf("sizeof(int): %d\n", sizeof(char));
  struct foo x;
  printf("&x: %x\n", &x);
  printf("&x.m3: %x\n", &x.m3);
}
```

This prints:

```txt
sizeof(foo): 8
sizeof(int): 1
&x: ef57890
&x.m3: ef57895
```

Notice how the `sizeof(foo) - sizeof(int)` calculation yields an offset of
`7`, whereas if we look at the differences between the pointers / addresses,
we see an offset starting at `5`!

The solution is to use `offsetof(struct foo, m3)` here, which does the correct thing.
(`#include <stddef.h>` to use it). Not only is the right thing done, but
it's also clear *what* you're trying to do in the first place, which is
another nice benefit.

