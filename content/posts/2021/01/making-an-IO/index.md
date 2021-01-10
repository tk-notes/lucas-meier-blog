---
title: "Making an IO"
date: 2021-01-10T09:45:40+01:00
draft: false
katex: false
tags:
  - Haskell
  - "Compiler"
---

Recently I've been wrapping up my work on
[a compiler](https://github.com/cronokirby/haskell) for a subset of Haskell.
While my subset doesn't (yet) have any support for `IO`, I've been thinking
about how to implement it.
<!--more-->

There's actually some interesting design choices involved,
and I've understood a lot more about how `IO` *actually works*
by trying to see how I'd go about implementing it in my own compiler.

Now, this is not really an attempt on explaining monads, or `IO`.
This post is just a progression of ideas around implementing
something like `IO`. The [IO inside](http://wiki.haskell.org/IO_inside)
article might be a good introduction before diving into this post.

# Why IO is necessary

One reason `IO` exists in Haskell is to have side effects. Or, at least,
to be able to describe programs with side-effects.
For example, you can write:

```haskell
main :: IO ()
main = do
  line <- getLine
  putStrLn line
```

to make a little program that gets a string from the user, and then prints
it right back out. Both of these bits of code actually have *side-effects*
when executed: they interact with the outside world.

In other languages, like Python, you'd just write this function like any other:

```python
def main():
  line = input()
  print(line)
```

This is just a normal function, and calling other functions might execute side effects.
This would also work even in some other Functional languages, like OCaml,
where you can just do side effects, without really worrying about them.

This works out just fine, because most languages are somewhat imperative,
and functions are more like *procedures*. You read them from top to bottom,
each line being a statement that has some kind of effect on the state of the world.

With this order well defined, it's easy to reason about the side effects in the program.
The order in which side effects happen is the same as the order of of statements in our
program.

## Laziness

Haskell throws a wrench into this whole process, because it doesn't have a strict order
of evaluation. Let's imagine that Haskell functions could transparently have side effects
outside of `IO`. Our `getLine` and `putStrLn` functions would become:

```haskell
getLine :: () -> String

putStrLn :: String -> ()
```

Then a program like:

```haskell
main =
  let line = getLine ()
  in putStrLn line
```

should ostensibly work, since we're forced to evaluate the function that gives
us input before evaluating `putStrLn`, so both side effects should happen. The problem
is that this is a fragile setup. For example, take this slight modification:

```haskell
main =
  let line = getLine ()
      _ = putStrLn line
  in ()
```

This program now ignores the result of `putStrLn line`. Because of this,
this side effects doesn't have to run, since we don't need to evaluate
this expression!

We can also get side effects running out of order:

```haskell
main =
  let a = (putStrLn "A" >> readLn)
      b = (putStrLn "B" >> readLn)
  in b + a
```

Now, `+` is (most likely) going to force `b` before `a`,
which has us printing out:

```haskell
B
A
```

which is different than the order suggested by our program, if it were strict.

Herein lies the problem with side effects in Haskell: since we use lazy evaluation,
it's difficult to reason about the exact order and presence of side effects.

We need to introduce `IO` in order to provide an explicit way of sequencing
effects together, ensuring that all effects happen, and happen in the right order.

{{<note>}}
`IO` also has another purpose, which is tagged certain values as being
*computations with side-effects*. This is a useful mechanism in and of itself,
and would still be nice to have in a strict language.

But laziness forces us to have some kind of tagging mechanism, because we need
to enforce ordering between effectful computations.
{{</note>}}

# IO as an opaque type

From the perspective of the user of Haskell, or someone specifying
the language, `IO` can be seen as an opaque type. You have some
type `data IO a` defined somehow, along with some primitive actions like:

```haskell
getLine :: IO String

putStrLn :: String -> IO ()
```
You also need to make `IO` a *Monad*, allowing us to lift values
to make an action with no effects, and to sequence actions together:

```haskell
return :: a -> IO a

(>>=) :: IO a -> (a -> IO b) -> IO b
```

From the perspective of the compiler, these are all *builtin functions*. They
have no implementation. Instead, the compiler has to recognize that these
exist, and insert special code handling them. 

From the perspective of all stages before you start translating Haskell code down
to lower level code, you can work with all of these functions without inspecting their
implementation. For example, all the type-checker needs to know about these functions
are their types. The implementations don't matter for code that uses them.

By having an opaque type and implementation, we also prvent any kind of preemptive
optimization before we've implemented IO in a way that's not going to be susceptible
to being broken, which is another nice aspect of this approach.

# Hacking into STG

At some point, we do need to replace these stub functions with actual implementations.

A good point to do this is at the *STG* stage. You can think of STG as a kind
of low-level functional IR, which is used as an intermediate stage between Haskell
and something like C.

I won't go into complete depth into *STG*, as I plan on explaining it much further
in some other posts, but just think of it like a version of Haskell, where you explicitly
keep track of metadata like "which variables are free in this function",
and you can't have nested expressions when calling functions.

As an example, something like:

```haskell
f x (1 + y) (3 + z)
```

Would become:

```haskell
let $0 = {y} \ -> 1 + y
    $1 = {z} \ -> 2 + z
in f {x, $0, $1}
```

We've removed currying, with explicit multiple arguments, and we've also
created let bindings for each argument that wasn't already simple. Let bindings
are explicitly lambdas, and also have to mark every single free variable they use.
This is because they will become *closures*, and we need to know to save
these free variables when creating that closure.

Anyways, the important thing about STG is that it has a clear semantics in terms
of what kind of machine code it generates. For example, that above snippet would
become something like this:

```txt
Allocate a closure for $0
Allocate a closure for $1
Push x, $0, $1 onto the argument stack
Enter the code for f
```

We don't evaluate any of the bindings at all, beacuse we want laziness. Instead,
the mechanism to force evaluation of values is the humble *case expression*.
If we have something like:

```haskell
let x = {x} \ -> Cons 3 x
in case x of
  Nil -> 0
  Cons y _ -> y
```

Then this would roughly correspond to:

```
Allocate a closure for x
(yes, this closure references itself)
Evaluate x
When done, inspect the result:
 If Nil ...
 If Cons ...
```

This is actually done by pushing a continuation onto a stack, and
then the code for `x` will jump to that continuation after producing a value.
Of course, the arguments to the constructor `Cons` are themselves thunks,
so this expression wouldn't enter an infinite loop, which is what we expect.

The point is that if you want to force evaluation of something, you need to use `case`.
What we can do to implement `IO` is to just create a special case construct that
deals with `IO`, and have `IO` actions just be normal values. For the builtin
actions that have side effects, we'd end up generating code that *actually does them*,
when evaluated. So, for example:

```haskell
putStrLn = {} \s -> #builtin(putStrLn) {s}
```

could be the STG implementation of this function, and then when we compile
our STG down to C, we replace `#builtin(putStrLn) {s}` with `puts(s);`.
Or, something like that, at least.

The important part comes with `return` and `>>=`.

Now, for return, we can actually just use something like `id`, since
`IO a` is really just `a`:

```haskell
return = {} \a -> a
```

The fun part is the implementation of `>>=`:

```haskell
(>>=) = {} \m f ->
  case(io) m of
    a -> f a
```

The idea is that if we have an action of type `IO a`, and a function `a -> IO b`,
the action is really just a value of type `a`, that has side effects when we evaluate it.
What we want to do is force the evaluation of that action,
before then starting the evaluation of `f` using the argument we've just found.

In STG, at least the way I've done it, you have a special `case` construct for each
different kind of value you're working with. So, you have one case construct
for boxed integers, another for boxed strings, one for constructors, etc.
This makes compiling the STG down to C much simpler.

You'd have a separate case construct for IO, which also lets the optimizer know *not*
to touch this, and try to pull any funny business, like optimizing things to:

```haskell
(>>=) = {} \ m f -> f m
```
Now, this would "work" in terms of the types lining up, but semantically, this is a *very*
different function. We no longer force the evaluation
of `m` before entering the code for `f`. If `f` has some side effects,
before evaluating its argument, then we'll have the side effects for `f` *after*
those of `m`, which is exactly what we want to avoid by using `>>=`!

Having a special case construct for IO avoids having this kind of optimization ruin the semantics.

Let's illustrate the workings of this a bit more with a more complicated sequence.

If you had:

```haskell
main = do
  putStrLn "1"
  putStrLn "2"
  putStrLn "3"
```

then this would desugar to:

```haskell
main = putStrLn "1" >>= \_ -> putStrLn "2" >>= \_ -> putStrLn "3"
```
And the STG for this could be:

```haskell
f2 = {} \ _ -> putStrLn "3"

f1 = {} \ _ ->
  let $2 = {} \ -> putStrLn "2"
  in (>>=) {$2, f2}

main =
  let $1 = {} \ -> putStrLn "1"
  in (>>=) {$1, f1}
```

`f1` and `f2` can be lifted to the top level, since they have no bound arguments.

Operationally, what we do is create a thunk for `$1`, and then push
`$1` and `f1` as arguments to `>>=`. The code for `>>=` with these arguments
looks like:

```haskell
case(io) $1 of
  a -> f1 a
```

So, we first evaluate `$1`, printing out `1`, and then enter `f1`.

`f1` creates a thunk for `$2`, and then pushes this along with `f2`
on the argument stack, and then enters `>>=`. Then we have code
that looks like:

```haskell
case(io) $2 of
  a -> f2 a
```

So we have to evaluate `$2`, printing out `2`, and then entering `f3`.
`f3` has the side effect of printing out `3`.

So, even though we create thunks for different actions, becuase they were all chained
together with `>>=`, the order of side effects in our program is well defined!

# Lifting things up

Now, this is a very ad hoc approach to doing things, and relies on our knowledge of
how STG is desugared down to C, as well as our optimizer *knowing* about our IO
shenanigans and not touching them.

Haskell has a more general approach, that relies on higher-level
semantics, instead of lower level implementation details. The problem
`IO a ~ a` is that if you have a top level binding, like:

```haskell
readInt :: IO a
```

then if this is just:

```haskell
readInt :: a
```

you have a problem, because this is semantically a single value. What we want
is for each usage to fully evaluate the expression again. In a strict language,
you explicit mark that this is a function taking an argument:

```haskell
readInt :: () -> a
```

Semantically, now it means that each invocation of `readInt` has to evaluate
its body again, and run the side effects.

So, `IO a ~ () -> a`, at least.

Now, another problem is that there's nothing chaining IO actions together, even if I did:

```haskell
m >>= f = \s -> case m s of
  a -> f a
```

nothing stops me from replacing this with:

```haskell
m >>= f = \s -> f (m s)
```

Which is semantically different, at least if this makes it down to STG, because
now we're not forcing `m ()` before entering `f`, which is a problem for ordering side effects.
So, we need to make it so that you have to deconstruct the result in order to get an `a`.

Let's do `IO a ~ () -> ((), a)`. Now you have to do:

```haskell
m >>= f = \s -> case m s of
  (s', a) -> (f a) s'
```

Now, we need to deconstruct the tuple first, in order to get our `a` out. This
forces evaluation of `m`, at least up to creating the tuple, which is enough to
get the side effects.

Even if we used let bindings, like:

```haskell
m >>= f = \s ->
  let (s', a) = m s
  in (f a) s'
```

then this would still desugar to using a `case` expression around `m s`,
so we're fine.

Another problem is that `()` is too visible to the compiler, and it might realize that `s`
is always going to be `()`, and decide to insert it at certain places, and that's a no-no.

We need to replace this "token" we're passing around with an opaque value the compiler knows
not to touch. This is usually called something like:

```haskell
data RealWorld
```

Finally, we can have a definition of `IO` as an actual data type:

```haskell
newtype IO a = IO (RealWorld -> (RealWorld, a))
```

Of course, the compiler still needs to provide a few builtin functions that conform
to this interface, and provide us with the basic side effects, like file IO,
that we can compose up to larger actions.

# Avoiding thunks

Now, in GHC, you won't actually see this definition, for two reasons.

The first is that `IO` is actually a special case of the `ST` monad,
so the token type is `State# RealWorld`, but the idea is the same. You have
an opaque token that the compiler can't insert willy nilly.

The second optimization is that we use *unboxed tuples* instead:

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

Now, this is in the realm of GHC specific optimizations, but the idea is that previously
we evaluated our actions right up to the point where it produced a tuple pointing
to two thunks, but we didn't evaluate those thunks.

If you had something like:

```haskell
(+) <$> readInt <*> readInt
```

You'd end up with a thunk for `x + y`, having done none of the arithmetic.
This is kind of a waste of space, since you gain nothing out of lazily evaluating
the produced values, since you have to evaluate everything right up to what you return
anyways.

Using unboxed tuples avoids this laziness, and also provides a slightly more efficient
mechanism to return values, at least with how GHC compiles STG down to assembly.

# Conclusion

Anyways, this post mainly served to gather my thoughts around this topic, and
hopefully provide a bit more insight into how you might go about implementing
this kind of thing yourself. Perhaps I should make a more complete extension
chapter in my Haskell-in-Haskell series talking about this in much more detail.

For now though, I think this is a rough but somewhat complete introduction to
a couple implementation ideas.

