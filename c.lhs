= C Below =

Let's descend from the realm of pure functions and step into the real world.

== Effectively ==

We add the IO monad and support FFI. The VM passes around a combinator `?` that
represents the real world. Reducing this combinator is a bug; it should always
be an argument to other combinators.

A value `x` of type `IO a` should behave as follows:

------------------------------------------------------------------------
x w c --> c y w
------------------------------------------------------------------------

where `y` is some value of type `a`. In normal operation, `w` is the real-world
token `?`, and the continuation `c` should expect two arguments but never
reduce the second one.

Thus in the IO monad:

------------------------------------------------------------------------
pure y = \w c -> c y w
x >>= f = \w c -> x w f c
------------------------------------------------------------------------

Hence we can use the V and C combinators for `pure` and `(>>=)`.

We add a crude syntax for FFI, with crude code for generating the requisite C
wrappers. An `F` combinator invokes these foreign functions.

Threading the unused token `?` protects us from lazily updating the result of
a call to an impure function. Can we get rid of it? If we simply drop the
token, we wind up with the continuation monad:

------------------------------------------------------------------------
pure :: a -> (a -> r) -> r
pure y = \c -> c y
(>>=) :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r) -> r
x >>= f = \c -> x f c
------------------------------------------------------------------------

This time `pure` and `(>>=)` are the T and I combinators. With this scheme, we
must skip lazy updates for every IO function. Generated wrappers for FFI
imports in the IO monad should create an ephemeral node to push on top of the
stack, rather than reduce in place.

This ought to work, except that it breaks an invariant that our runtime depends
on, namely, the stack always holds part of the spine starting from the root. In
particular, the garbage collector only evacuates the bottom of the stack and
relies on the invariant to re-expand the spine. Ephemeral cells would require
our GC to carefully evacuate the whole stack.

It's unclear if these changes are worth the effort, especially since Haskell
code tends to avoid the IO monad as much as possible.

++++++++++
<script>
function hideshow(s) {
  var x = document.getElementById(s);
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
}
</script>
<p><a onclick='hideshow("effectively");'>&#9654; Toggle `effectively.hs`.</a></p>
<div id='effectively' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::effectively.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

Rather than a bunch of numbers, our compiler generates C code that should be
appended to the following C implementation of the VM:

++++++++++
<p><a onclick='hideshow("rts");'>&#9654; Toggle RTS.</a></p>
<div id='rts'>
++++++++++

[source,c]
------------------------------------------------------------------------
include::rts.c[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

We employ a stop-the-world copying garbage collector. It turns out we should
reduce projection functions (such as `fst` and `snd`) as we collect garbage.
See:

  * https://www.cs.ox.ac.uk/publications/publication3788-abstract.html[John
Hughes 1983, _The design and implementation of programming languages_]
  * https://homepages.inf.ed.ac.uk/wadler/papers/leak/leak.ps.gz[Philip Wadler 1987, _Fixing some space leaks with a garbage collector_]

We partially achieve this by reducing `K I T` nodes during garbage collection.

Eliminating applications of `I` during garbage collection makes up for not
doing so during evaluation.

Our simple design means the only garbage collection root we need is the top of
the stack.

== Lonely ==

We've made it to the real world. Our next compiler has a main function of type
`IO ()`, and calls `getchar()` and `putchar()` via FFI. Running `effectively`
on this compiler produces C source. Appending this to `rts.c` and compiling
yields a standalone compiler. This contrasts with our previous compilers, which
require a program that understands ION assembly or a bunch of integers
representing VM memory contents.

We also add support for `if` expressions and infix patterns in case expressions.

++++++++++
<p><a onclick='hideshow("lonely");'>&#9654; Toggle `lonely.hs`.</a></p>
<div id='lonely' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::lonely.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++
