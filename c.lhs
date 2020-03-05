= C Below =

Let's descend from the realm of pure functions and step into the real world.

== Effectively ==

We add the IO monad and support FFI. The VM passes around a combinator `?` that
represents the real world. Reducing this combinator is a bug; it should always
be an argument to other combinators.

A value `x` of type `IO a` should behave as follows:

------------------------------------------------------------------------
xwc --> cyw
------------------------------------------------------------------------

where `y` is some value of type `a`. In normal operation, `w` is the real-world
token `?`, and the continuation `c` should expect two arguments but never
reduce the second one.

We add three combinators `n` `r` `w` for `newIORef` `readIOref` `writeIORef`.
An IORef holding a value `x` of type `a` is represented as `?x` where `?` is
again a combinator that should never be reduced. We store it as `?x` instead of
a plain `x` so it takes up one app-cell in our VM. This adds a layer of
indirection: the address of this app-cell may be freely copied all over the
and yet `writeIORef` need only change a single entry to update all these copies
at once.

We add a crude syntax for FFI, with crude code for generating the requisite C
wrappers. An `F` combinator invokes these foreign functions.

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
Hughes 1983, 'The design and implementation of programming languages']
  * https://homepages.inf.ed.ac.uk/wadler/papers/leak/leak.ps.gz[Philip Wadler 1987, 'Fixing some space leaks with a garbage collector']

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
