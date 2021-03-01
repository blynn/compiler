= Minimum Viable Product =

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
++++++++++

What features would make our compiler friendlier? My biggest gripes are that
symbols must be defined before use, a finicky parser that lacks support for
indentation rules, and pathetic error handling. We work on the first two problems,
and make a tiny dent on the third, while taking care of a few other issues.

This leads to a compiler that I found surprisingly usable, at least when I could
mostly spot mistakes on my own.

== Mutually ==

C requires symbols to be declared before use. Our compilers are fussier still,
as they require symbols to be completely defined before use. This irks
programmers, especially when mutual recursion is desired, and also irks our
compilers, because we must process functions and instance methods in the order
they appear. This is particularly annoying when the two are interleaved.

Supporting arbitrary orderings of definitions requires changing multiple stages
of our compiler.

We break type inference into 3 steps:

  1. As we parse, we generate the type and abstract syntax tree of each data
  constructor and each typeclass method, adding them to pre-defined primitives.

  2. We infer the types of top-level definitions. For this stage, we construct
  a dependency graph (that is, we determine the symbols required by each
  symbol) then find its strongly connected components. Each member of a
  component mutually depends on each other member, and we infer their types
  together. Our `inferno` function continually piles on more type constraints
  for each member of a component, and only resolves them after all have been
  processed.

  3. We infer the type of instance method definitions, and check they are
  correct. A later compiler supports default class method definitions, which
  are also handled in this phase.

During code generation, we no longer necessarily know the address of a
dependent symbol. Instead, we must leave space for an address and fill it in
later. We take advantage of lazy tying-the-knot style so the code appears to
effortlessly solve this problem.

As our previous now has a predefined `Bool` type, we take this opportunity to
refactor to use if-then-else instead of matching on `True` and `False`.

++++++++++
<p><a onclick='hideshow("mutually");'>&#9654; Toggle `mutually.hs`</a></p>
<div id='mutually' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::mutually.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Uniquely ==

Now that code generation requires a map of addresses, it's a good time to
experiment with hash consing. We reduce heap usage my maximizing sharing.
However, it's not necessary a good idea, as this iteration of our compiler is
appreciably slower!

The code relies on `optiComb` ensuring each definition node has two children,
or is a basic combinator.

We clean up top-level definitions as mutual recursion is now possible.

We add support for definitions appearing in any order in a let block. This
is trickier than at the top-level, because of shared variable bindings floating
around. Again, we find the strongly connected components to detect mutual
dependencies, but instead of a table of addresses, we apply simple lambda lifting.
See https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/[Peyton Jones and Lester, 'Implementing Functional Languages: a tutorial'], Chapter 6.

In brief, we order the members of each component arbitrarily and insert
variables so they can all reach each other; we automate what we did by hand
when writing mutually recursive functions for older versions of our compiler.

++++++++++
<p><a onclick='hideshow("uniquely");'>&#9654; Toggle `uniquely.hs`</a></p>
<div id='uniquely' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::uniquely.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Virtually ==

Concatenating the runtime system with the compiler output is tiresome.  Our
next compiler also generates the source for the virtual machine.

We change `Int` from unsigned to signed.
We rename `(/)` and `(%)` to match Haskell's `div` and `mod`, though they
really should be `quot` and `rem`; we'll fix this later.

We add support for `newIORef`, `readIOref`, and `writeIORef`.
An IORef holding a value `x` of type `a` is represented as `REF x` where `REF`
behaves like `NUM`:

------------------------------------------------------------------------
REF x f --> f (REF x)
------------------------------------------------------------------------

Thus an IORef takes one app-cell in our VM, which adds a layer of indirection.
The address of this app-cell may be freely copied, and `writeIORef` can update
all these copies at once, by changing a single entry. We hardwire the following:

------------------------------------------------------------------------
newIORef value world cont = cont (REF value) world
readIORef ref world cont = ref READREF world cont
writeIORef ref value world cont = ref (WRITEREF value) world cont
READREF (REF x) world cont = cont x world
WRITEREF value (REF _) world cont = cont () world
------------------------------------------------------------------------

WRITEREF also has a side effect: it overwrites the given app-cell with `REF
value` before returning `cont`. It is the only combinator that can modify the
values in the heap, excluding changes caused by lazy updates and garbage
collection.

++++++++++
<p><a onclick='hideshow("virtually");'>&#9654; Toggle `virtually.hs`</a></p>
<div id='virtually' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::virtually.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Marginally ==

https://en.wikipedia.org/wiki/Off-side_rule[Landin's off-side rule]
is sorely missed. Although cosmetic, layout parsing rules give Haskell a clean
mathematical look.

We split off a lexer from our parser, and follow the rules in section 10.3 of
https://www.haskell.org/onlinereport/haskell2010/haskellch10.html[the Haskell
2010 spec].

We add support for multiple predicates in the context of an instance. We should
have done this before, as it's just a small parser tweak; the rest of the code
can already handle it.

This is a good moment to support `do` notation. We deviate from the spec.
Trailing `let` statements are legal; they just have no effect. It is also legal
for the last statement to be a binding, in which case we implicitly follow it
with `pure ()`.

++++++++++
<p><a onclick='hideshow("marginally");'>&#9654; Toggle `marginally.hs`</a></p>
<div id='marginally' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::marginally.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Methodically ==

We correct a glaring defect. Up until now, the methods of an `instance` must be
defined in the same order they are declared in their `class`, otherwise bad
code is silently produced.

We add support for default methods as it involves the same code.

We ruthlessly remove semicolons and braces from our source.

++++++++++
<p><a onclick='hideshow("methodically");'>&#9654; Toggle `methodically.hs`</a></p>
<div id='methodically' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::methodically.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++
