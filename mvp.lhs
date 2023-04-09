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
indentation rules, and pathetic error handling.

We work on the first two problems, and make a tiny dent on the third, while
taking care of a few other issues. This leads to a compiler that I found
surprisingly usable, at least when I could spot mistakes on my own.

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

During code generation, we no longer know the address of a dependent symbol.
Instead, we must leave space for an address and fill it in later. We take
advantage of lazy tying-the-knot style so the code appears to effortlessly
solve this problem.

We also support definitions appearing in any order in a let block. This is
trickier than at the top-level, because of shared variable bindings floating
around. Again, we find the strongly connected components to detect mutual
dependencies, but instead of a table of addresses, we apply simple lambda
lifting.
See https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/[Peyton Jones and Lester, 'Implementing Functional Languages: a tutorial'], Chapter 6.

In brief, we order the members of each component arbitrarily and insert
variables so they can all reach each other; we automate what we did by hand
when writing mutually recursive functions for older versions of our compiler.
For example:

------------------------------------------------------------------------
let
  a = foo a b c
  b = bar a b c
  c = baz a b c
in qux a b c
------------------------------------------------------------------------

is rewritten to the cycle-free:

------------------------------------------------------------------------
let
  a b c = foo (a b c) (b c) c
  b c   = bar (a b c) (b c) c
  c     = baz (a b c) (b c) c
in qux (a b c) (b c) c
------------------------------------------------------------------------

A triangle appears on the left-hand side, explaining our choice of function
name, and while the idea is straightforward, the implementation is tedious
because we recurse in all sorts of ways over the non-empty tails of lists of
variables, such as `[[a, b, c], [b, c], [c]]` and because we perform
substitutions in the syntax tree while it still possibly contains case
expressions and pattern matches.

As we now have a predefined `Bool` type, we use if-then-else instead of
matching on `True` and `False`.

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

We ignore the following subtle bug for now.
Consider definitions whose right-hand side is a lone variable. Our `optiComb`
function follows lone variables so that:

------------------------------------------------------------------------
f = g
g = h
h = f
x = (f, g, h)
y = x
z = y
w = (x, y, z)
------------------------------------------------------------------------

compiles to:

------------------------------------------------------------------------
f = g
h = g
g = Y I
x = (g, g, g)
y = x
z = x
w = (x, x, x)
------------------------------------------------------------------------

That is, afterwards, a variable with a lone variable definition only appears on
the right-hand side if its definition has been rewritten to `fix id`, so is no
longer a lone variable. Our `asm` function relies on this, because it skips
anything whose right-hand side is a lone variable.

This causes a corner case to fail: our compiler crashes on attempting to export
a symbol whose right-hand side remains a lone variable after `optiComb`.

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
newIORef = NEWREF
readIORef ref world cont = ref READREF world cont
writeIORef ref value world cont = ref (WRITEREF value) world cont
NEWREF value world cont = cont (REF value) world
READREF (REF x) world cont = cont x world
WRITEREF value (REF _) world cont = cont () world
------------------------------------------------------------------------

NEWREF has a subtle side effect: it ensures the `REF value` cell it creates is
new. Originally, we defined `newIORef value world cont = cont (REF value)
world` but this is unsafe because the `REF value` might be shared, causing
writes to stomp over each other.

WRITEREF also has a side effect: it overwrites the given app-cell with `REF
value` before returning `cont`. It is the only combinator that can modify the
values in the heap, excluding changes caused by lazy updates and garbage
collection.

We clean up top-level definitions as mutual recursion is now possible.

The `leftyPat` function supports patterns on the left-hand side of definitions,
for example:

------------------------------------------------------------------------
[a,b,c] = expr
------------------------------------------------------------------------

Our solution is simplistic. We find all pattern variables, such as  `a,b,c`. If
nonempty, we prepend `@` to the first variable, for example `@a`, to generate a
symbol unique to the current scope (a cheap trick to approximate Lisp's
`gensym`). Then we define this generated symbol to be the expression on the
right-hand side, for example `@a = expr`, and then we generate case expressions
for each pattern variable to define them, for example

------------------------------------------------------------------------
@a = expr
a = case @a of [a,b,c] -> a
b = case @a of [a,b,c] -> b
c = case @a of [a,b,c] -> c
------------------------------------------------------------------------

Our scheme fails to handle the wild-card pattern `_` correctly, which we'll
fix in a later compiler. Until then, we tread carefully with patterns on the
left.

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

We replace `fpair` and `flst` with more idiomatic Haskell.

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

We add support for default methods as it involves the same code.  Our simple
approach insists the type of the default implementation of a method in a class
`Foo` to have the constraint of the form `Foo a =>`, because we always pass a
dictionary as the first argument. We could improve this slightly by inserting
`const` in the syntax tree if we deduce no constraints are present.

We ruthlessly remove semicolons and braces from our source.

Now that the syntax is slightly more pleasant:

  * We refine `leftyPat` so it correctly handles the wild-card pattern `_` in
  the left-hand side of a definition.

  * We support ranges, except for those that specify a step size.

  * We support list comprehensions.

We prepare to change `getChar` to match Haskell's, which throws an exception on
end of input. Up until now, ours simply calls the `getchar` function of C,
which returns -1 on end of input. Also, we would like Haskell's `isEOF` so we
can avoid this exception.

This takes two steps. The current compiler, whose RTS was generated by the
previous compiler, must use the `getChar` and `isEOF` of the previous
compiler. The most we can do is have it generate a new RTS supporting the
new `getChar` and `isEOF`. Programs compiled by this compiler will use these.
Our next compiler is one such program.

Complications arise because C's `feof(stdin)` only reports the end of input
after `getChar` has attempted to read past it and returned -1, while Haskell's
more clairvoyant version returns `True` before `getChar` would throw an error
because of the end of input. Additionally, our primitive FFI mechanism has
no way to convert a C int to `Bool`.

We write wrappers to get `getChar` and `isEOF` with the desired behaviour, and
add them to the C source to the runtime in the `RTS` module. Thus our next
compiler will print the new runtime in its output. However, it is unable to use
any new runtime features itself; only the programs it builds can do that.

If an FFI call encounters an error, instead of unceremoniously calling
`exit()`, we ought to push an exception-handling combinator on the stack. With
this in mind, I experimented with setting a global flag on failure to trigger
exception handling, but it caused a massive performance hit. Compiler build
times went up from around 7 seconds to 10 seconds on my laptop, mostly caused
by checking the flag for every `getChar`, `isEOF`, and `putChar` call. The
compiler source is about 70000 characters, and the output is about 200000
characters. Each input byte needs one `isEOF` and one `getChar` call, and each
output byte needs one `putChar` call, which suggests we're eating close to 10
extra microseconds per check.

I tried removing the flag and reordering foreign function calls so that they
occur after the stack has been primed to return results; this way, the foreign
call wrapper can simply push an exception combinator on the stack on error. But
I ran into a smaller but still significant performance hit. Even without
conditional branching in the happy path, the reordering is evidently enough to
mess up C compiler optimizations.

We can work around this problem with a better `getContents` implementation,
and indeed, perhaps this would already improve current build times.
For now we'll just put up with `exit()` instead of exceptions.

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
