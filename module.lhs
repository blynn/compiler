= Modules =

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

It's about time to add support for modules: our last compiler is almost 2000
lines of code.

== Party ==

Continuing our RPG analogy, we liken modules to parties of heroes. Even in
single-player games, we often direct a group of specialists rather than one
powerful multi-talented being. Perhaps we enjoy observing teamwork, or
following gaming traditions.

But perhaps we also like parties for the same reasons we decompose a large
problem into digestible subproblems; for example, we might only need to think
about the intricacies of magic spells when controlling the wizard character.

As usual, our first stab has limitations.

  * To avoid dealing with files, our compiler reads the concatenation of all
  the modules on standard input.

  * To keep parser changes minimal, all symbols are exported, and all module
  imports are unqualified.

  * Fixity declarations must precede all occurrences of their corresponding
  operators in the standard input.

  * At most one module should declare FFI imports, and
  at most one module should declare FFI exports.

  * Cyclic module dependencies cause an infinite loop.

On the other hand, we proudly support `import` statements anywhere in the file,
and multiple modules in the same file.

In fact, this is why fixity declarations must appear first in the input. GHC
insists on one module per file with imports appearing before other
declarations, hence its parser can process imports before reaching any
expressions and determine the fixity of any operators that appear when it later
reaches them. With our scheme, we may encounter an operator in an expression
before learning its fixity.

We can fix this by adding a parsing phase: when parsing expressions, we treat
all infix operators as, say, right-associative with the same precedence, and
after learning the fixities, a second phase re-associates all expressions.
Perhaps in a future compiler.

We tweak the parser to support `module` and `import`, and add a new field to
`Neat` that hold the imports of a module. A module with no explicit `module`
declaration is taken to be the `Main` module. Concatenation implies such a
module would have to appear first.

We add a new `Link` field to the `Extra` data type, which holds the module,
symbol, and type of a top-level variable defined in another module. During
inference, we replace a `V` field with a `Link` field if we find it is exported
from one of the imported modules.

We introduce a one-off `Dep` monad because we lack monad transformers, and
would like a combination of the `Either` and `State` monads when finding the
dependencies of a definition.

Up until now, all symbols were global across a single file. As we Scott-encoded
ADTs and generated types and selector functions for typeclass methods, we
simply threw them on a big pile in a `Neat` value being passed around.
Modules force us to be more careful.

We invent a special module "#" preloaded with built-in defintions required by
the Haskell syntax we support:

  * The unit type and value `()` is part of the language.
  * If expressions and guards require `Bool`, `True`, and `False`.
  * Pairs are part of the language, even though suitably defined ADTs could be
    used instead (link:ioccc.html[the IOCCC edition of our compiler] does this
    to save room). Curiously, Haskell has no built-in type for the dual of
    pairs; requiring the programmer to define `Either`.
  * Lists are famously built into Haskell.
  * String literals require lists.
  * We compile recursive let definitions with `fix`.
  * Operations involving native integer types: `chr ord intAdd intMul`
    and so on.
  * Primitives for IO monad methods.

Then each module implicity imports this special "#" module, so these built-in
primitives are accessible to all.

This is a good time to mention that rewriting means:

  * Ranges become expressions involving `enumFromTo` and `enumFrom`.
  * Failed pattern matches are `undefined`.
  * We need `pure` (for `pure ()`) and `>>=` to support `do` notation.
  * Patterns containing integer and character literals require `(==)`.
  * List comprehensions are expressed in terms of `concatMap` and `pure`.

None of these are built-in; they must be explicity defined at the top level if
these language features are used. The last of these implies we must define an
`Applicative` instance for lists if `pure` has its standard meaning. To remove
these gotchas, we could define low-level primitives as we do for the others.

Back link:mvp.html[when we added hash consing], we ignored a potential crash
from an unlikely corner case: a foreign export whose right-hand side is a lone
variable. But now every symbol is exported to other modules, and there is also
a new corner case where the right-hand side is a lone symbol defined in some
import. We therefore ensure every symbol corresponds to some heap address by
by applying the I combinator to lone variables or imported symbols, which
creates an app cell that gets interned.

Code generation now has two phases. The first corresponds to GHC incrementally
compiling a module: it resolves all locally defined symbols, and leaves `Link`
values indicating where to put addresses of symbols defined elsewhere. The
generated code is not position-independent; rather, for each module, we are
given the current heap pointer, and return an updated heap pointer.

The second phase replaces all `Link` values with heap addresses, as all
entities are in the heap by this point.

Modules make us regret older expedient decisions regarding typeclasses.
We threw default method definitions in one data structure, and lumped together
method type signatures and instances in another. But now we might find a
typeclass definition in one module, and an instance of it in another, so our
code that searches imports for this information is messy.

Also, we toss dictionary selector functions on to a big pile of ordinary
definitions, so to find the type of a method we add `typeOfMethod`, whose logic
is similar to `findImportSym`, but differs enough that we implement it
independently.

++++++++++
<p><a onclick='hideshow("party");'>&#9654; Toggle `party.hs`</a></p>
<div id='party' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::party.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Multiparty ==

We put our changes to the test by splitting `party.hs` into modules.
(We really should do the same for our "marginally" compiler, namely create
an intermediate stage that is exactly the same except we use indentation
instead of braces and semicolons. This would make it easier to compare against
its successor "methodically".)

------------------------------------------------------------------------
cat true.Base.hs Ast.hs Map.hs Parser.hs Kiselyov.hs Unify.hs true.RTS.hs Compiler.hs party.hs
------------------------------------------------------------------------

++++++++++
<p><a onclick='hideshow("true.Base");'>&#9654; Toggle `Base.hs`</a></p><div id='true.Base' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/true.Base.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("Map");'>&#9654; Toggle `Map.hs`</a></p><div id='Map' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Map.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("Kiselyov");'>&#9654; Toggle `Kiselyov.hs`</a></p><div id='Kiselyov' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Kiselyov.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

(There are more files, which I'll include if I get around to writing a tool to
help show several source files in HTML. For now, see the git repo.)

== GHC compatibility ==

The main obstacle to compiling our modules with GHC is the Prelude. We define
entities such as `Monad` and `(==)` from scratch, which breaks `do` notation
for example because GHC always uses `Prelude.Monad`.

We remove this obstacle by simply removing any overlap with the Prelude. We
use `true.Base.hs` with our compilers, and use a stripped-down `Base.hs` when
testing with GHC. This implies much of our `Base` code is untested, but for
this special case, perhaps we can add a wrapper to test it on its own with GHC.

++++++++++
<p><a onclick='hideshow("Base");'>&#9654; Toggle `Base.hs` for GHC</a></p><div id='Base' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Base.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

Another obstacle is our built-in support for quasi-quoted raw strings. We solve
this by confining all such strings to the `RTS` module, which we call
`true.RTS.hs` then create a wrapper `RTS.hs` that uses the C pre-processor to
make it work with GHC.

++++++++++
<p><a onclick='hideshow("RTS");'>&#9654; Toggle `RTS.hs` for GHC</a></p><div id='RTS' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/RTS.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

Our source now works with GHC. In the `inn` subdirectory:

------------------------------------------------------------------------
$ ghci -XBlockArguments -XLambdaCase -XNoMonomorphismRestriction \
  -XMonoLocalBinds -XTupleSections party.hs ../stub.o
------------------------------------------------------------------------

Here, the `stub.o` has been created from `stub.c` with `clang -c` or similar.

We gave the nice filenames to GHC, which expects to find modules in files with
matching names. Our compilers tolerate weird filename prefixes and suffixes
because we can simply concatenate different files. An alternative is to manage
different subdirectories containing the same filenames.

== Party1 ==

Modules feel revolutionary. Our source becomes clearer, because modularization
forces us to think about interdepedencies. (Behind the scenes, I refactored so
breaking up was less hard to do.) And we can progress by making a small change
to a small file, like our earliest compilers back in the day.

However, we face new challenges. Addressing the limitations listed above will
require effort. Prepending a little wrapper no longer suffices for GHC
interoperability. And how are we going to keep track of many versions of many
files?

Our first answer to the last question is to tweak an existing filename and
`Makefile` rule. The module name remains the same but we concatenate a
different file.

An alternative to our strange "#" module is to preload each `Neat` value with
the built-in primitives, at the cost of an extra case when checking for
ambiguity. Every module now defines and exports `True`, for example, and we
must exempt such entities from duplicate detection. On the other hand, our
compiler can better optimize locally defined primitives.

To explore this solution, we copy `Compiler.hs` to `Compiler1.hs`, modify a few
lines, and add a new `Makefile` rule.

We also remove `encTop` and the I combinator insertion trick, and instead
recursively resolve `Local` and `Global` symbols until we reach an address.
This relies on `optiComb` removing cycles involving lone variables on the
right-hand side, and the absence of cycles among module dependencies.

While we're in the neighbourhood, we eliminate `flst` and `fpair`.

------------------------------------------------------------------------
cat true.Base.hs Ast.hs Map.hs Parser.hs Kiselyov.hs Unify.hs true.RTS.hs Compiler1.hs party.hs
------------------------------------------------------------------------

++++++++++
<p><a onclick='hideshow("Compiler1");'>&#9654; Toggle `Compiler1.hs`</a></p><div id='Compiler1' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Compiler1.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++
