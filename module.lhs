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
problem into digestible subproblems. For example, we might only need to think
about the intricacies of magic spells when controlling the wizard character.

As usual, our first stab has limitations.
This party is just getting started.

  * To avoid dealing with files, our compiler reads the concatenation of all
  the modules on standard input.

  * To keep parser changes minimal, all symbols are exported, and all module
  imports are unqualified.

  * Fixity declarations must precede all occurrences of their corresponding
  operators in the standard input.

  * At most one module should declare FFI imports.

  * Cyclic module dependencies cause an infinite loop.

On the other hand, we proudly support `import` statements anywhere in the file,
and multiple modules in the same file.

In fact, this is why fixity declarations must appear first in the input. GHC
insists on one module per file with imports appearing before other
declarations, hence its parser can process imports before reaching any
expressions and determine the fixity of any operators that appear when it later
reaches them. With our scheme, we may encounter an operator in an expression
before learning its fixity, which confuses our simple parser. In a later
compiler we'll address this issue.

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
  * The RTS reduces `fail#` on failed case matches.

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
code that searches imports for this information is messy. For example,
the `fillSigs` helper raids other modules for the types of methods.

We had previously substituted the syntax trees for default method
implementations straight into instances. If we one day want incremental
compilation, then it is likely easier to compile a default implementation
once, then access it from other modules via a layer of indirection. With this
in mind, for each method `foo`, we generate a method called `{default}foo`
whose body is the default method implementation of `foo` if given, and
`fail#` otherwise.

Since we toss dictionary selector functions on to a big pile of ordinary
definitions, to find the type of a method we add `typeOfMethod`, whose logic is
similar to `findImportSym`, but differs enough that we implement it
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
cat true.Base.hs Ast.hs Map.hs Parser.hs Kiselyov.hs Unify.hs RTS.hs Compiler.hs party.hs
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
this by adding the line:

------------------------------------------------------------------------
import_qq_here = import_qq_here
------------------------------------------------------------------------

immediately after the import statements. Then we enable the C pre-processor and
define `import_qq_here` to be `import Text.RawString.QQ --`.

We perform similar tricks to hide `Prelude` symbols we define in the `System`
module.

++++++++++
<p><a onclick='hideshow("RTS");'>&#9654; Toggle `RTS.hs` for GHC</a></p><div id='RTS' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/RTS.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

Our source now works with GHC with the following options:

------------------------------------------------------------------------
include::inn/compat.ghci[]
------------------------------------------------------------------------

In the `inn` subdirectory:

------------------------------------------------------------------------
$ ghci -ghci-script compat.ghci party.hs ../stub.o
------------------------------------------------------------------------

Here, the `stub.o` has been created from `stub.c` with `clang -c` or similar.

We gave the nice filenames to GHC, which expects to find modules in files with
matching names. Our compilers tolerate weird filename prefixes and suffixes
because we can simply concatenate different files. An alternative is to manage
different subdirectories containing the same filenames.

We can test later iterations with GHCi by symlinking appropriate versions of
each file in a dedicated subdirectory.

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
cat true.Base.hs Ast.hs Map.hs Parser.hs Kiselyov.hs Unify.hs RTS.hs Compiler1.hs party.hs
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

== Party2 ==

For link:mvp.html[mutual let definitions] we wrote code that traversed a syntax
tree to substitute certain variables. An alternative is to build a syntax tree
that describes this substitution. After all, lambda calculus is substitution
incarnate. In other words, we rely more on dynamic rather than static
semantics, a distinction that sometimes blurs because beta-reducing may occur
during optimization.

One advantage of this approach is we can remove `overFreePro`, which is helper
that traverses over syntax trees before case expressions and pattern matches
have been transformed away.

We add support for named record fields. We extend the parser to support
data type declarations such as:

------------------------------------------------------------------------
data Foo = Foo { bar :: Int, baz :: String } | Qux
------------------------------------------------------------------------

One function definition per field suffices for accessors.
For example, we genarate:

------------------------------------------------------------------------
bar = \case Foo bar baz -> bar
------------------------------------------------------------------------

except at a lower level, exploiting our knowledge that our data types are
Scott-encoded.

Record updates and initialization are more challenging. We need more than plain
function definitions, and furthermore, we only have all valid field names after
parsing. This means we ought to extend our syntax tree to hold lists of field
bindings for record updates and initializations.

Instead of adding a new data constructor to our `Ast` type, we invent two
basic combinators `Basic "{="` and `Basic "=}"` which act as delimiters
for a list of field bindings, where the `A` data constructor acts like a cons.
An alternative is to use recursion schemes for our many variants of syntax
trees.

By pattern compilation, we know all the field names, so at this point we call
`resolveFieldBinds` to transform, say:

------------------------------------------------------------------------
x { bar = 42 }
------------------------------------------------------------------------

into:

------------------------------------------------------------------------
case x of \Foo {orig}bar {orig}baz -> Foo 42 {orig}baz
------------------------------------------------------------------------

though again using a lower level representation since we know we're
Scott-encoding the data types. The `{orig}` added by our code to each variable
name guards against variable capture.

For record initializations, we only generate the right-hand side of the case
match and use `undefined` for missing fields instead of `{orig}` variables.

We implement `deriving` for `Eq` and `Show`. It would be nice to automatically
derive `Eq` for our primitive data types (unit, boolean, pairs, lists) but this
would require all programs to define the `Eq` class.

We prepare to change `getChar` to match Haskell's, which throws an exception on
end of input. Up until now, ours simply calls the `getchar` function of C,
which returns -1 on end of input. Also, we would like Haskell's `isEOF` so we
can avoid this exception.

Complications arise because C's `feof(stdin)` only reports the end of input
after `getChar` has attempted to read past it and returned -1, while Haskell's
more clairvoyant version returns `True` before `getChar` would throw an error
because of the end of input. Additionally, our primitive FFI mechanism has
no way to convert a C int to `Bool`.

We write wrappers to get `getChar` and `isEOF` with the desired behaviour, and
add them to the C source to the runtime in the `RTS` module. Thus our next
compiler will print the new runtime in its output. However, it is unable to use
any new runtime features itself; only the programs it builds can do that.

If an FFI call enocunters an error, instead of unceremoniously calling
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

We also fix a bug with FFI imports that return values and have been declared
to be pure functions. Directly pushing `_NUM` and a value is wrong, because
our code relies on numbers being held in app nodes. We should backport this
fix.

Recall for data types, we maintain a map from a data constructor name to the
list of all data constructors of the same type, along with the types of any
field they may have. Even though we need to generate a unique and predictable
symbol per type to represent corresponding case expressions, the function
`specialCase` simply builds this symbol from the first data constructor.

We barely modify this map for named fields. As a result, there's no easy way
for `findField` to look up relevant information based on a field name. We
ineffiicently search linearly through possibly repeated entries. It may be
better to add a separate map for named fields, but it's tedious to add fields
to the `Neat` type when our current compiler lacks support for naming them!
Once again, a proto-chicken comes first.

To test with GHC, we create a new directory containing appropriately named
symlinks to the desired versions of the modules. Incremental development means
we only need to change a few symlinks at a time, but in the long run, we ought
to write a program to generate all symlinks from a given set of module files.

++++++++++
<p><a onclick='hideshow("Ast1");'>&#9654; Toggle `Ast1.hs`</a></p><div id='Ast1' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Ast1.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("Parser1");'>&#9654; Toggle `Parser1.hs`</a></p><div id='Parser1' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Parser1.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("Compiler2");'>&#9654; Toggle `Compiler2.hs`</a></p><div id='Compiler2' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Compiler2.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("RTS1");'>&#9654; Toggle `RTS1.hs`</a></p><div id='RTS1' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/RTS1.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Party3 ==

We fix the problem with foreign imports across multiple modules. In the
lone-module days, we numbered the imports as we parsed the source. Now,
the numbering must be consistent across all modules.

In the spirit of incremental compilation, we replace the number of an import
with its name in the syntax tree, which we map to a number during our code
generation that corresponds to linking.

We reuse the `Link` data constructor for this. The special `{foreign}` module
indicates the function name is foreign. Thus we can discard the `ForeignFun`
data constructor.

We also check for name conflicts among foreign imports and exports.

We remove our ancient `fpair` and `flst` functions, a long overdue cleanup.
We take advantage of our new ability to derive `Eq` and `Show` instances,
and also name the fields of the `Neat` data type.

We continue our revamp of `getChar`, as our previous iteration laid the
groundwork.

++++++++++
<p><a onclick='hideshow("true.Base1");'>&#9654; Toggle `true.Base1.hs`</a></p><div id='true.Base1' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/true.Base1.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("Ast2");'>&#9654; Toggle `Ast2.hs`</a></p><div id='Ast2' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Ast2.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("Parser2");'>&#9654; Toggle `Parser2.hs`</a></p><div id='Parser2' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Parser2.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("Compiler3");'>&#9654; Toggle `Compiler3.hs`</a></p><div id='Compiler3' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Compiler3.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("RTS2");'>&#9654; Toggle `RTS2.hs`</a></p><div id='RTS2' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/RTS2.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("party1");'>&#9654; Toggle `party1.hs`</a></p><div id='party1' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/party1.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Party4 ==

Recall we require a fixity declaration to precede the use of its corresponding
operator, which forces us to concatenate module sources in a particular order.
We remove this wart by adding a new phase. Once done, not only may we paste
together modules in any order, but we may also declare fixities anywhere within
a module.

During parsing, operators have the same precedence. When a chain of two or more
appear in a row, we abuse the syntax tree to store them in a right-associative
list, for example: `[1 + 2, * 3, - 4, + 5]`.

For patterns, we use the list field of a `PatCon` value; a made-up data
constructor `"{+"` indicates the beginning of such a list. Expressions are
clumsier; we bookend chains with the made-up basic combinators `"{+"` and
`"+}"`, and fashion a list out of `A` and `V` nodes.

By the time we call `patternCompile`, we have access to all modules. During
this phase, we traverse the syntax tree, and we re-associate each specially
marked infix chain now that we can look up the fixities of all operators.

The algorithm is conceptually straightforward. Starting from the first binary
infix expression, that is, two operands and one operator, for each operator and
operand we add on the right, we walk down the right spine of the current syntax
tree until we reach a node of higher precedence; leaf nodes are considered to
have maximum precedence. Then we insert the operator and operand at this point.
We also check for illegal infix operator conflicts.

The code is messy due to a couple of wrinkles. Firstly, we have two distinct ad
hoc representations of lists for holding infix chains. Secondly, we temporarily
mark operands with more ad hoc conventions to avoid descending too far when
reshaping syntax trees. For example, in the expression `1 + (2 + 3) * 4`, the
subexpression `(2 + 3)` is atomic.

We only allow top-level fixity declarations. We could add support for scoped
fixity declarations with yet more ad hoc encodings that we later use to create
scoped fixity lookup tables that override the global ones.

We do some housekeeping. Given a `Neat`, type inference had produced a tuple of
a particular type that contained the data needed by the next phase. We change
it to produce a new `Neat` with an updated `typedAsts` field, so there's one
fewer data type to occupy our thoughts and APIs. We no longer need to pick out
specific fields to pass to the next phase, as we simply pass everything.

We also change `typedAsts` from a list to a map, which should be faster.
Perhaps we should propagate this change further back, because compilation is
growing even slower.

We add support for top-level type declarations, and check they agree with
definitions, and in some cases specialize by substituting in typeclass
dictionaries.

Adding modules has made a mess of our various functions for looking up data
constructors, top-level variables, typeclasses, and so on. We reorganize them
a little to standardize the logic for searching through the list of imports.
This makes it easier to add support for lists of export symbols.

++++++++++
<p><a onclick='hideshow("Ast3");'>&#9654; Toggle `Ast3.hs`</a></p><div id='Ast3' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Ast3.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("Parser3");'>&#9654; Toggle `Parser3.hs`</a></p><div id='Parser3' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Parser3.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("Compiler4");'>&#9654; Toggle `Compiler4.hs`</a></p><div id='Compiler4' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/Compiler4.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

++++++++++
<p><a onclick='hideshow("party2");'>&#9654; Toggle `party2.hs`</a></p><div id='party2' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::inn/party2.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++
