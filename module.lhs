= Modules =

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

We invent a special module "#" preloaded with built-in definitions required by
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

Each module implicitly imports this special "#" module, so these built-in
primitives are accessible to all.

This is a good time to mention that rewriting means:

  * Ranges become expressions involving `enumFromTo` and `enumFrom`.
  * Failed pattern matches are `undefined`.
  * We need `pure` (for `pure ()`) and `>>=` to support `do` notation.
  * Patterns containing integer and character literals require `(==)`.
  * List comprehensions are expressed in terms of `concatMap` and `pure`.

None of these are built-in; they must be explicitly defined at the top level if
these language features are used. The last of these implies we must define an
`Applicative` instance for lists. To remove these gotchas, we could define
low-level primitives as we do for the others.

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

We modify the code to insert dictionaries one strongly-connected-component at a
time rather than one function at a time. This is required to correctly compile
mutually recursive functions that use typeclasses. Each function of the
component may wind up calling any other, so it needs all the relevant
dictionaries.

Up until now we had preserved topological order of the top-level definitions
as they made their way through our compiler. We change the code generator so
it no longer needs this precondition, so that we can store compiled functions
and modules in maps rather than delicately manicured lists.

We introduce a single combinator to act as `BK` which frequently occurs due to
Scott encoding.

If `B K x y z = x y` is reduced individually, our virtual machine allocates
a new app-cell for `K (x y)`, only to immediately rewrite it as `I (x y)`,
which again must be reduced to yield `x y` at last. The `BK` combinator
avoids this needless song and dance.

A dedicated `BK` combinator is also aesthetically pleasing. Consider some
three-argument combinator given `x y z`. We can leave `x` alone or apply it to
`z`, and similarly for `y`, and then apply the first thing to the second:

------------------------------------------------------------------------
(x  )(y  )
(x  )(y z)
(x z)(y  )
(x z)(y z)
------------------------------------------------------------------------

The last 3 are the `B C S` combinators. The first one is `BK`.
Smullyan appears not to have assigned a bird to this combinator, so we resort
to the clunky name `BK` throughout our code.

The BK combinator makes it easy for `optim` to rewrite `B BK V` as `CONS`.

We add the `LEFT` combinator, which is equivalent to `B BK T` and also arises
frequently in Scott encodings; indeed, the data constructor `Left` compiles to
`LEFT`. We add the KI combinator to shave off a few more reductions.

[#party.toggleshow]
---------
include::party.hs[]
---------

== Multiparty ==

We put our changes to the test by splitting `party.hs` into modules.
(We really should do the same for our "marginally" compiler, namely create
an intermediate stage that is exactly the same except we use indentation
instead of braces and semicolons. This would make it easier to compare against
its successor "methodically".)

------------------------------------------------------------------------
cat Base0.hs Ast.hs Map.hs Parser.hs Kiselyov.hs Unify.hs RTS.hs Typer.hs party.hs
------------------------------------------------------------------------

[#Base0.toggleshow]
---------
include::inn/Base0.hs[]
---------

[#Map.toggleshow]
---------
include::inn/Map.hs[]
---------

[#Kiselyov.toggleshow]
---------
include::inn/Kiselyov.hs[]
---------

(There are more files, which I'll include if I get around to writing a tool to
help show several source files in HTML. For now, see the git repo.)

== GHC compatibility ==

The main obstacle to compiling our modules with GHC is the Prelude. We define
entities such as `Monad` and `(==)` from scratch, which breaks `do` notation
for example because GHC always uses `Prelude.Monad`.

We remove this obstacle by simply removing any overlap with the Prelude. We use
a stripped-down `Base.hs` when testing with GHC, whereas our compilers really
use files like `Base0.hs`.

This implies much of our `Base` code is untested, but for this special case,
perhaps we can add a wrapper to test it on its own with GHC.

[#Base-GHC.toggleshow]
---------
include::inn/Base.hs[]
---------

Another obstacle is our built-in support for quasi-quoted raw strings. We solve
this by adding the line:

------------------------------------------------------------------------
import_qq_here = import_qq_here
------------------------------------------------------------------------

immediately after the import statements. Then we enable the C pre-processor and
define `import_qq_here` to be `import Text.RawString.QQ --`.

We perform similar tricks to hide `Prelude` symbols we define in the `System`
module.

[#RTS-GHC.toggleshow]
---------
include::inn/RTS.hs[]
---------

Our source now works with GHC with the following options:

---------
include::inn/compat.ghci[]
---------

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
forces us to think about interdependencies, which guided refactoring so
breaking up was less hard to do. And we can progress by making a small change
to a small file, like our earliest compilers back in the day.

However, we face new challenges. Addressing the limitations listed above will
require effort. Prepending a little wrapper no longer suffices for GHC
interoperability. And how are we going to keep track of many versions of many
files?

For now we answer the last question by tweaking an existing filename and
`Makefile` rule. The module name remains the same but we concatenate a
different file.

For link:mvp.html[mutual let definitions] we wrote code that traversed a syntax
tree to substitute certain variables. An alternative is to build a syntax tree
that describes this substitution. After all, lambda calculus is substitution
incarnate. In other words, we rely more on dynamic rather than static
semantics, a distinction that sometimes blurs because beta-reducing may occur
during optimization.

One advantage of this approach is we can remove `overFreePro`, a helper that
traverses over syntax trees before case expressions and pattern matches have
been transformed away.

We extend the parser to support named record fields in data type declarations
such as:

------------------------------------------------------------------------
data Foo = Foo { bar :: Int, baz :: String } | Qux
------------------------------------------------------------------------

For accessors, we generate one function definition per field. For example:

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

Recall for data types, we maintain a map from a data constructor name to the
list of all data constructors of the same type, along with the types of any
field they may have. Even though we need to generate a unique and predictable
symbol per type to represent corresponding case expressions, the function
`specialCase` simply builds this symbol from the first data constructor.

We barely modify this map for named fields. As a result, there's no easy way
for `findField` to look up relevant information based on a field name. We
inefficiently search linearly through possibly repeated entries. It may be
better to add a separate map for named fields, but it's tedious to add fields
to the `Neat` type when our current compiler lacks support for naming them!
Once again, a proto-chicken comes first.

To test with GHC, we create a new directory containing appropriately named
symlinks to the desired versions of the modules. Incremental development means
we only need to change a few symlinks at a time, but in the long run, we ought
to automate symlinking from a given set of module files.

[#Ast1.toggleshow]
---------
include::inn/Ast1.hs[]
---------

[#Parser1.toggleshow]
---------
include::inn/Parser1.hs[]
---------

[#Typer1.toggleshow]
---------
include::inn/Typer1.hs[]
---------

We replace a list with a `Map` for faster type unification.

[#Unify1.toggleshow]
---------
include::inn/Unify1.hs[]
---------

== Party2 ==

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
clumsier; we bookend chains with `L "("` and `V ")"`, and fashion a list out of
`A` and `V` nodes.

Later, once all fixity declarations are known, we traverse the syntax tree, and
we re-associate each specially marked infix chain. The algorithm starts with
the first binary infix expression, that is, two operands and one operator such
as `1 + 2`. For each operator and operand we add on the right, we walk down the
right spine of the current syntax tree until we reach a node of higher
precedence; leaf nodes are considered to have maximum precedence. Then we
insert the operator and operand at this point. We also check for illegal infix
operator conflicts.

The code is messy due to a couple of wrinkles. Firstly, we have two distinct ad
hoc representations of lists for holding infix chains. Secondly, we temporarily
store the AST being reshaped in one-off tree structures.

However, we're still cheating: we maintain one giant fixity declaration table
for all operators across all modules, which relies on operators being distinct.
Also, we only allow top-level fixity declarations. We could add support for
scoped fixity declarations with yet more ad hoc encodings that we later use to
create scoped fixity lookup tables that override the global ones.

We fix the problem with foreign imports across multiple modules. In the
lone-module days, we numbered the imports as we parsed the source. Now,
the numbering must be consistent across all modules.

In the spirit of incremental compilation, we replace the number of an import
with its name in the syntax tree, which we map to a number during our code
generation that corresponds to linking.

We reuse the `Link` data constructor for this. The special `{foreign}` module
indicates the function name is foreign.

We also check for name conflicts among foreign imports and exports.

We take advantage of our new ability to derive `Eq` and `Show` instances,
and also name the fields of the `Neat` data type.

https://www.haskell.org/onlinereport/syntax-iso.html[The Haskell report]
describes layout parsing as a separate phase, and we had followed this closely
in our first implementation of the feature.

It turns out to be less code to inline the various bits and pieces of the
layout logic in the parser. We do wind up with a touch more complexity, as
comments and whitespace must be parsed in a different code path, but it's
manageable.

For parsing one of `let where do of`, and also `\case` since we act as if the
`LambdaCase` is enabled, the `curlyCheck` function temporarily disables the
angle-bracket indentation rule by placing an indent value of 0 at the head of
the `indents` list.

[#Base1.toggleshow]
---------
include::inn/Base1.hs[]
---------

[#Ast2.toggleshow]
---------
include::inn/Ast2.hs[]
---------

[#Parser2.toggleshow]
---------
include::inn/Parser2.hs[]
---------

[#Typer2.toggleshow]
---------
include::inn/Typer2.hs[]
---------

[#RTS1.toggleshow]
---------
include::inn/RTS1.hs[]
---------

[#party1.toggleshow]
---------
include::inn/party1.hs[]
---------

+++++++++
include::toggleshow.js[]
+++++++++
