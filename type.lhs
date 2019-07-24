= Types and Typeclasses =

Types enable humans and computers to reason about our programs. The halting
problem is not a problem, because types can prove code must terminate. They can
do even more and prove a program terminates with the right answer: for example,
we can prove a given function correctly sorts a list.

Types can automate programming, namely,
https://www.youtube.com/watch?reload=9&v=mOtKD7ml0NU[a human supplies the type
of a desired function and the computer programs itself].

Types can be lightweight. Indeed, they can be invisible. A compiler can
use 'type inference' to type-check a program completely free of any type
annotations. However, it's good to throw a few type annotations in the source,
as they are a form of documentation that is especially reliable because the
compiler ensures they stay in sync with the code.

Therefore, we ought to add types to our language. We mimic Haskell, with at
least one deliberate difference:
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf[Let
should not be generalized]. We only generalize top-level definitions.

== Typically ==

We shamelessly lift code from https://web.cecs.pdx.edu/~mpj/thih/thih.pdf[Mark
P. Jones, 'Typing Haskell in Haskell']. Our version is simpler because we lack
support for mutual recursion and pattern matching.

Since we're using the Scott encoding, from a data type declaration:

------------------------------------------------------------------------
data Adt a b c = Foo a | Bar | Baz b c
------------------------------------------------------------------------

we generate types for the data constructors:

------------------------------------------------------------------------
("Foo", a -> Adt a b c)
("Bar", Adt a b c)
("Baz", b -> c -> Adt a b c)
------------------------------------------------------------------------

Along with:

------------------------------------------------------------------------
("|Foo|Bar|Baz", Adt a b c -> (a -> x) -> x -> (b -> c -> x) -> x)
------------------------------------------------------------------------

which represents the type of `case` in:

------------------------------------------------------------------------
case x of
  Foo a -> f a
  Bar -> g
  Baz b c -> h b c
------------------------------------------------------------------------

The `case` keyword is replaced with the identity combinator
during compilation:

------------------------------------------------------------------------
I x (\a -> f a) (\ -> g) (\b c -> h b c)
------------------------------------------------------------------------

Our type checker is missing several features, such as kind checking and
rejecting duplicate definitions.

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
<p><a onclick='hideshow("typically");'>&#9654; Toggle Source</a></p>
<div id='typically' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::typically.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Classy ==

In the worst case, types are a burden, and force us to wrestle with the
compiler. We twist our code this way and that, and add eye-watering type
annotations until it finally compiles.

In contrast, well-designed types do more with less. Haskell's typeclasses give
us principled overloading. By bestowing Prolog-like powers to the type
checker, the compiler can predictably generate tedious code so humans can
ignore irrelevant details.

Again, 'Typing Haskell in Haskell' provides some background. Since we're
generating code as well as checking types, we also need techniques described in
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.3952&rep=rep1&type=pdf[John
Peterson and Mark Jones, 'Implementing Type Classes'].

We choose the dictionary approach. A dictionary is a record of functions that
is implicitly passed around. For example, if we infer the function `foo` has
type:

------------------------------------------------------------------------
foo :: Eq a => Show a => a -> String
------------------------------------------------------------------------

then we may imagine our compiler turning fat arrows into thin arrows:

------------------------------------------------------------------------
foo :: Eq a -> Show a -> a -> String
------------------------------------------------------------------------

Our compiler then seeks dictionaries that fit the two new arguments of types
`Eq a` and `Show a`, and inserts them into the syntax tree.

With this in mind, we modify the type inference functions so they return a
syntax tree along with its type. Most of the time, they just return the input
syntax tree unchanged, but if type constraints are inferred, then we create a
`Proof` node for each constraint, and apply the syntax tree to these new nodes.

In our example, if `t` is the syntax tree of `foo`, then our type inference
function would change it to `A (A t (Proof "Eq a")) (Proof "Show a")`.
Here, we're using strings to represent constraints for legibiity; in reality,
we have a dedicated data type to hold constraints, though later on, we
do in fact turn them into strings when generating variable names.

We call such a node a `Proof` because it's a cute short word, and we think of a
dictionary as proof that a certain constraint is satisfied. Peterson and Jones
instead write "Placeholder".

Typeclass methods are included in the above. For example, while processing
the expression:

------------------------------------------------------------------------
(==) (2+2) 5
------------------------------------------------------------------------

we infer that `(==)` has type `Eq a => a -> a -> Bool`, so we modify the
syntax tree to:

------------------------------------------------------------------------
(select-==) (Proof "Eq a") (2+2) 5
------------------------------------------------------------------------

After type unification, we learn `a` is `Int`:

------------------------------------------------------------------------
(select-==) (Proof "Eq Int") (2+2) 5
------------------------------------------------------------------------

The next phase constructs records of functions to be used as proofs. We
loosely follow 'Typing Haskell in Haskell' once more, and search for instances
that match a given constraint. A matching instance may create more constraints.

We walk through how our compiler finds a proof for:

------------------------------------------------------------------------
Proof "Eq [[Int]]"
------------------------------------------------------------------------

Our compiler finds an instance match: `Eq a => Eq [a]`, so it rewrites the
above as:

------------------------------------------------------------------------
(V "Eq [a]") (Proof "Eq [Int]")
------------------------------------------------------------------------

The "Eq [a]" string is taken verbatim from an instance declaration, while the
"Eq [Int]" is the result of a type substitution found during unification
on "Eq a".

Our compiler recursively seeks an instance match for the new `Proof`. Again it
finds `Eq a => Eq [a]`, so the next rewrite yields:

------------------------------------------------------------------------
(V "Eq [a]") ((V "Eq [a]") (Proof "Eq Int"))
------------------------------------------------------------------------

and again it recursively looks for an instance match. It finds the `Eq Int`
instance, and we have:

------------------------------------------------------------------------
(V "Eq [a]") ((V "Eq [a]") (V "Eq Int"))
------------------------------------------------------------------------

This works, because our compiler has previously processed all class and
instance declarations, and has prepared the symbol table so that maps "Eq Int"
to a record of functions for integer equality testing, and "Eq [a]" to a
function that takes a "Eq a" and returns a record of functions for equality
testing on lists of type `a`.

Overloading complicates our handling of recursion. For example,
each occurrence of `(==)` in:

------------------------------------------------------------------------
instance Eq (Int, String) where (x, y) == (z, w) = x == z && y == w
------------------------------------------------------------------------

refers to a distinct function, so introducing the `Y` combinator here is
incorrect. We should only look for recursion after type inference expands
them to `select-== Dict-Eq-Int` and `select-== Dict-Eq-String`, and we
look for recursion at the level of dictionaries.

Among the many deficiencies in our compiler: we lack support for class
contexts; our code allows instances to stomp over one another; our algorithm
for finding proofs may not terminate.

Without garbage collection, this compiler requires unreasonable amounts of
memory.

++++++++++
<p><a onclick='hideshow("classy");'>&#9654; Toggle Source</a></p>
<div id='classy' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::classy.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++
