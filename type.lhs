= Types =

Types enable us to reason about our programs.
We can solve the halting problem by using type to prove our code must
terminate. But so what? Even if we know a program halts, we want to know if it
gives the right answer! Types can do this too. We can prove, for example, that
a given function correctly sorts a list.

Types can also automate programming, namely,
https://www.youtube.com/watch?reload=9&v=mOtKD7ml0NU[a human supplies the type
of a function and the computer fills it in].

Types can also be lightweight. Indeed, they can be invisible. A compiler can
use 'type inference' to type-check a program completely free of any type
annotations.

Therefore, we ought to add types to our language. We follow Haskell's
example, with at least one deliberate difference:
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf[Let
should not be generalized], so we only generalize expressions defined at the top
level.

== Typically ==

We shamelessly lift code from
https://web.cecs.pdx.edu/~mpj/thih/thih.pdf['Typing Haskell in Haskell'] by
Mark P. Jones. We sidestep some of the more complicated parts of the paper
because we have no support for mutual recursion, or pattern matching.

Since we're using the Scott encoding, from a data type declaration:

------------------------------------------------------------------------------
data Adt a b c = Foo a | Bar | Baz b c
------------------------------------------------------------------------------

we generate types for the data constructors:

------------------------------------------------------------------------------
("Foo", a -> Adt a b c)
("Bar", Adt a b c)
("Baz", b -> c -> Adt a b c)
------------------------------------------------------------------------------

and we generate the type:

------------------------------------------------------------------------------
("|Foo|Bar|Baz", Adt a b c -> (a -> x) -> x -> (b -> c -> x) -> x)
------------------------------------------------------------------------------

which represents the type of `case` in:

------------------------------------------------------------------------------
case x of
  Foo a -> f a
  Bar -> g
  Baz b c -> h b c
------------------------------------------------------------------------------

which we compile as:

------------------------------------------------------------------------------
x (\a -> f a) (\ -> g) (\b c -> h b c)
------------------------------------------------------------------------------

Our type checker is missing several features, such as kind checking and duplicate
definitions,

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

------------------------------------------------------------------------------
include::typically.hs[]
------------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Classy ==

Types can be burdensome. In the worst cases, it feels like we're wrestling
with the compiler, twisting our code this way and that so it will type check.

However, types also enable us to write less code. Haskell's typeclasses give
us principled overloading. By bestowing Prolog-like powers to the type checker,
the compiler can predictably generate tedious code so humans can ignore
irrelevant details.

Again, Jones' paper provides the background. However, we do not merely wish
to infer types and check they match. We also must generate code according to
the types we infer. See John Peterson and Mark Jones, 'Implementing Type
Classes'.

We choose the dictionary approach. A dictionary is a record of functions that
is implicitly passed around. For example, if we infer the function `foo` has
type:

------------------------------------------------------------------------------
foo :: Eq a => Show a => a -> String
------------------------------------------------------------------------------

then we may imagine our compiler turning fat arrows into thin arrows:

------------------------------------------------------------------------------
foo :: Eq a -> Show a -> a -> String
------------------------------------------------------------------------------

Then it seeks dictionaries that fit the two new arguments of types `Eq a` and
`Show a`, and inserts them into the syntax tree.

With this in mind, we modify the type inference functions so they return a
syntax tree along with its type. Most of the time, the syntax tree is returned
unchanged, but if type constraints are inferred, then we create a `Proof` node
for each constraint, and apply the syntax tree to these new nodes.

In our example, if `t` is the syntax tree of `foo`, then our type inference
function would change it to `A (A t (Proof "Eq a")) (Proof "Show a")`.
Here, we're using strings to represent constraints for legibiity; in reality,
we have a dedicated data type to hold constraints, though later on, we
do in fact turn them into strings when generating variable names.

We call such a node a `Proof` because it's a cute short word, and we think of a
dictionary as proof that a certain constraint is satisfied. (Peterson and Jones
instead write "Placeholder".)

Typeclass methods are included in the above. For example, while processing
the expression:

------------------------------------------------------------------------------
(==) (2+2) 5
------------------------------------------------------------------------------

we infer that `(==)` has type `Eq a => a -> a -> Bool`, so we replace it
with `(==) (Proof "Eq a")`, so eventually the expression reads:

------------------------------------------------------------------------------
(==) (Proof "Eq a") (2+2) 5
------------------------------------------------------------------------------

After type unification, we learn `a` is `Int`:

------------------------------------------------------------------------------
(==) (Proof "Eq Int") (2+2) 5
------------------------------------------------------------------------------

The next phase constructs records of functions to be used as proofs. We
loosely follow 'Typing Haskell in Haskell' once more, and search for instances
that match a given constraint. A matching instance may create more constraints.

Let's walk through how our compiler finds a proof for:

------------------------------------------------------------------------------
Proof "Eq [[Int]]"
------------------------------------------------------------------------------

Our compiler finds an instance match: "Eq a => Eq [a]", so it rewrites the
above as:

------------------------------------------------------------------------------
(V "Eq [a]") (Proof "Eq [Int]")
------------------------------------------------------------------------------

The "Eq [a]" string is taken verbatim from an instance declaration, while the
"Eq [Int]" is the result of a type substitution on "Eq a" found during
unification.

Our compiler recursively seeks an instance match for the new `Proof`. Again it
finds "Eq a => Eq [a]", so the next rewrite results in:

------------------------------------------------------------------------------
(V "Eq [a]") ((V "Eq [a]") (Proof "Eq Int"))
------------------------------------------------------------------------------

and again it recursively looks for an instance match. It finds the "Eq Int"
instance, and we have:

------------------------------------------------------------------------------
(V "Eq [a]") ((V "Eq [a]") (V "Eq Int"))
------------------------------------------------------------------------------

Our compiler has previously processed all class and instance declarations,
and has prepared a table that maps "Eq Int" to a record of functions for integer
equality testing, and "Eq [a]" to a function that takes a "Eq a" and returns
a record of functions for equality testing on lists of type `a`.

There is more to do. We lack support for class contexts. Our code allows
instances to stomp over one another, among many other issues.

++++++++++
<p><a onclick='hideshow("classy");'>&#9654; Toggle Source</a></p>
<div id='classy' style='display:none'>
++++++++++

------------------------------------------------------------------------------
include::classy.hs[]
------------------------------------------------------------------------------

++++++++++
</div>
++++++++++
