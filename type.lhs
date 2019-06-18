= Types =

At a company I once worked for, many a project was written in a certain
language with dynamic typing. The project would get off the ground quickly,
perhaps because the edit-compile-run cycle was so short, or because the only
allowed statically typed languages at the time stifled the programmer with
boilerplate.

Without exception, each of these projects would soon become impossible to
maintain, and eventually they would have to completely rewrite it from scratch
in a statically typed language.

These real-life experiments indicate a programming language ought to be
statically typed.

On the theory side, static types are how we can prove properties about our
programs. Not only can we solve the halting problem by proving our code must
terminate, but we can show it must, for example, correctly sort a list. Types
can also automate programming, namely,
https://www.youtube.com/watch?reload=9&v=mOtKD7ml0NU[a human supplies the type
of a function and the computer fills it in].

Therefore, we ought to add types to our language. As we wish to avoid
boilerplate, we opt for type inference, with at least one deliberate difference
to standard Haskell: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf[Let
should not be generalized], so we only generalize expressions defined at the top
level.

== Typically ==

https://web.cecs.pdx.edu/~mpj/thih/thih.pdf['Typing Haskell in Haskell'] by
Mark P. Jones has the basics, and we shamelessly lift code from this excellent
paper. We sidestep some of the more complicated parts of the paper because
we have no support for mutual recursion, or pattern matching.

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

However, types also enable us to write less code.
Haskell's typeclasses are a principled way of overloading functions.
By giving the type checker some powers akin to those of Prolog, the compiler
can predicatbly generate tedious code so humans can work at an abstract
high level.

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
