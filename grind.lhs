= Level grinding =

We incrementally improve our compiler over and over again, which is like
grinding levels in a computer role-playing game. There is even a skill tree of
sorts. Do we want to add language features? Or optimize the generated code? Or
improve error reporting? And so on.

== Stringy ==

Algorithm 4.1 of Kiselyov's paper; strings and character constants.

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
<p><a onclick='hideshow("stringy");'>&#9654; Toggle Source</a></p>
<div id='stringy' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::stringy[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Binary ==

Lists; binary operators on the right-hand side.

We can now write `xs ++ ys` in expressions, though the function itself must be
defined with `(++) xs ys = ...`.

++++++++++
<p><a onclick='hideshow("binary");'>&#9654; Toggle Source</a></p>
<div id='binary' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::binary[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Algebraically ==

Algebraic data types, sections, case expressions, recursive definitions, but
not mutually recursive definitions.

Because of the link:scott.html[simplistic way we convert case expressions to
lambda calculus], our compiler expects case expressions to list out each data
constructor in the order they are given in their `data` declaration.

We pay a heavy price for simplicity. When a case expression is evaluated, we
copy the address of each alternative to the stack, only to eventually eliminate
all but one. Furthermore, we copy and delete these addresses by evaluating
intricate sequences of B and K combinators.

++++++++++
<p><a onclick='hideshow("algebraically");'>&#9654; Toggle Source</a></p>
<div id='algebraically' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::algebraically[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Parity ==

Achievement unlocked. GHC accepts our next compiler if we insert the following
preamble:

------------------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Prelude ((+), (-), (*), Char, Int, String, succ)
import Data.Char (chr, ord)
import qualified Prelude
a <= b = if a Prelude.<= b then True else False
(/) = Prelude.div
(%) = Prelude.mod
class Eq a where { (==) :: a -> a -> Bool };
instance Eq Char where { (==) x y = if (x Prelude.== y) then True else False };
instance Eq Int where { (==) x y = if (x Prelude.== y) then True else False };
------------------------------------------------------------------------

We can now develop using GHC with its powerful type checking and friendly
error messages. Naturally, we switch back to our compiler when it all works,
though we must be mindful that in our language, all operators have the same
precedence, every identifier in an expression we're parsing must have already
been defined, and case expressions require all data constructors to appear
exactly once and in order.

We drop support for the `@` prefix. Our language has advanced enough that we no
longer need direct access to primitive combinators.

This compiler supports integer constants. We've survived without them for
so long because the `succ` function has been enough for our numerical needs
so far.

++++++++++
<p><a onclick='hideshow("parity");'>&#9654; Toggle Source</a></p>
<div id='parity' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::parity.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Fixity ==

This compiler supports `infix, infixl, infixr` declarations at the beginning of
the source.

++++++++++
<p><a onclick='hideshow("fixity");'>&#9654; Toggle Source</a></p>
<div id='fixity' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::fixity.hs[]
------------------------------------------------------------------------
