= Patterns and Guards =

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

Patterns and guards make programming much more pleasant.

== Patty ==

We support definitions spread across multiple equations, and patterns in
lambdas, case expressions, and the arguments of function definitions.

We leave supporting patterns as the left-hand side of an equation for another day.

*Definitions with multiple equations*

Consider a top-level function defined with multiple equations:

\begin{code}
f (Left (Right x)) y     z@(_, 42) = expr1:
f (Right a)        "foo" y         = expr2;
\end{code}

This is parsed as:

\begin{code}
f = Pa [ ([(Left (Right x)), y,     z@(_, 42)], expr1)
       , ([(Right a)       , "foo", y        ], expr2)
       ]
\end{code}

where `Pa` is a data constructor for holding a collection of lists of patterns
and their corresponding expressions.

We rewrite this as a lambda expression.  Since there are 3 parameters, we
generate 3 variable names and begin defining `f` as:

\begin{code}
f = \1# 2# 3# -> ...
\end{code}

In our example, we start numbering the generated variable names from 1, but in
general they start from the value of a carefully maintained ocunter.

We bind a `join#` variable that represents a join point which we later
construct from the other defining equations.

\begin{code}
f = \1# 2# 3# -> \join# -> ...
\end{code}

The first pattern of the first equation is `(Left (Right x))`, so we add:

\begin{code}
f = \1# 2# 3# -> \join# -> case 1# of
{ Left 4# -> case 4# of
  { Left _ -> join#
  ; Right 5# -> ...
  }
; Right _ -> join#
}
\end{code}

We encounter a variable `x` so we replace all free occurrences of `x` in
`expr1` with `5#` which we denote `expr1[5#/x]`.

The second pattern is `y`, so we replace all free occurrences of this
variable in `expr1` with `2#` to get `expr1[5#/x,2#/y]`.

For the third pattern, we start by replacing all free occurrences of `z` in
with `3#`. We have finished the first equation so we apply what we have so far
to the expression we will obtain from rewriting the other definitions.

\begin{code}
f = \1# 2# 3# -> (\join# -> case 1# of
{ Left 4# -> case 4# of
  { Left _ -> join#
  ; Right 5# -> case 3# of
    { (6#, 7#) -> if 7# == 42 then expr1[5#/x,2#/y,3#/z] else join#
    }
  }
; Right _ -> join#
}) $ ...
\end{code}

The first pattern of the second equation is `Right a`.

\begin{code}
f = \1# 2# 3# -> (\join# -> case 1# of
{ Left 4# -> case 4# of
  { Left _ -> join#
  ; Right 5# -> case 3# of
    { (6#, 7#) -> if 7# == 42 then expr1[5#/x,2#/y,3#/z] else join#
    }
  }
; Right _ -> join#
}) $ \join# -> case 1# of
{ Left _ -> join#
; Right 8# -> ...
}
\end{code}

We replace all free occurrences of `a` in `expr2` with `8#`, which we denote
`expr2[8#/a]`.

Continuing in this fashion, by the end of the second equation we arrive at:

\begin{code}
f = \1# 2# 3# -> (\join# -> case 1# of
{ Left 4# -> case 4# of
  { Left _ -> join#
  ; Right 5# -> case 3# of
    { (6#, 7#) -> if 7# == 42 then expr1[5#/x,2#/y,3#/z] else join#
    }
  }
; Right _ -> join#
}) $ (\join# -> case 1# of
{ Left _ -> join#
; Right 8# -> if 2# == "foo" then expr2[8#/a,3#/y] else join#
}) $ ...
\end{code}

As there are no more equations, we finish off with `fail#`, which causes
program termination on execution:

\begin{code}
f = \1# 2# 3# -> (\join# -> case 1# of
{ Left 4# -> case 4# of
  { Left _ -> join#
  ; Right 5# -> case 3# of
    { (6#, 7#) -> if 7# == 42 then expr1[5#/x,2#/y,3#/z] else join#
    }
  }
; Right _ -> join#
}) $ (\join# -> case 1# of
{ Left _ -> join#
; Right 8# -> if 2# == "foo" then expr2[8#/a,3#/y] else join#
}) $ fail#
\end{code}

*Case expressions*

We could apply the above to rewrite case expressions, but then we'd lose
efficiency from performing a series of binary decisions instead of a single
multi-way decision.

Instead, suppose we have:

\begin{code}
case scrutinee of
  Foo (Left 42) -> expr1
  Baz           -> expr2
  Foo (Right a) -> expr3
  Bar x "bar"   -> expr4
  z             -> expr5
  w             -> expr6
  Baz           -> expr7
  Bar x y       -> expr8
  x             -> expr9
\end{code}

Conceptually, we combine contiguous data constructor alternatives into maps,
where the keys are the data constructors, and the values are the corresponding
expressions appeneded in the order they appear.

\begin{code}
  [ (Foo, [(Left 42) -> expr1, (Right a) -> expr3])
  , (Bar, [x "bar" -> expr4])
  , (Baz, [ -> expr2])
  ]

z -> expr5

w -> expr6

  [ (Bar, [x y -> expr8])
  , (Baz, [ -> expr7])
  ]

x -> expr9
\end{code}

We rewrite this to:

\begin{code}
(\v -> (\cjoin# -> case v of
  Foo 1# -> Pa [(Left 42) -> expr1, (Right a) -> expr3]
  Bar    -> Pa [x "bar" -> expr4]
  Baz    -> Pa [ -> expr2]
) $ (\cjoin# -> case v of
  6# -> expr5[6#/z]
  _  -> cjoin#
) $ (\cjoin# -> case v of
  7# -> expr6[7#/z]
  _  -> cjoin#
) $ (\cjoin# -> case  v of
  Foo _ -> cjoin#
  Bar   -> [x y -> expr8]
  Baz   -> Pa [ -> expr7]
) $ (V "fail#")
) scrutinee
\end{code}

We then apply the first rewrite algorithm to get:

\begin{code}
(\v -> (\cjoin# -> case v of
  Foo 1# -> case 1# of
    Left 2# -> if 2# == 42 then expr1 else cjoin#
    Right 3# -> expr3[3#/a]
  Bar 4# 5# -> if 5# == "bar" then expr 4 else cjoin#
  Baz -> expr2
) $ (\cjoin# -> case v of
  6# -> expr5[6#/z]
  _ -> cjoin#
) $ (\cjoin# -> case v of
  7# -> expr6[7#/z]
  _ -> cjoin#
) $ (\cjoin# -> case v of
  Foo 8# -> cjoin#
  Bar 9# 10# -> expr8[9#/x 10#/y]
  Baz -> expr7
) $ (V "fail#")
) scrutinee
\end{code}

We use two different names for the join points because when rewriting a case
expression, we may place a subexpression in a `Pa` value. If we used the same
name for the join points, then variable capture could ensue when rewriting the
`Pa` value.

We try to avoid dead code with the `optiApp` helper which beta-reduces
applications of lambdas where the bound variable appears at most once in the
body, but this is imperfect because of the `Pa` value that may appear during
`Ca` rewrites: we look for the bound variable before rewriting the `Pa` value,
thus our count is wrong if the variable is later eliminated when rewriting the
`Pa` value.

++++++++++
<p><a onclick='hideshow("patty");'>&#9654; Toggle `patty.hs`</a></p>
<div id='patty' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::patty.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Guardedly ==

Our last compiler passed an unfortunate milestone: it's over 1000 lines long.

We use language features we just added to shrink the code. At the same time, we
add support for guards.

Before, the right-hand sides of lambdas, equations, and case alternatives were
simply `Ast` values.  We change to the type `[(Ast, Ast]`, that is, a list of
pairs of expressions. During parsing, the guard condition becomes the first
element of a pair, and the corresponding expression is the second element. We
use a list because there can be multiple guards.

We rewrite guards as chains of if-then-else expressions, where the last else
branch is the pattern- or case-join point.

Now that the syntax is nicer, we move `optiApp` to a dedicated optimization
phase.

++++++++++
<p><a onclick='hideshow("guardedly");'>&#9654; Toggle `guardedly.hs`</a></p>
<div id='guardedly' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::guardedly.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== Assembly ==

We split off rewriting cases and patterns into a separate function, and change
it from top-down to bottom-up.

We split off and delay address lookup for symbols from bracket abstraction, and
also delay converting literals to combinators as late as possible.

All this slows our compiler and adds more lines of code, but it disentangles
various phases of the pipeline.

The refactoring makes it easy to dump the output of bracket abstraction on the
source code, which is somewhat analogous to a typical compiler printing the
assembly it generates.

++++++++++
<p><a onclick='hideshow("assembly");'>&#9654; Toggle `assembly.hs`</a></p>
<div id='assembly' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::assembly.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++
