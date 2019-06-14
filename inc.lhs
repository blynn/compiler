= Incremental improvements =

We implement algorithm 4.1 of Kiselyov's paper and support strings and
character constants:

------------------------------------------------------------------------------
include::stringy[]
------------------------------------------------------------------------------

We support binary operators on the right-hand side (so we no longer to
represent 1 + 1 as `#1(#1(+))`), and lists.

------------------------------------------------------------------------------
include::binary[]
------------------------------------------------------------------------------

We support algebraic data types, sections, and case expressions, handle
recursion (but not mutual recursion).

------------------------------------------------------------------------------
include::algebraically[]
------------------------------------------------------------------------------
