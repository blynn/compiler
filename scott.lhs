= Scott encoding =

Encoding data as functions may seem strange at first, but after playing around
with combinators for a while, the Scott encoding seems natural.

Take some algebraic data type:

------------------------------------------------------------------------------
data Adt a b c = Foo a | Bar | Baz b c
------------------------------------------------------------------------------

Suppose we come across a value `x` of type `Adt`. What can we do with it?
The only possible action is to examine it with a case statement:

------------------------------------------------------------------------------
case x of
  Foo a -> f a
  Bar -> g
  Baz b c -> h b c
------------------------------------------------------------------------------

What's the quickest way to turn this expression into a closed lambda calculus
term?

One idea that springs to mind is to delete the keywords `case` and `of` and
replace each data constructor in the pattern with `\`. Then we get:

------------------------------------------------------------------------------
x (\a -> f a) (\ -> g) (\b c -> h b c)
------------------------------------------------------------------------------

Here, a lambda abstraction with no variables has the obvious meaning, that is,
`\ -> g` is the same as `g`.

Then the Scott encoding of the value `x` of type `Adt` is whatever it takes to
make the above work like the original case expression.
For example, the value `Foo 42` must be the function:

------------------------------------------------------------------------------
\f _ _ -> f 42
------------------------------------------------------------------------------

the value `Bar` must be:

------------------------------------------------------------------------------
\_ g _ -> g
------------------------------------------------------------------------------

and the value `Baz "qux" 9000` must be:

------------------------------------------------------------------------------
\_ _ h -> h "qux" 9000
------------------------------------------------------------------------------

More generally:

------------------------------------------------------------------------------
Foo a   = \f _ _ -> f a
Bar     = \_ g _ -> g
Baz a b = \_ _ h -> h a b
------------------------------------------------------------------------------

== Numbers ==

Peano numbers are the following algebraic data type:

------------------------------------------------------------------------------
data Peano = Zero | Succ Peano
------------------------------------------------------------------------------

From above, the Scott encoding is:

------------------------------------------------------------------------------
Zero   = \f _ -> f
Succ n = \_ g -> g n
------------------------------------------------------------------------------

Unlike Church numerals, the predecessor function (where we define the
predecessor of zero to be zero) is easy to find:

------------------------------------------------------------------------------
predecessor n = case n of
  Zero -> Zero
  Succ n -> n
------------------------------------------------------------------------------

With a Scott-encoded input, this becomes:

------------------------------------------------------------------------------
predecessor n = n Zero (\n -> n)
------------------------------------------------------------------------------

Pure lambda calculus has an undeserved reputation for sloth; for example,
Chaitin write "you can’t really run programs that way, they’re too
slow" and picks LISP instead.

Perhaps one of the misconceptions is arithmetic must be performed in unary.
Not so! We may define numbers in binary instead of unary:

------------------------------------------------------------------------------
data Binary = One Binary | Nil Binary | End
------------------------------------------------------------------------------

But we may as well use a list of booleans instead.

Haskell's predefined lists and booleans are:

------------------------------------------------------------------------------
data [a] = [] | a : [a]
data Bool = False | True
------------------------------------------------------------------------------

which implies their Scott encodings are:

------------------------------------------------------------------------------
[]       = \f _ -> f
(:) a as = \_ g -> g a as

False = \f _ -> f
True  = \_ g -> g
------------------------------------------------------------------------------

Here's a starter pack of functions for numbers encoded in binary:

\begin{code}
type Binary = [Bool]

suc :: Binary -> Binary
suc bs = case bs of
  [] -> [True]
  False : bt -> True  : bt
  True  : bt -> False : suc bt

addC :: Binary -> Binary -> Bool -> Binary
addC as bs c = case as of
  [] -> if c then suc bs else bs
  (a:at) -> case bs of
    [] -> if c then suc as else as
    (b:bt) -> ((a /= b) /= c) : addC at bt (a && b || c && (a || b))

add :: Binary -> Binary -> Binary
add as bs = addC as bs False

strip :: Binary -> Binary
strip bs = case bs of
  [] -> []
  True  : bt -> True : strip bt
  False : bt -> case strip bt of
    [] -> []
    bt' -> False : bt'

pre' bs = case bs of
  [] -> []
  True : bs' -> False : bs'
  False : bs' -> True : pre' bs'

pre :: Binary -> Binary
pre bs = strip (pre' bs)

decode' n acc bs = case bs of
  [] -> acc
  False : bt -> decode' (2*n)  acc      bt
  True  : bt -> decode' (2*n) (acc + n) bt

decode :: Binary -> Int
decode = decode' 1 0

encode :: Int -> Binary
encode n = if n == 0 then [] else case divMod n 2 of
  (n', 0) -> False : encode n'
  (n', 1) -> True  : encode n'
\end{code}
