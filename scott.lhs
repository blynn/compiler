= The Scott encoding =

Encoding data as functions may seem strange at first, but after playing
with combinators for a while,
https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding[the Scott
encoding] suggests itself.

Take an algebraic data type:

------------------------------------------------------------------------
data Adt a b c = Foo a | Bar | Baz b c
------------------------------------------------------------------------

Suppose we come across a value `x` of type `Adt`. What can we do with it?

The only nontrivial course of action is to scrutinize it with a case statement:

------------------------------------------------------------------------
case x of
  Foo a -> f a
  Bar -> g
  Baz b c -> h b c
------------------------------------------------------------------------

What's the quickest way to turn this expression into a closed lambda calculus
term?

One idea that springs to mind is to delete the keywords `case` and `of` and
replace each data constructor in a pattern with `\`. Then we get:

------------------------------------------------------------------------
x (\a -> f a) (\ -> g) (\b c -> h b c)
------------------------------------------------------------------------

Here, a lambda abstraction with no variables has the obvious meaning, namely,
`+\ -> g+` is the same as `g`.

Then the Scott encoding of the value `x` of type `Adt` is whatever it takes to
make the above work like the original case expression.
For example, the value `Foo 42` must be the function:

------------------------------------------------------------------------
\f _ _ -> f 42
------------------------------------------------------------------------

the value `Bar` must be:

------------------------------------------------------------------------
\_ g _ -> g
------------------------------------------------------------------------

and the value `Baz "qux" 9000` must be:

------------------------------------------------------------------------
\_ _ h -> h "qux" 9000
------------------------------------------------------------------------

More generally:

------------------------------------------------------------------------
Foo a   = \f _ _ -> f a
Bar     = \_ g _ -> g
Baz a b = \_ _ h -> h a b
------------------------------------------------------------------------

== Booleans, Numbers, Lists ==

We would like to define booleans as:

------------------------------------------------------------------------
data Bool = False | True
------------------------------------------------------------------------

Since `False` appears before `True`, a compiler seeing this would naturally
index them with the numbers 0 and 1 respectively, which matches common
practice. It matches Haskell, too, as can be seen in the functions: `fromEnum
toEnum minBound maxBound succ pred`.

Unfortunately, for the time being, we must define:

------------------------------------------------------------------------
data Bool = True | False
------------------------------------------------------------------------

so our compiler produces the Scott encodings:

------------------------------------------------------------------------
True  = \x _ -> x
False = \_ y -> y
------------------------------------------------------------------------

This is because long before computers were commonplace, Alonzo Church devised
this particular encoding of booleans, which subsequently became standard.
We bow down to this convention to avoid confusion.

Peano numbers are defined by the following algebraic data type:

------------------------------------------------------------------------
data Peano = Zero | Succ Peano
------------------------------------------------------------------------

The Scott encoding is:

------------------------------------------------------------------------
Zero   = \f _ -> f
Succ n = \_ g -> g n
------------------------------------------------------------------------

Unlike Church numerals, the predecessor function (where we define the
predecessor of zero to be zero) is easy to write down:

------------------------------------------------------------------------
predecessor n = case n of
  Zero -> Zero
  Succ n -> n
------------------------------------------------------------------------

Using the Scott encoding:

------------------------------------------------------------------------
predecessor n = n Zero (\n -> n)
------------------------------------------------------------------------

Pure lambda calculus has an undeserved reputation for sloth.
Perhaps one of the misconceptions is arithmetic must be performed in unary.
Not so! We may define numbers in binary instead of unary:

------------------------------------------------------------------------
data Binary = End | Nil Binary | One Binary
------------------------------------------------------------------------

But we may as well use a list of booleans instead.
A list is defined by:

------------------------------------------------------------------------
data [a] = [] | a : [a]
------------------------------------------------------------------------

yielding the Scott encodings:

------------------------------------------------------------------------
[]       = \f _ -> f
(:) a as = \_ g -> g a as
------------------------------------------------------------------------

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

We have no need for these since we'll extend CL to support the native integer
types of the underlying machine. But we stress again pure lambda calculus
numerals are not condemned to be unary!
