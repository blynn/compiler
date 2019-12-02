= An award-winning compiler =

link:.[I may have failed that stupid Compilers exam] but at least I can boast
of authoring an award-winning compiler!

(Even if the award is from the https://www.ioccc.org/2019/whowon.html[26th
International Obfuscated C Code Contest].)

To see the source:

------------------------------------------------------------------------
  $ curl https://www.ioccc.org/2019/2019.tar.bz2 | tar xj
  $ cd 2019/lynn
------------------------------------------------------------------------

== Thanks! ==

Many thanks to http://ioccc.org/judges.html[the contest organizers] for
volunteering so much time and energy. I've followed this incredible
competition for years, and thrilled to finally participate.

== Spoilers ==

I implemented a variant of link:ION.html[the ION machine]
along wth a variant of link:grind.html[the "Parity" compiler].
Rather than link:asm.html[ION assembly], it emits the values that should
be loaded directly into memory.

Instead of allowing up to 128 combinators and using ASCII codes to make
combinator indices friendlier, the combinators are numbered consecutively
starting from 0 so the main loop could use a jump table, and there are
exactly 24 of them (so the heap starts at 24), but not all are in the jump
table.

An arithmetic operator, say `(+)`, compiles to the index of the combinator
`(+)` plus 9, which is past the end of the jump table. The code detects this
and acts as if it encountered `Q(+)`, where the `Q = B(BT)T`, that is, it
reduces the term `Q(+)xy` to `y(x(+))`. This ad hoc scheme beats having an
explicit `Q` combinator because it is less clear and it saves room.
[I chose "Q" in honour of one of the combinators Smullyan writes with the help
of the letter Q.]

Originally, I planned to represent the compiler as link:parse.html[ION
assembly] in a C string. But soon it was obvious I needed more drastic
measures. The prevalence of applications and certain combinators suggested
entropy encoding. I chickened out of arithmetic coding and took refuge with
Huffman.

The rules hint that high bits in the source are risky, so I represented the
encoded output in base 85, using 5 bytes to represent a 32-bit word:

\begin{code}
base85 :: Int -> String
base85 n = take 5 $ toEnum . (32+) . (`mod` 85) <$> iterate (`div` 85) n
\end{code}

Everything else in the program, that is, anything but an application or a
popular combinator, wound up in another buffer. String constants were stored
verbatim, while other values were encoded as a space-terminated mixed-radix
number, where the digits were carefully chosen to be invisible to `iocccsize`
and also to be legal in C string literals.

\begin{code}
spaceOut :: Int -> [Char]
spaceOut n = spaceOut' n True where
  spaceOut' n d = if n == 0 then "" else
    ((if d then "{\v}\f;\t" else "\v\f\t")!!b) : spaceOut' q (not d)
    where (q, b) = divMod n (if d then 6 else 3)
\end{code}

Partitioning the code into two buffers meant the decoder had to frequently
switch between them, aiding obfuscation.

Unlike ION assembly, references to previous definitions are relative to the
definition in which they appear: the number `n` refers to the term that
appeared `n` definitions ago. I hoped judicious ordering of definitions
would result in smaller numbers and hence smaller encodings.

On the Haskell side, I broke functions into small pieces and experimented with
inlining and rewrite rules to reduce code size.

The order that a C function's arguments are evaluated depends on the
compiler, and hence so does the behaviour of my program. But on closer
inspection, only heap allocation is affected. Whether one application cell gets
allocated before another is of no consequence, so the code works either way.

By abuse of C block comments, concatenating the output of the compiler with the
source of the compiler initializes the heap with ION machine code of the given
program. It also initializes the first memory location to the address of the
bottom of the heap, which must be at least 24, and the second to the entry
point of the program.

The `main()` function checks if the first memory location is zero. If so, it
decodes the compiler into the heap. Otherwise, it sets the heap pointer and
entry point. This check also determines whether to print a certain footer on
program termination.

Instead of `T1`, the equivalent of link:parse.html[`wrapIO`] uses what we might
call `Q1I`. Since `Q1Iht` reduces to `I(h1)t`, this achieves the same effect
while being slightly more confusing.

The top memory cells are initially zero, and the stack begins slightly below
the top. Our version of the I combinator takes two arguments, lazily reducing
`Ixy` to `xy`, which overwrites the application cell at `sp[2]`.

Then what if `sp` is near the top and we reduce an I combinator? The details
are tricky, which is perfect for the competition, but less so when trying to
remember how it works. I believe in such cases, since the top of memory
contains zeroes, it overwrites the contents of the first two cells, which is
harmless because the heap starts from 24.

== The Ideas of March ==

Shortly after the contest deadline, I found I could easily shrink the code
further. Firstly, there were many expressions of the form:

------------------------------------------------------------------------
f <$> x <*> y
------------------------------------------------------------------------

I should have defined `liftA2` to save room.

A different mixed-radix encoding would still be invisible to
`iocccsize` but takes less room. The corresponding decoder needs more C, but
the savings are worth it:

\begin{code}
spaceOutDeluxe :: Int -> [Char]
spaceOutDeluxe n = zeros !! (2 * length cs) : cs where
  cs = spaceOut' n True
  spaceOut' n d = if n == 0 then "" else
    (zeros!!(b*(if d then 1 else 2))) : spaceOut' q (not d)
    where (q, b) = divMod n (if d then 7 else 4)
  zeros = "\t{ }\v;\f"
\end{code}

It makes me wonder if I could have achieved my original aim of squeezing in
link:type.html[a compiler with type checking].

== Related work ==

Compilers and interpreters (for C, Basic, Lisp, Forth, APL, 6502(!), 8080(!!),
PDP-7/11(!!!), ...) are rife among
https://www.ioccc.org/years-spoiler.html[previous IOCCC winners].

One previous winner is built around a recursion using the Y combinator, which
is also how our compiler supports recursive definitions.

One previous winner even translates lambda calculus to combinatory logic, but
luckily for me, was written before Kiselyov published his algorithm.
