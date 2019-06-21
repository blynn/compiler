= An award-winning compiler =

link:.[I may have failed that stupid Compilers exam] but at least I can boast
of authoring an award-winning compiler!

(Even if the award is from the https://www.ioccc.org/2019/whowon.html[26th
International Obfuscated C Code Contest].)

== Spoilers ==

I implemented a variant of link:ION.html[the ION machine]
along wth a variant of link:grind.html[the "Parity" compiler].

Instead of allowing up to 128 combinators and using ASCII codes to make
combinator indices friendlier, the combinators are numbered consecutively
starting from 0 so the main loop could use a jump table, and there are
exactly 24 of them (so the heap starts at 24), but not all are in the jump
table.

An arithmetic operator, say `(+)`, compiles to the index of the combinator
`(+)` plus 9, which is past the end of the jump table. The code detects this
and acts as if it encountered `Q(+)`, where the `Q = B(BT)T`, that is, it
reduces the term `Q(+)xy` to `y(x(+))`. This ad hoc scheme beats having an
explicit `Q` combinator because it is more confusing and it saves room.

(The choice of `Q` stems from one of the combinators that Smullyan writes with
the help of the letter Q.)

Originally, I thought I could represent the compiler as link:parse.html[ION
assembly] in a C string. But I quickly realized more drastic measures were
required.

The prevalence of applications and certain combinators suggested entropy
encoding. I chickened out of arithmetic coding and took refuge with Huffman
coding.

The rules hint that high bits in the source are risky, so I represented the
encoded output in base 85, using 5 bytes to represent a 32-bit word:

\begin{code}
base85 :: Int -> String
base85 n = take 5 $ toEnum . (32+) . (`mod` 85) <$> iterate (`div` 85) n
\end{code}

Everything else in the program (anything but an application or a popular
combinator) wound up as a space-terminated mixed-radix number in another
buffer, where the digits were carefully chosen to be invisible to `iocccsize`
and also to be legal in C string constants.

\begin{code}
spaceOut :: Int -> [Char]
spaceOut n = spaceOut' n True where
  spaceOut' n d = if n == 0 then "" else
    ((if d then "{\v}\f;\t" else "\v\f\t")!!b) : spaceOut' q (not d)
    where (q, b) = divMod n (if d then 6 else 3)
\end{code}

Partitioning the code into two buffers as above meant the decoder had to
frequently switch between them, aiding obfuscation.

Unlike ION assembly, references to previous definitions are relative to the
definition in which they appear: the number `n` refers to the term that
appeared `n` definitions ago. I hoped judicious ordering of the definitions
would result in smaller numbers and hence smaller encodings.

On the Haskell side, I broke functions into small pieces and experimented with
inlining and rewrite rules to reduce code size.

The order that a C function's arguments are evaluated depends on the
compiler, and hence so does the behaviour of my program. But on closer
inspection, the part that is affected is the heap allocator. Whether one
application cell gets allocated before another is of no consequence, so the
code works either way.

By abuse of C block comments, concatenating the output of the compiler with
source of the compiler initializes the heap with ION machine code of the input
program. It also initializes the first memory location to the address of the
bottom of the heap, and the second to the entry point of the program.

The `main()` function checks if the first memory location is zero. If so, it
decodes the compiler into the heap. Otherwise, it sets the heap pointer and
entry point. This check also decides whether to print a certain footer on
program termination.

Instead of `T1`, the equivalent of link:parse.html[`wrapIO`] uses what we might
call `Q1I`. Since `Q1Iht` reduces to `I(h1)t`, this achieves the same effect
while being slightly more confusing.

The top memory cells are initially zero, and the stack begins slightly below
the top. The I combinator only takes one argument, but because of how
applications are laid out on the ION machine, reducing the I combinator causes
the application cell at `sp[2]` to be rewritten.

This means we must take care if `sp` is near the top and we reduce an I
combinator. The details are confusing, which is perfect for the competition,
but now I'm unsure if I'm remembering correctly! I believe in such cases, since
the top of memory contains zeroes, it overwrites the contents of the first two
cells, which is fine because the heap starts from 24.

== Epilogue ==

Shortly after the contest deadline, I found I could easily shrink the code
further. Firstly, there were many expressions of the form:

------------------------------------------------------------------------
f <$> x <*> y
------------------------------------------------------------------------

I should have defined `liftA2` so they could refer to one previous definition
rather than two operators.

Secondly, a different mixed-radix encoding would still be invisible to
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

It makes me wonder if I could squeeze in type checking, perhaps after removing
support for some kinds of syntax sugar. This would be quite a coup, since
it seems many previous IOCCC interpreters or compilers skimp on type checking.

== Thanks! ==

Many thanks to http://ioccc.org/judges.html[the contest organizers] for putting
in so much volunteer time and energy into such a fun competition.
I've followed it for years, and thrilled to finally participate.
