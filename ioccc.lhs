= An award-winning compiler =

link:.[I may have failed that stupid Compilers exam] but at least I can
boast of single-handedly authoring an award-winning compiler!

(Even if the award is from the https://www.ioccc.org/2019/whowon.html[26th
International Obfuscated C Code Contest].)

== Spoilers ==

I implemented a variant of link:ION.html[the ION machine]
along wth a variant of link:grind.html[the "Parity" compiler].

The combinators were numbered consecutively starting from 0 so the main
loop could use a jump table.

An arithmetic operator, say `(+)`, compiles to a combinator whose index is
past the end of the jump table. The code detects this and applies the `Q`
reduction to turn `(+) x y` into `y(x plus)`, where `plus` is identical to our
ION machine's addition combinator. The combinator indices were chosen so that 
the pre-`Q` and post-`Q` versions differ by 9. This ad hoc scheme is
inherently hard to follow, which was perfect for the competition.

(We call the above `Q`, because it is similar to one of the combinators that
Smullyan writes with the help of the letter Q.)

Originally, I thought I could represent the compiler as link:parse.html[ION
assembly] in a C string. But I quickly realized more drastic measures were
required.

The prevalence of applications and certain combinators suggested arithmetic
coding or Huffman coding. I chickened out of trying arithmetic coding and ran
to the simpler Huffman coding.

The rules hint that high bits in the source are risky, so I represented the
encoded output in base 85, using 5 bytes to represent a 32-bit word:

\begin{code}
base85 :: Int -> String
base85 n = take 5 $ toEnum . (32+) . (`mod` 85) <$> iterate (`div` 85) n
\end{code}

Everything else in the program was encoded as a space-terminated mixed radix
number in another buffer, where the digits were carefully chosen to be
invisible to `iocccsize` and also to be legal in C string constants.

\begin{code}
spaceOut :: Int -> [Char]
spaceOut n = spaceOut' n True where
  spaceOut' n d = if n == 0 then "" else
    ((if d then "{\v}\f;\t" else "\v\f\t")!!b) : spaceOut' q (not d)
    where (q, b) = divMod n (if d then 6 else 3)
\end{code}

Partitioning the code into two buffers as above meant the decoder had to
frequently switch between them, aiding obfuscation!

Unlike ION assembly, references to previous definitions are relative to the
definition in which they appear. That is, the number `n` refers to the term
that appeared `n` definitions ago. I hoped judicious ordering of the
definitions would result in smaller indices and hence smaller encodings.

On the Haskell side, I broke functions into small pieces and experimented with
inlining to reduce code size.

The order that a C function's arguments are evaluated is
implementation-dependent, and hence so is the behaviour of part of my program.
But on closer inspection, the part that is affected is the heap allocator,
and whether one application cell gets allocated before another is unimportant,
so the code works either way.

== Epilogue ==

Shortly after the contest deadline, I found I could easily shrink the code
further. Firstly, there were many expressions of the form:

------------------------------------------------------------------------------
f <$> x <*> y
------------------------------------------------------------------------------

I should have defined `liftA2` so they could refer to one previous definition
rather than two operators.

Secondly, a different mixed-radix encoding would still be invisible to
`iocccsize` but saved even more room. The corresponding decoder needs more C,
but the savings are worth it:

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
it seems many previous IOCCC entries that are interpreters or compilers
skimp on type checking.

Many thanks to http://ioccc.org/judges.html[the contest organizers] for putting
in so much volunteer time and energy into such a fun competition!
