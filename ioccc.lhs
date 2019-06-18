= An award-winning compiler =

For the https://www.ioccc.org/2019/whowon.html[26th International Obfuscated C
Code Contest], I designed and simulated something like link:ION.html[the ION
machine] in C and wrote a compiler much link:grind.html[our "Parity" compiler].

The encoding of the program is tailored to the way size limits are computed.
One buffer holds applications and the basic combinators, Huffman-coded, then
base-85 encoded (with 5 bytes representing 4) because the contest rules
suggested high bits in the source are risky.

\begin{code}
base85 :: Int -> String
base85 n = take 5 $ toEnum . (32+) . (`mod` 85) <$> iterate (`div` 85) n
\end{code}

The other buffer holds various space-terminated integer constants encoded in
mixed radix where the digits are chosen so they can legally appear in C string
constants and appear invisible to `iocccsize`:

\begin{code}
spaceOut :: Int -> [Char]
spaceOut n = spaceOut' n True where
  spaceOut' n d = if n == 0 then "" else
    ((if d then "{\v}\f;\t" else "\v\f\t")!!b) : spaceOut' q (not d)
    where (q, b) = divMod n (if d then 6 else 3)
\end{code}

Unlike our description of ION assembly, references to previous definitions are
relative to the definition in which they appear. That is, if `n` is the ASCII
code of the index, and `m` is the index of the term being parsed, then the
index `n` refers to the term that appeared `m - n` definitions ago. The hope
was that with judicious ordering of the definitions, we wind up with smaller
indexes and hence smaller encodings.

I broke functions into small pieces and inlined certain functions to reduce
code size.

Shortly after the contest deadline, I found I could easily shrink the code
further. Firstly, there were many expressions of the form:

------------------------------------------------------------------------------
f <$> x <*> y
------------------------------------------------------------------------------

I should have defined `liftA2` so such terms refer to one previous definition
rather than two operators.

Secondly, a different mixed-radix encoding would still be invisible to
`iocccsize` but saved even more room. The corresponding decoder is takes a bit
more C, but the savings are worth it:

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
