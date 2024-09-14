= The Compiler Singularity =

Our next compiler is self-hosting: it can read its 100% organic artisanal
handmade source and produce the corresponding ION assembly.

----------------------------------------------------------------
include::singularity[]
----------------------------------------------------------------

As the comment says, our parser now handles comments, whitespace, and variables
consisting of lowercase letters. We also added code to look up the index of a
top-level definition. Prefixing a character with `@` gives us direct access to
the primitive combinators. (Not to be confused with the `@` operator of ION
assembly.)

Our language is now friendly enough that we are willing to work in it
with our bare hands. However, bootstrapping to reach this singularity requires
manual labour. To obtain input acceptable to our previous compiler, we:

  * Remove spaces and newlines.
  * Remove comments.
  * Rename variables like `xs` and `tab` to single letters.
  * Strip the `@` tag from combinators like `@K`.
  * Rewrite `foo x y =` as `\x.\y.`
  * Rewrite `+\x y ->+` as `\x.\y.`
  * Replace defined symbols with `@` and a character indicating where they
  appeared: we refer to the nth definition with the character with ASCII code
  n + 31.

This can be done by hand, though it's probably best to use `sed` and `awk` to
generate a lookup table.

----------------------------------------------------------------
$ cat singularity | sed  -n '/.* =/{s/ .*//;p}' | awk '{printf "@%c",NR+31; print " " $0}'
@  or
@! lsteq
@" foldr
@# append
@$ pair
@% just
@& pure
@' sat
@( bind
@) ap
@* fmap
@+ alt
@, liftaa
@- many
@. some
@/ char
@0 liftki
@1 liftk
@2 com
@3 sp
@4 spc
@5 spch
@6 and
@7 var
@8 lcr
@9 lcv
@: lca
@; lcl
@< anyone
@= pre
@> lam
@? atom
@@ apps
@A expr
@B def
@C program
@D rank
@E show
@F isfree
@G unlam
@H babs
@I dump
@J main
----------------------------------------------------------------

This is a little brittle, as our regex assumes the an equals sign is preceded
by a space if and only if it's the equals sign of a definition.

However, while it's feasible to manually convert our code to lambda terms and
`@` references, this grows tiresome and error-prone if we often make changes
to our source. Thus we use `sed` and `awk` to automate these edits.

----------------------------------------------------------------
include::bootsingularity.sh[]
----------------------------------------------------------------

This comes at the expense of more brittleness and gotchas. For example:

  * We assume there are at most 5 variables in a lambda. One regex
  rewrites `+\a .. y z ->+` as `+\a .. y -> \z.+`, and we simply repeat this
  another 4 times, followed by a regex that removes empty lambda abstractions.

  * Due to lack of regex lookaround, we temporarily replace patterns like
  literal spaces and newlines with underscores, which will be restored after
  certain other patterns are removed. Thus we assume no underscores appear in
  the original source.

  * We need an extra regex to remove spaces after an escaped escape character.
  This assumes our source never contains an escaped space after an escaped
  escape character.

  * The ampersand has a special meaning in regexes that we must escape.

  * We've hardwired the variables that need renaming, along with replacements
  carefully chosen to avoid conflicts with the rest of the code. We could
  have written the source with single-letter variables, but then we'd miss
  an opportunity to show off and test new features.

Our previous compiler should accept the result:

----------------------------------------------------------------
include::singularity.boot[]
----------------------------------------------------------------
