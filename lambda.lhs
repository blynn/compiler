= The ultimate compiler =

We previously defined a programming language with the help of Turing machines:

------------------------------------------------------------------------
interpret :: Language -> TM
------------------------------------------------------------------------

We chose poorly.

In the classes I took, any exercises asking for a Turing machine invariably
involved pitifully trivial problems, such as copying some number of 1s.

Later, they introduced Universal Turing Machines, but without actually showing
us one because it was too much trouble. With universal machines, they explained
why it's impossible to decide if a given Turing machine ever halts.

So from day one, they warned us Turing machines are clumsy beasts where easy
tasks are hard to accomplish, and generally hang unpredictably.

== Lambda Calculus ==

Everyone knows lambda abstractions nowadays. C++ got them. JavaScript got
them. Even everything-is-an-object Java got them.

Less well-known is that link:../lambda/[lambdas alone are equal in
power to Turing machines]. We can toss out states, tapes, read/write heads, and
do nothing but repeatedly substitute a variable with an expression, yet still
compute just as effectively. If `LC` denotes the set of all closed lambda
terms, then the following mutually inverse functions are known to exist:

------------------------------------------------------------------------
fromTM :: TM -> LC
fromLC :: LC -> TM
------------------------------------------------------------------------

We're taking some artistic license with the word "inverse" as a round trip may
take us to a different program. The point is it'll behave identically to the
original.

Lambdas are easy for humans to understand. Practicians love them so much that
popular languages eventually support them. Theoreticians define programming
languages with lambda calculus. In fact, link:socrates.html[our implementation
of Turing Machines] is written in syntactically sweetened lambda calculus so is
close to a candidate for `fromTM`.

Lambdas are easy for computers to understand. Compiler textbooks describe
transforming source code into
https://www.cs.princeton.edu/~appel/papers/ssafun.pdf["SSA form", another name
for lambda calculus], so it can be readily analyzed and manipulated.

Thus one way or another, a source language is defined in terms of lambda
calculus. Accordingly, we revise our definition of a programming language to be
a function taking a program to a closed lambda calculus term:

------------------------------------------------------------------------
interpret :: Language -> LC
------------------------------------------------------------------------

Naturally, composing with `fromLC` takes us back to Turing machines,
but let's not go there. 'Tis a silly place.

== What is lambda calculus? ==

Lambda calculus terms can be represented by trees of the form:

------------------------------------------------------------------------
type VarId = String
data LC = Var VarId | Lam VarId LC | LC :@ LC
------------------------------------------------------------------------

A program is represented by a 'closed' lambda term, which means every `Var`
node must be a descendant of a `Lam` node with a matching `VarId`.

As for its semantics: we define a closed lambda term to be notation for a
combinatory logic term, which we define to be a full binary tree whose leaves
can be one of 6 different values:

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
<p><a onclick='hideshow("ugh");'>&#9654; Toggle extensions and imports</a></p>
<div id='ugh' style='display:none'>
++++++++++

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State
\end{code}

++++++++++
</div>
++++++++++

\begin{code}
data Com = S | K | I | B | C | T
\end{code}

We actually only need S and K; the others are basically useful macros.

\begin{code}
data CL = Lf Com | CL :# CL | Ext String
\end{code}

The `Ext String` field comes into play later, when we want to mix external
functions with our combinators.

The `rewrite` function below rewrites a closed LC term as a CL term, using an
algorithm known as 'bracket abstraction'. See Smullyan's "To Mock a
Mockingbird" for a particularly enjoyable explanation.

\begin{code}
type VarId = String
data LC = Var VarId | Lam VarId LC | LC :@ LC | Other Com

deLam :: LC -> LC
deLam t = case t of
  Lam v u -> case deLam u of
    Var w | v == w    -> Other I
          | otherwise -> Other K :@ Var w
    Other c           -> Other K :@ Other c
    x :@ y            -> Other S :@ deLam (Lam v x) :@ deLam (Lam v y)
  x :@ y  -> deLam x :@ deLam y
  _       -> t

rewrite :: LC -> CL
rewrite t = case deLam t of
  Other c -> Lf c
  x :@ y  -> rewrite x :# rewrite y
\end{code}

We tweaked our earlier definition of `LC` so they are combinator-aware; during
bracket abstraction, we produce intermediate Frankenstein terms that are an
amalgam of CL and LC terms.

== What is combinatory logic? ==

In a combinatory logic term, subterms matching certain patterns 'reduce' to
other subterms:

\begin{code}
reduce :: CL -> Maybe CL
reduce t = case t of
  Lf I :# x           -> Just x
  Lf K :# x :# y      -> Just x
  Lf T :# x :# y      -> Just (y :# x)
  Lf S :# x :# y :# z -> Just (x :# z :# (y :# z))
  Lf B :# x :# y :# z -> Just (x      :# (y :# z))
  Lf C :# x :# y :# z -> Just (x :# z :# (y     ))
  _                   -> Nothing
\end{code}

The S combinator duplicates one of its arguments. Although we think of the
result as a tree, in our implementation, we wind up with two nodes pointing to
the same copy of the argument that is duplicated, that is, we employ 'sharing'
to conserve memory. The S combinator also means a tree need not shrink after a
so-called reduction.

If none of the patterns appear, then no reductions are possible and the term
is said to be in 'normal form'. Otherwise there are one or more subterms that
can be reduced, and we must choose which to reduce.

One choice is to reduce them in 'normal order': repeatedly reduce the leftmost
subtree that can be reduced.

If a term can be reduced to a normal form (which is in some sense unique by
https://plato.stanford.edu/entries/logic-combinatory/#ChurTheoConsTheo[Church-Rosser]),
then this strategy will find it. Other evaluation orders might never terminate
even when a normal form exists, which correspond to Turing machines that never
halt.

The left 'spine' of the tree is the path that starts from the root node and
recursively follows the left child. To evaluate in normal order,
we walk down the left spine until we bottom out, then reduce as we walk back
up to the root; on each reduction, we must walk back down again in case the
replacement subtree can be reduced. Afterwards, we walk down the left spine
again to the bottom, and this time as we walk back up again, we recursively
normalize the right branches.

\begin{code}
normalize :: CL -> CL
normalize t = down t [] up1 where
  down t args k = case t of
    x :# y -> down x (y:args) k
    _ -> k t args

  up1 t args = case reduce t of
    Just t' -> down t' args up1
    Nothing -> case args of
      [] -> down t [] up2
      a:as -> up1 (t :# a) as

  up2 t args = case args of
    [] -> t
    a:as -> up2 (t :# normalize a) as
\end{code}

For our computations, it turns out we only need our terms to reach 'weak head
normal form', which means we skip the second trip.

\begin{code}
run :: CL -> CL
run t = down t [] where
  down t args = case t of
    x :# y -> down x (y:args)
    _ -> up t args

  up t args = case reduce t of
    Just t' -> down t' args
    Nothing -> case args of
      [] -> t
      a:as -> up (t :# a) as
\end{code}

== Input and output ==

Turing machines have a tape to handle input and output. With combinatory
logic, the program is a term, the input string is encoded as a term, and the
output string is the decoding of the program term applied to the input term.

We use
link:scott.html[the 'Scott encoding']. Strings are Scott-encoded lists of
Scott-encoded Peano numbers.

Encoding and decoding requires us to evaluate combinators alongside our own
functions, hence the `runM` function and the state monad.

\begin{code}
runM :: Monad m => (CL -> m (Maybe CL)) -> CL -> m CL
runM f t = down t [] where
  down t args = case t of
    x :# y -> down x (y:args)
    _ -> up t args

  up t args = do
    m <- f t
    case m of
      Just t' -> down t' args
      Nothing -> case args of
        [] -> pure t
        a:as -> up (t :# a) as

encodeChar :: Char -> CL
encodeChar c = go (fromEnum c) where
  go 0 = Lf K
  go n = Lf K :# (Lf T :# go (n - 1))

decodeChar :: CL -> Char
decodeChar t = toEnum $ execState (runM red $ t :# Ext "Z" :# Ext "S") 0 where
  red t = case t of
    Ext "S" :# a -> do
      modify (1+)
      pure $ Just $ a :# Ext "Z" :# Ext "S"
    _ -> pure $ reduce t

encode :: [Char] -> CL
encode "" = Lf K
encode (c:cs) = Lf K :# (Lf C :# (Lf T :# encodeChar c) :# encode cs)

decode :: CL -> [Char]
decode t = execState (runM red $ t :# Ext "nil" :# Ext "cons") id "" where
  red t = case t of
    Ext "cons" :# x :# xs -> do
      modify (. (decodeChar x:))
      pure $ Just $ xs :# Ext "nil" :# Ext "cons"
    _ -> pure $ reduce t
\end{code}

== One size fits all ==

We can now run a combinatory logic program:

\begin{code}
runCom :: CL -> String -> String
runCom t s = decode (run (t :# encode s))
\end{code}

If we believe it is easy to rewrite `runCom` and `reduce` in the target
language, then we can whip up the following function in no time:

------------------------------------------------------------------------
gen :: CL -> TargetLanguage
------------------------------------------------------------------------

Thus we have a compiler which works on lambda calculus:

------------------------------------------------------------------------
ultimateCompiler :: LC -> TargetLanguage
ultimateCompiler = gen . rewrite
------------------------------------------------------------------------

By our definitions, this implies we can compile any given language by
composing the above function with the definitional interpreter
of the language:

------------------------------------------------------------------------
compile :: Language -> TargetLanguage
compile = ultimateCompiler . interpret
------------------------------------------------------------------------

Mission accomplished?

== Well-defined ==

Can all programming languages be defined with our version of lambda calculus?
After all, we chose a particular order of evaluation.

https://homepages.inf.ed.ac.uk/wadler/papers/papers-we-love/reynolds-definitional-interpreters-1998.pdf[Reynolds'
landmark paper of 1972] surveys multiple languages defined in multiple variants
of lambda calculus, then reveals this variety is unnecessary. Thanks to
'defunctionalization' and 'continuation-passing style' (CPS), we can get by
with a lambda calculus without first-class functions (is this the same as
https://en.wikipedia.org/wiki/Kappa_calculus[kappa calculus]?), and which
evaluates in any given order.

A few years later, https://en.wikisource.org/wiki/Lambda_Papers[Steele and
Sussman wrote the famous lambda papers]: a series of cookbooks describing how
to define many practical programming constructs in lambda calculus.

http://www.cs.nott.ac.uk/~pszgmh/fifty.pdf[Hutton and Bahr 'calculate' a
correct compiler from its specification], showing the power of precise
definitions. They have also http://www.cs.nott.ac.uk/~pszgmh/cutting.pdf[fused
CPS transformation and defunctionalization] into a single step.

http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf[Elliott's
work on compiling to categories] uses bracket abstraction to yield a compelling
alternative to domain-specific languages.

http://adam.chlipala.net/papers/CtpcPLDI07/CtpcPLDI07.pdf[Chlipala wrote
'A Certified Type-Preserving Compiler from Lambda Calculus to Assembly
Language'], which we take as a license to be informal when we please.
We can always fall back to this paper to see how it's really done!
