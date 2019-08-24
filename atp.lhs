= Automated Theorem Proving =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src='atp.js'></script>
<p>
<button id='swap'>swap</button>
<button id='dup'>dup</button>
<button id='implies'>implies</button>
<button id='demorgan'>De Morgan</button>
<button id='lem'>LEM</button>
<button id='curry'>curry</button>
<button id='uncurry'>uncurry</button>
<button id='jonk'>jonk</button>
</p>
<p>
<textarea id='in' rows='1' style='box-sizing:border-box;width:100%;'></textarea>
</p>
<p>
<button id='go'>Prove</button>
</p>
<p>
<textarea id='out' rows='4' style='box-sizing:border-box;width:100%;'></textarea>
</p>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Prove that for any propositions P and Q:

\[
(\neg P \vee Q) \rightarrow (P \rightarrow Q)
\]

Easy, right? Just enumerate all possibilities $P = 0, 1$ and $Q = 0, 1$, and
check we always get $1$.

------------------------------------------------------------------------
easy = and $ do
  p <- [False, True]
  q <- [False, True]]
  pure $ (not p || q) `implies` (p `implies` q)

implies False True = True
implies True True = True
implies True False = False
implies False False = True
------------------------------------------------------------------------

Perhaps the toughest part is remembering the truth table for $(\rightarrow)$.
It's odd that $0 \rightarrow 1$ holds, that is, "false implies true". But it's
odder still that we say "implies" for this symbol. Should we be saying things
like: "a hydrogen atom contains two protons implies Beethoven wrote nine
symphonies"?

It sounds illogical because in everyday use, the word "implies" implies
causation. How can a proposition possibly "imply" an unrelated proposition? Yet
discarding the word "implies" is out of the question, because logic ought to be
able to deal with causality.

We patch this discrepancy by replacing 0 and 1 with mathematical objects called
'proofs'. We represent a proof with an abstract syntax tree (that turns out
be a good old lambda term). Then a proof of $P \rightarrow Q$ is a syntax tree
representing a function that takes a proof of $P$ and returns a proof of $Q$.
The word "implies" has regained its dignity: we can only prove $P \rightarrow
Q$ if we show how any proof of $P$ leads to a proof of $Q$.

Instead of truth tables, we build proofs (syntax trees) from other proofs. A
proof of the conjunction $P \wedge Q$ is a syntax tree representing the pair
`(p, q)` where `p` is a proof of $P$ and `q` is a proof of $Q$. A proof of the
disjunction of $P$ and $Q$ is either `Left p` where `p` is a proof of $P$ or
`Right q` where `q` is a proof of $Q$.

As for negation, we define a proposition $\bot$ and stipulate that a proof
of $\bot$ immediately yields a proof of any arbitrary proposition;
https://en.wikipedia.org/wiki/Principle_of_explosion[ex falso quodlibet]. We
define $\neg P$ to be $P \rightarrow \bot$.

Apart from fixing "implies", our logic is also 'intuitionistic', which just
means https://web.math.princeton.edu/~nelson/papers/rome.pdf[we've added
accuracy to classical logic]. Roughly speaking, all the theorems are the same
except that rather than prove $\exists x P(x)$, we sometimes prove $\neg
\forall x \neg P(x)$, and similarly, rather than prove $A \vee B$ we sometimes
prove $\neg(\neg A \wedge \neg B)$.

Classical logic equates these formulas, while intuitionistic logic keeps them
apart to gain one extra bit of information. This bit signals whether the proof
is 'constructive'.

For example, in intuitionistic logic, the law of the excluded middle (LEM) when
written as $A \vee \neg A$ has no proof, but the classically equivalent $\neg (A
\wedge \neg A)$ does. A proof of the former would be a decision procedure, that
is, it would describe an algorithm to construct a proof of $A$ or a proof of
$\neg A$ from any given $A$; a tall order. The latter merely states it is
impossible to have both a proof of $A$ and a proof of $\neg A$. [You can't have
your cake and not have your cake.]

Addressing philosophical concerns is fun, but we really went to all this
trouble for practical reasons.
If we build a system that can generate a constructive proof of  a given
proposition, then we can automatically generate a function of a given type,
a fact known as the Curry-Howard correspondence.

== Too much too early ==

Alas, our original proof strategy is wildly inappropriate. Recall we simply
tried two different values for each proposition. Unfortunately, there are
infinitely many abstract syntax trees.

Even if we could magically try each one, what good would it do? Suppose we wish
to prove $P \rightarrow P$. Our goal is to find a function that takes a proof
of $P$ and returns a proof of $P$. The identity function clearly does the job,
but how would enumerating all possible proofs of $P$ lead to it?

Instead of enumerating all trees for each proposition, we could try enumerating
them to find the proof: we type-check each tree and see if it matches the
given proposition. However, this is only tolerable for the tiniest of proofs.
Also this procedure never terminates when no proof exists.

Chapter 4 of
https://www.cs.cmu.edu/~fp/courses/atp/handouts/atp.pdf[Frank Pfenning's
notes] describes a strategy based on 'sequents'. Summarizing, and perhaps
oversimplifying, a sequent consists of:

  * a list of 'passive' propositions $P_1, ..., P_m$
  * a list of 'active' propositions $A_1, ..., A_n$
  * a 'succedent' proposition $B$

We write:

\[ P_1, ..., P_m ; A_1, ..., A_n \vdash B \]

We interpret this sequent to mean "given proofs of all the propositions in both
lists, we can produce a proof of $B$".

Given a proposition $B$ to prove, our strategy is to start with a sequent
meaning that we can prove $B$ with no assumptions:

\[
\vdash B
\]

then apply a logic rule to transform it into one or more sequents that imply
it. We iterate on each of these new sequents until we reach 'initial sequents',
that is, sequents of the form:

\[
..., P, ...; \vdash P
\]

which means given a proof of $P$ (and possibly other proofs), we can produce a
proof of $P$.

Some logic rules are 'invertible': they preserve completeness, that is, they
never change a viable sequent into sequents that eventually get stuck. Other
rules are not. This suggests the following algorithm:

  1. Apply as many invertible rules as possible to the succedent. This may
  add propositions to the active list, but never to the passive list.
  Pfenning calls these cases 'right asynchronous'.

  2. Until the active list is empty, apply as many invertible rules as possible
  to the head of the list. If no invertible rules apply, then move the head to
  the passive list. Pfenning calls these cases 'left asynchronous'.

  3. At this point, any remaining rules are non-invertible. The search branches
  on each such rule. We can use standard strategies such as breadth-first
  search, depth-first search, or iterative deepening. A non-invertible rule
  leads to an initial sequent or takes us to one of the previous two steps.

Since we're restricting ourselves to propositional logic (or equivalently,
we have implicit universal quantifiers at the beginning of the proposition for
each free variable), I believe we can get away with deleting a proposition $P
\rightarrow Q$ from the passive list when exploring the branch it represents.
This guarantees termination.

The `oracle` function below determines if a proof exists for a given proposition
using the above algorithm.

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
<p><a onclick='hideshow("boilerplate");'>&#9654; Toggle boilerplate</a></p>
<div id='boilerplate' style='display:none'>
++++++++++

\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative ((<**>))
import Control.Arrow
import Data.List
#ifdef __HASTE__
import Control.Monad
import Haste.DOM
import Haste.Events
import Text.Parsec hiding (space)
type Parser = Parsec String ()
lowerChar = lower; alphaNumChar = alphaNum; space = spaces
(<>) = (++)
#else
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char
type Parser = Parsec () String
#endif
\end{code}

++++++++++
</div>
++++++++++

\begin{code}
data Prop = Var String
  | Prop :& Prop
  | Prop :| Prop
  | Prop :-> Prop
  | Falso deriving (Show, Eq)

oracle :: Prop -> Bool
oracle = rAsyn [] [] where
  rAsyn psv act prop = case prop of
    a :& b -> rAsyn psv act a && rAsyn psv act b
    a :-> b -> rAsyn psv (a:act) b
    _ -> search psv act prop

  search psv (x:xs) rhs = case x of
    Falso -> True
    a :& b -> search psv (a:b:xs) rhs
    a :| b -> search psv (a:xs) rhs && search psv (b:xs) rhs
    _ -> search (x:psv) xs rhs

  search psv [] prop = elem prop psv || any implyCases psv || orCases
    where
    orCases = case prop of
      a :| b -> rAsyn psv [] a || rAsyn psv [] b
      _ -> False
    implyCases p = case p of
      a :-> b -> search psv' [b] prop && rAsyn psv' [] a
        where psv' = delete p psv
      _ -> False
\end{code}

We write a parser for propositions so we can easily test our code.
We use Haskell notation:

  * The conjunction of `p` and `q` is written `(p, q)`.
  * The disjunction of `p` and `q` is written `Either p q`.
  * Implication is `->` and associates right.
  * The proposition $\bot$ is written `Void` (from the `Data.Void` package).

\begin{code}
propo :: Parser Prop
propo = foldr1 (:->) <$> sepBy1 arg (spstr "->") where
  arg = Var <$> sp ((:) <$> lowerChar <*> many alphaNumChar)
    <|> between (spch '(') (spch ')') prodOrProp
    <|> const Falso <$> spstr "Void"
    <|> spstr "Either" *> ((:|) <$> arg <*> arg)
  prodOrProp = propo <**> (option id $ spch ',' *> (flip (:&) <$> propo))
  sp = (<* space)
  spch = sp . char
  spstr = sp . string
\end{code}

Success! We find our first example has a proof:

------------------------------------------------------------------------
oracle <$> parse propo "" "Either (p -> Void) q -> p -> q"
------------------------------------------------------------------------

== Proof by construction ==

Our function has discovered a truly remarkable proof of our theorem, but the
Boolean return value is too small to contain it.

We remedy this by building lamba terms as we go, instead of merely returning
whether it's possible to build a term for a given sequent. This requires
assigning names to various propositions when we first encounter them; we pass
around an integer to help generate unique names. Each proposition in the active
and passive lists is associated with a lambda term representing its proof. We
build larger lambda terms out of smaller ones using standard Haskell functions.

We also take this opportunity to switch to the algorithm from Section 4.3 of
Pfenning's notes: a generalization of Prolog's search strategy. Again, since
we've restricted ourselves to propositional logic, we can get away with
removing an implication from the passive list when branching on it.

Below, the `prove` function performs a depth-first search to find programs
of a given type.

\begin{code}
data Proof = V String | Lam String Proof | Proof :@ Proof

prove :: Prop -> [Proof]
prove prop = snd <$> rAsyn 0 [] [] prop where
  rAsyn n psv lhs prop = case prop of
    a :-> b -> do
      let s = 'a':show n
      second (Lam s) <$> rAsyn (n + 1) psv ((V s, a):lhs) b
    a :& b -> do
      (n1, f) <- rAsyn n psv lhs a
      (n2, s) <- rAsyn n1 psv lhs b
      pure (n2, V "," :@ f :@ s)
    _ -> lAsyn n psv lhs prop
  lAsyn n psv [] rhs = concat $ lguesses <> rguesses where
    lguesses = [lSyn n (filter (\p -> snd p /= snd l) psv) l rhs | l <- psv]
    rguesses = case rhs of
      _ :| _ -> [rSyn n psv rhs]
      _ -> []
  lAsyn n psv (pp@(prf, prop):ps) rhs = case prop of
    Falso  -> pure (n, V "absurd" :@ prf)
    a :& b -> lAsyn n psv ((V "fst" :@ prf, a):(V "snd" :@ prf, b):ps) rhs
    a :| b -> do
      let l = 'a':show n
      (n1, lp) <- lAsyn (n + 1) psv ((V l, a):ps) rhs
      let r = 'a':show n1
      (n2, rp) <- lAsyn (n1 + 1) psv ((V r, b):ps) rhs
      pure (n2, V "either" :@ Lam l lp :@ Lam r rp :@ prf)
    _ -> lAsyn n (pp:psv) ps rhs
  rSyn n psv rhs = case rhs of
    a :| b -> as <> bs where
      as = second (V "Left" :@) <$> rSyn n psv a
      bs = second (V "Right" :@) <$> rSyn n psv b
    _ -> rAsyn n psv [] rhs
  lSyn n psv (prf, l) rhs = if l == rhs then pure (n, prf) else case l of
    a :-> b -> do
      let v = 'x':show n
      (n1, f) <- lSyn (n + 1) psv (prf :@ V v, b) rhs
      (n2, arg) <- rSyn n1 psv a
      pure (n2, sub v arg f)
    a :& b -> as <> bs where
      as = lSyn n psv (V "Left" :@ prf, a) rhs
      bs = lSyn n psv (V "Right" :@ prf, b) rhs
    _ :| _ -> lAsyn n psv [(prf, l)] rhs
    Falso -> lAsyn n psv [(prf, l)] rhs
    _ -> []

sub v arg f = case f of
  V s | v == s -> arg
  Lam s t -> Lam s $ rec t
  x :@ y -> rec x :@ rec y
  _ -> f
  where rec = sub v arg
\end{code}

We prettyprint the proofs so they look like Haskell programs.

\begin{code}
instance Show Proof where
  show prf = case prf of
    V s             -> s
    V "," :@ x :@ y -> "(" <> show x <> ", " <> show y <> ")"
    x :@ y          -> "(" <> show x <> " " <> show y <> ")"
    Lam s t         -> "\\" <> s <> " -> " <> show t
\end{code}

Then:

------------------------------------------------------------------------
prove <$> parse propo "" "Either (p -> Void) q -> p -> q"
------------------------------------------------------------------------

gives the proof:

------------------------------------------------------------------------
\a0 -> \a1 -> (((either \a2 -> (absurd (a2 a1))) \a3 -> a3) a0)
------------------------------------------------------------------------

++++++++++
<p><a onclick='hideshow("ui");'>&#9654; Toggle UI code</a></p>
<div id='ui' style='display:none'>
++++++++++

\begin{code}
#ifdef __HASTE__
main :: IO ()
main = withElems ["in", "out", "go"] $ \[iEl, oEl, goB] -> do
  let
    setup button text = do
      Just b <- elemById button
      let
        act = do
          setProp iEl "value" text
          setProp oEl "value" ""
      void $ b `onEvent` Click $ const act
      when (button == "implies") act
    go = do
      s <- getProp iEl "value"
      let
        t = case parse propo "" s of
          Left _ -> "parse error"
          Right p -> case prove p of
            [] -> "no proof found"
            (h:_) -> show h
      setProp oEl "value" t
  setup "swap" "(a, b) -> (b, a)"
  setup "dup" "a -> (a, a)"
  setup "demorgan" "(a -> Void, b -> Void) -> Either a b -> Void"
  setup "implies" "Either (p -> Void) q -> p -> q"
  setup "lem" "(p, p -> Void) -> Void"
  setup "uncurry" "(a -> b -> c) -> (a, b) -> c"
  setup "curry" "((a, b) -> c) -> a -> b -> c"
  setup "jonk" "(a -> b) -> ((a -> i) -> i) -> ((b -> i) -> i)"
  void $ goB `onEvent` Click $ const $ go
  void $ iEl `onEvent` KeyDown $ \key -> when (key == mkKeyData 13)
    $ go >> preventDefault
#endif
\end{code}

++++++++++
</div>
++++++++++

== Q.E.D. ==

http://comcom.csail.mit.edu/comcom/#Synquid[Synquid uses refinement types] to
automatically write programs that sort, or transform a Boolean formula to
negation normal form (NNF). See also the other amazing provers on that website,
as well as https://www.youtube.com/watch?v=HnOix9TFy1A[Nadia Polikarpova]'s talk.

https://www.youtube.com/watch?reload=9&v=mOtKD7ml0NU[Idris 2 uses dependent
types and linear types] to discover a function for performing matrix
transposition.

https://reasonablypolymorphic.com/blog/typeholes/[Sandy Maguire uses GHC's type
holes to think less when programming]. But on his `jonk` example, the computer
can in fact take over completely. We type in:

------------------------------------------------------------------------
(a -> b) -> ((a -> i) -> i) -> ((b -> i) -> i)
------------------------------------------------------------------------

and our prover finds:

------------------------------------------------------------------------
\a0 -> \a1 -> \a2 -> (a1 \a4 -> (a2 (a0 a4)))
------------------------------------------------------------------------
