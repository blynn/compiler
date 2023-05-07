= Automated Theorem Proving =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<p>
<button id='swap'>swap</button>
<button id='implies'>implies</button>
<button id='demorgan'>De Morgan</button>
<button id='lem'>De Morgan LEM</button>
<button id='nnlem'>&not;&not;LEM</button>
<button id='vorobev'>Vorobev</button>
<button id='uncurry'>uncurry</button>
<button id='jonk'>jonk</button>
</p>
<p><textarea id='in' rows='1' style='box-sizing:border-box;width:100%;'></textarea></p>
<p><button id='go'>Prove</button></p>
<p id='mathy'></p>
<p><textarea id='out' rows='4' style='box-sizing:border-box;width:100%;'></textarea></p>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Prove that for any propositions P and Q:

\[
(\neg P \vee Q) \rightarrow (P \rightarrow Q)
\]

Easy, right? Just enumerate all possibilities \(P = 0, 1\) and \(Q = 0, 1\),
and check we always get \(1\).

------------------------------------------------------------------------
easy = all (== 1) [max (1 - p) q --> (p --> q) | p <- [0, 1], q <- [0, 1]]

0 --> 0 = 1
0 --> 1 = 1
1 --> 0 = 0
1 --> 1 = 1
------------------------------------------------------------------------

Perhaps the toughest part is remembering the truth table for
\((\rightarrow)\). It's odd that \(0 \rightarrow 1\) holds, that is, "false
implies true". But it's odder still that we say "implies" for this symbol.
Should we be saying things like: "a hydrogen atom contains two protons implies
Beethoven wrote nine symphonies"?

It sounds illogical because in everyday use, the word "implies" implies
causation. How can a proposition imply an unrelated proposition? Yet banishing
the word "implies" is out of the question, because logic ought to be able to
deal with causality.

We patch this discrepancy by replacing 0 and 1 with mathematical objects
called _proofs_. We represent a proof with an abstract syntax tree (that turns
out be a good old lambda term). Then a proof of \(P \rightarrow Q\) is a
syntax tree representing a function that maps a proof of \(P\) to a proof of
\(Q\). The word "implies" has regained its dignity: we can only say "\(P\)
implies \(Q\)" if we show how evidence of \(P\) leads to evidence of \(Q\).

Instead of truth tables, we build proofs (syntax trees) from other proofs. A
proof of the conjunction \(P \wedge Q\) is a syntax tree representing the pair
`(p, q)` where `p` is a proof of \(P\) and `q` is a proof of \(Q\). A proof of
the disjunction \(P \vee Q\) is either the syntax tree `Left p` where `p` is a
proof of \(P\) or `Right q` where `q` is a proof of \(Q\).

As for negation, we define a proposition \(\bot\) and stipulate that a proof
of \(\bot\) immediately yields a proof of any arbitrary proposition;
https://en.wikipedia.org/wiki/Principle_of_explosion[the principle of
explosion; ex falso quodlibet]. We define \(\neg P\) to be \(P \rightarrow
\bot\).

Apart from fixing "implies", our logic is also _intuitionistic_, which just
means https://web.math.princeton.edu/~nelson/papers/rome.pdf[we've added
accuracy to classical logic]. Roughly speaking, all the theorems are the same
except that rather than prove \(\exists x P(x)\), we sometimes prove \(\neg
\forall x \neg P(x)\), and similarly, rather than prove \(A \vee B\) we
sometimes prove \(\neg(\neg A \wedge \neg B)\).

Classical logic equates these formulas, while intuitionistic logic keeps them
apart to gain one extra bit of information. This bit signals whether the proof
is _constructive_.

For example, in intuitionistic logic, the law of the excluded middle (LEM)
when written as \(A \vee \neg A\) has no proof, but the classically equivalent
\(\neg (A \wedge \neg A)\) does. A proof of the former would be a decision
procedure, that is, it would describe an algorithm to construct a proof of
\(A\) or a proof of \(\neg A\) from any given \(A\); a tall order. The latter
merely states it is impossible to have both a proof of \(A\) and a proof of
\(\neg A\).

It's fun to split philosophical hairs, but we really went to all this trouble
for practical reasons. If we can automatically generate a constructive proof
of a given proposition, then we can automatically generate the source code of
a function of a given type, a fact known as the Curry-Howard correspondence.

== Too much too early ==

We restrict ourselves to propositional logic; no predicates nor quantifiers.
We know classical logic has a sound and complete decision procedure under
these conditions based on truth tables. Can we adapt it for intuitionistic
logic?

Alas, our simple strategy is wildly inappropriate. Recall we tried two
different values for each atomic proposition. Unfortunately, there are
infinitely many abstract syntax trees, and even if we could magically try each
one, what good would it do? Suppose we wish to prove \(P \rightarrow P\). Our
goal is to find a function that takes a proof of \(P\) and returns a proof of
\(P\). The identity function clearly does the job, but how would enumerating
all possible proofs of \(P\) lead to it?

We could try to enumerate all syntax trees, link:../lambda/hm.html[infer the
type of each tree] as we go, and see if it happens to be the proposition we're
trying to prove. However, besides being grossly inefficient, this procedure
fails to terminate when no proof exists.

Chapter 4 of https://www.cs.cmu.edu/~fp/courses/atp/handouts/atp.pdf[Frank
Pfenning's notes] describes a strategy to find proofs in first-order logic
based on _sequents_. We take the parts relevant to propositional logic.

A sequent consists of:

  * a list of _antecedent_ propositions \(A_1, ..., A_n\) a _succedent_
  * proposition \(B\)

and is written:

\[ A_1, ..., A_n \Rightarrow B \]

This sequent means: "given proofs of the antecedent propositions
\(A_1, ..., A_n\), we can produce a proof of \(B\)".

Given a proposition \(B\) to prove, we start with a sequent meaning we can
prove \(B\) with no assumptions:

\[
\Rightarrow B
\]

Then we apply one of the rules described below to transform it into one or
more sequents that logically lead to this sequent.

We recursively apply rules on these new sequents until we reach a self-evident
sequent. There are two kinds of such sequents. Either we have an _initial
sequent_:

\[
..., P, ... \Rightarrow P
\]

which means we can produce a proof of \(P\) if given a proof of \(P\) (and
possibly other proofs), or we have a sequent with \(\bot\) as an antecedent:

\[
..., \bot, ... \Rightarrow P
\]

which is an incarnation of ex falso quodlibet.

A rule preserving completeness is _invertible_; such a rule never changes a
viable sequent into sequents that eventually get stuck. Otherwise the rule is
_non-invertible_.

The rules lead to the following algorithm. We partition the antecedent
propositions into two lists: the _passive_ list and the _active_ list. Every
antecedent proposition starts off in the active list. Then:

  1. Apply as many invertible rules as possible to the succedent.
  Pfenning calls these cases _right asynchronous_.

  2. Until the active list is empty, apply as many invertible rules as possible
  to the head of the list, then move the head to the passive list.
  Pfenning calls these cases _left asynchronous_.

  3. The active list is empty and we must choose a non-invertible rule.
  We branch on each such rule. We can apply standard strategies such as
  breadth-first search, depth-first search, or iterative deepening. Pfenning
  calls these cases _synchronous_.

We've swept a potential problem under the rug. How can we avoid going around in
circles as we apply these rules? It turns out, with one exception, all the
rules are one-way. They each replace a sequent with simpler sequents.

The problematic exception crops up in the last step. If we branch on an
implication \(P \rightarrow Q\) in the passive list, then a new antecedent is
added while no existing ones are removed, which means we may later choose to
branch on the same implication again.

If a proposition has a proof, then a breadth-first search will eventually find
it, but in general, this algorithm might never terminate on a proposition with
no proof.

For now, we sacrifice completeness to address this, and remove an implication
antecedent when exploring it. This guarantees termination, but prevents us
from finding proofs of certain theorems such as \(\neg \neg (A \vee \neg A)\).
We fix this later.

Let's walk through a proof of our first example.

\[
\Rightarrow ((P \rightarrow\bot) \vee Q) \rightarrow (P \rightarrow Q)
\]

The succedent is an implication, so the corresponding rule yields:

\[
(P\rightarrow\bot) \vee Q \Rightarrow P \rightarrow Q
\]

The succedent is an implication again, so we get:

\[
(P\rightarrow\bot) \vee Q, P \Rightarrow Q
\]

The antecedent disjunction leads to the two sequents:

\[
P \rightarrow \bot , P \Rightarrow Q
\]
\[
Q, P \Rightarrow Q
\]

The second sequent is an initial sequent, while the only way to progress from
the first sequent is to apply a non-invertible rule to the antecedent
implication, yielding two sequents:

\[
\bot , P \Rightarrow Q
\]
\[
P \Rightarrow P
\]

The first sequent has a \(\bot\) antecedent, while the second is initial.
We have successfully found a proof.

Below, the `incompleteOracle` function determines if a proof exists via a
depth-first search. Hopefully our code alone is enough to explain the rules.
For example, the first rule states if the sequent has the form:

\[ A_1, ..., A_n \Rightarrow P \wedge Q \]

then we recurse on:

\[ A_1, ..., A_n \Rightarrow P \]

and:

\[ A_1, ..., A_n \Rightarrow Q \]

A logician might summarize this as:

\[
\frac{\Gamma \Rightarrow P \quad \Gamma \Rightarrow Q} { \Gamma \Rightarrow P \wedge Q }
\]

where \(\Gamma\) is a list of antecedent propositions. Roughly speaking, this
notation says that to prove the thing below the horizontal line, we can prove
the things above the line.

Since we classify the antecedents as passive or active, we ought to be more
pedantic and split \(\Gamma\) into two lists; see Pfenning for details.

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
module Main where
import Base
import Charser
import System
foreign export ccall "main" main
{- GHC edition:
import Control.Applicative ((<**>))
import Control.Arrow
import Data.List
import Data.Function ((&))
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char
type Charser = Parsec () String
-}
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

incompleteOracle :: Prop -> Bool
incompleteOracle = rAsyn [] [] where
  rAsyn pas act rhs = case rhs of
    a :& b -> rAsyn pas act a && rAsyn pas act b
    a :-> b -> rAsyn pas (a:act) b
    _ -> lAsyn pas act rhs

  lAsyn pas (x:xs) rhs = case x of
    Falso -> True
    a :& b -> lAsyn pas (a:b:xs) rhs
    a :| b -> lAsyn pas (a:xs) rhs && lAsyn pas (b:xs) rhs
    _ -> lAsyn (x:pas) xs rhs

  lAsyn pas [] rhs = elem rhs pas || any implyCases pas || orCases
    where
    orCases = case rhs of
      a :| b -> rAsyn pas [] a || rAsyn pas [] b
      _ -> False
    implyCases p = case p of
      a :-> b -> lAsyn pas' [b] rhs && rAsyn pas' [] a
        where pas' = filter (/= p) pas
      _ -> False
\end{code}

We write a parser for propositions so we can easily test our code.
We use Haskell notation:

  * The conjunction of `p` and `q` is written `(p, q)`.
  * The disjunction of `p` and `q` is written `Either p q`.
  * Implication is written `(->)` and associates right.
  * The proposition \(\bot\) is written `Void` (from the `Data.Void` package).

\begin{code}
propo :: Charser Prop
propo = space *> aps <* eof where
  aps = foldr1 (:->) <$> sepBy1 arg (spstr "->")
  arg = Var <$> sp ((:) <$> lowerChar <*> many alphaNumChar)
    <|> between (spch '(') (spch ')') prodOrProp
    <|> const Falso <$> spstr "Void"
    <|> spstr "Either" *> ((:|) <$> arg <*> arg)
  prodOrProp = (&) <$> aps <*> (spch ',' *> (flip (:&) <$> aps) <|> pure id)
  sp :: Charser a -> Charser a
  sp = (<* space)
  spch = sp . char
  spstr = sp . string
\end{code}

Evaluating the following confirms we can find a proof of our first example:

------------------------------------------------------------------------
incompleteOracle <$> parse propo "" "Either (p -> Void) q -> p -> q"
------------------------------------------------------------------------

== Proof by construction ==

Our program has discovered a truly remarkable proof of our theorem, but the
Boolean return value is too small to contain it.

We remedy this by building lambda terms instead of just indicating whether it's
possible to do so. This requires assigning names to various propositions when
we first encounter them; we pass around an integer to help generate unique
names.

We associate each proposition in the active and passive lists with a lambda
term representing its proof.

We take this opportunity to properly fix our problematic implication rule:

\[
\frac
  {A \rightarrow B,\Gamma\Rightarrow A \quad B,\Gamma\Rightarrow G }
  {A \rightarrow B,\Gamma\Rightarrow G}
\]

It turns out our prover is sound and complete if we replace it with these 4
rules:

\[
\frac
  {B,A,\Gamma\Rightarrow G \quad \text{atomic }A }
  {A \rightarrow B,A,\Gamma\Rightarrow G}
\]

\[
\frac
  {A\rightarrow (B\rightarrow C),\Gamma\Rightarrow G}
  {(A\wedge B)\rightarrow C,\Gamma\Rightarrow G}
\]

\[
\frac
  {A\rightarrow C, B\rightarrow C,\Gamma\Rightarrow G}
  {(A\vee B)\rightarrow C,\Gamma\Rightarrow G}
\]

\[
\frac
  {B\rightarrow C,\Gamma\Rightarrow A\rightarrow B \quad C,\Gamma\Rightarrow G}
  {(A\rightarrow B)\rightarrow C,\Gamma\Rightarrow G}
\]

The `prove` function below performs a depth-first search to find programs of a
given type.

\begin{code}
data Proof = V String | Lam String Proof | Proof :@ Proof

prove :: Prop -> [Proof]
prove prop = snd <$> rAsyn 0 [] [] prop where
  rAsyn n pas act prop = case prop of
    a :-> b -> do
      let s = 'a':show n
      second (Lam s) <$> rAsyn (n + 1) pas ((V s, a):act) b
    a :& b -> do
      (n1, f) <- rAsyn n pas act a
      (n2, s) <- rAsyn n1 pas act b
      pure (n2, V "," :@ f :@ s)
    _ -> lAsyn n pas act prop
\end{code}

The first 3 new implication rules are invertible.
The first of them triggers on two antecedents; if the current active antecedent
matches one of the patterns, then we search for suitable companions matching
the other pattern in the passive list. If there are any, we proceed with the
first matching pair we find.

We also drop antecedents of the form \(X \rightarrow X\), as these are
superfluous.

\begin{code}
  lAsyn n pas (pp@(prf, prop):ps) rhs = case prop of
    a :-> b | a == b -> lAsyn n pas ps rhs
    a@(Var _) :-> b | Just (prfA, _) <- find (\(_, a') -> a' == a) pas ->
      lAsyn n pas ((prf :@ prfA, b):ps) rhs
    a@(Var _) -> lAsyn n (pp:pas') (take 1 act' <> ps) rhs
      where
      (pas', act') = foldl partitionMatch ([], []) pas
      partitionMatch (ps, as) x
        | (prfImp, a'@(Var _) :-> b) <- x, a == a' = (ps, (prfImp :@ prf, b):as)
        | otherwise = (x:ps, as)
    (a :& b) :-> c -> lAsyn n pas
      ((V "curry" :@ prf, a :-> (b :-> c)):ps) rhs
    (a :| b) :-> c -> lAsyn n pas
      ((V "." :@ prf :@ V "Left", a :-> c)
      :(V "." :@ prf :@ V "Right", b :-> c):ps) rhs
\end{code}

The other invertible rules are straightforward:

\begin{code}
    Falso  -> pure (n, V "absurd" :@ prf)
    a :& b -> lAsyn n pas ((V "fst" :@ prf, a):(V "snd" :@ prf, b):ps) rhs
    a :| b -> do
      let l = 'a':show n
      (n1, lp) <- lAsyn (n + 1) pas ((V l, a):ps) rhs
      let r = 'a':show n1
      (n2, rp) <- lAsyn (n1 + 1) pas ((V r, b):ps) rhs
      pure (n2, V "either" :@ Lam l lp :@ Lam r rp :@ prf)
    _ -> lAsyn n (pp:pas) ps rhs
\end{code}

It remains to describe the non-invertible rules:

\begin{code}
  lAsyn n pas [] rhs = lguesses <> rguesses where
    lguesses = pas >>= \l -> lSyn n (filter (\p -> snd p /= snd l) pas) l rhs
    rguesses = case rhs of
      _ :| _ -> rSyn n pas rhs
      _ -> []
  rSyn n pas rhs = case rhs of
    a :| b -> as <> bs where
      as = second (V "Left" :@) <$> rSyn n pas a
      bs = second (V "Right" :@) <$> rSyn n pas b
    _ -> rAsyn n pas [] rhs
\end{code}

We've saved the the non-invertible implication rule for last. It relies on the
theorem:

\[
\begin{align}
((A \rightarrow B) \rightarrow C) \rightarrow (A \rightarrow B) \\
\iff (B \rightarrow C) \rightarrow (A \rightarrow B)
\end{align}
\]

I had trouble finding the program corresponding to this rule, so I first coded
our incomplete search from above:

------------------------------------------------------------------------
    a :-> b -> do
      let v = 'a':show n
      (n1, fArg) <- lAsyn (n + 1) pas [(prf :@ V v, b)] rhs
      (n2, arg) <- rSyn n1 pas a
      pure (n2, sub v arg fArg)
------------------------------------------------------------------------

I ran it on one direction of the theorem:

------------------------------------------------------------------------
prove <$> parse propo ""
  "((b -> c) -> (a -> b)) -> ((a -> b) -> c) -> (a -> b))"
------------------------------------------------------------------------

which found:

------------------------------------------------------------------------
Right [\a0 -> \a1 -> \a2 -> ((a0 \a5 -> (a1 \a7 -> a5)) a2)]
------------------------------------------------------------------------

This lambda term is equivalent to:

------------------------------------------------------------------------
\a0 -> \a1 -> a0 (a1 . const)
------------------------------------------------------------------------

which teaches us how to generate the code for our fiddly implication rule:

\begin{code}
  lSyn n pas (prf, l) rhs = if l == rhs then pure (n, prf) else case l of
    (a :-> b) :-> c -> do
      let v = 'a':show n
      (n1, fArg) <- lAsyn (n + 1) pas [(prf :@ V v, c)] rhs
      let w = 'a':show n1
      (n2, premiss1) <- rAsyn (n1 + 1) pas [(V w, b :-> c)] (a :-> b)
      let arg = sub w (V "." :@ prf :@ V "const") premiss1
      pure (n2, sub v arg fArg)
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
    V "." :@ x :@ y -> "(" <> show x <> " . " <> show y <> ")"
    V "," :@ x :@ y -> "(" <> show x <> ", " <> show y <> ")"
    x@(Lam _ _) :@ y -> "((" <> show x <> ")" <> show y <> ")"
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
showLogic :: Prop -> String
showLogic p = case p of
  a :-> Falso -> "&not;" <> showLogic a
  a :-> b -> "(" <> showLogic a <> "&rarr;" <> showLogic b <> ")"
  a :& b -> "(" <> showLogic a <> "&and;" <> showLogic b <> ")"
  a :| b -> "(" <> showLogic a <> "&or;" <> showLogic b <> ")"
  Falso -> "&perp;"
  Var s -> s

main = do
  s <- getContents
  case parse propo "" s of
    Left e -> putStr ":" >> putStr "parse error" >> putStr s
    Right p -> do
      putStr (showLogic p)
      putStr ":"
      putStr $ case prove p of
        [] -> "no proof found"
        prfs -> unlines $ show <$> prfs
\end{code}
++++++++++
</div>
++++++++++

== Q.E.D. ==

See Dyckhoff, _Contraction-Free Sequent Calculi for Intuitionistic Logic_,
for why we replace the non-terminating rule with 4 rules. Dyckhoff attributes
the underlying method to Vorobev, and mentions it's been rediscovered a few
times.

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
\a0 -> \a1 -> \a2 -> (a1 \a5 -> (a2 (a0 a5)))
------------------------------------------------------------------------

http://hackage.haskell.org/package/djinn[The Djinn tool] is similar to what we
built, though has considerably more features.

https://arxiv.org/abs/1805.07518[Michael Shulman describes an intimate
connection between linear logic and constructive mathematics].

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script>
function setup(name, t) {
  function act() {
    document.getElementById("in").value = t;
    document.getElementById("out").value = "";
    document.getElementById("mathy").innerHTML = "";
  }
  document.getElementById(name).addEventListener("click", act);
  if (name == "implies") act();
}
setup("swap", "(a, b) -> (b, a)");
setup("demorgan", "(a -> Void, b -> Void) -> Either a b -> Void");
setup("implies", "Either (p -> Void) q -> p -> q");
setup("lem", "(p, p -> Void) -> Void");
setup("nnlem", "(Either p (p -> Void) -> Void) -> Void");
setup("uncurry", "(a -> b -> c) -> (a, b) -> c");
setup("vorobev", "(((a -> b) -> c) -> (a -> b)) -> ((b -> c) -> (a -> b))");
setup("jonk", "(a -> b) -> ((a -> i) -> i) -> ((b -> i) -> i)");

const ctx = {};
function run() {
  ctx.inp = (new TextEncoder()).encode(document.getElementById("in").value);
  ctx.out = [], ctx.cursor = 0;
  ctx.instance.exports.main();
  const out = (new TextDecoder()).decode(Uint8Array.from(ctx.out)).split(':');
  document.getElementById("mathy").innerHTML = out[0];
  document.getElementById("out").value = out[1];
}
async function loadWasm() {
  try {
    ctx.instance = (await WebAssembly.instantiateStreaming(fetch('atp.wasm'), {env:
      { putchar: c  => ctx.out.push(c)
      , eof    : () => ctx.cursor == ctx.inp.length
      , getchar: () => ctx.inp[ctx.cursor++]
      }})).instance;

    document.getElementById("in").addEventListener("keydown", (event) => { if (event.key == "Enter") { run(); event.preventDefault(); }});
    document.getElementById("go").addEventListener("click", (event) => run());
  } catch(err) {
    console.log(err);
  }
}
loadWasm();
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
