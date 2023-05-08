= Fighting for Equality =

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src='eq.js'></script>
Demos:
<p>
<button id='peano'>Peano</button>
<button id='sort'>Sort</button>
<button id='one'>One Rule</button>
<button id='group'>Group</button>
</p>
Rewrite rules:
<p>
<textarea id='rules' rows='8' style='box-sizing:border-box;width:100%;'></textarea>
</p>
<p>
<button id='knuthbendix'>Knuth-Bendix</button>
LPO precedence: <textarea id='order' rows='1' cols='12'></textarea>
</p>
<p>
<textarea id='term' rows='2' style='box-sizing:border-box;width:100%'></textarea>
</p>
<p>
<button id='rewrite'>Rewrite</button>
</p>
<p>
<textarea id='out' rows='2' style='box-sizing:border-box;width:100%'></textarea>
</p>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

== Be reasonable ==

Suppose A = B. Then it seems reasonable to replace any instance of A with B,
and vice versa.

Programmers of all stripes perform equational reasoning. For example, a
seasoned C programmer might apply distributivity and symmetry laws to rewrite:

------------------------------------------------------------------------
bits = head_bytes * 8 + 8 * body_bytes;
------------------------------------------------------------------------

as:

------------------------------------------------------------------------
bits = 8 * (head_bytes + body_bytes);
------------------------------------------------------------------------

Referentially transparent languages unleash the full power of equational
reasoning, which is to say they let programmers do what mathematicians have
been doing for centuries. For example, using several identities, we can
rewrite:

------------------------------------------------------------------------
map (foo . (4*)) (iterate (1+) 0)
------------------------------------------------------------------------

as:

------------------------------------------------------------------------
map foo (iterate (4+) 0)
------------------------------------------------------------------------

In C, we could conceivably replace:

------------------------------------------------------------------------
for (unsigned i = 0;; i++) {
  foo(4*i);
}
------------------------------------------------------------------------

with:

------------------------------------------------------------------------
for (unsigned i = 0;; i+=4) {
  foo(i);
}
------------------------------------------------------------------------

but the reasoning is marred by details such as the variable `i`. As code grows
more complex, such details multiply.

In contrast, arbitrarily complex referentially transparent expressions remain
easy to manipulate with equational reasoning. This is important for humans, but
perhaps more important for computers. For example, GHC effortlessly optimizes
code with identities such as the following fusion law:

------------------------------------------------------------------------
map f . map g = map (f . g)
------------------------------------------------------------------------

Leibniz remarked: "It is unworthy of excellent men to lose hours like slaves in
the labour of calculation which could safely be relegated to anyone else if
machines were used." A few centuries later, Whitehead wrote: "Civilization
advances by extending the number of important operations which we can perform
without thinking about them."

Offloading equational reasoning to computers may be for the common good of all.

== Equality now! ==

How good are computers at equational reasoning?

In 1996, ftp://ftp.mcs.anl.gov/pub/tech_reports/reports/P642.pdf[McCune ran the
EQP program] to prove that
https://en.wikipedia.org/wiki/Robbins_algebra[Robbins algebras are Boolean],
solving an open problem that had defied generations of gifted mathematicians.
What's more, it only took 8 days and 30 megabytes on an
https://en.wikipedia.org/wiki/IBM_RISC_System/6000[RS/6000]. As modern
computers are more powerful, today it may be possible to write a short program
that uses equational reasoning to find the proof that eluded humanity for
decades.

We follow along chapter 4 of John Harrison, 'Handbook of Practical Logic and
Automated Reasoning'. Much of the below is a reworking of his OCaml code in
Haskell.

We want to parse equations such as the rules of Peano arithmetic when given in
the following form:

------------------------------------------------------------------------
0 + x = x
S x + y = S (x + y)
0 * x = 0
S x * y = y + (x * y)
------------------------------------------------------------------------

We store them in same link:lambda.html[tree that we used for CL terms].  We
employ some fancier Haskell extensions to automate some of the programming.
For instance, to determine if a variable is free, we employ `Data.Foldable` and
simply call `elem` on the CL term.

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
<p><a onclick='hideshow("boilerplate");'>&#9654; Toggle boilerplate and UI</a></p>
<div id='boilerplate' style='display:none'>
++++++++++

\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase #-}
import Control.Arrow
import Data.Foldable
import Data.Function
import Data.List (intersect, union, delete, elemIndex)
#ifdef __HASTE__
import Control.Monad
import Haste.DOM
import Haste.Events
import Text.Parsec hiding (space)
(<>) = (++)
type Charser = Parsec String ()
lowerChar = lower; upperChar = upper; alphaNumChar = alphaNum;
digitChar = digit; space = spaces; some = many1
#else
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char
type Charser = Parsec () String
#endif

#ifdef __HASTE__
main :: IO ()
main = withElems ["out", "rules", "order", "term", "knuthbendix", "rewrite"] $
    \[oEl, rEl, ordEl, tEl, kbB, rwB] -> do
  let
    setup buttonName rs precs t = do
      Just b <- elemById buttonName
      let
        go = do
          setProp oEl "value" ""
          setProp rEl "value" rs
          setProp ordEl "value" precs
          setProp tEl "value" t
      void $ b `onEvent` Click $ const $ go
      when (buttonName == "group") go
  setup "group" (unlines
    [ "(x * y) * z = x * (y * z)"
    , "1 * x = x"
    , "I x * x = 1"
    ]) "1 * I" "(I x * x) * y"
  setup "one" "F (F x) = G x" "" "F(F(F(F(F(x)))))"
  setup "peano" (unlines
    [ "0 + x = x"
    , "S x + y = S (x + y)"
    , "0 * x = 0"
    , "S x * y = y + (x * y)"]) "" "S(S(S(S(0)))) * S(S(0)) + S(S(S(0)))"
  setup "sort" (unlines
    [ "Max 0 x = x"
    , "Max x 0 = x"
    , "Max (S x) (S y) = S (Max x y)"
    , "Min 0 x = 0"
    , "Min x 0 = 0"
    , "Min (S x) (S y) = S (Min x y)"
    , "Sort Nil = Nil"
    , "Sort (Cons x y) = Insert x (Sort y)"
    , "Insert x Nil = Cons x Nil"
    , "Insert x (Cons y z) = Cons (Max x y) (Insert (Min x y) z)"
    ]) "" $ concat
      [ "Sort ("
      , "Cons (S (S (S 0))) ("
      , "Cons (S 0) ("
      , "Cons (S (S (S (S 0)))) ("
      , "Cons (S 0) ("
      , "Cons (S (S (S (S (S 0)))))  Nil)))))"
      ]

  let parseRules = sequence . map (parse rule "") . lines
  void $ kbB `onEvent` Click $ const $ do
    parseRules <$> getProp rEl "value" >>= \case
      Left _ -> setProp oEl "value" "bad rules: parse error"
      Right rs -> do
        opList <- words <$> getProp ordEl "value"
        case knuthBendix (lpoGT $ weigh opList) rs of
          Nothing -> setProp oEl "value" "completion failed"
          Just rs' ->  do
            setProp oEl "value" ""
            setProp rEl "value" $ unlines $ show <$> rs'
  void $ rwB `onEvent` Click $ const $ do
    parseRules <$> getProp rEl "value" >>= \case
      Left _ -> setProp oEl "value" "bad rules: parse error"
      Right rs -> parse expr "" <$> getProp tEl "value" >>= \case
        Left _ -> setProp oEl "value" "bad term: parse error"
        Right x -> do
          setProp oEl "value" $ show $ fixRewrite rs x
#endif
\end{code}

Ugh.

++++++++++
</div>
++++++++++

\begin{code}
data ExpF a = C String | V a | ExpF a :@ ExpF a deriving (Eq, Functor, Foldable)
type Exp = ExpF String
data Rule = Rule Exp Exp deriving Eq

instance Show (ExpF String) where
  show = \case
    V v -> v
    C s :@ x :@ y | s `elem` ["+", "*"] ->
       showR x <> " " <> s <> " " <> showR y
    C s
      | s `elem` ["+", "*"] -> "(" <> s <> ")"
      | otherwise -> s
    x :@ y -> show x <> " " <> showR y
    where
    showR x | _ :@ _ <- x = "(" <> show x <> ")"
            | otherwise   =        show x

instance Show Rule where show (Rule l r) = show l <> " = " <> show r

expr :: Charser Exp
expr = foldl (&) <$> apps <*> many ((\f b a -> C f :@ a :@ b) <$> op <*> apps) where
  op   = pure <$> sp (oneOf "+*")
  apps = foldl1 (:@) <$> some (con <|> var <|> between (spch '(') (spch ')') expr)
  sp   = (<* space)
  spch = sp . char
  con  = C <$> sp (some digitChar <|> (:) <$> upperChar <*> many alphaNumChar)
  var  = V <$> sp ((:) <$> lowerChar <*> many alphaNumChar)

rule :: Charser Rule
rule = Rule <$> expr <*> (char '=' *> space *> expr)

mustParse :: Charser a -> String -> a
mustParse p = either undefined id . parse p ""

peano = mustParse rule <$>
  ["0 + x = x", "S x + y = S (x + y)", "0 * x = 0", "S x * y = y + (x * y)"]
\end{code}

The `mgu` and `match` functions from link:type.html[type checking] turn out to
be exactly the tools we need, except now we run them on terms instead of types:

\begin{code}
apply :: [(String, Exp)] -> Exp -> Exp
apply sub t = case t of
  a :@ b -> apply sub a :@ apply sub b
  V v    -> maybe t id $ lookup v sub
  _      -> t

(@@) s1 s2 = map (second (apply s1)) s2 <> s1

varBind s t = case t of
  C v -> Just [(s, t)]
  V v -> Just $ if v == s then [] else [(s, t)]
  a :@ b -> if elem s t then Nothing else Just [(s, t)]

mgu :: Exp -> Exp -> Maybe [(String, Exp)]
mgu t u = case t of
  C a -> case u of
    C b -> if a == b then Just [] else Nothing
    V v -> varBind v t
    _ -> Nothing
  V a -> varBind a u
  a :@ b -> case u of
    C _ -> Nothing
    V v -> varBind v t
    c :@ d -> mgu a c >>= \s -> (@@ s) <$> mgu (apply s b) (apply s d)

merge s1 s2 = if all (\v -> apply s1 (V v) == apply s2 (V v))
  $ map fst s1 `intersect` map fst s2 then Just $ s1 <> s2 else Nothing

match :: Exp -> Exp -> Maybe [(String, Exp)]
match h t = case h of
  C a | C b <- t, a == b -> Just []
  V a -> Just [(a, t)]
  a :@ b | c :@ d <- t -> do
    ac <- match a c
    bd <- match b d
    merge ac bd
  _ -> Nothing
\end{code}

This is because we have already been using equality.
We used `mgu` to see if two types can be made equal with the appropriate
substitutions, and we used `match` to find matching class instances.
Now we want more: having determined two things are equal, we want to replace
one with the other to see what happens.

Given a term, there could be multiple rules that apply. And multiple places to
apply them. For example, the term "(0 + 0) + (0 * (0 + 0))" has two subterms
that fit the first rule and one subterm that fits the third.

We pick the following rewriting strategy. Earlier rules take precedence over
later rules. Subterms on the left take precedence over subterms on the right.
Parents take precedence over their children.

We use `match` to find a left-hand side of a rule that fits, and replace
it with the right-hand side, suitably substituted.

\begin{code}
findRewrite :: [Rule] -> Exp -> Maybe Exp
findRewrite eqs t = asum $ (\(Rule l r) -> (`apply` r) <$> match l t) <$> eqs

rewrite :: [Rule] -> Exp -> Maybe Exp
rewrite eqs t = asum $ findRewrite eqs t : case t of
  x :@ y -> [(:@ y) <$> rewrite eqs x, (x :@) <$> rewrite eqs y]
  _      -> []
\end{code}

We want to keep rewriting until no more rules apply.

\begin{code}
fixRewrite :: [Rule] -> Exp -> Exp
fixRewrite eqs t = maybe t (fixRewrite eqs) $ rewrite eqs t
\end{code}

We have coupled some recycled code with a simple rewrite function.
Will it work?

------------------------------------------------------------------------
fixRewrite peano $ mustParse expr
  "S(S(S(S(0)))) * S(S(0)) + S(S(S(0)))"
------------------------------------------------------------------------

This is "(4*2)+3", and we miraculously arrive at "11":

------------------------------------------------------------------------
S (S (S (S (S (S (S (S (S (S (S 0))))))))))
------------------------------------------------------------------------

== Group equality ==

Take the axioms of group theory:

\begin{code}
groupAxioms = mustParse rule <$>
  [ "(x * y) * z = x * (y * z)"
  , "1 * x = x"
  , "I x * x = 1"
  ]
\end{code}

and run `fixRewrite` on "(I x * x) * y":

------------------------------------------------------------------------
fixRewrite groupAxioms $ mustParse expr "(I x * x) * y"
------------------------------------------------------------------------

We'd like to see "y", via the application of the third and then second axioms,
but instead we get:

------------------------------------------------------------------------
I x * (x * y)
------------------------------------------------------------------------

The associative law was applied first, preventing further rewrites.

We fix this by simply adding a rewrite rule:

------------------------------------------------------------------------
I x * (x * y) = y
------------------------------------------------------------------------

Clearly we can iterate this. We can try other terms, and if we encounter a
rewrite we dislike, then we add another rule to fix it.

This raises thorny questions:

  * How do we find rewrites we dislike?
  * What does it mean to dislike a term anyway?
  * Even if we could define bad rewrites and find them easily, is there a bound
on the number of rules we need to add? Might our algorithm never terminate?
  * How do we know if `fixRewrite` terminates in the first place? There might always be at least one rule that applies.

These questions are easily answered in general by the following rules:

\begin{code}
skRules = mustParse rule <$>
  [ "S x y z = x z (y z)"
  , "K x y = x"
  ]
\end{code}

That is, combinatory logic is a rewriting system. (The rewriting strategy we
chose above corresponds to normal-order evaluation.) All those horrible
undecidability results from computer science carry over to rewriting systems in
general.

== Some are more equal than others ==

Fortunately, for particular cases we care about, the answers are more
comforting.

The first step is to introduce a well-founded partial order among terms.
In other words, in our pursuit of equational reasoning, we begin by proclaiming
some terms to be greater than others, even though they are equal according to
the rules!

Then we demand that the left-hand side of each rule is strictly greater than
the right-hand side. This ensures each rewrite shrinks the term according to
our well-founded partial order, so we eventually reach a term for which no
rewrite rules apply.

We introduce the _lexicographic path order_ (LPO) on our terms.
See references for the motivation, and why it works.
Our `funargs` helper uncurries the function and arguments needed by `lpoGT`.
Harrison has no need for this because of a different data structure.

Which function should be greater than another? It depends on the rewriting
system; some orderings work better than others.

\begin{code}
type Weigh = (String, Int) -> (String, Int) -> Bool

lpoGT :: Weigh -> Exp -> Exp -> Bool
lpoGT weigh s t
  | V v <- t = s /= t && v `elem` s
  | Just (f, fargs) <- funargs s [],
    Just (g, gargs) <- funargs t [] =
      any (\a -> a == t || rec a t) fargs ||
      all (rec s) gargs &&
        (f == g && lexord fargs gargs ||
        weigh (f, length fargs) (g, length gargs))
  | otherwise = False
  where
  rec = lpoGT weigh
  lexord l1 l2 = case (l1, l2) of
    (h1:t1, h2:t2) -> rec h1 h2 && length t1 == length t2 ||
      h1 == h2 && lexord t1 t2
    _ -> False
  funargs t acc = case t of
    C f -> Just (f, acc)
    x :@ y -> funargs x $ y:acc
    _ -> Nothing

weigh :: [String] -> Weigh
weigh precs (f, m) (g, n)
  | f == g    = m > n
  | otherwise = case (wt f, wt g) of
    (Just i, Just j) -> i > j
    (Nothing, Nothing) -> f > g
    (Nothing, _) -> False
    (_, Nothing) -> True
  where wt s = elemIndex s precs
\end{code}

Our group axioms all have left-hand sides greater than their right-hand sides
by lexicographic path order, no matter what ordering we pick for the identity,
group operation, and inverse. We confirm this in the case of ASCII ordering:

\begin{code}
prop_groupAxiomsValid =
  and $ (\(Rule l r) -> lpoGT (weigh []) l r) <$> groupAxioms
\end{code}

Let `t` be a term, `rules` be a rewriting system and let `x = fixRewrite rules
t`. Next, apply the rewrite rules in an arbitrary order on `t` to obtain `y`, a
term on which no further rewrites are possible.

If we always have `y` equals `x`, then the rewriting system is _confluent_.
After enough steps, every term reaches its unique _normal form_, no matter
which order we apply the rewrites.

On the other hand, if we find cases where `x` and `y` differ, then we introduce
a rewrite rule from the larger to the smaller to fix it.
If `x` and `y` are incomparable, then we're stuck, unless we later happen to
stumble upon more rewrite rules that can change at least one of them.

== Knuth-Bendix completion ==

To find all the obstacles getting in the way of confluence, it can be shown
that, loosely speaking, we need only apply the rewrite rules to themselves.
More precisely, for each rewrite rule, we find matches of its left-hand side
among all sub-expressions of the rewrite rules. There's no need to guess a term
and try various rewrite strategies on it.

We walk through an example. Take our first and third group axioms, and
rename the variables to avoid confusion:

------------------------------------------------------------------------
(x1 * y1) * z1 = x1 * (y1 * z1)
I x3 * x3 = 1
------------------------------------------------------------------------

The LHS of rule 3 matches a subterm in the LHS of rule 1 via the substitution
`[(x1, I x3), (y1, x3)]`:

------------------------------------------------------------------------
(I x3 * x3) * z1
------------------------------------------------------------------------

Thus we can either use rule 1 or rule 3 to rewrite this term.
The resulting two terms are a _critical pair_ of these two rules.

------------------------------------------------------------------------
1 * z1 = (I x3 * x3) * z1 = I x3 * (x3 * z1)
------------------------------------------------------------------------

The `criticalPairs` function finds all critical pairs of two given rules.

\begin{code}
findSubs :: Rule -> Exp -> [(Exp, [(String, Exp)])]
findSubs (Rule l r) t = case t of
  V _ -> []
  C _ -> maybe [] ((:[]) . (,) r) (mgu l t)
  x :@ y -> concat
    [ maybe [] ((:[]) . (,) r) (mgu l t)
    , first (:@ y) <$> findSubs (Rule l r) x
    , first (x :@) <$> findSubs (Rule l r) y
    ]

overlaps :: Rule -> Rule -> [Rule]
overlaps (Rule l r) (Rule l2 r2) =
  (\(t, sub) -> Rule (apply sub t) (apply sub r2)) <$> findSubs (Rule l r) l2

criticalPairs :: Rule -> Rule -> [Rule]
criticalPairs a b
  | a == b = overlaps ruleA ruleB
  | otherwise = overlaps ruleA ruleB `union` overlaps ruleB ruleA
  where
  both f (Rule a b) = Rule (f a) (f b)
  ruleA = both (('a':) <$>) a
  ruleB = both (('b':) <$>) b
\end{code}

Continuing our example, we have `1 * z1 = z1` by another axiom, while no rules
apply to `I x3 * (x3 * z1)`. At this point, neither term can be rewritten
further, and since `z1` has the smaller LPO, we add the rewrite rule:

------------------------------------------------------------------------
I x3 * (x3 * z1) = z1
------------------------------------------------------------------------

This new rule may create more critical pairs.

If the elements of a critical pair are incomparable, then we defer its
processing until later. After finding and processing other crtical pairs,
we revisit the list of deferred pairs, and hope that new rewrite rules can
help.

If we've successfully processed all critical pairs, then we have a terminating
rewriting system that finds a unique normal form for all input terms.

Otherwise, Knuth-Bendix fails because either there are obstinate critical pairs
that are beyond help, or we can never add enough rules to reach confluence.

\begin{code}
normalizeThenOrient :: (Exp -> Exp -> Bool) -> [Rule] -> Rule -> Maybe Rule
normalizeThenOrient cmp eqs (Rule rawS rawT)
  | s == t || cmp s t = Just (Rule s t)
  | cmp t s           = Just (Rule t s)
  | otherwise         = Nothing
  where
  s = fixRewrite eqs rawS
  t = fixRewrite eqs rawT

complete cmp eqs todo crits = case crits of
  (eq:rest) -> case normalizeThenOrient cmp eqs eq of
    Just new@(Rule s t)
      | s == t    -> rec eqs todo rest
      | otherwise -> rec (new:eqs) todo $ rest <> (concatMap (criticalPairs new) $ new:eqs)
    Nothing       -> rec eqs (eq:todo) rest
  [] | null todo  -> Just eqs
     | otherwise  -> find (maybe False (const True) . normalizeThenOrient cmp eqs) todo
       >>= (\e -> rec eqs (delete e todo) [e])
  where rec = complete cmp

knuthBendix :: (Exp -> Exp -> Bool) -> [Rule] -> Maybe [Rule]
knuthBendix cmp eqs = complete cmp eqs [] $ foldr1 union $ criticalPairs <$> eqs <*> eqs
\end{code}

The following rewrite system only has one rule. Its left-hand side matches a
subterm of itself to make a nontrivial critical pair. Knuth-Bendix turns it
into a rule to add to the system, and with this second rule, the system is
confluent.

\begin{code}
selfCritical :: [Rule]
selfCritical = [mustParse rule "F (F x) = G x"]

selfSystem :: [Rule]
Just selfSystem = knuthBendix (lpoGT (weigh [])) selfCritical
\end{code}

As for groups, it turns out a certain ordering of the identity, group
operation, and inverse is enough for our basic implementation of Knuth-Bendix
completion to find a confluent system.

\begin{code}
Just groupSystem = knuthBendix (lpoGT (weigh ["1", "*", "I"])) groupAxioms
\end{code}

Proving `x * I x = 1` from the axioms is trickier than it looks.
Infuriatingly, the computer can now quickly confirm the truth of this
equation, with no explanation:

------------------------------------------------------------------------
fixRewrite groupSystem $ mustParse expr "x * I x"
------------------------------------------------------------------------

Getting the program to tell us what it's doing is left as an exercise. Which,
once completed, can do some homework exercises for us in courses on group
theory!

== Equal Opportunities ==

The first chapter of the https://www.math.upenn.edu/~wilf/AeqB.html[free book
"A=B", by Marko Petkovsek, Herbert Wilf and Doron Zeilberger] shows off the
power of normalizing terms in various settings. The whole book is worth
reading, as it teaches how to systematically find hypergeometric identities,
which hitherto had been accomplished by a mathematical analogue of alchemy.

We didn't make it to Robbins algebras. A big obstacle is commutativity: how
can we handle "x + y = y + x" in our framework? See McCune or Harrison for
references or section 7 of
http://www.cs.tau.ac.il/~nachum/papers/taste-fixed.pdf[Dershowitz, _A Taste of
Rewrite Systems_]. (Our sorting demo comes from the latter.)

With the help of a few identities and a cost model,
http://www.cs.cornell.edu/~ross/publications/eqsat/[automating equational
reasoning leads to emergent optimizations]. The computer systematically
applies advanced optimizations traditionally discovered and implemented by
experienced programmers.
