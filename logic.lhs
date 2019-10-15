= Logic Programming =

So far, substitutions have been a means to an end. Once we found the most
general unifier, we discarded any work we did along the way.

This time, the substitutions take center stage. We start with our trusty `mgu`:

\begin{code}
{-# LANGUAGE BlockArguments, DeriveFunctor, DeriveFoldable #-}
import Control.Arrow (second)
import Data.List (nub, transpose)
import Text.Megaparsec
import Text.Megaparsec.Char
type Parser = Parsec () String

data ExpF a = C String | V a | ExpF a :@ ExpF a deriving (Eq, Functor, Foldable, Show)
type VarId = String
type Exp = ExpF VarId
type Subst = [(VarId, Exp)];

apply :: Subst -> Exp -> Exp
apply sub t = case t of
  a :@ b -> apply sub a :@ apply sub b
  V v    -> maybe t id $ lookup v sub
  _      -> t

mgu :: Exp -> Exp -> Subst -> [Subst]
mgu t u sub = case (apply sub t, apply sub u) of
  (C a, C b) | a == b  -> [sub]
  (V a, V b) | a == b  -> [sub]
  (V a, x)             -> varBind a x
  (x, V a)             -> varBind a x
  (x1 :@ y1, x2 :@ y2) -> mgu x1 x2 sub >>= mgu y1 y2
  _                    -> []
  where
  varBind v t = if elem v t then [] else
    [(v, t):map (second $ apply [(v, t)]) sub]
\end{code}

In the past, we applied substitutions before calling `mgu`. Here, we pass in
substitutions and `mgu` applies them on our behalf, so we can easily try out
different substitutions on the same term.

Instead of a `Maybe Subst` we return a list of 0 or 1 substitutions.

== Who's your daddy? ==

We define a simple grammar for parsing `Exp` values:

\begin{code}
fact :: Parser Exp
fact = apps where
  apps = foldl1 (:@) <$> some
    (con <|> var <|> between (spch '(') (spch ')') apps)
  con  = C <$> sp ((:) <$> (digitChar <|> lowerChar) <*> many alphaNumChar)
  var  = V <$> sp ((:) <$> upperChar <*> many alphaNumChar)

sp   = (<* many (char ' '))
spch = sp . char

mustFact :: String -> Exp
mustFact = either undefined id . parse fact ""
\end{code}

Suppose we are told `alfred` is a parent of `aethelflaed`, and then we are
asked for a parent of `aethelflaed`. We can answer this query via unification:

\begin{code}
exampleTrivial = mgu fact q [] where
  fact = mustFact "parent alfred aethelflaed"
  q    = mustFact "parent X aethelflaed"
\end{code}

------------------------------------------------------------------------
[[("X",C "alfred")]]
------------------------------------------------------------------------

Let's build on this simple idea.

== Do you know where your children are? ==

Given multiple facts, we can find all that unify with a given query:

\begin{code}
births :: [Exp]
births = mustFact <$>
  [ "parent alfred aethelflaed"
  , "parent aethelflaed aelfwynn"
  , "parent alfred edward"
  , "parent edward aethelstan"
  , "parent edward edmund"
  , "parent edward eadred"
  , "parent edmund eadwig"
  , "parent edmund edgar"
  ]

exampleMultiple = concat [mgu p q [] | p <- births]
  where q = mustFact "parent edward X"
\end{code}

------------------------------------------------------------------------
[[("X",C "aethelstan")],[("X",C "edmund")],[("X",C "eadred")]]
------------------------------------------------------------------------

== Grand slam ==

We extend our system so a predicate depends on any number of other predicates:

\begin{code}
type Rule = [Exp]
rule :: Parser Rule
rule = (:) <$> fact <*> (option [] (sp (string ":-") *> sepBy1 fact (spch ',')))

mustRule :: String -> Rule
mustRule = either undefined id . parse rule ""

grand :: [Rule]
grand = mustRule "grandparent A B :- parent A X, parent X B" :
  ((:[]) <$> births)
\end{code}

The `births` are predicates which are unconditionally true because they have no
dependencies, while `grandparent A B` is true only if we can unfiy `parent A X`
with a true predicate and unify `parent X B` with a true predicate.

We find all solutions with a depth-first search:

\begin{code}
dfs :: [Rule] -> Exp -> Subst -> [Subst]
dfs rules q sub = concat [f t =<< mgu h q sub | (h:t) <- rules]
  where
  f xs s' = case xs of
    []     -> [s']
    (x:xt) -> f xt =<< dfs rules x s'

exampleGrand = dfs grand (mustFact "grandparent alfred Y") []
\end{code}

------------------------------------------------------------------------
[[("Y",C "aelfwynn"),("X",C "aethelflaed"),("B",V "Y"),("A",C "alfred")],
[("Y",C "aethelstan"),("X",C "edward"),("B",V "Y"),("A",C "alfred")],
[("Y",C "edmund"),("X",C "edward"),("B",V "Y"),("A",C "alfred")],
[("Y",C "eadred"),("X",C "edward"),("B",V "Y"),("A",C "alfred")]]
------------------------------------------------------------------------

== Keep it fresh ==

In our previous example, we had to carefully chose distinct variable names.
It makes more sense for variables to be scoped to the rule in which they
appear. This makes the language friendlier, and also allows recursive rules.

We'll achieve this by automatically generating fresh variables each time we
encounter a rule. But first, we break down our search for substitutions into
easily digestible pieces.

The design of our code is similar to
http://www.cs.uu.nl/research/techreps/repo/CS-2008/2008-044.pdf[the basic
parser combinators described by Swierstra]. A parser combinator is a function
taking a string and returning the list of all possible ways it can parse the
beginning of the input string. Included with each solution is the unparsed
remainder of the string.

There are a few primitive parser combinators that, for instance, parse a single
character satisfying a given condition. Then there are functions that stitch
together parser combinators from existing parser combinators.

We define "predicate combinators" similarly. An `Env` consists of the
substitutions required so far and a stream of integers representing unused
variables. A `Pred` takes an `Env` and returns a list of all `Env` values that
satisfy the predicate.

\begin{code}
type Env = (Subst, [Int])
type Pred = Env -> [Env]

pristine :: Env
pristine = ([], [0..])
\end{code}

The only `Pred` we can build from scratch is one that attempts to unify two
given expressions:

\begin{code}
(~~) :: Exp -> Exp -> Pred
s ~~ t = \(sub, vs) -> [(s', vs) | s' <- mgu s t sub]
\end{code}

Otherwise we build a `Pred` value from existing `Pred` values or indirectly
calling `(~~)`.

\begin{code}
fresh :: (Exp -> Pred) -> Pred
fresh f = \(sub, v:vs) -> f (V $ '_':show v) (sub, vs)

(\/!) :: Pred -> Pred -> Pred
(\/!) x y e = x e <> y e

(/\!) :: Pred -> Pred -> Pred
(/\!) x y e = x e >>= y

nono :: Pred -> Pred
nono x e = if null $ x e then [e] else []
\end{code}

Our depth-first search becomes:

\begin{code}
dfs' rules q = fst <$> f q pristine where
  f q = foldr1 (\/!)
    $ foldr1 (/\!) . zipWith ($) ((q ~~):repeat f) <$> rules

exampleGrand' = dfs' grand $ mustFact "grandparent alfred Y"
\end{code}

However, we want fresh variables:

\begin{code}
varsOf :: Rule -> [VarId]
varsOf = nub . concatMap (foldr (:) [])

pretty :: Foldable t => t VarId -> Env -> Subst
pretty q (subs, _) = map (second $ apply subs)
  $ filter (\(v, _) -> elem v q) subs

prolog :: [Rule] -> Exp -> [Subst]
prolog rules q = pretty q <$> f q pristine where
  f q = foldr1 (\/!)
    [refresh [] (varsOf r) $ zip ((q ~~):repeat f) r | r <- rules]
  refresh sub vs zs = case vs of
    []     -> foldr1 (/\!) $ map (\(f, x) -> f $ apply sub x) zs
    (v:vt) -> fresh \w -> refresh ((v, w):sub) vt zs
\end{code}

We called the function `prolog` because it mimics the logical part of Prolog.
We've also taken this opportunity to clean up the output. We only print
substitutions for variables in the query, and we fully apply all substitutions.

We test a classic example:

\begin{code}
anc :: [Rule]
anc =
    mustRule "ancestor A B :- parent A B"
  : mustRule "ancestor A B :- parent A X, ancestor X B"
  : grand

exampleAnc = prolog anc $ mustFact "ancestor X eadwig"
\end{code}

------------------------------------------------------------------------
[[("X",C "edmund")],[("X",C "alfred")],[("X",C "edward")]]
------------------------------------------------------------------------

== Out of our depth ==

The following `bitty` predicate matches all bitstrings of all lengths:

\begin{code}
bitty = mustRule <$>
  [ "bitty end"
  , "bitty (0 X) :- bitty X"
  , "bitty (1 X) :- bitty X"
  ]
\end{code}

But when we look for them, we only see 0s:

\begin{code}
exampleBitty = take 5 $ prolog bitty $ mustFact "bitty X"
\end{code}

------------------------------------------------------------------------
[[("X",C "end")],
[("X",C "0" :@ C "end")],
[("X",C "0" :@ (C "0" :@ C "end"))],
[("X",C "0" :@ (C "0" :@ (C "0" :@ C "end")))],
[("X",C "0" :@ (C "0" :@ (C "0" :@ (C "0" :@ C "end"))))]]
------------------------------------------------------------------------

Here, depth-first search never tires of the first branch it explores, because
it contains infinite matches.
Surprisingly, by simply replacing `concat` with a function that interleaves two
lists, the depth-first search gains elements of a breadth-first search in the
way it it backtracks.

\begin{code}
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

(\/) :: Pred -> Pred -> Pred
(\/) x y e = x e `interleave` y e

(/\) :: Pred -> Pred -> Pred
(/\) x y e = foldr interleave [] $ map y (x e)

prolog' :: [Rule] -> Exp -> [Subst]
prolog' rules q = pretty q <$> f q pristine where
  f q = foldr1 (\/) $ (\r -> refresh [] (varsOf r) $ zip ((q ~~):repeat f) r) <$> rules
  refresh sub vs zs = case vs of
    []     -> foldr1 (/\) $ map (\(f, x) -> f $ apply sub x) zs
    (v:vt) -> fresh \w -> refresh ((v, w):sub) vt zs
\end{code}

Our search now reaches any given bitstring:

\begin{code}
exampleBitty' = take 5 $ prolog' bitty $ mustFact "bitty X"
\end{code}

------------------------------------------------------------------------
[[("X",C "end")],
[("X",C "1" :@ C "end")],
[("X",C "0" :@ C "end")],
[("X",C "1" :@ (C "1" :@ C "end"))],
[("X",C "0" :@ (C "1" :@ C "end"))]]
------------------------------------------------------------------------

A cute trick is to define `interleave` as:

------------------------------------------------------------------------
interleave xs ys = concat $ transpose [xs, ys]
------------------------------------------------------------------------

More generally, the function `transpose` can interleave multiple lists, which
suggests the following variation with even more emphasis on breadth over depth:

\begin{code}
disj :: [Pred] -> Pred
disj xs e = concat . transpose $ map ($ e) xs

conj :: [Pred] -> Pred
conj = foldr1 \x y e -> concat . transpose $ map y (x e)

prolog'' :: [Rule] -> Exp -> [Subst]
prolog'' rules q = pretty q <$> f q pristine where
  f q = disj $ (\r -> refresh [] (varsOf r) $ zip ((q ~~):repeat f) r) <$> rules
  refresh sub vs zs = case vs of
    []     -> conj $ map (\(f, x) -> f $ apply sub x) zs
    (v:vt) -> fresh \w -> refresh ((v, w):sub) vt zs
\end{code}

== Predicate combinators ==

Just as we mix and match parser combinators to build parsers, we can mix and
match the above functions to build logic programs. For example, the following
finds all pairs of lists that concatenate to [3, 1]:

\begin{code}
appendo :: Exp -> Exp -> Exp -> Pred
appendo x y z =
     ((x ~~ C "nil") /\ (y ~~ z))
  \/ fresh \h -> fresh \xt -> fresh \zt ->
       (x ~~ (C "cons" :@ h :@ xt))
    /\ (z ~~ (C "cons" :@ h :@ zt))
    /\ appendo xt y zt

exampleAppendo = pretty ["X", "Y"] <$>
  appendo (V "X") (V "Y") (mustFact "cons 3 (cons 1 nil)") pristine
\end{code}

------------------------------------------------------------------------
[[("Y",(C "cons" :@ C "3") :@ ((C "cons" :@ C "1") :@ C "nil")),("X",C "nil")],
[("Y",(C "cons" :@ C "1") :@ C "nil"),("X",(C "cons" :@ C "3") :@ C "nil")],
[("Y",C "nil"),("X",(C "cons" :@ C "3") :@ ((C "cons" :@ C "1") :@ C "nil"))]]
------------------------------------------------------------------------

== Prolog forever ==

To perform true breadth-first search, we define a variant of `Pred` that takes an
`Env` to a stream of lists of possible `Env` values that satisfy the predicate.
All solutions in the same list have the same cost. The nth list holds all
solutions of cost n.

\begin{code}
type PredB = Env -> [[Env]]
\end{code}

The unification predicate costs nothing:

\begin{code}
(~~~) :: Exp -> Exp -> PredB
s ~~~ t = \(sub, vs) -> [[(s', vs) | s' <- mgu s t sub]]
\end{code}

Disjunction concatenates all solutions with the same cost:

\begin{code}
mct = map concat . transpose

disB :: [PredB] -> PredB
disB xs e = mct $ ($ e) <$> xs
\end{code}

To form the conjunction of `f` and `g`, we order the solutions of `f` by
cost, then solve each one of them for `g`, adding in the cost of solving `f`
before ordering them. We add a cost of 1 to everything by prepending an empty
list of solutions, which indicates there are no solutions of cost 0.
Conceptually, we want:

------------------------------------------------------------------------
\f g e ->
   [[], mct $ zipWith (++) (inits $ repeat []) $ mct . map g <$> f e]
------------------------------------------------------------------------

This only works for finite cases, because we may wind up concatenating an
infinite list of `[]`. We know this does nothing, but the code doesn't! We
instead explicitly limit concatenation, resulting in something like a discrete
convolution applied to many lists:

\begin{code}
andB :: PredB -> PredB -> PredB
andB f g e = [] : map concat (conv [] $ mct . map g <$> f e) where
  conv acc xs = case xs of
    [] -> transpose acc
    (xsh:xst) -> hs : conv (xsh : ts) xst
    where
    (hs, ts) = unzip [(h, t) | h:t <- acc]

conB :: [PredB] -> PredB
conB = foldr1 andB
\end{code}

Generating a fresh variable remains the same, and with that, we have a
Prolog that performs breadth-first search:

\begin{code}
freshB :: (Exp -> PredB) -> PredB
freshB f = \(sub, v:vs) -> f (V $ '_':show v) (sub, vs)

prologB :: [Rule] -> Exp -> [Subst]
prologB rules q = pretty q <$> concat (f q pristine) where
  f q = disB $ (\r -> refresh [] (varsOf r) $ zip ((q ~~~):repeat f) r) <$> rules
  refresh sub vs zs = case vs of
    []     -> conB $ map (\(f, x) -> f $ apply sub x) zs
    (v:vt) -> freshB \w -> refresh ((v, w):sub) vt zs
\end{code}

Poorly written recursive rules in Prolog can get trapped in depth-first search.
Breadth-first search is more tolerant:

\begin{code}
wrong :: [Rule]
wrong =
    mustRule "ancestor A B :- parent A B"
  : mustRule "ancestor A B :- ancestor X B, parent A X"
  : grand

exampleWrong = prologB wrong $ mustFact "ancestor X eadwig"
\end{code}

The code still runs forever, but at least it finds all solutions:

------------------------------------------------------------------------
[[("X",C "edmund")],[("X",C "edward")],[("X",C "alfred")]
------------------------------------------------------------------------

Putting the output of a `Pred` in a singleton list to gives us a `PredB`.
Thus we can mix in our other functions and combine different search strategies.

== Program synthesis ==

We employ breadth-first search to find combinatory logic programs `F`
satisfying `F x y = y`.

\begin{code}
step :: Exp -> Exp -> PredB
step lhs rhs = freshB \x -> freshB \y -> freshB \z -> disB
  [ conB [lhs ~~~ (C "k" :@ x :@ y)     , rhs ~~~ x]
  , conB [lhs ~~~ (C "t" :@ x :@ y)     , rhs ~~~ (y :@ x)]
  , conB [lhs ~~~ (C "s" :@ x :@ y :@ z), rhs ~~~ (x :@ z :@ (y :@ z))]
  , conB [lhs ~~~ (C "b" :@ x :@ y :@ z), rhs ~~~ (x :@ (y :@ z))]
  , conB [lhs ~~~ (C "c" :@ x :@ y :@ z), rhs ~~~ (x :@ z :@ y)]
  , conB [lhs ~~~ (x :@ z), rhs ~~~ (y:@ z), step x y]
  ]
cl :: Exp -> Exp -> PredB
cl lhs rhs = disB
  [ step lhs rhs
  , freshB \x -> conB [step lhs x, cl x rhs]
  ]

elemConst :: String -> Exp -> Bool
elemConst s t = case t of
  C s' | s == s' -> True
  a :@ b ->  elemConst s a || elemConst s b
  _ -> False

exampleCL = filter (\[(_, t)] -> not (elemConst "y" t)) $ pretty ["F"] <$>
  concat (cl (mustFact "F x y") (mustFact "y") pristine)
\end{code}

------------------------------------------------------------------------
[[("F",C "s" :@ C "k")],
[("F",C "c" :@ C "k")],
[("F",C "k" :@ ((C "s" :@ C "k") :@ V "_9"))],
[("F",C "k" :@ ((C "c" :@ C "k") :@ V "_12"))],
[("F",(C "b" :@ (C "s" :@ C "k")) :@ V "_5")],
[("F",(C "b" :@ (C "c" :@ C "k")) :@ V "_5")],
[("F",C "s" :@ (C "c" :@ (C "s" :@ C "k")))],
[("F",C "s" :@ (C "c" :@ (C "c" :@ C "k")))],
[("F",C "c" :@ (C "c" :@ (C "s" :@ C "k")))],
[("F",C "c" :@ (C "c" :@ (C "c" :@ C "k")))],
[("F",(C "k" :@ (C "s" :@ C "k")) :@ V "_8")],
...
------------------------------------------------------------------------

== The solution to confusion is substitution ==

Long ago, I was underwhelmed by Prolog. Introductory examples suggested I could
declare facts and rules, then watch the computer magically answer my queries.
This was true, up to a point. But past this point, the programmer is forced to
peek under the hood, where it turns out a mundane depth-first search is running
the show.

We can break free of its limitations with "extra-logical" features such as
`cut`. http://minikanren.org/minikanren-and-prolog.html[Understanding side
effects even lets us perform breadth-first searches]. I admire the ingenuity,
but why not use a conventional language if I must muck with low-level details
anyway?

I'm happy that modern logic programming seems more principled.
https://en.wikipedia.org/wiki/Answer_set_programming[Answer set programming
(ASP)] really does magically answer queries after logical facts and rules are
declared. Instead of depth-first search, it uses state-of-the-art SAT-solver
techniques.

Researchers are also tidying up by investigating, for example,
https://gup.ub.gu.se/file/207634[how types can help], or
https://pdfs.semanticscholar.org/3e0a/ff0f1e287b06deb427d56a2696af8228dd61.pdf[how to apply
equational reasoning].

http://www.silvija.net/0000OxfordPublications/seres_thesis.pdf[Seres' thesis on
the algebra of logic programming] features multiple search strategies using
what we called "predicate combinators". The http://minikanren.org/[miniKanren
language family] seems similar. We took our `appendo` example from a miniKanren
demo.

https://www.youtube.com/watch?v=D7rlJWc3474[Edward Kmett's talk on Guanxi]
contains good references and applications. My initial impression is he is
combining all of the above: speedy SAT-solving techniques coupled with a set of
building blocks for rich data structures such as abstract syntax trees.

My starting point was
http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf[the paper on
microKanren], which uses a Lisp-like language. I found simplifications thanks
to Haskell's lazy evaluation, and list monad (only to discover later these
improvements were well-known). On the other hand, Lisp's homoiconicity is
certainly seductive.

The literature mentions monads frequently (there's even a
http://hackage.haskell.org/package/logict[LogicT monad transformer]) but I
shied away; I'm apprehensive about committing to particular choices for `(<|>)`
and `(>>=)`.
