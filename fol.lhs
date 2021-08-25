= First-order logic =

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src='fol.js'></script>
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
Formula:
<div>
<textarea id='inp' rows='4' style='box-sizing:border-box;width:100%;'>exists x. forall y. P(x) ==> P(y)</textarea></div>
<p>Output:</p>
<div><textarea id='out' rows='8' readonly style='box-sizing:border-box;width:100%;'></textarea></div>
<p>Log:</p>
<div><textarea id='log' rows='4' readonly style='box-sizing:border-box;width:100%;'></textarea></div>
<p>
Negate, then:
<button id='skolemize'>skolemize</button>
<button id='qff'>QFF</button>
<button id='dnf'>DNF</button>
<button id='cnf'>CNF</button>
<button id='defcnf'>Definitional CNF</button>
</p>
<p>
Prove:
<button id='gilmore'>Gilmore</button>
<button id='dpll'>DPLL</button>
<button id='tableau'>Tableau</button>
<button id='faithful'>MESON by the book</button>
<button id='meson'>Lazy MESON</button>
</p>
<p>
<span><button onclick='hideshow("presets");'>Presets</button>
<style>#presets {
position:absolute;
background-color:white;
border:1px solid grey;
padding:2px;
}#presets div:hover{
background-color:grey;
}
</style>
<div id='presets' style='display:none;'></div>
</span>
</p>
<p><a onclick='hideshow("sortdemo");'>&#9654; Sort demo</a></p>
<div id='sortdemo' style='display:none'>
<textarea id ='sortcode' rows='5' style='box-sizing:border-box;width:100%;'></textarea>
<br>
<button id='sort'>Run!</button>
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Boilerplate abounds in programs that manipulate syntax trees. Consider a
function transforming a particular kind of leaf node. With a typical tree data
type, we must add recursive calls for every recursive data constructor. If we
later add a recursive data constructor we must update the function.

Or consider annotating a syntax tree. The most obvious way is to copy the
syntax tree then add an extra annotation field for each data constructor.
For example, compare the definitions of `Expr` and `AnnExpr` in
https://github.com/ghc/ghc/blob/master/compiler/coreSyn/CoreSyn.hs[GHC's source
code].

https://github.com/ninegua/reduxer/blob/master/src/Lambda/Term.hs[Paul Hai Liu
showed me how to avoid code duplication]. The trick is to use recursion schemes
along with certain helper functions. With GHC's pattern synonyms extension,
our code resembles ordinary recursion.

We demonstrate by building classic theorem provers for first-order logic, by
taking a whirlwind tour through chapters 2 and 3 of John Harrison, 'Handbook of
Practical Logic and Automated Reasoning'.

++++++++++
<p><a onclick='hideshow("imports");'>&#9654; Toggle extensions and imports</a></p>
<div id='imports' style='display:none'>
++++++++++

\begin{code}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE CPP #-}
#ifdef ASTERIUS
import Asterius.Types
#endif
import Control.Monad.State
import Control.Monad.Writer
import Data.Char (isAlphaNum)
import Data.Foldable (asum)
import qualified Data.Map.Strict as M
import Data.List (delete, union, partition, find, maximumBy, intercalate, unfoldr)
import Data.Ord (comparing)
import qualified Data.Set as S
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
\end{code}

++++++++++
</div>
++++++++++

== Recursion Schemes ==

We represent 'terms' with an ordinary recursive data structure.
Terms consist of 'variables', 'constants', and 'functions'.
Constants are functions that take zero arguments.

\begin{code}
data Term = Var String | Fun String [Term] deriving (Eq, Ord)
\end{code}

We use a recursion scheme for the formulas of first-order predicate logic.
These are like propositional logic formulas, except:

  * An atomic proposition is a 'predicate': a string constant accompanied by a
  list of terms.
  * Subformulas may be 'quantified' by a 'universal' (`Forall`) or 'existential'
  (`Exists`) quantifier. A quantifier 'binds' a variable in the same manner
  as a lambda.

\begin{code}
data Quantifier = Forall | Exists deriving (Eq, Ord)
data Formula a = FTop | FBot | FAtom String [Term]
  | FNot a | FAnd a a | FOr a a | FImp a a | FIff a a
  | FQua Quantifier String a
  deriving (Eq, Ord, Functor, Foldable, Traversable)
\end{code}

A data type akin to the fixpoint combinator powers the recursion:

\begin{code}
data FO = FO (Formula FO) deriving (Eq, Ord)
\end{code}

Setting up pattern synonyms is worth the boilerplate:

\begin{code}
pattern Atom s ts = FO (FAtom s ts)
pattern Top = FO FTop
pattern Bot = FO FBot
pattern Not p = FO (FNot p)
pattern p :/\ q = FO (FAnd p q)
pattern p :\/ q = FO (FOr p q)
pattern p :==> q = FO (FImp p q)
pattern p :<=> q = FO (FIff p q)
pattern Qua q x p = FO (FQua q x p)
\end{code}

Next, functions to aid recursion.
I chose the name `unmap` because its type seems to be the inverse of `fmap`.

\begin{code}
bifix :: (a -> b) -> (b -> a) -> a
bifix f g = g $ f $ bifix f g
ffix :: Functor f => ((f a -> f b) -> a -> b) -> a -> b
ffix = bifix fmap
unmap :: (Formula FO -> Formula FO) -> FO -> FO
unmap h (FO t) = FO (h t)
\end{code}

See also
https://hackage.haskell.org/package/data-fix/docs/Data-Fix.html[the `Data.Fix`
package].

== Parsing and pretty-printing ==

Variables start with lowercase letters, while constants, functions, and
predicates start with uppercase letters. We treat `(<=)` as an infix binary
predicate for the sake of some of our examples below.

++++++++++
<p><a onclick='hideshow("parse");'>&#9654; Toggle parser and pretty-printer</a></p>
<div id='parse' style='display:none'>
++++++++++

\begin{code}
firstorderformula :: Parsec () String FO
firstorderformula = space *> iff where
  iff = foldr1 (:<=>) <$> sepBy1 impl (want "<=>")
  impl = foldr1 (:==>) <$> sepBy1 disj (want "==>")
  disj = foldl1 (:\/) <$> sepBy1 conj (want "\\/" <|> want "|")
  conj = foldl1 (:/\) <$> sepBy1 nope (want "/\\" <|> want "&")
  nope = do
    fs <- many (const Not <$> (want "~" <|> want "!"))
    t <- vip
    pure $ foldr (\_ x -> Not x) t fs
  vip = between (want "(") (want ")") firstorderformula
    <|> const Top <$> want "1"
    <|> const Bot <$> want "0"
    <|> qua
    <|> atom
  qua = do
    f <- Qua <$> (want "forall" *> pure Forall
             <|>  want "exists" *> pure Exists)
    vs <- some var
    fo <- want "." *> firstorderformula
    pure $ foldr f fo vs
  atom = (bin =<< Var <$> var) <|> do
    (f, xs) <- call
    option (Atom f xs) $ bin (Fun f xs)
  call = (,) <$> con <*> option [] (between (want "(") (want ")")
    $ sepBy term $ want ",")
  bin x = do
    want "<="
    y <- term
    pure $ Atom "<=" [x, y]
  term = Var <$> var <|> uncurry Fun <$> call
  var = (:) <$> lowerChar <*> (many alphaNumChar <* space)
  con = (:) <$> upperChar <*> (many alphaNumChar <* space)
  want :: String -> Parsec () String ()
  want s = try $ do
    s' <- (some (oneOf "<=>\\/&|~!")
      <|> some alphaNumChar
      <|> ((:[]) <$> oneOf "(),.")) <* space
    if s == s' then pure () else fail $ "want " <> s

mustFO :: String -> FO
mustFO = either (error . show) id . parse firstorderformula ""

instance Show Term where
  showsPrec d = \case
    Var s -> (s <>)
    Fun s xs -> (s <>) . showParen (not $ null xs) (showArgs xs)
    where
    showArgs = \case
      [] -> id
      (x:xt) -> showsPrec d x . showArgs' xt
    showArgs' xt = foldr (\f g -> (", " <>) . f . g) id $ showsPrec d <$> xt

instance Show FO where
  showsPrec d = \case
    Atom s ts
      | not (isAlphaNum $ head s), [p, q] <- ts -> showsPrec d p . ("<=" <>) . showsPrec d q
      | otherwise -> (s <>) . showParen (not $ null ts) ((<>) $ intercalate ", " $ show <$> ts)
    Top -> ('\8868':)
    Bot -> ('\8869':)
    Not p -> ('\172':) . showsPrec 6 p
    p :/\ q -> showParen (d > 5) $ showsPrec 5 p . ('\8743':) . showsPrec 5 q
    p :\/ q -> showParen (d > 4) $ showsPrec 4 p . ('\8744':) . showsPrec 4 q
    p :==> q -> showParen (d > 3) $ showsPrec 4 p . ('\8658':) . showsPrec 3 q
    p :<=> q -> showParen (d > 2) $ showsPrec 3 p . ('\8660':) . showsPrec 3 q
    Qua q x p -> showParen (d > 0) $ (charQ q:) . (x <>) . ('.':) . showsPrec 0 p
      where charQ Forall = '\8704'
            charQ Exists = '\8707'
\end{code}

++++++++++
</div>
++++++++++

== Free variables ==

To find the free variables of a `Term`, we must handle every data constructor
and make explicit recursive functions calls.
Fortunately, this data type only has two constructors: `Var` and `Fun`.

\begin{code}
fvt :: Term -> [String]
fvt = \case
  Var x -> [x]
  Fun _ xs -> foldr union [] $ fvt <$> xs
\end{code}

Contrast this with the `FO` edition.
Thanks to our recursion scheme, we write two special cases for `Atom` and `Qua`,
then a terse catch-all expression does the obvious for all other cases.

This includes, for example, recursively descending into both arguments of an
AND operator. Furthermore, if we add more operators to `Formula`, this code
handles them automatically.

\begin{code}
fv :: FO -> [String]
fv = ffix \h -> \case
  Atom _ ts -> foldr union [] $ fvt <$> ts
  Qua _ x p -> delete x $ fv p
  FO t -> foldr union [] $ h t
\end{code}

== Simplification ==

Thanks to pattern synonyms, recursion schemes are as easy as regular recursive
data types.

Again, we write special cases for the formulas we care about, along with
something perfunctory to deal with all other cases.

We `unmap h` before attempting rewrites because we desire bottom-up behaviour.
For example, the inner subformula in $\neg(x\wedge\bot)$ should first be
rewritten to yield $\neg\bot$ so that another rewrite rule can simplify this to
$\top$.

\begin{code}
simplify :: FO -> FO
simplify = ffix \h fo -> case unmap h fo of
  Not (Bot) -> Top
  Not (Top) -> Bot
  Not (Not p) -> p
  Bot :/\ _ -> Bot
  _ :/\ Bot -> Bot
  Top :/\ p -> p
  p :/\ Top -> p
  Top :\/ _ -> Top
  _ :\/ Top -> Top
  Bot :\/ p -> p
  p :\/ Bot -> p
  _ :==> Top -> Top
  Bot :==> _ -> Top
  Top :==> p -> p
  p :==> Bot -> Not p
  p :<=> Top -> p
  Top :<=> p -> p
  Bot :<=> Bot -> Top
  p :<=> Bot -> Not p
  Bot :<=> p -> Not p
  Qua _ x p | x `notElem` fv p -> p
  t -> t
\end{code}

== Negation normal form ==

A handful of rules transform a simplified formula to 'negation normal form'
(NNF), namely, the formula consists only of 'literals' (atoms or negated
atoms), conjunctions, disjunctions, and quantifiers.

This time, the recursion is top-down. We `unmap h` after the rewrite.

\begin{code}
nnf :: FO -> FO
nnf = ffix \h -> unmap h . \case
  p :==> q -> Not p :\/ q
  p :<=> q -> (p :/\ q) :\/ (Not p :/\ Not q)
  Not (Not p) -> p
  Not (p :/\ q) -> Not p :\/ Not q
  Not (p :\/ q) -> Not p :/\ Not q
  Not (p :==> q) -> p :/\ Not q
  Not (p :<=> q) -> (p :/\ Not q) :\/ (Not p :/\ q)
  Not (Qua Forall x p) -> Qua Exists x (Not p)
  Not (Qua Exists x p) -> Qua Forall x (Not p)
  t -> t
\end{code}

== Substitution ==

Again we pit recursion schemes against plain old data structures.
As before, the `Term` version must handle each case and its recursive calls are
explicitly spelled out, while the `FO` version only handles the cases it cares
about, provides a generic catch-all case, and relies on `ffix` and `unmap` to
recurse. They are about the same size despite `FO` having many more data
constructors.

This time, for variety, we `unmap h` in the catch-all case.
We could also place it just inside or outside the case expression as above.
It is irrelevant whether the recursion is top-down or bottom-up because
only leaves are affected.

\begin{code}
tsubst :: (String -> Maybe Term) -> Term -> Term
tsubst f t = case t of
  Var x -> maybe t id $ f x
  Fun s as -> Fun s $ tsubst f <$> as

subst :: (String -> Maybe Term) -> FO -> FO
subst f = ffix \h -> \case
  Atom s ts -> Atom s $ tsubst f <$> ts
  t -> unmap h t
\end{code}

== Skolemization ==

Skolemization transforms an NNF formula to an 'equisatisfiable' formula with no
existential quantifiers, that is, the output is satisifiable if and only if the
input is. Skolemization is "lossy" because validity might not be preserved.

We may need to mint new function names along the way. To avoid name clashes,
the `functions` helper returns all functions present in a given formula.
It also returns the arity of each function because we need this later to
enumerate ground terms.

It is possible to Skolemize a non-NNF formula, but if negations can go
anywhere, we may as well remove existential quantifiers by converting them to
universal quantifiers via duality and preserve logical equivalence.

\begin{code}
functions :: FO -> [(String, Int)]
functions = ffix \h -> \case
  Atom s ts -> foldr union [] $ funcs <$> ts
  FO t -> foldr union [] $ h t
  where
  funcs = \case
    Var x -> []
    Fun f xs -> foldr union [(f, length xs)] $ funcs <$> xs

skolemize :: FO -> FO
skolemize t = evalState (skolem' $ nnf $ simplify t) (fst <$> functions t) where
  skolem' :: FO -> State [String] FO
  skolem' fo = case fo of
    Qua Exists x p -> do
      fns <- get
      let
        xs = fv fo
        f = variant ((if null xs then "C_" else "F_") <> x) fns
        fx = Fun f $ Var <$> xs
      put $ f:fns
      skolem' $ subst (`lookup` [(x, fx)]) p
    FO t -> FO <$> mapM skolem' t
\end{code}

== Prenex normal form ==

We can pull all the quantifiers of an NNF formula to the front by generating
new variable names. This is known as 'prenex normal form' (PNF).

\begin{code}
variant :: String -> [String] -> String
variant s vs
  | s `elem` vs = variant (s <> "'") vs
  | otherwise   = s

prenex :: FO -> FO
prenex = ffix \h t -> let
  recursed = unmap h t
  f qua s g = Qua qua z $ prenex $ g \x -> subst (`lookup` [(x, Var z)])
    where z = variant s $ fv recursed
  in case recursed of
  Qua Forall x p :/\ Qua Forall y q -> f Forall x \r -> r x p :/\ r y q
  Qua Exists x p :\/ Qua Exists y q -> f Exists x \r -> r x p :\/ r y q
  Qua qua x p :/\ q -> f qua x \r -> r x p :/\ q
  p :/\ Qua qua y q -> f qua y \r -> p :/\ r y q
  Qua qua x p :\/ q -> f qua x \r -> r x p :\/ q
  p :\/ Qua qua y q -> f qua y \r -> p :\/ r y q
  t -> t

pnf :: FO -> FO
pnf = prenex . nnf . simplify
\end{code}

== Quantifier-free formulas ==

A 'quantifier-free' formula is one where every variable is free. Each variable
is implicitly universally quantified, that is, for each variable `x`, we behave
as if `forall x.` has been prepended to the formula.

We can remove all quantifiers from a skolemized NNF formula by pulling all the
universal quantifiers to the front and then dropping them.

Our `specialize` helper appears exactly as it would if `FO` were an ordinary
recursive data structure. Explicit recursion suits this function because we
only want to transform the top of the tree.

\begin{code}
deQuantify :: FO -> FO
deQuantify = specialize . pnf where
  specialize = \case
    Qua Forall x p -> specialize p
    t -> t
\end{code}

== Ground terms ==

A 'ground term' is a term containing no variables, that is, a term exclusively
built from constants and functions.

We describe how to enumerate all possible terms given a set of constants and
functions. For example, given `X, Y, F(_,_), G(_)`, we want to generate
something like:

------------------------------------------------------------------------
X, Y, F(X,X), G(X), F(X,Y), F(Y,X), F(Y,Y), G(Y), F(G(X),G(Y)), ...
------------------------------------------------------------------------

In general, there are infinite ground terms, but we can enumerate them in an
order that guarantees any given term will appear: start with the terms with no
functions, namely constant terms, then those that contain exactly one function
call, then those that contain exactly two function calls, and so on.

\begin{code}
groundTerms cons funs n
  | n == 0 = cons
  | otherwise = concatMap
    (\(f, m) -> Fun f <$> groundTuples cons funs m (n - 1)) funs

groundTuples cons funs m n
  | m == 0 = if n == 0 then [[]] else []
  | otherwise = [h:t | k <- [0..n], h <- groundTerms cons funs k,
                 t <- groundTuples cons funs (m - 1) (n - k)]
\end{code}

== Herbrand universe ==

The 'Herbrand universe' of a formula are the ground terms made from all the
constants and functions that appear in the formula, with one special case: if
no constants appear, then we invent one to avoid an empty universe.

For example, the Herbrand universe of:

\[
(P(y) \implies Q(F(z))) \wedge (P(G(x)) \vee Q(x))
\]

is:

------------------------------------------------------------------------
C, F(C), G(C), F(F(C)), F(G(C)), G(F(C)), G(G(C)), ...
------------------------------------------------------------------------

We add the constant `C` because there were no constants to begin with. Since
`P` and `Q` are predicates and not functions, they are not part of the Herbrand
universe.

\begin{code}
herbTuples :: Int -> FO -> [[Term]]
herbTuples m fo
  | null funs = groundTuples cons funs m 0
  | otherwise = concatMap (reverse . groundTuples cons funs m) [0..]
  where
  (cs, funs) = partition ((0 ==) . snd) $ functions fo
  cons | null cs   = [Fun "C" []]
       | otherwise = flip Fun [] . fst <$> cs
\end{code}

We reverse the output of `groundTuples` because it happens to work better on a
few test cases.

== Automated Theorem Proving ==

It can be shown a quantifier-free formula is satisfiable if and only if it is
satisfiable under a 'Herbrand interpretation'.
Loosely speaking, we treat terms like the abstract syntax trees that represent
them; if a theorem holds under some interpretation, then it also holds for
syntax trees.

Why? Intuitively, given a formula and an interpretation where it holds, we can
define a syntax tree based on the constants and functions of the formula, and
rig predicates on these trees to behave enough like their counterparts in the
interpretation.

For example, the formula $\forall x . x + 0 = x$ holds under many familiar
interpretations. Here's a Herbrand interpretation:

------------------------------------------------------------------------
data Term = Plus Term Term | Zero
eq :: (Term, Term) -> Bool
eq _ = True
------------------------------------------------------------------------

For our next example we take a formula that holds under interpretations such as
integer arithmetic:

\[
\forall x . Odd(x) \iff \neg Odd(Succ(x))
\]

Here's a Herbrand interpretation:

------------------------------------------------------------------------
data Term = Succ Term | C
odd :: Term -> Bool
odd = \case
  C -> False
  Succ x -> not $ odd x
------------------------------------------------------------------------

This important result suggests a strategy to prove any first-order formula `f`.
As a preprocessing step, we prepend explicit universal quantifiers for each
free variable:

\begin{code}
generalize fo = foldr (Qua Forall) fo $ fv fo
\end{code}

Then:

  1. Negate $f$ because validity and satisfiability are dual:
  the formula $f$ is valid if and only if $\neg f$ is unsatisfiable.
  2. Transform $\neg f$ to an equisatisfiable quantifier-free formula $t$.
  Let $m$ be the number of variables in $t$. Initialize $h$ to $\top$.
  3. Choose $m$ elements from the Herbrand universe of $t$.
  4. Let $t'$ be the result of substituting the variables of $t$ with
  these $m$ elements. Compute $h \leftarrow h \wedge t'$.
  5. If $h$ is unsatisfiable, then $t$ is unsatisfiable under any
  interpretation, hence $f$ is valid. Otherwise, go to step 3.

We have moved from first-order logic to propositional logic; the formula $h$
only contains ground terms which act as propositional variables when
determining satisfiability. In other words, we have
https://en.wikipedia.org/wiki/Boolean_satisfiability_problem[the classic SAT
problem].

If the given formula is valid, then this algorithm eventually finds a proof
provided the method we use to pick ground terms eventually selects any given
possibility. This is the case for our `groundTuples` function.

== Gilmore ==

It remains to detect unsatisfiability. One of the earliest approaches (Gilmore
1960) transforms a given formula to 'disjunctive normal form' (DNF):

\[
\bigvee_i \bigwedge_j x_{ij}
\]

where the $x_{ij}$ are literals. For example:
$(\neg a\wedge b\wedge c) \vee (d \wedge \neg e) \vee (f)$.

We represent a DNF formula as a set of sets of literals.
Given an NNF formula, the function `pureDNF` builds an equivalent DNF formula:

\begin{code}
distrib s1 s2 = S.map (uncurry S.union) $ S.cartesianProduct s1 s2

pureDNF = \case
  p :/\ q -> distrib (pureDNF p) (pureDNF q)
  p :\/ q -> S.union (pureDNF p) (pureDNF q)
  t -> S.singleton $ S.singleton t
\end{code}

Next, we eliminate conjunctions containing $\bot$ or the positive and negative
versions of the same literal, such as `P(C)` and `~P(C)`. The formula is
unsatisfiable if and only if nothing remains.

To reduce the formula size, we replace clauses containing $\top$ with the empty
clause (the empty conjunction is $\top$), and drop clauses that are supersets
of other clauses.

\begin{code}
nono = \case
  Not p -> p
  p -> Not p

isPositive = \case
  Not p -> False
  _ -> True

nontrivial lits = S.null $ S.intersection pos $ S.map nono neg
  where (pos, neg) = S.partition isPositive lits

simpDNF = \case
  Bot -> S.empty
  Top -> S.singleton S.empty
  fo -> let djs = S.filter nontrivial $ pureDNF $ nnf fo in
    S.filter (\d -> not $ any (`S.isProperSubsetOf` d) djs) djs
\end{code}

Now we fill in the other steps. Our main loop takes in 3 functions so we can
later try out different approaches to detecting unsatisfiable formulas.

We reverse the output of `groundTuples` because it happens to work better on a
few test cases.

\begin{code}
skno :: FO -> FO
skno = skolemize . nono . generalize

type Loggy = Writer ([String] -> [String])

output :: Show a => a -> IO ()
#ifdef ASTERIUS
output s = do
  out <- getElem "out"
  appendValue out $ show s <> "\n"
runThen cont wr = do
  let (a, w) = runWriter wr
  cb <- makeHaskellCallback $ stream cont (a, w [])
  js_setTimeout cb 0
foreign import javascript "wrapper" makeHaskellCallback :: IO () -> IO JSFunction
foreign import javascript "wrapper" makeHaskellCallback1 :: (JSObject -> IO ()) -> IO JSFunction
#else
output = print
runThen cont wr = do
  let (a, w) = runWriter wr
  mapM_ putStrLn $ w []
  cont a
#endif

herbrand conjSub refute uni fo = runThen output $ herbLoop (uni Top) [] herbiverse where
  qff = deQuantify . skno $ fo
  fvs = fv qff
  herbiverse = herbTuples (length fvs) qff
  t = uni qff
  herbLoop :: S.Set (S.Set FO) -> [[Term]] -> [[Term]] -> Loggy [[Term]]
  herbLoop h tried = \case
    [] -> error "invalid formula"
    (tup:tups) -> do
      tell (concat
        [ show $ length tried, " ground instances tried; "
        , show $ length h," items in list"
        ]:)
      let h' = conjSub t (subst (`M.lookup` (M.fromList $ zip fvs tup))) h
      if refute h' then pure $ tup:tried else herbLoop h' (tup:tried) tups

gilmore = herbrand conjDNF S.null simpDNF where
  conjDNF djs0 sub djs = S.filter nontrivial (distrib (S.map (S.map sub) djs0) djs)
\end{code}

== Davis-Putnam ==

The DPLL algorithm uses the 'conjunctive normal form' (CNF), which is the dual
of DNF:

\begin{code}
pureCNF = S.map (S.map nono) . pureDNF . nnf . nono
\end{code}

Constructing a CNF formula in this manner is potentially expensive, but at
least we only pay the cost once. The main loop just piles on more conjunctions.

As with DNF, we simplify:

\begin{code}
simpCNF = \case
  Bot -> S.singleton S.empty
  Top -> S.empty
  fo -> let cjs = S.filter nontrivial $ pureCNF fo in
    S.filter (\c -> not $ any (`S.isProperSubsetOf` c) cjs) cjs
\end{code}

We write DPLL functions and pass them to `herbrand`:

\begin{code}
oneLiteral clauses = do
  u <- S.findMin <$> find ((1 ==) . S.size) (S.toList clauses)
  Just $ S.map (S.delete (nono u)) $ S.filter (u `S.notMember`) clauses

affirmativeNegative clauses
  | S.null oneSided = Nothing
  | otherwise       = Just $ S.filter (S.disjoint oneSided) clauses
  where
  (pos, neg') = S.partition isPositive $ S.unions clauses
  neg = S.map nono neg'
  posOnly = pos S.\\ neg
  negOnly = neg S.\\ pos
  oneSided = posOnly `S.union` S.map nono negOnly

dpll clauses
  | S.null clauses = True
  | S.empty `S.member` clauses = False
  | otherwise = rule1
  where
  rule1 = maybe rule2 dpll $ oneLiteral clauses
  rule2 = maybe rule3 dpll $ affirmativeNegative clauses
  rule3 = dpll (S.insert (S.singleton p) clauses)
       || dpll (S.insert (S.singleton $ nono p) clauses)
  pvs = S.filter isPositive $ S.unions clauses
  p = maximumBy (comparing posnegCount) $ S.toList pvs
  posnegCount lit = S.size (S.filter (lit `elem`) clauses)
                  + S.size (S.filter (nono lit `elem`) clauses)

davisPutnam = herbrand conjCNF (not . dpll) simpCNF where
  conjCNF cjs0 sub cjs = S.union (S.map (S.map sub) cjs0) cjs
\end{code}

== Definitional CNF ==

We can efficiently translate any formula to an equisatisfiable CNF formula with
a definitional approach. Logical equivalence may not be preserved, but only
satisfiability matters, and in any case Skolemization may not preserve
equivalence.

We need a variant of NNF that preserves equivalences:

\begin{code}
nenf :: FO -> FO
nenf = nenf' . simplify where
  nenf' = ffix \h -> unmap h . \case
    p :==> q -> Not p :\/ q
    Not (Not p) -> p
    Not (p :/\ q) -> Not p :\/ Not q
    Not (p :\/ q) -> Not p :/\ Not q
    Not (p :==> q) -> p :/\ Not q
    Not (p :<=> q) -> p :<=> Not q
    Not (Qua Forall x p) -> Qua Exists x (Not p)
    Not (Qua Exists x p) -> Qua Forall x (Not p)
    t -> t
\end{code}

Then, for each node with two children, we mint a 0-ary predicate that acts as
its definition:

\begin{code}
satCNF fo = S.unions $ simpCNF p
  : map (simpCNF . uncurry (:<=>)) (M.assocs ds)
  where
  (p, (ds, _)) = runState (sat' $ nenf fo) (mempty, 0)
  sat' :: FO -> State (M.Map FO FO, Int) FO
  sat' = \case
    p :/\ q  -> def =<< (:/\)  <$> sat' p <*> sat' q
    p :\/ q  -> def =<< (:\/)  <$> sat' p <*> sat' q
    p :<=> q -> def =<< (:<=>) <$> sat' p <*> sat' q
    p        -> pure p
  def :: FO -> State (M.Map FO FO, Int) FO
  def t = do
    (ds, n) <- get
    case M.lookup t ds of
      Nothing -> do
        let v = Atom ("*" <> show n) []
        put (M.insert t v ds, n + 1)
        pure v
      Just v -> pure v
\end{code}

We define another DPLL prover using this definitional CNF algorithm:

\begin{code}
davisPutnam2 = herbrand conjCNF (not . dpll) satCNF where
  conjCNF cjs0 sub cjs = S.union (S.map (S.map sub) cjs0) cjs
\end{code}

== Unification ==

To refute $P(F(x), G(A)) \wedge \neg P(F(B), y)$, the above algorithms would
have to luck out and select, say, $(x, y) = (B, G(A))$.

Unification finds this assignment intelligently. This observation inspired a
more efficient approach to theorem proving.

Harrison's implementation of unification differs from that of Jones.
Accounting for existing substitutions is deferred until variable binding, where
we perform the occurs check, as well as a redundancy check. However, perhaps
lazy evaluation means the two approaches are more similar than they appear.

\begin{code}
istriv env x = \case
  Var y | y == x -> Right True
        | Just v <- M.lookup y env -> istriv env x v
        | otherwise -> Right False
  Fun _ args -> do
    b <- or <$> mapM (istriv env x) args
    if b then Left "cyclic"
         else Right False

unify env = \case
  [] -> Right env
  h:rest -> case h of
    (Fun f fargs, Fun g gargs)
      | f == g, length fargs == length gargs -> unify env $ zip fargs gargs <> rest
      | otherwise -> Left "impossible unification"
    (Var x, t)
      | Just v <- M.lookup x env -> unify env $ (v, t):rest
      | otherwise -> do
        b <- istriv env x t
        unify (if b then env else M.insert x t env) rest
    (t, Var x) -> unify env $ (Var x, t):rest
\end{code}

As well as terms, unification in first-order logic must also handle literals,
that is, predicates and their negations.

\begin{code}
literally nope f = \case
  (Atom p1 a1, Atom p2 a2) -> f [(Fun p1 a1, Fun p2 a2)]
  (Not p, Not q) -> literally nope f (p, q)
  _ -> nope

unifyLiterals = literally (Left "Can't unify literals") . unify
\end{code}

== Tableaux ==

After Skolemizing, we recurse on the structure of the formula, gathering
literals that must all play nice together, and branching when necessary.
If one literal in our collection unifies with the negation of another, then the
current branch is refuted. The theorem is proved once all branches are refuted.

When we encounter a universal quantifier, we instantiate a new variable then
move the subformula to the back of the list in case we need it again. This
creates tension. On the one hand, we want new variables so we can find
unifications to refute branches. On the other hand, it may be better to move on
and look for a literal that is easier to contradict.

Iterative deepening comes to our rescue. We bound the number of variables we
may instantiate to avoid getting lost in the weeds. If the search fails, we
bump up the bound and try again.

(We mean "branching" in a yak-shaving sense, that is, while trying to refute A,
we find we must also refute B, so we add B to our to-do list. At a higher
level, there is another sense of branching where we realize we made the wrong
decision so we have to undo it and try again; we call this 'backtracking'.)

\begin{code}
deepen :: (Show t, Num t) => (t -> Either b c) -> t -> Loggy c
deepen f n = do
  tell (("Searching with depth limit " <> show n):)
  either (const $ deepen f (n + 1)) pure $ f n

tabRefute fos = deepen (\n -> go n fos [] Right (mempty, 0)) 0 where
  go n fos lits cont (env,k)
    | n < 0 = Left "no proof at this level"
    | otherwise = case fos of
      [] -> Left "tableau: no proof"
      h:rest -> case h of
        p :/\ q -> go n (p:q:rest) lits cont (env,k)
        p :\/ q -> go n (p:rest) lits (go n (q:rest) lits cont) (env,k)
        Qua Forall x p -> let
          y = Var $ '_':show k
          p' = subst (`lookup` [(x, y)]) p
          in go (n - 1) (p':rest <> [h]) lits cont (env,k+1)
        lit -> asum ((\l -> cont =<< (, k) <$>
            unifyLiterals env (lit, nono l)) <$> lits)
          <|> go n rest (lit:lits) cont (env,k)

tableau fo = runThen output $ case skno fo of
  Bot -> pure (mempty, 0)
  sfo -> tabRefute [sfo]
\end{code}

Some problems can be split up into the disjunction of independent subproblems,
which we can solve individually:

\begin{code}
splitTableau = map (tabRefute . S.toList) . S.toList . simpDNF . skno
\end{code}

== Connection Tableaux ==

We can view tableaux as a lazy CNF-based algorithm. We convert to CNF as we go,
stopping immediately after showing unsatisifiability. In particular, our search
is driven by the order in which clauses appear in the input formula.

Perhaps it is wiser to select clauses less arbitrarily. How about requiring the
next clause we examine to be somehow connected to the current clause? For
example, maybe we should insist a certain literal in the current clause unifies
with the negation of a literal in the next.

With this in mind, we arrive at Prolog-esque unification and backtracking, but
with a couple of tweaks so that it works on any CNF formula rather than merely
on a bunch of Horn clauses:

  1. Employ iterative deepening instead of a depth-first search.
  2. Look for conflicting subgoals.

Any unsatisfiable CNF formula must contain a clause containing only negative
literals. We pick one to start the refutation.

\begin{code}
selections bs = unfoldr (\(as, bs) -> case bs of
  [] -> Nothing
  b:bt -> Just ((b, as <> bt), (b:as, bt))) ([], bs)

instantiate :: [FO] -> Int -> ([FO], Int)
instantiate fos k = (subst (`M.lookup` (M.fromList $ zip vs names)) <$> fos, k + length vs)
  where
  vs = foldr union [] $ fv <$> fos
  names = Var . ('_':) . show <$> [k..]

conTab clauses = deepen (\n -> go n clauses [] Right (mempty, 0)) 0 where
  go n cls lits cont (env, k)
    | n < 0 = Left "too deep"
    | otherwise = case lits of
      [] -> asum [branch ls (env, k) | ls <- cls, all (not . isPositive) ls]
      lit:litt -> let nlit = nono lit in asum (contra nlit <$> litt)
        <|> asum [branch ps =<< (, k') <$> unifyLiterals env (nlit, p)
        | cl <- cls, let (cl', k') = instantiate cl k, (p, ps) <- selections cl']
    where
    branch ps = foldr (\l f -> go (n - length ps) cls (l:lits) f) cont ps
    contra p q = cont =<< (, k) <$> unifyLiterals env (p, q)

mesonBasic fo = runThen output $ conTab
  $ S.toList <$> S.toList (simpCNF $ deQuantify $ skno fo)
\end{code}

We translate a Prolog sorting program and query to CNF to illustrate the
correspondence.

\begin{code}
sortExample = intercalate " & "
  [ "(Sort(x0,y0) | !Perm(x0,y0) | !Sorted(y0))"
  , "Sorted(Nil)"
  , "Sorted(C(x1, Nil))"
  , "(Sorted(C(x2, C(y2, z2))) | !(x2 <= y2) | !Sorted(C(y2,z2)))"
  , "Perm(Nil,Nil)"
  , "(Perm(C(x3, y3), C(u3, v3)) | !Delete(u3,C(x3,y3),z3) | !Perm(z3,v3))"
  , "Delete(x4,C(x4,y4),y4)"
  , "(Delete(x5,C(y5,z5),C(y5,w5)) | !Delete(x5,z5,w5))"
  , "Z <= x6"
  , "(S(x7) <= S(y7) | !(x7 <= y7))"
  , "!Sort(C(S(S(S(S(Z)))), C(S(Z), C(Z,C(S(S(Z)), C(S(Z), Nil))))), x8)"
  ]

prologgy fo = conTab $ S.toList <$> S.toList (simpCNF fo)

tsubst' :: (String -> Maybe Term) -> Term -> Term
tsubst' f t = case t of
  Var x -> maybe t (tsubst' f) $ f x
  Fun s as -> Fun s $ tsubst' f <$> as

sortDemo = runThen prSub $ prologgy $ mustFO sortExample where
  prSub (m, _) = output $ tsubst' (`M.lookup` m) $ Var "x8"
\end{code}

We refine `mesonBasic` by aborting whenever the current subgoal is equal to an
older subgoal under the substitutions found so far. In addition, as with
`splitTableau`, we split the problem into smaller independent subproblems when
possible.

\begin{code}
equalUnder :: M.Map String Term -> [(Term, Term)] -> Bool
equalUnder env = \case
  [] -> True
  h:rest -> case h of
    (Fun f fargs, Fun g gargs)
      | f == g, length fargs == length gargs -> equalUnder env $ zip fargs gargs <> rest
      | otherwise -> False
    (Var x, t)
      | Just v <- M.lookup x env -> equalUnder env $ (v, t):rest
      | otherwise -> either (const False) id $ istriv env x t
    (t, Var x) -> equalUnder env $ (Var x, t):rest

noRep n cls lits cont (env, k)
  | n < 0 = Left "too deep"
  | otherwise = case lits of
    [] -> asum [branch ls (env, k) | ls <- cls, all (not . isPositive) ls]
    lit:litt
      | any (curry (literally False $ equalUnder env) lit) litt -> Left "repetition"
      | otherwise -> let nlit = nono lit in asum (contra nlit <$> litt)
        <|> asum [branch ps =<< (, k') <$> unifyLiterals env (nlit, p)
        | cl <- cls, let (cl', k') = instantiate cl k, (p, ps) <- selections cl']
  where
  branch ps = foldr (\l f -> noRep (n - length ps) cls (l:lits) f) cont ps
  contra p q = cont =<< (, k) <$> unifyLiterals env (p, q)

meson fos = mapM_ (runThen output) $ map (messy . listConj) $ S.toList <$> S.toList (simpDNF $ skno fos)
  where
  messy fo = deepen (\n -> noRep n (toCNF fo) [] Right (mempty, 0)) 0
  toCNF = map S.toList . S.toList . simpCNF . deQuantify
  listConj = foldr1 (:/\)
\end{code}

Due to a misunderstanding, our code applies the depth limit differently to the
book. Recall in our `tableau` function, the two branches of a disjunction
receive the same quota for new variables. I had thought the same was true for
the branches of `meson`, and that is what appears above.

I later learned the quota is meant to be shared among all subgoals. I wrote a
version more faithful to the original `meson` (our demo calls it "MESON by the
book"). It turns out to be slow.

++++++++++
<p><a onclick='hideshow("limited");'>&#9654; Toggle faithful rendition</a></p>
<div id='limited' style='display:none'>
++++++++++
\begin{code}
faith' cls lits cont (budget, (env, k))
  | budget < 0 = Left "too deep"
  | otherwise = case lits of
    [] -> asum [branch ls cont (budget, (env, k)) | ls <- cls, all (not . isPositive) ls]
    lit:litt
      | any (curry (literally False $ equalUnder env) lit) litt -> Left "repetition"
      | otherwise -> let nlit = nono lit in asum (contra nlit <$> litt)
        <|> asum [branch ps cont =<< (,) budget . (, k') <$> unifyLiterals env (nlit, p)
        | cl <- cls, let (cl', k') = instantiate cl k, (p, ps) <- selections cl']
  where
  contra p q = cont =<< (,) budget . (, k) <$> unifyLiterals env (p, q)
  branch ps cont (n, ek) = branch' ps cont (n - length ps, ek)
  branch' ps cont (n, ek)
    | n < 0 = Left "too deep"
    | m <= 1 = foldr (\l f -> faith' cls (l:lits) f) cont ps (n, ek)
    | otherwise = expand cont ek ps1 n1 ps2 n2 (-1) <|> expand cont ek ps2 n1 ps1 n2 n1
    where
    m = length ps
    n1 = n `div` 2
    n2 = n - n1
    (ps1, ps2) = splitAt (m `div` 2) ps
  expand cont ek goals1 n1 goals2 n2 n3 =
    branch' goals1 (\(r1, ek1) ->
    branch' goals2 (\(r2, ek2) ->
    if n2 + r1 <= n3 + r2 then Left "pair"
    else cont (r2, ek2))
    (n2+r1, ek1))
    (n1, ek)

faithful fos = mapM_ (runThen output) $ map (messy . listConj) $ S.toList <$> S.toList (simpDNF $ skno fos)
  where
  messy fo = deepen (\n -> faith' (toCNF fo) [] Right (n, (mempty, 0))) 0
  toCNF = map S.toList . S.toList . simpCNF . deQuantify
  listConj = foldr1 (:/\)
\end{code}
++++++++++
</div>
++++++++++

== Results ==

Our `gilmore` and `davisPutnam` functions perform better than the book
suggests they should. In particular, `gilmore p20` finishes quickly.

I downloaded https://www.cl.cam.ac.uk/~jrh13/atp/index.html[Harrison's source
code] for a sanity check, and found the original `gilmore` implementation
easily solves `p20`. It seems the book is mistaken; perhaps the code was buggy
at the time.

The original source also contains several test cases:

++++++++++
<p><a onclick='hideshow("problems");'>&#9654; Toggle problems</a></p>
<div id='problems' style='display:none'>
++++++++++
\begin{code}
p18 = mustFO "exists x. forall y. P(x) ==> P(y)"
p19 = mustFO "exists x. forall y z. (P(y) ==> Q(z)) ==> (P(x) ==> Q(x))"
p20 = mustFO "(forall x y. exists z. forall w. P(x) & Q(y) ==> R(z) & U(w)) ==> (exists x y. P(x) & Q(y)) ==> (exists z. R(z))"
p21 = mustFO "(exists x. P ==> F(x)) & (exists x. F(x) ==> P) ==> (exists x. P <=> F(x))"
p22 = mustFO "(forall x. P <=> F(x)) ==> (P <=> forall x. F(x))"
p24 = mustFO "~(exists x. U(x) & Q(x)) & (forall x. P(x) ==> Q(x) | R(x)) & ~(exists x. P(x) ==> (exists x. Q(x))) & (forall x. Q(x) & R(x) ==> U(x)) ==> (exists x. P(x) & R(x))"
p26 = mustFO "((exists x. P(x)) <=> exists x. Q(x)) & (forall x y. P(x) & Q(y) ==> (R(x) <=> S(y))) ==> ((forall x. P(x) ==> R(x)) <=> forall x. Q(x) ==> S(x))"
p29 = mustFO "(exists x. P(x)) & (exists x. G(x)) ==> ((forall x. P(x) ==> H(x)) & (forall x. G(x) ==> J(x)) <=> (forall x y. P(x) & G(y) ==> H(x) & J(y)))"
p34 = mustFO "((exists x. forall y. P(x) <=> P(y)) <=> ((exists x. Q(x)) <=> (forall y. Q(y)))) <=> ((exists x. forall y. Q(x) <=> Q(y)) <=> ((exists x. P(x)) <=> (forall y. P(y))))"
p35 = mustFO "exists x y . (P(x, y) ==> forall x y. P(x, y))"
p38 = mustFO "(forall x. P(a) & (P(x) ==> (exists y. P(y) & R(x,y))) ==> (exists z w. P(z) & R(x,w) & R(w,z))) <=> (forall x. (~P(a) | P(x) | (exists z w. P(z) & R(x,w) & R(w,z))) & (~P(a) | ~(exists y. P(y) & R(x,y)) | (exists z w. P(z) & R(x,w) & R(w,z))))"
p39 = mustFO "~exists x. forall y. F(y, x) <=> ~F(y, y)"
p42 = mustFO "~exists y. forall x. (F(x,y) <=> ~exists z.F(x,z) & F(z,x))"
p45 = mustFO "(forall x. P(x) & (forall y. G(y) & H(x,y) ==> J(x,y)) ==> (forall y. G(y) & H(x,y) ==> R(y))) & ~(exists y. L(y) & R(y)) & (exists x. P(x) & (forall y. H(x,y) ==> L(y)) & (forall y. G(y) & H(x,y) ==> J(x,y))) ==> (exists x. P(x) & ~(exists y. G(y) & H(x,y)))"
steamroller = mustFO "((forall x. P1(x) ==> P0(x)) & (exists x. P1(x))) & ((forall x. P2(x) ==> P0(x)) & (exists x. P2(x))) & ((forall x. P3(x) ==> P0(x)) & (exists x. P3(x))) & ((forall x. P4(x) ==> P0(x)) & (exists x. P4(x))) & ((forall x. P5(x) ==> P0(x)) & (exists x. P5(x))) & ((exists x. Q1(x)) & (forall x. Q1(x) ==> Q0(x))) & (forall x. P0(x) ==> (forall y. Q0(y) ==> R(x,y)) | ((forall y. P0(y) & S0(y,x) & (exists z. Q0(z) & R(y,z)) ==> R(x,y)))) & (forall x y. P3(y) & (P5(x) | P4(x)) ==> S0(x,y)) & (forall x y. P3(x) & P2(y) ==> S0(x,y)) & (forall x y. P2(x) & P1(y) ==> S0(x,y)) & (forall x y. P1(x) & (P2(y) | Q1(y)) ==> ~(R(x,y))) & (forall x y. P3(x) & P4(y) ==> R(x,y)) & (forall x y. P3(x) & P5(y) ==> ~(R(x,y))) & (forall x. (P4(x) | P5(x)) ==> exists y. Q0(y) & R(x,y)) ==> exists x y. P0(x) & P0(y) & exists z. Q1(z) & R(y,z) & R(x,y)"

los = mustFO "(forall x y z. P(x,y) & P(y,z) ==> P(x,z)) & (forall x y z. Q(x,y) & Q(y,z) ==> Q(x,z)) & (forall x y. Q(x,y) ==> Q(y,x)) & (forall x y. P(x,y) | Q(x,y)) ==> (forall x y. P(x,y)) | (forall x y. Q(x,y))"
dpEx = mustFO "exists x. exists y. forall z. (F(x,y) ==> (F(y,z) & F(z,z))) & ((F(x,y) & G(x,y)) ==> (G(x,z) & G(z,z)))"
ewd1062 = mustFO "(forall x. x <= x) & (forall x y z. x <= y & y <= z ==> x <= z) & (forall x y. F(x) <= y <=> x <= G(y)) ==> (forall x y. x <= y ==> F(x) <= F(y)) & (forall x y. x <= y ==> G(x) <= G(y))"
gilmore_1 = mustFO "exists x. forall y z. ((F(y) ==> G(y)) <=> F(x)) & ((F(y) ==> H(y)) <=> G(x)) & (((F(y) ==> G(y)) ==> H(y)) <=> H(x)) ==> F(z) & G(z) & H(z)"
gilmore_9 = mustFO "forall x. exists y. forall z.  ((forall u. exists v. F(y,u,v) & G(y,u) & ~H(y,x)) ==> (forall u. exists v. F(x,u,v) & G(z,u) & ~H(x,z)) ==> (forall u. exists v. F(x,u,v) & G(y,u) & ~H(x,y))) & ((forall u. exists v. F(x,u,v) & G(y,u) & ~H(x,y)) ==> ~(forall u. exists v. F(x,u,v) & G(z,u) & ~H(x,z)) ==> (forall u. exists v. F(y,u,v) & G(y,u) & ~H(y,x)) & (forall u. exists v. F(z,u,v) & G(y,u) & ~H(z,y)))"
\end{code}

++++++++++
</div>
++++++++++

Our code sometimes takes a better path through the Herbrand universe than the
original. For example, our `davisPutnam` goes through 111 ground instances to
solve `p29` while the book version goes through 180.

Curiously, if we leave the output of our `groundtuples` unreversed, then
`gilmore p20` seems intractable.

Our `davisPutnam2` function is unreasonably effective.
Definitional CNF suits DPLL by producing fewer clauses and fewer literals per
clause, so rules fire more frequently.
The vaunted `p38` and even the dreaded `steamroller` ("216 ground instances
tried; 497 items in list") lie within its reach. The latter may be too
exhausting for a browser and should be confirmed with GHC.
Assuming https://nixos.org/nix/download.html[Nix is installed]:

------------------------------------------------------------------------
$ nix-shell -p "haskell.packages.ghc881.ghcWithPackages (pkgs: [pkgs.megaparsec])"
$ wget https://crypto.stanford.edu/~blynn/compiler/fol.lhs
$ ghci fol.lhs
------------------------------------------------------------------------

then type `davisPutnam2 steamroller` at the prompt.

Definitional CNF hurts our connection tableaux solvers. It introduces new
literals which only appear a few times each. Our code fails to take advantage
of this to quickly find unifiable literals.

Our connection tableaux functions `mesonBasic` and `meson` are mysteriously
miraculous. Running `mesonBasic steamroller` succeeds at depth 21, and `meson
gilmore1` at depth 13, though our usage of the depth limit differs from that in
the book.

They are so fast that I was certain there was a bug. After extensive tracing,
I've concluded laziness is the root cause. Although our `meson` appears to be a
reasonably direct translation of the OCaml version, Haskell's lazy evaluation
means we memoize expensive computations, and distributing the size bound among
subgoals erases these gains.

The first time the `cont` continuation is reached, certain reductions remain on
the heap so the next time we reach it, we can avoid repeating expensive
computations. It is true that each `cont` invocation gets its own `(env,k)`,
but we can accomplish a lot without looking at them, such as determining that
two literals cannot unify because the outermost function names differ.

We can push further and memoize more. Here's an obvious way to filter out
candidates that could never unify:

\begin{code}
couldMatchTerms = \case
  [] -> True
  h:rest -> case h of
    (Fun f fargs, Fun g gargs)
      | f == g, length fargs == length gargs -> couldMatchTerms $ zip fargs gargs <> rest
      | otherwise -> False
    _ -> True
couldMatch x y = case (x, y) of
  (Atom p1 a1, Atom p2 a2) -> couldMatchTerms [(Fun p1 a1, Fun p2 a2)]
  (Not p, Not q) -> couldMatch p q
  _ -> False

noRep' n cls lits cont (env, k)
  | n < 0 = Left "too deep"
  | otherwise = case lits of
    [] -> asum [branch ls (env, k) | ls <- cls, all (not . isPositive) ls]
    lit:litt
      | any (curry (literally False $ equalUnder env) lit) $ filter (couldMatch lit) litt -> Left "repetition"
      | otherwise -> let nlit = nono lit in asum (contra nlit <$> filter (couldMatch nlit) litt)
        <|> asum [branch ps =<< (, k') <$> unifyLiterals env (nlit, p)
        | cl <- cls, let (cl', k') = instantiate cl k, (p, ps) <- selections cl', couldMatch nlit p]
  where
  branch ps = foldr (\l f -> noRep' (n - length ps) cls (l:lits) f) cont ps
  contra p q = cont =<< (, k) <$> unifyLiterals env (p, q)

meson' fos = mapM_ (runThen output) $ map (messy . listConj) $ S.toList <$> S.toList (simpDNF $ skno fos)
  where
  messy fo = deepen (\n -> noRep' n (toCNF fo) [] Right (mempty, 0)) 0
  toCNF = map S.toList . S.toList . simpCNF . deQuantify
  listConj = foldr1 (:/\)
\end{code}

This is about 5% faster, despite wastefully traversing the same literals once
for `couldMatch` and another time for `unifyLiterals`. It would be better if
`unifyLiterals` could use what `couldMatch` has already learned.

Perhaps better still would be to divide the substitutions into those that are
known when the continuation is created, and those that are not.
Then `couldMatch` can take the first set into account while still being
memoizable.

== Front-end ==

We compile to wasm with https://github.com/tweag/asterius[Asterius].

++++++++++
<p><a onclick='hideshow("ui");'>&#9654; Toggle front-end</a></p>
<div id='ui' style='display:none'>
++++++++++
\begin{code}
parsePresets line = (k, v) where
  (k, rhs) = break (== ' ') line
  v = init $ tail $ dropWhile (/= '"') rhs

#ifdef ASTERIUS
appendLog s = do
  x <- getElem "log"
  appendValue x $ s <> "\n"
  scrollToBottom x

stream cont (a, xs) = case xs of
  [] -> cont a
  (x:xt) -> do
    appendLog x
    cb <- makeHaskellCallback $ stream cont (a, xt)
    js_setTimeout cb 0

setProp :: JSVal -> String -> String -> IO ()
setProp e k v = js_setProperty e (toJSString k) (toJSString v)
getProp :: JSVal -> String -> IO String
getProp e k = fromJSString <$> js_getProperty e (toJSString k)
getElem :: String -> IO JSVal
getElem k = js_getElementById (toJSString k)
addEventListener :: JSVal -> String -> (JSObject -> IO ()) -> IO ()
addEventListener target event handler = do
  callback <- makeHaskellCallback1 handler
  js_addEventListener target (toJSString event) callback
createElem :: String -> IO JSVal
createElem = js_createElement . toJSString
appendValue :: JSVal -> String -> IO ()
appendValue e s = js_appendValue e $ toJSString s

foreign import javascript "document.getElementById($1)"
  js_getElementById :: JSString -> IO JSVal
foreign import javascript "$1[$2] = $3"
  js_setProperty :: JSVal -> JSString -> JSString -> IO ()
foreign import javascript "$1[$2]"
  js_getProperty :: JSVal -> JSString -> IO JSString
foreign import javascript "$1.addEventListener($2,$3)"
  js_addEventListener :: JSVal -> JSString -> JSFunction -> IO ()
foreign import javascript "document.createElement($1)"
  js_createElement :: JSString -> IO JSVal
foreign import javascript "$1.appendChild($2)"
  appendChild :: JSVal -> JSVal -> IO ()
foreign import javascript "$1.value += $2"
  js_appendValue :: JSVal -> JSString -> IO ()
foreign import javascript "setTimeout($1,$2)"
  js_setTimeout :: JSFunction -> Int -> IO ()
foreign import javascript "$1.scrollTop = $1.scrollHeight"
  scrollToBottom :: JSVal -> IO ()

main :: IO ()
main = do
  tx <- flip getProp "innerText" =<< getElem "problems"
  pr <- getElem "presets"
  inp <- getElem "inp"
  out <- getElem "out"
  log <- getElem "log"
  x <- getElem "sortcode"
  setProp x "value" sortExample
  let
    addPreset (k, v) = do
      a <- createElem "div"
      setProp a "innerHTML" k
      addEventListener a "click" $ const $ do
        setProp out "value" ""
        setProp log "value" ""
        setProp inp "value" v
        setProp pr "style" "display:none;"
      appendChild pr a
    addSolver buttonId solver = do
      b <- getElem buttonId
      addEventListener b "click" $ const $ do
        setProp out "value" ""
        setProp log "value" ""
        s <- getProp inp "value"
        case parse firstorderformula "" s of
          Left e -> do
            appendLog "parse error"
            appendLog $ show e
          Right fo -> solver fo *> pure ()
  mapM_ addPreset $ map parsePresets . filter (not . null) . lines $ tx
  addSolver "skolemize" $ output . skno
  addSolver "qff" $ output . deQuantify . skno
  addSolver "dnf" $ output . simpDNF . deQuantify . skno
  addSolver "cnf" $ output . simpCNF . deQuantify . skno
  addSolver "defcnf" $ output . satCNF . deQuantify . skno
  addSolver "gilmore" gilmore
  addSolver "dpll" davisPutnam2
  addSolver "tableau" tableau
  addSolver "faithful" faithful
  addSolver "meson" meson
  sortButton <- getElem "sort"
  addEventListener sortButton "click" $ const $ do
    setProp inp "value" ""
    setProp out "value" ""
    setProp log "value" ""
    sortDemo
#endif
\end{code}

++++++++++
</div>
++++++++++

To force the browser to render log updates, we use zero-duration timeouts. The
change in control flow means that the web version of `meson` interleaves the
refutations of independent subformulas.

We may be running into https://github.com/tweag/asterius/issues/696[an Asterius
bug involving callbacks and garbage collection]. The callbacks created in the
`stream` function are all one-shot, but if we declare them as `"oneshot"` then
our code crashes on the `steamroller` problem.

Letting them build up on the heap means we can solve `steamroller` with "Lazy
MESON", but only once. The second time we click the button, we run into a strange
JavaScript error:

------------------------------------------------------------------------
Uncaught (in promise) JSException "RuntimeError: function signature mismatch
------------------------------------------------------------------------
