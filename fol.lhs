= First-order logic =

Boilerplate abounds in code that manipulate syntax trees. For example,
a function might transform a particular kind of leaf node. If we use an
ordinary recursive data type, then we must add recursive calls for every
recursive data constructor. If we later add a recursive data constructor we
must remember to update the function.

Or consider annotating a syntax tree. The most obvious way is to declare
another data type just like the original syntax tree except it has an extra
annotation field.

https://github.com/ninegua/reduxer/blob/master/src/Lambda/Term.hs[Paul Hai Liu
showed me a nice solution to these problems]. The trick is to use recursion
schemes along with a helper function that acts like `fix` on data types.

Pattern synonynms improve usability.

We demonstrate by building classic theorem provers for first-order logic, by
taking a whirlwind tour through chapters 2 and 3 of John Harrison, 'Handbook of
Practical Logic and Automated Reasoning'.

\begin{code}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
import Data.Foldable (asum)
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.List (delete, union, partition, sort, find, maximumBy, intercalate)
import Data.Ord (comparing)
import qualified Data.Set as S
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Debug.Trace
\end{code}

== Recursion Schemes ==

We represent 'terms' with an ordinary recursive data structure.
Terms consist of 'variables', 'constants', and 'functions'.
Constants are functions that take zero arguments.

\begin{code}
data Term = Var String | Fun String [Term] deriving (Eq, Ord)
\end{code}

We use a recursion scheme to hold a formula of first-order predicate logic.
These are like propositional logic formulas, except:

  * An atomic proposition is a 'predicate': a string constant accompanied by a
  list of terms.
  * Subformulas may be 'quantified' by a 'universal' or 'existential'
  quantifier. A quantifier 'binds' a variable.

\begin{code}
data Quantifier = Forall | Exists deriving (Eq, Ord)
data Formula a = FTop | FBot | FAtom String [Term]
  | FNot a | FAnd a a | FOr a a | FImp a a | FIff a a
  | FQua Quantifier String a
  deriving (Eq, Ord, Functor, Foldable, Traversable)
\end{code}

A data type akin to the `fix` function powers the recursion:

\begin{code}
data FO = FO (Formula FO) deriving (Eq, Ord)
\end{code}

Setting up pattern synonyms is worth the boilerplate.

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

The following functions help us recurse on first-order formulas.

I chose the name `unmap` because its type seems to be the inverse of `fmap`.

\begin{code}
bifix :: (a -> b) -> (b -> a) -> a
bifix f g = g $ f $ bifix f g
ffix :: Functor f => ((f a -> f b) -> a -> b) -> a -> b
ffix = bifix fmap
unmap :: (Formula FO -> Formula FO) -> FO -> FO
unmap h (FO t) = FO (h t)
\end{code}

== Parsing and pretty-printing ==

Variables start with lowercase letters, while constants, functions, and
predicates start with uppercase letters.

\begin{code}
firstorderformula :: Parsec () String FO
firstorderformula = iff where
  iff = foldr1 (:<=>) <$> sepBy1 impl (want "<=>")
  impl = foldr1 (:==>) <$> sepBy1 disj (want "==>")
  disj = foldl1 (:\/) <$> sepBy1 conj (want "\\/" <|> want "|" <|> want "+")
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
    f <- Qua <$> (keyword "forall" *> pure Forall
             <|>  keyword "exists" *> pure Exists)
    vs <- some var
    fo <- want "." *> firstorderformula
    pure $ foldr f fo vs
  atom = Atom <$> con <*> option [] (between (want "(") (want ")") $ sepBy term $ want ",")
  term = Var <$> var
    <|> Fun <$> con <*>
        option [] (between (want "(") (want ")") $ sepBy term $ want ",")
  var = ((:) <$> lowerChar <*> (many alphaNumChar <* space))
  con = ((:) <$> upperChar <*> (many alphaNumChar <* space))
  want :: String -> Parsec () String ()
  want s = string s *> space
  keyword :: String -> Parsec () String ()
  keyword s = try $ string s *> notFollowedBy alphaNumChar *> space

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
    Atom s ts -> (s <>) . showParen (not $ null ts) ((<>) $ intercalate ", " $ show <$> ts)
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

Thanks to pattern synonyms, using recursion schemes is almost the same as using
regular recursive data types.

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
  p :<=> Bot -> Not p
  Bot :<=> p -> Not p
  Qua _ x p | x `notElem` fv p -> p
  t -> t
\end{code}

== Negation normal form ==

A handful of rules transform a simplified formula to 'negation normal form'
(NNF), namely, the formula consists only of literals (literals are atoms or
negated atoms), conjunctions, disjunctions, and quantifiers.

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

Substitution gives us another opportunity to compare recursion schemes against
plain old data structures.

As before, the `Term` version must handle each case and its recursive calls are
explicitly spelled out. The `FO` version only handles the cases it cares about,
provides a generic catch-all case, and relies on `ffix` and `unmap` to recurse.

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
existential quantifiers. "Equisatisfiable" means satisfiability is preserved,
that is the output is satisifable if and only if the input is.

Skolemization is "lossy" because the output formula may not be logically
equivalent; validity might not be preserved.

We may need to mint new function names along the way. The function `functions`
finds all functions present in a given formula, so we can avoid name clashes.
It also records the arity of each function because we'll need this later to
enumerate ground terms.

It is possible to Skolemize a non-NNF formula, but if negations are allowed
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
      skolem' (subst (`lookup` [(x, fx)]) p)
    FO t -> FO <$> mapM skolem' t
\end{code}

== Prenex normal form ==

We can pull all the quantifiers of an NNF formula to the front by generating
new variable names. The result is a formula in 'prenex normal form' (PNF).

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

A 'quantifier-free' formula is a formula where each variable is free.
In other words, each variable is implicitly universally quantified, that is,
for each variable `x`, we act as if `forall x.` has been prepended to the
forumla.

We can remove all quantifiers from a skolemized NNF formula by pulling all the
universal quantifiers to the front and then dropping them.

Our `specialize` helper shows we can truly pretend we're working with an
ordinary recursive data structure. The recursion is explicit, which suits this
function because we only want to transform the top of the tree.

\begin{code}
deQuantify :: FO -> FO
deQuantify = specialize . pnf where
  specialize = \case
    Qua Forall x p -> specialize p
    t -> t
\end{code}

== Ground terms ==

A 'ground term' is a term containing no variables, that is, it is exclusively
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

------------------------------------------------------------------------
(P(y) ==> Q(F(z))) ==> (P(G(x)) ==> Q(x))
------------------------------------------------------------------------

is:

------------------------------------------------------------------------
C, F(C), G(C), F(F(C)), F(G(C)), G(F(C)), G(G(C)), ...
------------------------------------------------------------------------

We've added the constant `C` because there were no others. Since `P` and `Q`
are predicates and not functions, they are not part of the Herbrand universe.

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
Loosely speaking, this means we treat terms like the abstract syntax trees that
represent them; if a theorem holds under some intepretation, then it also holds
for syntax trees.

Why? Intuitively, given a formula and an interpretation where it holds, we can
define a syntax tree based on the constants and functions of the formula, and
rig predicates on these trees to behave enough like their counterparts in the
interpretation.

For example, the formula $\forall x . x + 0 = x$ holds under many familiar
interpretations. It also holds for syntax trees:

------------------------------------------------------------------------
data Term = Plus Term Term | Zero
------------------------------------------------------------------------

with the predicate:

------------------------------------------------------------------------
eq :: (Term, Term) -> Bool
eq _ = True
------------------------------------------------------------------------

Another example: the following holds under interpretations such as
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

The above result suggests the following strategy to prove any first-order
formula `f`. As a preprocessing step, we prepend explicit universal quantifiers
for each free variable.

  1. Negate $f$, because validity and satisfiability are dual. That is,
  the formula $f$ is valid if and only if $\neg f$ is unsatisfiable.
  2. Transform $\neg f$ to an equisatisfiable quantifier-free formula $t$.
  3. Initialize $h$ to $\top$.  Let $m$ be the number of variables in $t$.
  4. Pick $m$ elements from the Herbrand universe of $t$.
  5. Replace $h$ with the conjunction of $h$ and the result of substituting
  the variables of $t$ with the ground terms we chose in step 4.
  6. If $h$ is unsatisfiable, then $t$ is unsatisfiable under any
  interpretation, hence $f$ is valid. Otherwise, go to step 4.

We have moved from first-order logic to propositional logic; the formula $h$
only contains ground terms which act as propositional variables when
determining satisfiability. In other words, we're left with
https://en.wikipedia.org/wiki/Boolean_satisfiability_problem[the classic SAT
problem].

If the given formula is valid, then this algorithm eventually finds a proof
provided the method used to pick ground terms in step 4 eventually selects any
given possbiility. This is the case for our `groundTuples` function.

We've already done the work for the first two steps, and can easily throw in a
preprocessing step to add universal quantifiers for each free variable:

\begin{code}
skno :: FO -> FO
skno = skolemize . nono . generalize where
  generalize fo = foldr (Qua Forall) fo $ fv fo
\end{code}

== Gilmore ==

It remains to write an algorithm to solve SAT.
One of the earliest approaches (Gilmore 1960) used 'disjunctive normal form'
(DNF): a formula is in this form if it is the disjunction of conjunctions of
literals, such as:

\[
(\neg a\wedge b\wedge c) \vee (d \wedge \neg e) \vee (f)
\]

We represent a DNF formula as a set of sets of literals.
Given an NNF formula, the function `pureDNF` builds an equivalent DNF formula:

\begin{code}
distrib s1 s2 = S.map (uncurry S.union) $ S.cartesianProduct s1 s2

pureDNF = \case
  p :/\ q -> distrib (pureDNF p) (pureDNF q)
  p :\/ q -> S.union (pureDNF p) (pureDNF q)
  t -> S.singleton $ S.singleton t
\end{code}

To solve SAT, we eliminate conjunctions containing $\bot$ or the positive and
negative versions of the same literal, such as `P(C)` and `~P(C)`.
The formula is unsatisfiable if and only if nothing remains.

To reduce the formula size, we also replace clauses containing $\top$ with the
empty clause (since the empty conjunction is $\top$), and drop clauses that are
supersets of other clauses.

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
herbrand conjSub refute uni fo = herbLoop (uni Top) [] herbiverse where
  qff = deQuantify . skno $ fo
  fvs = fv qff
  herbiverse = herbTuples (length fvs) qff
  t = uni qff
  herbLoop h tried = \case
    [] -> error "invalid formula"
    (tup:tups)
      | trace (concat
        [ show $ length tried, " ground instances tried; "
        , show $ length h," items in list"
        ])
        refute h' -> tup:tried
      | otherwise  -> herbLoop h' (tup:tried) tups
      where
      h' = conjSub t (subst (`M.lookup` (M.fromList $ zip fvs tup))) h

gilmore = herbrand conjDNF S.null simpDNF where
  conjDNF djs0 sub djs = S.filter nontrivial (distrib (S.map (S.map sub) djs0) djs)
\end{code}

== Davis-Putnam ==

DPLL is a better algorithm. It uses the 'conjunctive normal form' (CNF) which
is the dual of DNF:

\begin{code}
pureCNF = S.map (S.map nono) . pureDNF . nnf . nono
\end{code}

This method of constructing a CNF formula is potentially costly, but we get
away with this because we only pay this cost once in step 2 to convert a
relatively short theorem. Step 5 just piles on more conjunctions.

An alternative is to use a 'definitional CNF'; in general, this may fail to
preserve logical equivalence, but it preserves satisfiability which is all we
care about, and we eventually Skolemize anyway.

As with DNF, we simplify:

\begin{code}
simpCNF = \case
  Bot -> S.singleton S.empty
  Top -> S.empty
  fo -> let cjs = S.filter nontrivial $ pureCNF fo in
    S.filter (\c -> not $ any (`S.isProperSubsetOf` c) cjs) cjs
\end{code}

We implement DPLL to solve SAT, and pass it to `herbrand`:

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

== Unification ==

To refute $P(F(x), G(A)) \wedge \neg P(F(B), y)$, the above algorithms would
have to luck out and select, say, $(x, y) = (B, G(A))$.

Unification can find this assignment efficiently. This observation inspired a
more efficient approach to theorem proving.

Harrison's implementation of unification differs from that of Jones.
Accounting for existing substitutions is deferred until variable binding, where
we perform the occurs check, as well as a redundancy check. However, perhaps
laziness means the two approaches are more similar than they appear.

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
unifyLiterals env = \case
  (Atom p1 a1, Atom p2 a2) -> unify env [(Fun p1 a1, Fun p2 a2)]
  (Not p, Not q) -> unifyLiterals env (p, q)
  (Bot, Bot) -> Right env
  _ -> Left "Can’t unify literals"
\end{code}

== Tableaux ==

After Skolemizing, our formula consists of literals, conjunctions,
disjunctions, and universal quantifiers. We can recurse on the structure of the
formula, gathering literals that must all play nice together, and branching if
necessary.

These literals may contain variables. Each time we add a new literal to our
collection, we see if it unifies with the complement of any other literal,
which refutes the current branch. The theorem is proved once all branches are
refuted. We can view this as a lazy version of a CNF-based algorithm: there's
no need to complete the conversion if we can already show what we have so far
is unsatisifiable.

When we encounter a universally quantified subformula we create a new variable
then move the subformula to the back of the list in case we need it again,
which creates tension. On the one hand, we want variables so we can unify
them with the complement of other literals to refute branches. On the other
hand, if we're having trouble finding a refutation, it may be best to move on
and hope for a literal that is easier to contradict.

Iterative deepening comes to our rescue. We bound the number of variables a
path can create to avoid getting lost in the weeds. If the search fails,
we bump up the bound and try again.

\begin{code}
deepen f n = trace ("Searching with depth limit " <> show n)
  either (const $ deepen f (n + 1)) id $ f n

tab' n fos lits cont (env,k)
  | n < 0 = Left "no proof at this level"
  | otherwise = case fos of
    [] -> Left "tableau: no proof"
    h:rest -> case h of
      p :/\ q -> tab' n (p:q:rest) lits cont (env,k)
      p :\/ q -> tab' n (p:rest) lits (tab' n (q:rest) lits cont) (env,k)
      Qua Forall x p -> let
        y = Var $ '_':show k
        p' = subst (`lookup` [(x, y)]) p
        in tab' (n - 1) (p':rest <> [h]) lits cont (env,k+1)
      fo -> case asum $ (\l -> cont =<< flip (,) k <$>
          unifyLiterals env (fo,nono l)) <$> lits of
        Left _ -> tab' n rest (fo:lits) cont (env,k)
        Right x -> Right x

tabRefute fos = deepen (\n -> tab' n fos [] Right (mempty, 0)) 0

tableau fo = case skno fo of
  Bot -> (mempty, 0)
  sfo -> tabRefute [sfo]
\end{code}

Some problems can be split up into the disjunction of independent subproblems,
which can be solved individually.

\begin{code}
dnfTableau = map (tabRefute . S.toList) . S.toList . simpDNF . skno
\end{code}

== Problems ==

Harrison tests out his programs on problems listed in Pelletier,
'Seventy-Five Problems for Testing Automatic Theorem Provers',
and also problems due to Los, and Davis and Putnam.

\begin{code}
p18 = mustFO "exists x. forall y. P(x) ==> P(y)"
p19 = mustFO "exists x. forall y z. (P(y) ==> Q(z)) ==> (P(x) ==> Q(x))"
p20 = mustFO "(forall x y. exists z. forall w. P(x) & Q(y) ==> R(z) & U(w)) ==> (exists x y. P(x) & Q(y)) ==> (exists z. R(z))"
p21 = mustFO "(exists x. P ==> F(x)) & (exists x. F(x) ==> P) ==> (exists x. P <=> F(x))"
p22 = mustFO "(forall x. P <=> F(x)) ==> (P <=> forall x. F(x))"
p24 = mustFO "~(exists x. U(x) & Q(x)) & (forall x. P(x) ==> Q(x) + R(x)) & ~(exists x. P(x) ==> (exists x. Q(x))) & (forall x. Q(x) & R(x) ==> U(x)) ==> (exists x. P(x) & R(x))"
p26 = mustFO "((exists x. P(x)) <=> exists x. Q(x)) & (forall x y. P(x) & Q(y) ==> (R(x) <=> S(y))) ==> ((forall x. P(x) ==> R(x)) <=> forall x. Q(x) ==> S(x))"
p34 = mustFO "((exists x. forall y. P(x) <=> P(y)) <=> ((exists x. Q(x)) <=> (forall y. Q(y)))) <=> ((exists x. forall y. Q(x) <=> Q(y)) <=> ((exists x. P(x)) <=> (forall y. P(y))))"
p35 = mustFO "exists x y . (P(x, y) ==> forall x y. P(x, y))"
p38 = mustFO "(forall x. P(a) & (P(x) ==> (exists y. P(y) & R(x,y))) ==> (exists z w. P(z) & R(x,w) & R(w,z))) <=> (forall x. (~P(a) + P(x) + (exists z w. P(z) & R(x,w) & R(w,z))) & (~P(a) + ~(exists y. P(y) & R(x,y)) + (exists z w. P(z) & R(x,w) & R(w,z))))"
p39 = mustFO "~exists x. forall y. F(y, x) <=> ~F(y, y)"
p42 = mustFO "~exists y. forall x. (F(x,y) <=> ~exists z.F(x,z) & F(z,x))"
p45 = mustFO "(forall x. P(x) & (forall y. G(y) & H(x,y) ==> J(x,y)) ==> (forall y. G(y) & H(x,y) ==> R(y))) & ~(exists y. L(y) & R(y)) & (exists x. P(x) & (forall y. H(x,y) ==> L(y)) & (forall y. G(y) & H(x,y) ==> J(x,y))) ==> (exists x. P(x) & ~(exists y. G(y) & H(x,y)))"

los = mustFO "(forall x y z. P(x,y) & P(y,z) ==> P(x,z)) & (forall x y z. Q(x,y) & Q(y,z) ==> Q(x,z)) & (forall x y. Q(x,y) ==> Q(y,x)) & (forall x y. P(x,y) + Q(x,y)) ==> (forall x y. P(x,y)) + (forall x y. Q(x,y))"
dpex = mustFO "exists x. exists y. forall z. (F(x,y) ==> (F(y,z) & F(z,z))) & ((F(x,y) & G(x,y)) ==> (G(x,z) & G(z,z)))"
\end{code}

Our implementation above seems to be better at solving some of the problems.
For example, `gilmore p20` finishes almost immediately, and `gilmore p45` is
reasonably fast.

I'm chalking up the discrepancies to:

  * Newer hardware: my laptop is younger than the book.
  * Different ordering of ground terms: our program may be fortuitously picking
  suitable members of the Herbrand universe early on.
  * Better data structures: instead of using lists to represent sets, we use
  `Data.Set`.
  * Lazy evaluation: this can help or hinder efficiency, and maybe it's
  fortuitously the former.
