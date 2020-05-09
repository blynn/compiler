= HOL - Propositional Logic =

We build a intuitionistic propositional logic theorem prover with
link:Hol.html[our version of HOL Light].

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
import Control.Applicative
import Control.Arrow (second)
import Control.Monad.State
import Data.Foldable (asum)
import Data.List (union, unfoldr, find)
import Hol
\end{code}

HOL Light is simply typed lambda calculus with the primitive type $\mathbb{B}$
of Booleans and an equality function $(=) : a \rightarrow a \rightarrow
\mathbb{B}$ satisfying certain rules.
All the logic symbols we know and love must be built on top.

To define truth, inspired by the `refl` rule, we pick the identity function on
booleans $\lambda p : \mathbb{B} . p$ and equate it to itself.

The universal quantifier `(!)` takes a term $p$ of type $a \rightarrow
\mathbb{B}$ and states $p = \lambda x . T$, that is, we say $p$ is equivalent
to the lambda abstraction that maps all values of type $a$ to truth. We've
arranged the parser so that `(!)` is a binder, that is, it first acts as a
lambda before applying itself to the resulting abstraction.

For falsehood, we use $\forall p : \mathbb{B} . p$, that is, roughly speaking,
"everything is true". This definition embodies the principle of explosion, or
ex falso quodlibet; if we can prove falsehood, then we can prove any given
theorem.

We define conjunction with the aid of currying.
And so on: we get the whole gang with a few elementary definitions. Perhaps the
least comprehensible is the existential quantifier `(?)`, but on closer
inspection, it's just a disguised De Morgan dual.

\begin{code}
must cns = either error id . peruse cns

basic = execState basic' []
basic' :: State [Thm] ()
basic' = do
  def "T" "(^p:Bool.p) = (^p:Bool.p)"
  def "!" "^p:a->Bool.p = ^x.T"
  def "F" "!p:Bool.p"
  def "&" "^p q.(^f:Bool->Bool->Bool.f p q) = (^f.f T T)"
  def "==>" "^p q. p & q <=> p"
  def "~" "^p. p ==> F"
  def "|" "^p q. !r. (p ==> r) ==> ((q ==> r) ==> r)"
  def "?" "^p:a->Bool. !q. (!x. p x ==> q) ==> q"
  where
  def lhs rhs = do
    cns <- get
    let x = must cns rhs
    put $ (newConstant lhs x):cns

falso = must basic "F"
\end{code}

Some utilities to produce theorems from given theorems and the above basic
definitions:

\begin{code}
hyp (a :|- _) = a
concl (_ :|- w) = w

defThm cns s = maybe (error "bad constant") id $ find f cns where
  f (_ :|- Con s' _ := _) = s == s'

-- A |- x = y  /  A |- f x = f y
apTerm :: Hol -> Thm -> Thm
apTerm f xy = app (refl f) xy

-- A |- f = g  /  A |- f x = g x
apThm :: Thm -> Hol -> Thm
apThm fg x = app fg (refl x)

-- A |- l = r  /  A |- r = l
sym :: Thm -> Thm
sym th = ePonens (app (apTerm e th) lth) lth where
  e@(Con "=" _) :$ l :$ _ = concl th
  lth = refl l

--  |- T
truth :: Thm
truth = ePonens (sym $ defThm basic "T") (refl $ must basic "^p:Bool. p")

-- A |- t <=> T  /  A |- t
eqtElim :: Thm -> Thm
eqtElim th = ePonens (sym th) truth

-- A |- t  /  A |- t <=> T
eqtIntro :: Thm -> Thm
eqtIntro th = ePonens (inst [(concl th, b)] pth) th where
  b = must basic "b:Bool"
  th1 = deduct (assume b) truth
  pth = deduct (eqtElim (assume $ concl th1)) th1
\end{code}

A `Conv` tries to convert a term into a theorem.

\begin{code}
type Conv = Hol -> Either String Thm
betaConv :: Conv
betaConv (f@(Lam v _) :$ x) = inst [(x, v)] . beta <$> f @@ v
betaConv _ = Left "betaConv failed"

convRule :: Conv -> Thm -> Thm
convRule conv th | Right x <- conv $ concl th = ePonens x th

randConv :: Conv -> Conv
randConv conv (f :$ x) = app (refl f) <$> conv x

ratorConv :: Conv -> Conv
ratorConv conv (f :$ x) = flip apThm x <$> (conv f)

redepthConv c t = Right $ rep $ go t where
  rep (b, th@(_ :|- _ := r)) = if b
    then case go r of
      (False, _) -> th
      (True, r') -> rep (True, trans th r')
    else th

  go t = let
    (b, z@(_ :|- _ := r)) = case t of
      x :$ y -> (xb || yb, app x' y') where
        (xb, x') = go x
        (yb, y') = go y
      Lam x y -> second (lam x) $ go y  -- TODO: Rename var if lam fails.
      _ -> (False, refl t)
    in case trans z <$> c r of
      Left _ -> (b, z)
      Right z' -> (True, z')

betaRule = convRule $ redepthConv betaConv

proveHyp ath bth = if any (concl ath ==) (hyp bth)
  then ePonens (deduct ath bth) ath
  else bth

f @! x = either error id $ f @@ x

alt_conj lth rth = ePonens (proveHyp lth th) rth where
  f = must basic "f:Bool->Bool->Bool"
  p = must basic "p:Bool"
  q = must basic "q:Bool"
  th1 = convRule (randConv betaConv) (apThm (defThm basic "&") p)
  th2 = convRule (randConv betaConv) (apThm th1 q)
  th3 = ePonens th2 $ assume $ must basic "(&)" @! p @! q
  pth1 = eqtElim $ betaRule $ apThm th3 $ must basic "^(p:Bool) (q:Bool).q"
  th5 = lam f $ app (apTerm f $ eqtIntro $ assume p) $ eqtIntro $ assume q
  th6 = betaRule $ apThm (apThm (defThm basic "&") p) q
  pth2 = ePonens (sym th6) th5
  pth = deduct pth1 pth2
  th = inst [(concl lth, p), (concl rth, q)] pth

apThmBeta fg x = convRule (randConv betaConv) $ apThm fg x

-- t1  t2  /  |- t1 & t2 = ((\f.f t1 t2) <=> (\f.f T T))
andThm t1 t2 = apThmBeta (apThmBeta (defThm basic "&") t1) t2

-- t1  t2  / |- (t1 ==> t2) = (t1 & t2 <=> t1)
impThm t1 t2 = apThmBeta (apThmBeta (defThm basic "==>") t1) t2

-- t1  t1  / |- t1 | t2 = !r.(t1 ==> r) ==> (t2 ==> r) ==> r
orThm t1 t2 = apThmBeta (apThmBeta (defThm basic "|") t1) t2

-- t  /  |- ~t = (t ==> F)
notThm t = apThmBeta (defThm basic "~") t

fvThm (a :|- w) = foldr union (fv w) (fv <$> a)

-- A |- t1  B |- t2  /  A + B |- t1 & t2
conj th1 th2 = ePonens (sym (andThm (concl th1) (concl th2))) th where
  f = variant (fvThm th1 `union` fvThm th2) $ must basic "f:Bool->Bool->Bool"
  th = lam f $ app (apTerm f (eqtIntro th1)) (eqtIntro th2)

comK = must basic "^(x:Bool) (y:Bool).x"
comKI = must basic "^(x:Bool) (y:Bool).y"

-- A |- l & r  /  A |- l
conjunct1 th@(_ :|- Con "&" _ :$ l :$ r) = eqtElim
  $ sym
  $ convRule (randConv betaConv)
  $ convRule (randConv $ ratorConv betaConv)
  $ convRule (randConv betaConv)
  $ sym
  $ convRule (randConv betaConv)
  $ convRule (randConv $ ratorConv betaConv)
  $ apThmBeta (ePonens (andThm l r) th) comK

-- A |- l & r  /  A |- l
conjunct2 th@(_ :|- Con "&" _ :$ l :$ r) = eqtElim
  $ sym
  $ convRule (randConv betaConv)
  $ convRule (randConv $ ratorConv betaConv)
  $ convRule (randConv betaConv)
  $ sym
  $ convRule (randConv betaConv)
  $ convRule (randConv $ ratorConv betaConv)
  $ apThmBeta (ePonens (andThm l r) th) comKI

-- A |- t ==> F  /  A |- ~t
notIntro th@(_ :|- t :==> _) = ePonens (sym (notThm t)) th

-- u  A |- t  /  A - {u} |- u ==> t
disch u th@(_ :|- t) = ePonens (sym (impThm u t))
  $ deduct (conj (assume u) th)
  (conjunct1 $ assume $ must basic "(&)" @! u @! t)

dischAll th@([] :|- _) = th
dischAll th@((a:_) :|- _) = dischAll $ disch a th

-- A |- t1 ==> t2  /  A + {t1} |- t2
undisch th@(_ :|- t1 :==> _) = ponens th $ assume t1
\end{code}

== Tactics ==

In practice, to prove a thoerem, sometimes we work backwards. For example,
we may suspect induction is the right approach. then work on the base case
while leaving the inductive case for later.

We introduce data structures to help organize such efforts. A `Goal` is a
theorem we wish to prove from given assumptions. Each assumption of a `Goal`
is a labeled theorem, and the conclusion is a `Hol` term.

Given a `Hol` term to prove, we begin by making it a goal with no assumptions.
Then we apply a 'tactic' to replace the goal with subgoals, along with a
function (`Crumb`) that describes how to prove the original goal if each of the
subgoals are proved. We label each `Crumb` to aid development.
We may apply tactics recursively, thus we wind up with a tree. Ultimately, if
each we prove each leaf theorem, then a bottom-up recursion following the
crumbs we've left behind proves our original goal.

Hence, each node of a `GoalTree` represents a single theorem. Leaf nodes are
either theorems we have succesfully proved (`Thm`) or theorems we are still
working on (`Goal`). Each internal node contains a function that returns a
theorem given the proved theoems that its children represent.

A `Tactic` is a function taking a `Goal` to `Either String GoalTree`.
It might prove a goal, turning it from a `Goal` to a `Thm` in a leaf node.
It might split a goal into several subgoals. Or it might fail.

\begin{code}
infix 1 :?-
type Crumb = (String, [Thm] -> Thm)
data Goal = [(String, Thm)] :?- Hol
instance Show Goal where show = flip briefGoal ""
data GoalTree = Nd Crumb [GoalTree] | Lf (Either Goal Thm)
type Tactic = Goal -> Either String GoalTree
\end{code}

Functions for printing goals, and for navigating the tree:

\begin{code}
brief = \case
  Con s _ -> (s <>)
  Var s _ -> (s <>)
  x :$ y -> ('(':) . brief x . (' ':) . brief y . (')':)
  Lam s t -> ("(\\"<>) . brief s . (". "<>) . brief t . (')':)
briefThm (a :|- w) = foldr (.) id ((("  "<>) .) . brief <$> a) . (" |- "<>) . brief w
briefGoal (a :?- w) = foldr (.) id (briefThm . snd <$> a) . (" ?- "<>) . brief w

instance Show GoalTree where
  show (Nd (s, _) ks) = s <> show ks
  show (Lf e) = either show show e

data GoalPos = Loc
  { content :: GoalTree
  , before :: [GoalTree]
  , after :: [GoalTree]
  , parents :: [([GoalTree], Crumb, [GoalTree])]
  }

forest loc = foldl (flip (:)) (content loc : after loc) (before loc)

parent loc = case parents loc of
  [] -> Nothing
  (ls,a,rs):ps -> Just Loc
    { content = Nd a (forest loc)
    , before = ls
    , after = rs
    , parents = ps
    }

nextTree loc = case after loc of
  [] -> Nothing
  (a:as) -> Just Loc
    { content = a
    , before = content loc:before loc
    , after = as
    , parents = parents loc
    }

firstChild loc = case content loc of
  Lf _ -> Nothing
  Nd a (k:ks) -> Just $ Loc
    { content = k
    , before = []
    , after = ks
    , parents = (before loc, a, after loc):parents loc
    }

replace loc t = loc { content = t }

root loc = maybe loc root $ parent loc

fromTree t = Loc t [] [] []

firstLeaf loc = maybe loc firstLeaf $ firstChild loc

nextLeaf loc = firstLeaf <$> go loc where
  go l = nextTree l <|> (go =<< parent l)

leaves t = unfoldr ((\l -> (l, nextLeaf l)) <$>) $ Just $ firstLeaf $ fromTree t

goals = filter (\loc -> isGoal $ content loc) . leaves where
  isGoal (Lf (Left _)) = True
  isGoal _ = False
\end{code}

Given a `Goal`, we may already have a `Thm` that proves it, yielding an
elementary tactic. This `acceptTac` tactic is meant to be used with theorems
with no hypotheses.

\begin{code}
acceptTac :: Thm -> Tactic
acceptTac th (_ :?- w) | w == concl th =
  Right $ Nd ("accept", on1 id) [Lf $ Right th]
acceptTac _ _ = Left "acceptTac failed"
\end{code}

Any `Goal` of the form $r = r$ can be proved via the `refl` inference rule:

\begin{code}
reflTac :: Tactic
reflTac g@(_ :?- _ := r) = acceptTac (refl r) g
reflTac _ = Left "reflTac failed"
\end{code}

The discharge tactic takes a goal whose conclusion has the form `a \implies b`
then adds `a` to the hypotheses and changes the conclusion to `b`. The `Crumb`
function describes how to undo this transformation via the `disch` helper
defined above.

\begin{code}
dischTac :: Tactic
dischTac (asl :?- w) = case w of
  ant :==> c -> Right $ Nd ("==>", on1 $ disch ant)
    [Lf $ Left $ ("", assume ant):asl :?- c]
  Con "~" _ :$ ant -> Right $ Nd ("~", on1 $ notIntro . disch ant)
    [Lf $ Left $ ("", assume ant):asl :?- falso]
  _ -> Left "dischTac failed"

conjTac :: Tactic
conjTac (asl :?- Con "&" _ :$ l :$ r) = Right $ Nd ("&", \[x, y] -> conj x y)
  $ (Lf . Left) <$> [asl :?- l, asl :?- r]
conjTac _ = Left "conjTac failed"

idTac g = Right $ Lf $ Left g

noTac _ = Left "noTac"

endGoal g@(_ :?- w) | B <- typeOf w = idTac g
endGoal _ = Left "non-boolean goal"

tackle tac loc = case content loc of
  Lf (Left g) -> replace loc <$> tac g
  _ -> Left "want goal"

by tac tr = case goals tr of
  [] -> Left "no goals"
  (gLoc:_) -> content . root <$> tackle tac gLoc

tacProof :: Goal -> Tactic -> Either String Thm
tacProof g tac = justify =<< by tac =<< endGoal g

justify :: GoalTree -> Either String Thm
justify = \case
  Nd (_, ju) kids -> ju <$> sequence (justify <$> kids)
  Lf (Right th) -> Right th
  _ -> Left "unsolved goal"
\end{code}

While the module system guarantees we stick to our rules of inference when
producing `Thm` values, nothing prevents a tactic proving a different theorem
to the one we desire, so we must check for this.

\begin{code}
prove :: Hol -> Tactic -> Either String Thm
prove t tac = do
  th <- tacProof ([] :?- t) tac
  if t == concl th && null (hyp th) then Right th else Left $ "prove: wrong theorem: " <> flip briefThm "" th

anyHyp :: (Thm -> Tactic) -> Tactic
anyHyp ttac g@(as :?- _) = asum $ (\(_, th) -> ttac th g) <$> as

-- tac1 <||> tac2 = \g -> tac1 g <|> tac2 g
(<||>) = liftA2 (<|>)

aasum tacs = \g -> asum $ ($ g) <$> tacs

tac1 <&&> tac2 = \g -> fillGoals tac2 =<< tac1 g

fillGoals :: Tactic -> GoalTree -> Either String GoalTree
fillGoals tac tr = case tr of
  Nd a ks -> Nd a <$> (sequence $ map (fillGoals tac) ks)
  tr@(Lf (Right _)) -> pure tr
  Lf (Left g) -> tac g

selections bs = unfoldr (\(as, bs) -> case bs of
  [] -> Nothing
  b:bt -> Just ((b, as <> bt), (b:as, bt))) ([], bs)

labelTac s th (as :?- w) =
  Right $ Nd ("label", on1 $ proveHyp th) [Lf $ Left $ (s, th):as :?- w]

assumeTac :: Thm -> Tactic
assumeTac = labelTac ""

deleteHyp :: (Thm -> Tactic) -> Tactic
deleteHyp ttac (as :?- w) = asum
  [ttac th $ as' :?- w | ((_, th), as') <- selections as]

mustType cns = typeOf . must cns . ("x:" <>)

-- u  A |- !x. t  /  A |- t[u/x]
-- The side conditions of `inst` apply.
spec u th@(a :|- Con "!" _ :$ lxt@(Lam x t)) =
  eqtElim $ inst [(u, x)] $ betaRule th2
  where
  th1 = instType [(typeOf x, mustType basic "a")] $ defThm basic "!"
  th2 = apThm (convRule betaConv $ ePonens (apThm th1 lxt) th) x

-- x  A |- t  /  A |- !x.t  |  x not free in A
gen x th@(_ :|- t) = th3 where
  th1 = instType [(typeOf x, mustType basic "a")] $ defThm basic "!"
  th2 = sym $ convRule (randConv betaConv) $ apThm th1 $ mustLam x t
  th3 = ePonens th2 $ lam x $ eqtIntro th

-- A |- t1  t2  /  A |- t1 | t2
disj1 th@(_ :|- t1) t2 =
  proveHyp th $ inst [(t1, p1), (t2, p2)] $ ePonens th1 th3
  where
  th1 = sym $ orThm p1 p2
  th2 = ponens (assume p1r) $ assume p1
  th3 = gen (must basic "r:Bool") $ disch p1r $ disch p2r th2
  p1 = must basic "p1:Bool"
  p2 = must basic "p2:Bool"
  p1r = must basic "p1 ==> r"
  p2r = must basic "p2 ==> r"

-- t1  A |- t2  /  A |- t1 | t2
disj2 t1 th@(_ :|- t2) =
  proveHyp th $ inst [(t1, p1), (t2, p2)] $ ePonens th1 th3
  where
  th1 = sym $ orThm p1 p2
  th2 = ponens (assume p2r) $ assume p2
  th3 = gen (must basic "r:Bool") $ disch p1r $ disch p2r th2
  p1 = must basic "p1:Bool"
  p2 = must basic "p2:Bool"
  p1r = must basic "p1 ==> r"
  p2r = must basic "p2 ==> r"

-- A |- t1 ==> t2  B |- t1  /  A + B |- t2
ponens th1@(_ :|- t1 :==> t2) th2 =
  conjunct2 $ eqtElim $ trans (ePonens (impThm t1 t2) th1) (eqtIntro th2)

-- A |- t1 <=> t2  /  A |- t1 ==> t2  A |- t2 ==> t1
eqImpRule t = (go t, go $ sym t) where
  go th@(_ :|- eq@(t1 :<=> _))
    = ponens (disch eq $ disch t1 $ ePonens (assume eq) (assume t1)) th

-- t  A |- F  /  A |- t
efq w th@(_ :|- t) | t == falso = ponens
  (disch falso $ spec w $ undisch $ fst $ eqImpRule $ defThm basic "F")
  th

efqTac :: Thm -> Tactic
efqTac cth@(_ :|- t) (_ :?- w)
  | t == falso = Right $ Nd ("efq", on1 $ efq w) [Lf $ Right cth]
  | otherwise  = Left "efqTac failed"

disj1Tac (a :?- Con "|" _ :$ l :$ r) = Right
  $ Nd ("disj1", \[th] -> disj1 th r) [Lf $ Left $ a :?- l]
disj1Tac _ = Left "disj1Tac failed"
disj2Tac (a :?- Con "|" _ :$ l :$ r) = Right
  $ Nd ("disj2", \[th] -> disj2 l th) [Lf $ Left $ a :?- r]
disj2Tac _ = Left "disj2Tac failed"

disjCases th0@(_ :|- Con "|" _ :$ l :$ r) th1@(_ :|- lr) th2@(_ :|- lr') | lr == lr' =
  proveHyp (disch r th2) $ proveHyp (disch l th1) $ proveHyp th0 th
  where
  p1 = must basic "p1:Bool"
  p2 = must basic "p2:Bool"
  p3 = must basic "p3:Bool"
  th = inst [(l, p1), (r, p2), (lr, p3)] $ undisch $ undisch
    $ spec p3 $ ePonens (orThm p1 p2) $ assume $ must basic "p1 | p2"

disjCasesTac th@(_ :|- Con "|" _ :$ l :$ r) (a :?- w) = Right
  $ Nd ("disjCases", on2 $ disjCases th)
  $ Lf . Left <$> [("", assume l):a :?- w, ("", assume r):a :?- w]
disjCasesTac _ _ = Left "disjCasesTac failed"

t ==> u = must basic "(==>)" @! t @! u

on1 f [th1] = f th1
on2 f [th1, th2] = f th1 th2

--  A1 |- t1 ==> t2  A2 |- t2 ==> t1  /  (A1 - {t1}) + (A2 - {t2}) |- t1 <=> t2
impAntisymRule
  th1@(_ :|- t1 :==> t2)
  th2@(_ :|- t2' :==> t1')
  -- Intended for use with side condition:  | t1 == t1', t2 == t2'
  = deduct (undisch th2) (undisch th1)

eqTac (a :?- l := r) = Right $ Nd ("eq", on2 impAntisymRule) $ Lf . Left <$> [a :?- l ==> r, a :?- r ==> l]
eqTac _ = Left "eqTac failed"

ponensTac th@(_ :|- t) (a :?- w) = Right $ Nd ("ponens", on1 $ flip ponens th) [Lf $ Left $ a :?- t ==> w]
ponensTac _ _ = Left "ponens failed"

eqThm c d = inst [(c, must basic "a:Bool"), (d, must basic "b:Bool")] th
  where Right th = prove (must basic "(a<=>b) <=> ((a==>b) & (b==>a))") proper

-- Intuitionistic prover for propositional logic. Searches depth-first.
-- See Dyckhoff, "Contraction free sequent calculi for intuistionistic logic".
-- Also handles (!) on the right.
proper = (rightInvertible <&&> proper)
  <||> anyHyp acceptTac
  <||> acceptTac truth
  <||> anyHyp efqTac
  <||> (deleteHyp leftInvertible <&&> proper)
  -- Irreversible steps:
  <||> (disj1Tac <&&> proper)
  <||> (disj2Tac <&&> proper)
  <||> (deleteHyp vorobev <&&> proper)
  where
  rightInvertible = aasum
    [ conjTac    -- (&)
    , dischTac   -- (==>) (~)
    , eqTac      -- (<=>)
    , genAutoTac -- (!)
    ]
  leftInvertible thm = aasum $ ($ thm) <$>
    [ conjunctly assumeTac  -- (&)
    , disjCasesTac          -- (|)
    , notTac                -- (~)
    , leftEqTac             -- (<=>)
    , revImplTac            -- reversible (==>)
    , chooseAutoTac         -- (?)
    ]
  leftEqTac th@(_ :|- _ :<=> _) = conjunctly ponensTac $ uncurry conj $ eqImpRule th
  leftEqTac _ = const $ Left "leftEqTac failed"
  conjunctly ttac cth = case cth of
    (_ :|- Con "&" _ :$ _ :$ _) ->
      ttac (conjunct1 cth) <&&> ttac (conjunct2 cth)
    _ -> const $ Left "conjunctly"
  revImplTac (_ :|- t@(im@(Con "==>" _) :$ a :$ b)) g@(as :?- w)
    -- a, b, Gam |- w  /  a ==> b, a, Gam |- w
    | a `elem` (concl . snd <$> as) = assumeTac (undisch $ assume t) g
    -- c ==> (d ==> b), Gam |- w  /  (c & d) ==> b, Gam |- w
    | Con "&" _ :$ c :$ d <- a = assumeTac (disch c $ disch d $ proveHyp (conj (assume c) (assume d)) $ undisch $ assume t) g
    -- c ==> b, d ==> b, Gam |- w  /  (c | d) ==> b, Gam |- w
    | Con "|" _ :$ c :$ d <- a = assumeTac (disch c $ ponens (assume t) $ disj1 (assume c) d)
      <&&> assumeTac (disch d $ ponens (assume t) $ disj2 c $ assume d) $ g

    -- Rewrite (~) and (<=>) on left side of (==>).
    | Con "~" _ :$ c <- a = assumeTac (ePonens (apThm (apTerm im $ notThm c) b) $ assume t) g
    | c :<=> d <- a = assumeTac (ePonens (apThm (apTerm im $ eqThm c d) b) $ assume t) g

  revImplTac _ _ = Left "revImpl failed"
  -- (d ==> b) ==> (c ==> d) |- ((c ==> d) ==> b) ==> (c ==> d)
  vorobev (_ :|- (c :==> d) :==> b) (as :?- w) = Right
    $ Nd ("vorobev", \[th1, th2] -> ponens (disch b th2) $ ponens (assume $ (c ==> d) ==> b)
    $ ponens (disch (d ==> b) th1) $ disch d $ ponens (assume $ (c ==> d) ==> b) $ disch c $ assume d)
    $ Lf . Left <$> [("", assume (d ==> b)):as :?- c ==> d, ("", assume b):as :?- w]
  vorobev _ _ = Left "vorobev failed"

notTac asm@(_ :|- Con "~" _ :$ t) = assumeTac $ ePonens (notThm t) asm
notTac _ = const $ Left "notTac failed"

-- If tm1 and tm2 are alpha-equivalent:
--   |- tm1 = tm2
alpha :: Hol -> Hol -> Thm
alpha tm1 tm2 = trans (refl tm1) (refl tm2)

mustvsub = (either (error "BUG!") id .) . vsubst

alphaLam v tm@(Lam x t)
  | x == v = tm
  | otherwise = mustLam v $ mustvsub [(v, x)] t

alphaConv v tm = Right $ alpha tm $ alphaLam v tm

genAlphaConv v tm@(Lam _ _) = alphaConv v tm
genAlphaConv v tm@(f :$ x) = apTerm f <$> alphaConv v x

genTac v@(Var s t) (as :?- w@(Con "!" _ :$ Lam x b))
  | t /= typeOf x = Left "type mismatch"
  | any (elem s . fv) (w:(concl . snd <$> as)) = Left "variable capture"
  | otherwise = Right $ Nd ("gen", on1 $ convRule (genAlphaConv x) . gen v) [Lf $ Left $ as :?- mustvsub [(v, x)] b]
genTac _ _ = Left "genTac failed"

genAutoTac g@(as :?- w@(Con "!" _ :$ Lam x _)) = genTac x' g where
  x' = variant (foldr union (fv w) $ fv . concl . snd <$> as) x
genAutoTac _ = Left "genAutoTac failed"

untilFail tac = (tac <&&> untilFail tac) <||> idTac

popAssum :: (Thm -> Tactic) -> Tactic
popAssum ttac (((_, th):as) :?- w) = ttac th (as :?- w)
popAssum _ _ = Left "popAssum failed"

pinst tyin tmin = inst (second (tsubst tyin) <$> tmin) . instType tyin

choose (v@(Var s t), th1@(_ :|- Con "?" _ :$ ab@(Lam bv bod))) th2 = ponens (ponens th5 th4) th1
  where
  cmb = ab @! v
  pat = mustvsub [(v, bv)] bod
  th3 = convRule betaConv $ assume cmb
  th4 = gen v $ disch cmb $ ponens (disch pat th2) th3
  th5 = pinst [(t, mustType basic "a")] [(ab, p), (concl th2, q)] pth
  -- pth is |- (!x.p x ==> q) ==> ((?)p ==> q)
  p = must basic "p:a->Bool"
  q = must basic "q:Bool"
  x1 = convRule (randConv betaConv) $ apThm (defThm basic "?") p
  x2 = spec q $ undisch $ fst $ eqImpRule x1
  pth = dischAll $ disch (must basic "(?) (p:a->Bool)") (undisch x2)

chooseTac (x'@(Var s t)) xth@(_ :|- Con "?" _ :$ Lam x bod) (as :?- w)
  | t == typeOf x, s `notElem` avoid =
    Right $ Nd ("?", undefined) [Lf $ Left $ ("", assume pat):as :?- w]
  | otherwise = Left "chooseTac: invalid variable"
  where
  pat = either (error "BUG!") id $ vsubst [(x', x)] bod
  avoid = foldr (union . fvThm . snd) (union (fv w) (fvThm xth)) as

chooseAutoTac xth@(_ :|- Con "?" _ :$ x :$ _) g@(as :?- w) = chooseTac x' xth g where
  x' = variant (foldr (union . fvThm . snd) (union (fv w) (fvThm xth)) as) x

testBasic = putStr . unlines . map show $ basic
testEqt = eqtIntro $ assume $ must basic "x:Bool"
testSpec = spec (must basic "f:(Num->Num) z:Num") (assume $ must basic "!x:Num.(x = y)")
testTac = prove (must basic "(^a:Num.a) = (^b.b)") reflTac
testProper1 = prove (must basic "a ==> ~(~a)") proper
testProper2 = prove (must basic "((a ==> b) ==> c) ==> ((a & b) ==> (d | c | b))") proper
\end{code}
