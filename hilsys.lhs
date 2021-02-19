= Hilbert Systems =

(link:index.html?a=1&p=hilbert[Try this prover online]!)

Most
https://en.wikipedia.org/wiki/Computer-assisted_proof#Philosophical_objections[objections
to computer-assisted proofs] are themselves objectionable. However, one
concern is valid: how can a program prove a theorem correctly if its source
code is complex and therefore likely riddled with bugs?

Take the first proof of https://en.wikipedia.org/wiki/Four_color_theorem[the
four colour map theorem], which relied upon a one-off program written in IBM
370 assembly to tediously check numerous cases. Do we believe it did its job
perfectly? Could there be, say, an off-by-one bug in some loop that causes it
to skip a case that actually requires 5 colours?

How about our link:fol.html[MESON prover for first-order logic]? Are there
really no bugs in our code?

https://en.wikipedia.org/wiki/Logic_for_Computable_Functions[The LCF theorem
prover] pioneered a solution to this problem: develop a core module that
exports a few functions which provide the only ways to create theorems. These
functions correspond to the laws of logic.

If this core module is correctly implemented, the rest of our program can be
buggy, yet still produce only correct theorems because ultimately it is forced
to obey the core's rules of inference.

This is a proven approach in software engineering. For instance, a red-black
tree module typically exports functions that maintain particular invariants,
such as ensuring a tree is always approximately balanced and is a binary search
tree. No matter how we call the exported functions, these invariants always hold.

== Prelude ==

We write a theorem prover that our bootstrapped compiler can build.

We start with definitions that are normally at our fingertips thanks to `Prelude`:

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
<p><a onclick='hideshow("prelude");'>&#9654; Toggle prelude</a></p>
<div id='prelude' style='display:none'>
++++++++++

\begin{code}
infixr 9 .
infixl 7 *
infixl 6 + , -
infixr 5 ++
infixl 4 <*>
infix 4 == , <=
infixl 3 &&
infixl 2 ||
infixl 1 >> , >>=
infixr 0 $

ffi "putchar" putChar :: Int -> IO Int

class Functor f where fmap :: (a -> b) -> f a -> f b
class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
instance Applicative IO where
  pure = ioPure
  f <*> x = ioBind f \g -> ioBind x \y -> ioPure (g y)
instance Monad IO where return = ioPure ; (>>=) = ioBind
instance Functor IO where fmap f x = ioPure f <*> x
f >> g = f >>= \_ -> g
f $ x = f x
f || g = if f then True else g
f && g = if f then g else False
flst xs n c = case xs of [] -> n; (:) h t -> c h t
foldr c n l = flst l n (\h t -> c h(foldr c n t))
zipWith f xs ys = flst xs [] $ \x xt -> flst ys [] $ \y yt -> f x y : zipWith f xt yt
any f xs = foldr (\x a -> f x || a) False xs
not a = case a of True -> False; False -> True
f . g = \x -> f (g x)
id x = x
flip f x y = f y x
(++) = flip (foldr (:))
concat = foldr (++) []
map = flip (foldr . ((:) .)) []
mapM_ f = foldr ((>>) . f) (pure ())
putStr = mapM_ $ putChar . ord
error s = unsafePerformIO $ putStr s >> putChar (ord '\n') >> exitSuccess
undefined = error "undefined"
intersperse x as = flst as [] \a at -> a : foldr (\h t -> [x, h] ++ t) [] at
null [] = True
null _  = False

class Eq a where (==) :: a -> a -> Bool
instance Eq Int where (==) = intEq
instance Eq Char where (==) = charEq
instance Eq a => Eq [a] where
  xs == ys = case xs of
    [] -> case ys of
      [] -> True
      (:) _ _ -> False
    x:xt -> case ys of
      [] -> False
      y:yt -> x == y && xt == yt

class Show a where
  showsPrec :: Int -> a -> String -> String
  showsPrec _ x = (show x++)
  show :: a -> String
  show x = shows x ""

shows = showsPrec 0
showParen b f = if b then ('(':) . f . (')':) else f

(<=) = intLE
x > y = y <= x && not (y == x)
\end{code}

++++++++++
</div>
++++++++++

== Terms, Formulas, Theorems ==

We define data types for terms and first-order formulas. This time, we
avoid recursion schemes: our compiler can handle them, but lacks support for
pattern synonyms and deriving `Functor`.

\begin{code}
data Term = Var String | Fun String [Term] deriving Eq

data FO = Top | Bot | Atom String [Term]
  | Not FO | FO :/\ FO | FO :\/ FO | FO :==> FO | FO :<=> FO
  | Forall String FO | Exists String FO
  deriving Eq
\end{code}

A value of type `Theorem` should only exist if it contains a valid formula.
Only functions corresponding to the rules of inference or axiom schema should
be able to create `Theorem` values.

\begin{code}
data Theorem = Theorem FO
\end{code}

Functions for pretty-printing:

\begin{code}
instance Show Term where
  showsPrec _ = \case
    Var s -> (s ++)
    Fun s ts -> (s ++) . showParen (not $ null ts)
      (foldr (.) id $ intersperse (", "++) $ map shows ts)

instance Show FO where
  showsPrec p = \case
    Top -> ('1':)
    Bot -> ('0':)
    Atom s ts -> shows $ Fun s ts
    Not x -> ('~':) . showsPrec 4 x
    x :/\ y -> showParen (p > 3) $ showsPrec 3 x . (" /\\ " ++) . showsPrec 3 y
    x :\/ y -> showParen (p > 2) $ showsPrec 2 x . (" \\/ " ++) . showsPrec 2 y
    x :==> y -> showParen (p > 1) $ showsPrec 2 x . (" ==> " ++) . showsPrec 1 y
    x :<=> y -> showParen (p > 1) $ showsPrec 2 x . (" <=> " ++) . showsPrec 1 y
    Forall s x -> showParen (p > 0) $ ("forall " ++) . (s++) . (". "++) . showsPrec 0 x
    Exists s x -> showParen (p > 0) $ ("exists " ++) . (s++) . (". "++) . showsPrec 0 x
\end{code}

== Rules and Axioms ==

We follow along chapter 6 of John Harrison's 'Handbook of Practical Logic and
Automated Reasoning'.

We need a helper function to determine if a given variable appears in a term:

\begin{code}
occurs s t = s == t || case t of
  Var _ -> False
  Fun _ args -> any (occurs s) args
\end{code}

And another to determine if a given variable is free in a formula:

\begin{code}
isFree t = \case
  Top -> False
  Bot -> False
  Atom _ ts -> any (occurs t) ts
  Not x -> isFree t x
  x :/\ y -> isFree t x || isFree t y
  x :\/ y -> isFree t x || isFree t y
  x :==> y -> isFree t x || isFree t y
  x :<=> y -> isFree t x || isFree t y
  Forall v x -> not (occurs (Var v) t) && isFree t x
  Exists v x -> not (occurs (Var v) t) && isFree t x
\end{code}

A dash of syntax sugar to make constructing equalities prettier in our code:

\begin{code}
s =: t = Atom "=" [s, t]
\end{code}

The two rules of inference are modus ponens and generalization:

\begin{code}
ponens (Theorem (p :==> q)) (Theorem p') | p == p' = Theorem q
gen x (Theorem t) = Theorem $ Forall x t
\end{code}

Now for the axioms (cf.
https://en.wikipedia.org/wiki/Hilbert_system#Logical_axioms[Wikipedia's article
on Hilbert systems]). Our propositional fragment is built from implication and
falsehood, so we start with the S and K combinators, and the law of the
excluded middle (LEM). Then we add a few rules for quantified variables and a
few for equality.

\begin{code}
axiomK p q        = Theorem $ p :==> (q :==> p)
axiomS p q r      = Theorem $ (p :==> (q :==> r)) :==> ((p :==> q) :==> (p :==> r))
axiomLEM p        = Theorem $ ((p :==> Bot) :==> Bot) :==> p
axiomAllImp x p q = Theorem $ Forall x (p :==> q) :==> (Forall x p :==> Forall x q)
axiomImpAll x p | isFree (Var x) p = Theorem $ p :==> Forall x p
axiomExEq x t | occurs (Var x) t = Theorem $ Exists x $ Var x =: t
axiomRefl t       = Theorem $ t =: t
axiomFunCong  f ls rs = Theorem $ foldr (:==>) (Fun f ls =: Fun f rs) $ zipWith (=:) ls rs
axiomPredCong p ls rs = Theorem $ foldr (:==>) (Atom p ls :==> Atom p rs) $ zipWith (=:) ls rs
\end{code}

In theory, we could write all our formulas with just implication, falsehood and
the universal quantifier, but for our sanity's sake we support all the popular
connectives:

\begin{code}
axiomIffImp1 p q = Theorem $ (p :<=> q) :==> (p :==> q)
axiomIffImp2 p q = Theorem $ (p :<=> q) :==> (q :==> p)
axiomImpIff p q  = Theorem $ (p :==> q) :==> ((q :==> p) :==> (p :<=> q))
axiomTrue        = Theorem $ Top :<=> (Bot :==> Bot)
axiomNot p       = Theorem $ Not p :<=> (p :==> Bot)
axiomAnd p q     = Theorem $ (p :/\ q) :<=> ((p :==> (q :==> Bot)) :==> Bot)
axiomOr p q      = Theorem $ (p :\/ q) :<=> Not (Not p :/\ Not q)
axiomExists x p  = Theorem $ Exists x p :<=> Not (Forall x $ Not p)
\end{code}

We're done! We have built an interactive theorem prover.

Well, we would have if we had a REPL (though edit-compile-run is tolerable) and
if our compiler supported modules: the `Theorem` data constructor should never
be used again, and modules would enforce this. (Ideally we'd also provide a
unidirectional pattern synonym for matching purposes.)

Instead, we must rely on the honour system: from now on, `Theorem` may only
appear when pattern matching.

== Calculemus! ==

We prove theorems and derive rules:

\begin{code}
-- |-  p ==> p
impRefl p = ponens (ponens
  (axiomS p (p :==> p) p)
  (axiomK p $ p :==> p))
  (axiomK p p)

-- |- p ==> p ==> q  /  |- p ==> q
impDedup th@(Theorem (p :==> (_ :==> q))) = ponens (ponens (axiomS p p q) th) (impRefl p)

-- |- q  /  |- p ==> q
addAssum p th@(Theorem f) = ponens (axiomK f p) th

-- |- q ==> r  /  |- (p ==> q) ==> (p ==> r)
impAddAssum p th@(Theorem (q :==> r)) = ponens (axiomS p q r) (addAssum p th)

-- |- p ==> q  |- q ==> r  /  |- p ==> r
impTrans th1@(Theorem (p :==> _)) th2 = ponens (impAddAssum p th2) th1

-- |- p ==> r  /  |- p ==> q ==> r
impInsert q th@(Theorem (p :==> r)) = impTrans th (axiomK r q)

-- |- p ==> q ==> r  /  |- q ==> p ==> r
impSwap th@(Theorem (p :==> (q :==> r))) = impTrans (axiomK q p) $ ponens (axiomS p q r) th

-- |- (q ==> r) ==> (p ==> q) ==> (p ==> r)
impTransTh p q r = impTrans (axiomK (q :==> r) p) (axiomS p q r)

-- |- p ==> q  /  |- (p ==> r) ==> (q ==> r)
impAddConcl r th@(Theorem (p :==> q)) = ponens (impSwap (impTransTh p q r)) th

-- |- (p ==> q ==> r) ==> (q ==> p ==> r)
impSwapTh p q r = impTrans (axiomS p q r) $ impAddConcl (p :==> r) $ axiomK q p

-- |- (p ==> q ==> r) ==> (s ==> t ==> u)  /  |- (q ==> p ==> r) ==> (t ==> s ==> u)
impSwap2 th@(Theorem ((p :==> (q :==> r)) :==> (s :==> (t :==> u))))
  = impTrans (impSwapTh q p r) (impTrans th (impSwapTh s t u))

-- |- p ==> q ==> r  |- p ==> q  /  |- p ==> r
rightMP ith th = impDedup (impTrans th (impSwap ith))

-- |- p <=> q  /  |- p ==> q
iffImp1 th@(Theorem (p :<=> q)) = ponens (axiomIffImp1 p q) th

-- |- p <=> q  /  |- q ==> p
iffImp2 th@(Theorem (p :<=> q)) = ponens (axiomIffImp2 p q) th

-- |- p ==> q  |- q ==> p  /  |- p <=> q
impAntisym th1@(Theorem (p :==> q)) th2 = ponens (ponens (axiomImpIff p q) th1) th2

-- |- p ==> (q ==> 0) ==> 0  /  |- p ==> q
rightDoubleNeg th@(Theorem (p :==> ((_ :==> Bot) :==> Bot))) = impTrans th $ axiomLEM p

-- |- 0 ==> p
exFalso p = rightDoubleNeg $ axiomK Bot (p :==> Bot)

-- |- 1
truth = ponens (iffImp2 axiomTrue) (impRefl Bot)

-- |- s = t ==> t = s
eqSym s t = let
  rth = axiomRefl s
  f th = ponens (impSwap th) rth
  in f $ f $ axiomPredCong "=" [s, s] [t, s]

-- |- s = t ==> t = u ==> s = u
eqTrans s t u = let
  th1 = axiomPredCong "=" [t, u] [s, u]
  th2 = ponens (impSwap th1) (axiomRefl u)
  in impTrans (eqSym s t) th2

examples =
  [ axiomOr (Atom "x" []) (Atom "y" [])
  , impTransTh (Atom "Foo" []) (Atom "Bar" []) (Atom "Baz" [])
  , eqSym (Var "a") (Var "b")
  , eqTrans (Var "x") (Var "y") (Var "z")
  ]

concl (Theorem t) = t
main = mapM_ (putStr . flip shows "\n" . concl) examples
\end{code}
