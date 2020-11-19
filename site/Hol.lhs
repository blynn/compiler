= Let there be HOL Light =

https://www.cl.cam.ac.uk/~jrh13/hol-light/[HOL Light] is an elegant LCF-style
theorem prover that can be
https://cmartinez.web.wesleyan.edu/documents/FP.pdf[summarized in a page].

Let's play code golf on the core of a HOL Light-esque theorem prover (and hope
for a HOL-in-one), though rather than minimize the source code size, we try to
minimize the amount of thinking required when reading the source code.

Outside this module, all `Hol` values must be well-typed, and all `Thm` values
must be proven. At the same time, we still want to pattern-match on such values.
Thus we define data constructors such as `C V L` strictly for internal use
and unidirectional pattern synonyms such as `Con Var Lam` for export.

\begin{code}
{-# LANGUAGE FlexibleContexts, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, DeriveFoldable, DeriveFunctor #-}

module Hol
  ( Hol, Holy(Con, Var, Lam, (:$), (:=), (:<=>), (:==>))
  , Type(B)
  , Thm((:|-))
  , refl, trans, app, lam, beta, assume, ePonens, deduct, inst, instType
  , newConstant, newAxiom
  , peruse, tsubst, vsubst, (@@), mustLam
  , fv, variant, typeOf
  ) where

import Control.Arrow (first)
import Control.Monad.State
import Data.List (union, delete)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

infixr 5 :->
infix 3 =:= , := , :<=> , :==>
infix 1 :- , :|-
data Type = TV String | TC String | Type :-> Type deriving (Eq, Show)
-- TODO: Remove Functor. For security, must not expose fmap.
data Holy t = V String t | C String t | Holy t :@ Holy t | L (Holy t) (Holy t)
  deriving (Show, Foldable, Functor)
type Hol = Holy Type
data Thm = [Hol] :- Hol deriving Show

pattern B = TC "Bool"
pattern t := u <- C "=" _ :@ t :@ u
pattern t :<=> u <- C "=" (B :-> B :-> B) :@ t :@ u
pattern a :==> b <- C "==>" _ :@ a :@ b
t =:= u = C "=" (ty :-> ty :-> B) :@ t :@ u where ty = typeOf t
\end{code}

https://sites.math.northwestern.edu/~richter/HolInformalMath.pdf[Terms are
equal if they are alpha-equivalent], which simplifies some of our code.

The original HOL Light also defines an ordering on the terms, which makes for
faster unions of assumptions for example. We trade speed for clarity.

\begin{code}
instance Eq t => Eq (Holy t) where (==) = alphaEq []
\end{code}

== Inference rules! ==

We strive to have the rules of inference mimic their print versions.

Accordingly, we define partial functions instead of handling invalid inputs
with a sum type, to avoid the need for identifiers such as `pure` or `Right`.
This seems tolerable, because it is still the case that theorems produced by
untrusted code respect the rules of the core. However, there are drawbacks:
total functions may need to check a side condition before applying a rule of
inference, which unnecessarily repeats the check.

Except during parsing, HOL Light identifies variables by name and type, so
`x:Num` and `x:Bool` count as different variables.

We always identify variables by name only; any term with `x:Num` and `x:Bool`
in the same context is ill-typed and hence illegal.
This complicates some of our side conditions. Deviating from a battle-tested
design is risky, but it's also fun!

\begin{code}
refl

         t
 = -------------
   [] :- t =:= t

trans

   (a :- t := u)  (b :- u' := v) | u == u'
 = -----------------------------
       a `union` b :- t =:= v

app

     (a :- f := g)  (b :- x := y)   | t :-> _ <- typeOf f, t == typeOf x
 = --------------------------------
   a `union` b :- f @! x =:= g @! y

lam

   x@(V v tv) (a :- t := u) | all (notElem v) $ fv <$> a, good t, good u
 = ------------------------
    (a :- L x t =:= L x u)
  where
  good = \case
    C _ _ -> True
    V w tw -> v /= w || tv == tw
    f :@ y -> good f && good y
    L (V w _) y -> v == w || good y

beta

    (f@(L v t) :@ x)  | v == x
 = ------------------
   [] :- f :@ x =:= t

assume

      t     | B <- typeOf t
 = --------
   [t] :- t

ePonens

   (a :- t :<=> u)  (b :- t') | t == t'
 = --------------------------
       a `union` b :- u

deduct

             (a :- p) (b :- q)
 = ----------------------------------------
   delete q a `union` delete p b :- p =:= q

instType subs (a :- c) = (tsubst subs <$> a :- tsubst subs c)

inst subs (a :- c) | validSubs subs = (go <$> a) :- go c
  where
  go t = either error id $ vsub subs t
\end{code}

To kick start theories, we must will theorems into being:

\begin{code}
newConstant :: String -> Hol -> Thm
newConstant s x
  | not (null (fv x)) = error "free term variable in constant"
  | any (`notElem` ftv ty) $ ftvTerm x = error "free type variable in constant"
  | otherwise = [] :- C s ty =:= x
  where ty = typeOf x

newAxiom :: Hol -> Thm
newAxiom = ([] :-)
\end{code}

The theorems we can prove depend on the axioms we choose, so perhaps each `Thm`
value should contain a reference to the set of definitions it relies on.
Again, we have taken the less principled path to reduce clutter.

The parser takes a set of definitions as well as the input string. It's up to
the user to remember which definitions were used in creating a `Hol` value and
subsequent `Thm` values.

== Terms of endearment ==

We expose capture-avoiding variable substitution. As all terms returned by the
core must be well-typed, the exported `vsubst` must check the substitutions are
valid before calling `vsub`, like the `inst` rule.

We also export `variant`.

\begin{code}
tsubst :: [(Type, Type)] -> Hol -> Hol
tsubst subs = fmap go where
  go ty = case ty of
    x :-> y -> go x :-> go y
    TC _ -> ty
    TV _ -> maybe ty id $ lookup ty $ swp <$> subs

vsubst :: [(Hol, Hol)] -> Hol -> Either String Hol
vsubst subs | validSubs subs = vsub subs
            | otherwise      = const $ Left "bad subs"

validSubs = all valid where
  valid (t, V v tv) = typeOf t == tv
  valid _           = False

vsub :: [(Hol, Hol)] -> Hol -> Either String Hol
vsub subs = mixCheck . go subs where
 go subs t = case t of
    C _ _ -> t
    V _ _ -> maybe t id $ lookup t $ swp <$> subs
    f :@ x -> go subs f :@ go subs x
    L x@(V v _) s -> let
      subs' = filter (\(_, V w _) -> v /= w) subs
      s' = go subs' s
      in if any (\(dst, V w _) -> elem w (fv s) && elem v (fv dst)) subs'
        then let x' = variant (fv s') x in L x' $ go ((x', x):subs') s
        else L x s'

variant :: [String] -> Hol -> Hol
variant avoid (V v t) = V (go v) t where
  go s | s `elem` avoid = go $ s <> "'"
       | otherwise      = s
variant _ _ = error "want variable"

swp :: (a, b) -> (b, a)
swp (a, b) = (b, a)

mixCheck t | length (fvType t) == length (fv t) = Right t
           | otherwise = Left "mixed types"
\end{code}

Every HOL term must be well-typed, so our core requires a unification
algorithm. This also requires us to lock down functions that create and
manipulate terms; untrusted code can only call certain functions to do so.

\begin{code}
unify :: [(Type, Type)] -> [(String, Type)] -> Either String [(String, Type)]
unify [] subs = Right subs
unify (eq:eqs) subs = case eq of
  (TC a, TC b)
    | a == b -> unify eqs subs
    | otherwise -> Left "type constant mismatch"
  (a :-> b, c :-> d) -> unify ((a, c):(b, d):eqs) subs
  (TV a, TV b) | a == b -> unify eqs subs
  (TV a, t)
    | occurs t -> Left "occurs"
    | otherwise -> unify (both (subst (a, t)) <$> eqs) $ (a, t):subs
    where
    both f (x, y) = (f x, f y)
    occurs (x :-> y)       = occurs x || occurs y
    occurs (TV b) | a == b = True
    occurs _               = False
  (t, TV a) -> unify ((TV a, t):eqs) subs
  _ -> Left "unify failed"

subst :: (String, Type) -> Type -> Type
subst (a, t) = \case
  TV b | a == b -> t
  x :-> y       -> subst (a, t) x :-> subst (a, t) y
  x             -> x

freshen :: [(String, Type)] -> Holy (Maybe Type) -> State Int (Holy (Maybe Type, Type))
freshen binds = \case
  V s t | Just u <- lookup s binds -> pure $ V s (t, u)
  C s (Just t) -> C s . (Nothing,) . fst <$> insta t []
  L (V s t) x -> do
    u <- TV <$> fresh
    L (V s (t, u)) <$> freshen ((s, u):binds) x
  x :@ y -> (:@) <$> freshen binds x <*> freshen binds y
  where
  insta t m = case t of
    TC s -> pure (TC s, m)
    TV s -> case lookup s m of
      Nothing -> do
        s' <- (s ++) <$> fresh
        pure (TV s', (s, s'):m)
      Just s' -> pure (TV s', m)
    a :-> b -> do
      (u, m') <- insta a m
      (v, m'') <- insta b m'
      pure (u :-> v, m'')
  fresh = do
    n <- get
    put $ n + 1
    pure $ ':':show n

gather :: Holy (Maybe Type, Type) -> State ([(Type, Type)], Int) Type
gather = \case
  V _ (m, t) -> do
    maybe (pure ()) (equate t) m
    pure t
  C _ (_, t) -> pure t
  L x y -> (:->) <$> gather x <*> gather y
  a :@ b -> do
    v <- do
      (es, n) <- get
      put (es, n + 1)
      pure $ TV $ ':':show n
    t <- gather a
    u <- gather b
    equate t $ u :-> v
    pure v
  where
  equate t u = modify $ first $ (:) $ if isGenVar t then (t, u) else (u, t)
  isGenVar (TV (':':_)) = True
  isGenVar _ = False
\end{code}

Our parser is influenced by Haskell: non-symbol constants must begin with
uppercase letters, while variables must begin with lowercase letters.
We also forbid variables of the same name with distinct types in the same
context.

We accept `(^)` instead of `(\)` for abstractions so we can avoid escaping them
in string constants that represent terms.

\begin{code}
formula :: [(String, Hol)] -> Parsec () String (Holy (Maybe Type))
formula cns = space *> expr where
  mkCon s = maybe (error $ "bad constant " <> s) (fmap Just) $ lookup s cns
  expr = foldl (flip ($)) <$> aexp <*> many ((\f b a -> mkCon f :@ a :@ b) <$> op <*> aexp)
  op = some (oneOf "!#$%&*+./<=>?@\\^|-~") <* space
  wo = want op
  lowerId = (:) <$> lowerChar <*> (many alphaNumChar <* space)
  upperId = (:) <$> upperChar <*> (many alphaNumChar <* space)
  aexp = binder "!" <|> binder "?"
    <|> lamb <|> foldl1 (:@) <$> some atom
  binder q = sheep (mkCon q :@) (wo q)
  lamb = sheep id (wo "\\" <|> wo "^")
  sheep f b = do
    bin <- b
    flip (foldr $ (f .) . L) <$> many bindVar <* wo "." <*> expr <|> pure (mkCon bin)
  bindVar = var <|> paren var
  paren = between (spch '(') (spch ')')
  una = (:@) <$> (mkCon <$> wo "~") <*> atom
  atom = paren (expr <|> mkCon <$> op) <|> una
    <|> var <|> mkCon <$> upperId
  spch :: Char -> Parsec () String Char
  spch c = char c <* space
  tcv = TC <$> upperId <|> TV <$> lowerId <|> paren typ
  want p s = try $ do
    s' <- p
    unless (s == s') $ fail $ "want " <> s
    pure s
  typ = foldr1 (:->) <$> ((:) <$> tcv <*> many (wo "->" *> typ))
  var = V <$> lowerId <*> option Nothing (spch ':' *> (Just <$> typ))

initConstants =
  [ ("=", C "=" $ TV "a" :-> TV "a" :-> B)
  , ("<=>", C "=" $ B :-> B :-> B)
  ]

peruse cns inp = case parse (formula mods) "" inp of
  Left e -> Left $ show e
  Right h -> let
    (h', n) = runState (freshen fs h) $ length fs
    (eqs, _) = execState (gather h') ([], n)
    fs = zip (fv h) $ TV . (':':) . show <$> [0..]
    collapse (m, t) = maybe t id m
    in do
      subs <- unify eqs []
      pure $ fmap (flip (foldr subst) subs) (collapse <$> h')
  where
  mods = initConstants <> (abbr <$> cns)
  abbr ([] :- C "=" _ :@ t@(C s _) :@ _) = (s, t)
\end{code}

Some helpers, including variations on finding free variables.

\begin{code}
-- | Free variable names of term-ish things.
fv :: Holy a -> [String]
fv = go [] where
  go bs = \case
    V s t | s `elem` bs -> []
          | otherwise -> [s]
    C _ _  -> []
    x :@ y -> go bs x `union` go bs y
    L (V s _) x -> go (s:bs) x

-- | Free variable names and types of terms
fvType :: Hol -> [(String, Type)]
fvType = go [] where
  go bs = \case
    V s t | s `elem` bs -> []
          | otherwise -> [(s, t)]
    C _ _  -> []
    x :@ y -> go bs x `union` go bs y
    L (V s _) x -> go (s:bs) x

-- | Type variables of a type.
ftv :: Type -> [String]
ftv = \case
  TV v -> [v]
  TC _ -> []
  t :-> u -> ftv t `union` ftv u

-- | Type variables of all types of a term.
ftvTerm :: Hol -> [String]
ftvTerm = foldr union [] . fmap ftv

typeOf :: Hol -> Type
typeOf = \case
  V _ t -> t
  C _ t -> t
  L x t -> typeOf x :-> typeOf t
  f :@ _ | _ :-> b <- typeOf f -> b
         | otherwise -> error "bad type"

alphaEq :: Eq t => [(String, String)] -> Holy t -> Holy t -> Bool
alphaEq env f g = case (f, g) of
  (C x t, C y u) -> x == y && t == u
  (V x t, V y u)
    | x == y -> t == u  -- Compare types in case these are free variables.
    | otherwise -> maybe False (y ==) $ lookup x env
  (t1 :@ t2, u1 :@ u2) -> alphaEq env t1 u1 && alphaEq env t2 u2
  (L (V x tx) a, L (V y ty) b) -> tx == ty && alphaEq ((if x == y then id else ((x, y):)) env) a b
  _ -> False
\end{code}

Unidirectional pattern synonyms allow untrusted code to match on parts of
theorems and terms, while preventing unprincipled creation of new theorems and
terms. We also export functions for safely creating abstractions and
applications from well-typed terms.

\begin{code}
pattern a :$ b <- a :@ b
pattern Lam x y <- L x y
pattern Var x y <- V x y
pattern Con x y <- C x y
pattern a :|- b <- a :- b

(@@) :: Hol -> Hol -> Either String Hol
f @@ x | t :-> _ <- typeOf f, t' <- typeOf x, t == t' = mixCheck $ f :@ x
       | otherwise = Left $ "type mismatch"

f @! x = either error id $ f @@ x

mustLam :: Hol -> Hol -> Hol
mustLam x@(V v tv) t = maybe tm
  (\ty -> if ty == tv then tm else error "mixed types") $ lookup v $ fvType t
  where tm = L x t
\end{code}
