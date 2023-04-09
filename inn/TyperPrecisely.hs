-- Arbitrary precision integers.
module Typer where

import Base
import Map
import Ast
import Parser
import Unify

-- Pattern compiler.
rewritePats searcher = \case
  [] -> pure $ V "join#"
  vsxs@((as0, _):_) -> case as0 of
    [] -> pure $ foldr1 (A . L "join#") $ snd <$> vsxs
    _ -> do
      let k = length as0
      n <- get
      put $ n + k
      let vs = take k $ (`shows` "#") <$> [n..]
      cs <- forM vsxs \(a:at, x) -> (a,) <$> foldM (\b (p, v) -> rewriteCase searcher v Tip [(p, b)]) x (zip at $ tail vs)
      flip (foldr L) vs <$> rewriteCase searcher (head vs) Tip cs

scottCase q x = A (assertType (E $ Basic "I") q) x

patEq lit b x y = A (L "join#" $ A (A (A (V "if") (A (A (V "==") (E lit)) b)) x) $ V "join#") y

rewriteCase searcher caseVar tab = \case
  [] -> flush $ V "join#"
  ((v, x):rest) -> go v x rest
  where
  rec = rewriteCase searcher caseVar
  go v x rest = case v of
    PatLit lit -> flush =<< patEq lit (V caseVar) x <$> rec Tip rest
    PatVar s m -> let x' = fill s (V caseVar) x in case m of
      Nothing -> flush =<< A (L "join#" x') <$> rec Tip rest
      Just v' -> go v' x' rest
    PatCon con args -> rec (insertWith (flip (.)) con ((args, x):) tab) rest
  flush onFail = case toAscList tab of
    [] -> pure onFail
    -- TODO: Check rest of `tab` lies in cs.
    (firstC, _):_ -> do
      let (q, cs) = either error id $ findCon searcher firstC
      jumpTable <- mapM (\(Constr s ts) -> case mlookup s tab of
          Nothing -> pure $ foldr L (V "join#") $ const "_" <$> ts
          Just f -> rewritePats searcher $ f []
        ) cs
      pure $ A (L "join#" $ foldl A (scottCase q $ V caseVar) jumpTable) onFail

resolveFieldBinds searcher t = go t where
  go t = case t of
    E (Const _) -> A (V "fromInteger") t
    E _ -> t
    V _ -> t
    A (E (Basic "{=")) (A rawExpr fbsAst) -> let
      expr = go rawExpr
      fromAst t = case t of
        A (A (E (StrCon f)) body) rest -> (f, go body):fromAst rest
        E (Basic "=}") -> []
      fbs@((firstField, _):_) = fromAst fbsAst
      (con, fields) = findField searcher firstField
      (q, cs) = either error id $ findCon searcher con
      newValue = foldl A (V con) [maybe (V $ "[old]"++f) id $ lookup f fbs | (f, _) <- fields]
      initValue = foldl A expr [maybe (V "undefined") id $ lookup f fbs | (f, _) <- fields]
      updater = foldr L newValue $ ("[old]"++) . fst <$> fields
      inj x = map (\(Constr con' _) -> if con' == con then x else V "undefined") cs
      allPresent = all (`elem` (fst <$> fields)) $ fst <$> fbs
      isCon = case expr of
        V (h:_) -> 'A' <= h && h <= 'Z'
        _ -> False
      in if allPresent
        then if isCon then initValue else foldl A (scottCase q expr) $ inj updater
        else error "bad fields in update"
    A x y -> A (go x) (go y)
    L s x -> L s $ go x

fixFixity searcher t = case t of
  E _ -> pure t
  V _ -> pure t
  A (E (Basic "{+")) ch -> infixer searcher =<< go ch
  A x y -> A <$> go x <*> go y
  L s b -> L s <$> go b
  Pa vsxs -> Pa <$> mapM (\(ps, a) -> (,) <$> mapM pgo ps <*> go a) vsxs
  where
  go = fixFixity searcher
  pgo = pure . patFixFixity searcher

infixer searcher (A (A (A s x) y) t) = go seed t where
  seed = A (A s $ protect x) $ protect y
  protect t = A (E (Basic "!")) t
  unprotectAll = \case
    A (E (Basic "!")) x -> unprotectAll x
    A a b -> A (unprotectAll a) (unprotectAll b)
    t -> t
  go acc t = case t of
    E (Basic "+}") -> pure $ unprotectAll acc
    A (A op z) rest -> go (rebase op (protect z) acc) rest
    _ -> error "unreachable"
  rebase op z = \case
    A (A op' x) y -> let
      stay = A (A op $ A (A op' x) y) z
      down = A (A op' x) $ rebase op z y
      in extendChain searcher stay down op op'
    x -> A (A op x) z

patFixFixity searcher p = case p of
  PatLit _ -> p
  PatVar s m -> PatVar s $ go <$> m
  PatCon "{+" args -> patFixer searcher args
  PatCon con args -> PatCon con $ go <$> args
  where
  go = patFixFixity searcher

patFixer searcher (PatCon f [a, b]:rest) = unprotectAll $ foldl (flip rebase) seed rest where
  seed = PatCon f [protect a, protect b]
  protect x = PatCon "!" [x]
  unprotectAll = \case
    PatCon "!" [x] -> unprotectAll x
    PatCon con args -> PatCon con $ unprotectAll <$> args
    p -> p
  rebase sz@(PatCon s [z]) = \case
    PatCon s' [x, y] -> let
      stay = PatCon s [PatCon s' [x, y], z]
      down = PatCon s' [x, rebase sz y]
      in extendChain searcher stay down (V s) (V s')
    x -> PatCon s [x, z]

extendChain searcher stay down op op' =
  if prec <= prec'
    then if prec == prec'
      then if assoc == assoc'
        then case assoc of
          LAssoc -> stay
          RAssoc -> down
          NAssoc -> error $ "adjacent NAssoc: " ++ show op ++ " vs " ++ show op'
        else error $ "assoc mismatch: " ++ show op ++ " vs " ++ show op'
      else stay
    else down
  where
  (prec, assoc) = findPrec searcher op
  (prec', assoc') = findPrec searcher op'

secondM f (a, b) = (a,) <$> f b
patternCompile searcher t = astLink searcher $ resolveFieldBinds searcher $ evalState (go $ either error id $ fixFixity searcher t) 0 where
  go t = case t of
    E _ -> pure t
    V _ -> pure t
    A x y -> liftA2 A (go x) (go y)
    L s x -> L s <$> go x
    Pa vsxs -> mapM (secondM go) vsxs >>= rewritePats searcher

-- Type inference.
instantiate' t n tab = case t of
  TC s -> ((t, n), tab)
  TV s -> case lookup s tab of
    Nothing -> let va = TV $ show n in ((va, n + 1), (s, va):tab)
    Just v -> ((v, n), tab)
  TAp x y -> let
    ((t1, n1), tab1) = instantiate' x n tab
    ((t2, n2), tab2) = instantiate' y n1 tab1
    in ((TAp t1 t2, n2), tab2)

instantiatePred (Pred s t) ((out, n), tab) = first (first ((:out) . Pred s)) (instantiate' t n tab)

instantiate (Qual ps t) n = first (Qual ps1) $ fst $ instantiate' t n1 tab where
  ((ps1, n1), tab) = foldr instantiatePred (([], n), []) ps

proofApply sub a = case a of
  Proof (Pred cl ty) -> Proof (Pred cl $ apply sub ty)
  A x y -> A (proofApply sub x) (proofApply sub y)
  L s t -> L s $ proofApply sub t
  _ -> a

typeAstSub sub (t, a) = (apply sub t, proofApply sub a)

data Infer a = Infer { unInfer :: (Map String Type, Int) -> Either String (a, (Map String Type, Int)) }

instance Functor Infer where
  fmap f = \(Infer h) -> Infer $ fmap (first f) . h
instance Applicative Infer where
  pure x = Infer $ Right . (x,)
  (Infer f) <*> (Infer x) = Infer \csn -> do
    (g, csn') <- f csn
    first g <$> x csn'
instance Monad Infer where
  return x = Infer $ Right . (x,)
  (Infer x) >>= f = Infer \csn -> do
    (x', csn') <- x csn
    unInfer (f x') csn'

inferInstantiate q = Infer \(cs, n) -> let
  (q', n1) = instantiate q n
  in Right (q', (cs, n1))

inferUnify s a b = Infer \(cs, n) -> case unify a b cs of
  Left e -> Left $ (s++) . (":"++) $ e
  Right cs -> Right ((), (cs, n))

getConstraints = Infer \csn@(cs, _) -> Right (cs, csn)
putConstraints cs = Infer \(_, n) -> Right ((), (cs, n))
getFreshVar = Infer \csn@(cs, n) -> Right (TV $ show n, (cs, n + 1))

maybeFix s x = if go x then A (E $ Basic "Y") (L s x) else x where
  go = \case
    V v -> s == v
    A x y -> go x || go y
    L v x -> s /= v && go x
    _ -> False

nonemptyTails [] = []
nonemptyTails xs@(x:xt) = xs : nonemptyTails xt

del x = \case
  [] -> []
  h:t -> if h == x then t else h:del x t

findFree vs t = case vs of
  [] -> []
  _ -> case t of
    V v | v `elem` vs -> [v]
    A x y -> case findFree vs x of
      [] -> findFree vs y
      xs -> findFree (vs \\ xs) y ++ xs
    L s t -> findFree (del s vs) t
    _ -> []

triangulate tab x = foldr triangle x components where
  vs = fst <$> tab
  ios = foldr (\(s, t) (ins, outs) -> let dsts = findFree vs t in
    (foldr (\dst -> insertWith union dst [s]) ins dsts, insertWith union s dsts outs))
    (Tip, Tip) tab
  components = scc (\k -> maybe [] id $ mlookup k $ fst ios) (\k -> maybe [] id $ mlookup k $ snd ios) vs
  triangle names expr = let
    tnames = nonemptyTails names
    appem vs = foldl1 A $ V <$> vs
    suball x = foldl A (foldr L x $ init names) $ appem <$> init tnames
    redef tns x = foldr L (suball x) tns
    in foldr (\(x:xt) t -> A (L x t) $ maybeFix x $ redef xt $ maybe (error $ "oops: " ++ x) id $ lookup x tab) (suball expr) tnames

decodeLets x = ((vts, bods), expr) where
  (vts, rest) = stripVars id x
  (bods, expr) = stripBods id vts rest
  stripVars acc (L v t) = stripVars (acc . ((v, Nothing):)) t
  stripVars acc (A (L v t) (E (XQual q))) = stripVars (acc . ((v, Just q):)) t
  stripVars acc (A (E (Basic "in")) t) = (acc [], t)
  stripVars acc e = error $ show e
  stripBods acc [] t = (acc [], t)
  stripBods acc (vt:vtt) (A x y) = stripBods (acc . (x:)) vtt y
  stripBods acc _ e = error $ "bods: " ++ show e

infer' msg typed loc ast = case ast of
  E x -> case x of
    Const x -> pure (TC "Integer", ast)
    ChrCon _ -> pure (TC "Char", ast)
    StrCon _ -> pure (TAp (TC "[]") (TC "Char"), ast)
    Link _ _ -> error "BUG: type should have been found in earlier phase"
    bug -> error $ show bug
  V s -> case lookup s loc <|> Right . fst <$> mlookup s typed of
    Nothing -> Infer $ const $ Left $ "undefined: " ++ s
    Just t -> either (pure . (, ast)) (insta ast) t
  A (E (Basic "@")) (A raw (E (XQual q))) -> insta raw q
  A (E (Basic "::")) (A x (E (XQual q))) -> do
    (tx, ax) <- rec loc x
    (tAnno, aAnno) <- insta ax q
    cs <- getConstraints
    case match (apply cs tx) tAnno of
      Nothing -> Infer $ const $ Left $ msg ++ ": bad match"
      Just ms -> do
        putConstraints $ ms @@ cs
        pure (tAnno, aAnno)
  A (E (Basic "let")) lets -> do
    let ((vartypes, defs), x) = decodeLets lets
    newloc <- forM vartypes $ secondM \case
      Nothing -> Left <$> getFreshVar
      Just q -> pure $ Right q
    let loc' = newloc ++ loc
    axs <- forM (zip newloc defs) \((_, m), a) -> do
      (tx, ax) <- rec loc' a
      case m of
        Left t -> do
          inferUnify msg t tx
          pure ax
        Right q -> do
          Qual pAnno tAnno <- inferInstantiate q
          subs <- maybe (Infer $ const $ Left $ "no match") pure $ match tx tAnno
          cs <- getConstraints
          putConstraints $ subs @@ cs
          cs <- getConstraints
          let
            a1 = proofApply cs ax
            tab = zip pAnno $ ('*':) . show <$> [1..]
          pure $ foldr L (forProof (subProofVar tab) a1) $ snd <$> tab

    second (triangulate $ zip (fst <$> vartypes) axs) <$> rec loc' x
  A x y -> do
    (tx, ax) <- rec loc x
    (ty, ay) <- rec loc y
    va <- getFreshVar
    inferUnify msg tx (arr ty va)
    pure (va, A ax ay)
  L s x -> do
    va <- getFreshVar
    (tx, ax) <- rec ((s, Left va):loc) x
    pure (arr va tx, L s ax)
  bug -> error $ show bug
  where
  rec = infer' msg typed
  insta x q = do
    Qual preds q <- inferInstantiate q
    pure (q, foldl A x (map Proof preds))

infer msg typed loc ast csn = unInfer (infer' msg typed loc ast) csn

findInstance searcher qn@(q, n) p@(Pred cl ty) insts = case insts of
  []  -> case ty of
    TV _ -> let v = '*':show n in Right (((p, v):q, n + 1), V v)
    _ -> Left $ "no instance: " ++ show p
  (modName, Instance h name ps _):rest -> case match h ty of
    Nothing -> findInstance searcher qn p rest
    Just subs -> foldM (\(qn1, t) (Pred cl1 ty1) -> second (A t)
      <$> findProof searcher (Pred cl1 $ apply subs ty1) qn1) (qn, if modName == "" then V name else E $ Link modName name) ps

findProof searcher pred@(Pred classId t) psn@(ps, n) = case lookup pred ps of
  Nothing -> findInstance searcher psn pred $ findInstances searcher classId
  Just s -> Right (psn, V s)

subProofVar tab pred = case lookup pred tab of
  Just s -> V s
  _ -> Proof pred

forProof f = \case
  Proof pred -> f pred
  A x y -> A (rec x) (rec y)
  L s t -> L s $ rec t
  t -> t
  where rec = forProof f

prove searcher psn a = case a of
  Proof pred -> findProof searcher pred psn
  A x y -> prove searcher psn x >>= \(psn1, x1) ->
    second (A x1) <$> prove searcher psn1 y
  L s t -> second (L s) <$> prove searcher psn t
  _ -> Right (psn, a)

data Dep a = Dep ([String] -> Either String ([String], a))
instance Functor Dep where
  fmap f = \(Dep mf) -> Dep \g -> do
    (g', x) <- mf g
    pure (g', f x)
instance Applicative Dep where
  pure x = Dep \g -> Right (g, x)
  (Dep mf) <*> (Dep mx) = Dep \g -> do
    (g', f) <- mf g
    (g'', x) <- mx g'
    pure (g'', f x)
addDep s = Dep \deps -> Right (if s `elem` deps then deps else s : deps, ())
badDep s = Dep $ const $ Left s
runDep (Dep f) = f []

unifyMsg s a b c = either (Left . (s++) . (": "++)) Right $ unify a b c

forFree syms f t = go syms t where
  go syms t = case t of
    E _ -> t
    V s -> if s `elem` syms then f t else t
    A x y -> A (go syms x) (go syms y)
    L s t' -> L s $ go (del s syms) t'

inferno searcher decls typed defmap syms = let
  anno s = maybe (Left $ TV $ ' ':s) Right $ mlookup s decls
  loc = zip syms $ anno <$> syms
  principal ((acc, preds), (subs, n)) s = do
    expr <- maybe (Left $ "missing: " ++ s) Right (mlookup s defmap)
    ((t, a), (ms, n)) <- infer s typed loc expr (subs, n)
    case mlookup s decls of
      Nothing -> do
        soln <- unifyMsg s (TV (' ':s)) t ms
        Right (((s, (t, a)):acc, preds), (soln, n))
      Just qAnno -> do
        let (Qual pAnno tAnno, n1) = instantiate qAnno n
        soln <- maybe (Left $ s ++ ": match failed: " ++ show qAnno ++ " vs " ++ show (apply ms t)) Right $ match (apply ms t) tAnno
        Right (((s, (tAnno, a)):acc, pAnno ++ preds), (soln @@ ms, n1))
  gatherPreds (acc, psn) (s, (t, a)) = do
    (psn, a) <- prove searcher psn a
    pure ((s, (t, a)):acc, psn)
  in do
    ((stas, preds), (soln, _)) <- foldM principal (([], []), (Tip, 0)) syms
    let ps = zip preds $ ("anno*"++) . show <$> [0..]
    (stas, (ps, _)) <- foldM gatherPreds ([], (ps, 0)) $ second (typeAstSub soln) <$> stas
    (ps, subs) <- foldM (defaultRing searcher) (ps, []) stas
    let
      applyDicts preds dicts subs (s, (t, a)) = (s, (Qual preds t,
        foldr L (forFree syms (\t -> foldl A t $ V <$> dicts)
          $ foldr (uncurry fill) a subs) dicts))
    pure $ applyDicts (fst <$> ps) (snd <$> ps) subs <$> stas

defaultRing searcher (preds, subs) (s, (t, a)) = foldM go ([], subs) preds where
  rings = concatMap isRing $ fst <$> preds
  isRing (Pred "Ring" (TV v)) = [v]
  isRing _ = []
  go (ps, subs) p@(pred, dictVar) = case pred of
    Pred cl (TV v) | not $ v `elem` typeVars t ->
      if v `elem` rings then do
        (_, ast) <- findProof searcher (Pred cl $ TC "Integer") ([], 0)
        pure $ (ps, (dictVar, ast):subs)
      else Left $ "ambiguous: " ++ s ++ ": " ++ show pred
    Pred _ x | not $ all (`elem` typeVars t) $ typeVars x -> Left $ "ambiguous: " ++ show pred
    _ -> pure $ (p:ps, subs)

inferDefs searcher defs decls typed = do
  let
    insertUnique m (s, (_, t)) = case mlookup s m of
      Nothing -> Right $ insert s t m
      _ -> Left $ "duplicate: " ++ s
    addEdges (sym, (deps, _)) (ins, outs) = (foldr (\dep es -> insertWith union dep [sym] es) ins deps, insertWith union sym deps outs)
    graph = foldr addEdges (Tip, Tip) defs
  defmap <- foldM insertUnique Tip defs
  let
    ins k = maybe [] id $ mlookup k $ fst graph
    outs k = maybe [] id $ mlookup k $ snd graph
    inferComponent typed syms = foldr (uncurry insert) typed <$> inferno searcher decls typed defmap syms
  foldM inferComponent typed $ scc ins outs $ keys defmap

dictVars ps n = (zip ps $ map (('*':) . show) [n..], n + length ps)

inferTypeclasses searcher iMap typed = foldM inferInstance typed [(classId, inst) | (classId, insts) <- toAscList iMap, inst <- insts] where
  inferInstance typed (classId, Instance ty name ps idefs) = let
    dvs = map snd $ fst $ dictVars ps 0
    perMethod s = do
      let rawExpr = maybe (V $ "{default}" ++ s) id $ mlookup s idefs
      expr <- snd <$> patternCompile searcher rawExpr
      (ta, (sub, n)) <- either (Left . (name++) . (" "++) . (s++) . (": "++)) Right
        $ infer s typed [] expr (Tip, 0)
      qc <- typeOfMethod searcher s
      let
        (tx, ax) = typeAstSub sub ta
-- e.g. qc = Eq a => a -> a -> Bool
-- We instantiate: Eq a1 => a1 -> a1 -> Bool.
        (Qual [Pred _ headT] tc, n1) = instantiate qc n
-- Mix the predicates `ps` with the type of `headT`, applying a
-- substitution such as (a1, [a]) so the variable names match.
-- e.g. Eq a => [a] -> [a] -> Bool
        Just subc = match headT ty
        (Qual ps2 t2, n2) = instantiate (Qual ps $ apply subc tc) n1
      case match tx t2 of
        Nothing -> Left "class/instance type conflict"
        Just subx -> do
          ((ps3, _), tr) <- prove searcher (dictVars ps2 0) (proofApply subx ax)
          if length ps2 /= length ps3
            then Left $ ("want context: "++) . (foldr (.) id $ shows . fst <$> ps3) $ name
            else pure tr
    in do
      ms <- mapM perMethod $ findSigs searcher classId
      pure $ insert name (Qual [] $ TC "DICTIONARY", flip (foldr L) dvs $ L "@" $ foldl A (V "@") ms) typed

primAdts =
  [ (TC "()", [Constr "()" []])
  , (TC "Bool", [Constr "True" [], Constr "False" []])
  , (TAp (TC "[]") (TV "a"), [Constr "[]" [], Constr ":" $ map ("",) [TV "a", TAp (TC "[]") (TV "a")]])
  , (TAp (TAp (TC ",") (TV "a")) (TV "b"), [Constr "," $ map ("",) [TV "a", TV "b"]])
  ]

prims = let
  ro = E . Basic
  dyad s = TC s `arr` (TC s `arr` TC s)
  wordy = foldr arr (TAp (TAp (TC ",") (TC "Word")) (TC "Word")) [TC "Word", TC "Word", TC "Word", TC "Word"]
  bin s = A (ro "Q") (ro s)
  in map (second (first $ Qual [])) $
    [ ("doubleFromInt", (arr (TC "Int") (TC "Double"), A (ro "T") (ro "FLO")))
    , ("intFromDouble", (arr (TC "Double") (TC "Int"), A (ro "T") (ro "OLF")))
    , ("doubleAdd", (arr (TC "Double") (arr (TC "Double") (TC "Double")), A (ro "Q") (ro "FADD")))
    , ("doubleSub", (arr (TC "Double") (arr (TC "Double") (TC "Double")), A (ro "Q") (ro "FSUB")))
    , ("doubleMul", (arr (TC "Double") (arr (TC "Double") (TC "Double")), A (ro "Q") (ro "FMUL")))
    , ("doubleDiv", (arr (TC "Double") (arr (TC "Double") (TC "Double")), A (ro "Q") (ro "FDIV")))
    , ("doubleEq", (arr (TC "Double") (arr (TC "Double") (TC "Bool")), bin "FEQ"))
    , ("doubleLE", (arr (TC "Double") (arr (TC "Double") (TC "Bool")), bin "FLE"))
    , ("sqrt", (arr (TC "Double") (TC "Double"), A (ro "T") (ro "FSQRT")))
    , ("rawDouble", (arr (TC "Double") (TC "Word64"), A (ro "T") (ro "PAIR64")))
    , ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "EQ"))
    , ("intLE", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "LE"))
    , ("wordLE", (arr (TC "Word") (arr (TC "Word") (TC "Bool")), bin "U_LE"))
    , ("wordEq", (arr (TC "Word") (arr (TC "Word") (TC "Bool")), bin "EQ"))

    , ("charEq", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin "EQ"))
    , ("charLE", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin "LE"))
    , ("fix", (arr (arr (TV "a") (TV "a")) (TV "a"), ro "Y"))
    , ("if", (arr (TC "Bool") $ arr (TV "a") $ arr (TV "a") (TV "a"), ro "I"))
    , ("intFromWord", (arr (TC "Word") (TC "Int"), ro "I"))
    , ("wordFromInt", (arr (TC "Int") (TC "Word"), ro "I"))
    , ("chr", (arr (TC "Int") (TC "Char"), ro "I"))
    , ("ord", (arr (TC "Char") (TC "Int"), ro "I"))
    , ("ioBind", (arr (TAp (TC "IO") (TV "a")) (arr (arr (TV "a") (TAp (TC "IO") (TV "b"))) (TAp (TC "IO") (TV "b"))), ro "C"))
    , ("ioPure", (arr (TV "a") (TAp (TC "IO") (TV "a")), ro "V"))
    , ("primitiveError", (arr (TAp (TC "[]") (TC "Char")) (TV "a"), ro "ERR"))
    , ("newIORef", (arr (TV "a") (TAp (TC "IO") (TAp (TC "IORef") (TV "a"))), ro "NEWREF"))
    , ("readIORef", (arr (TAp (TC "IORef") (TV "a")) (TAp (TC "IO") (TV "a")),
      A (ro "T") (ro "READREF")))
    , ("writeIORef", (arr (TAp (TC "IORef") (TV "a")) (arr (TV "a") (TAp (TC "IO") (TC "()"))),
      A (A (ro "R") (ro "WRITEREF")) (ro "B")))
    , ("exitSuccess", (TAp (TC "IO") (TV "a"), ro "END"))
    , ("unsafePerformIO", (arr (TAp (TC "IO") (TV "a")) (TV "a"), A (A (ro "C") (A (ro "T") (ro "END"))) (ro "K")))
    , ("fail#", (TV "a", A (V "unsafePerformIO") (V "exitSuccess")))
    , ("join#", (TV "a", A (V "unsafePerformIO") (V "exitSuccess")))
    , ("normalizeInt", (arr (TC "Int") (arr (TV "a") (TV "a")), A (A (ro "C") (ro "B")) (ro "K")))
    , ("word64Add", (wordy, A (ro "QQ") (ro "DADD")))
    , ("word64Sub", (wordy, A (ro "QQ") (ro "DSUB")))
    , ("word64Mul", (wordy, A (ro "QQ") (ro "DMUL")))
    , ("word64Div", (wordy, A (ro "QQ") (ro "DDIV")))
    , ("word64Mod", (wordy, A (ro "QQ") (ro "DMOD")))
    , ("word64Shl", (wordy, A (ro "QQ") (ro "DSHL")))
    , ("word64Shr", (wordy, A (ro "QQ") (ro "DSHR")))
    , ("vmRunScratchpad", (arr (TC "Int") $ TAp (TC "IO") (TV "()"), A (ro "T") $ ro "VMRUN"))
    , ("vmPtr", (arr (TV "a") $ TAp (TC "IO") (TC "Word"), ro "VMPTR"))
    , ("vmSuspend", (arr (TAp (TC "IO") (TV "a")) $ TAp (TC "IO") (TC "()"), ro "SUSPEND"))
    ]
    ++ map (\(s, v) -> (s, (dyad "Int", bin v)))
      [ ("intAdd", "ADD")
      , ("intSub", "SUB")
      , ("intMul", "MUL")
      , ("intDiv", "DIV")
      , ("intMod", "MOD")
      , ("intQuot", "QUOT")
      , ("intRem", "REM")
      , ("intAnd", "AND")
      , ("intOr", "OR")
      , ("intXor", "XOR")
      , ("intShl", "SHL")
      , ("intShr", "SHR")
      ]
    ++ map (\(s, v) -> (s, (dyad "Word", bin v)))
      [ ("wordAdd", "ADD")
      , ("wordSub", "SUB")
      , ("wordMul", "MUL")
      , ("wordDiv", "U_DIV")
      , ("wordMod", "U_MOD")
      , ("wordQuot", "U_DIV")
      , ("wordRem", "U_MOD")
      , ("wordAnd", "AND")
      , ("wordOr", "OR")
      , ("wordXor", "XOR")
      , ("wordShl", "SHL")
      , ("wordShr", "U_SHR")
      ]

expandTypeAliases neat = pure $ if size als == 0 then neat else neat
  { typedAsts = subTA <$> typedAsts neat
  , dataCons = second (map subDataCons) <$> dataCons neat
  } where
  als = typeAliases neat
  subTA (Qual ps ty, t) = (Qual ps $ go ty, t)
  go ty = case ty of
    TC s -> maybe ty id $ mlookup s als
    TAp x y -> TAp (go x) (go y)
    _ -> ty
  subDataCons (Constr s sts) = Constr s $ second go <$> sts

tabulateModules mods = foldM ins Tip =<< mapM go mods where
  go (name, (mexs, prog)) = (name,) <$> (expandTypeAliases =<< maybe Right processExports mexs (prog neatEmpty {moduleImports = singleton "" [("#", const True)]}))
  ins tab (k, v) = case mlookup k tab of
    Nothing -> Right $ insert k v tab
    Just _ -> Left $ "duplicate module: " ++ k
  processExports exs neat = do
    mes <- Just . concat <$> mapM (processExport neat) exs
    pure neat { moduleExports = mes }
  processExport neat = \case
    ExportVar v -> case mlookup v $ topDefs neat of
      Nothing -> Left $ "bad export " ++ v
      Just _ -> Right [v]
    ExportCon c ns -> case mlookup c $ type2Cons neat of
      Just cnames
        | ns == [".."] -> Right cnames
        | null delta -> Right ns
        | True -> Left $ "bad exports: " ++ show delta
        where delta = [n | n <- ns, not $ elem n cnames]
      Nothing -> case mlookup c $ typeclasses neat of
        Nothing -> Left $ "bad export " ++ c
        Just methodNames
          | ns == [".."] -> Right methodNames
          | null delta -> Right ns
          | True -> Left $ "bad exports: " ++ show delta
          where delta = [n | n <- ns, not $ elem n methodNames]

data Searcher = Searcher
  { astLink :: Ast -> Either String ([String], Ast)
  , findPrec :: Ast -> (Int, Assoc)
  , findCon :: String -> Either String (Qual, [Constr])
  , findField :: String -> (String, [(String, Type)])
  , typeOfMethod :: String -> Either String Qual
  , findSigs :: String -> [String]
  , findInstances :: String -> [(String, Instance)]
  }

isLegalExport s neat = case moduleExports neat of
  Nothing -> True
  Just es -> elem s es

findAmong fun viz s = case concat $ maybe [] (:[]) . mlookup s . fun <$> viz s of
  [] -> Left $ "missing: " ++ s
  [unique] -> Right unique
  _ -> Left $ "ambiguous: " ++ s

assertType x t = A (E $ Basic "@") $ A x (E $ XQual t)

slowUnionWith f x y = foldr go x $ toAscList y where go (k, v) m = insertWith f k v m

searcherNew thisModule tab neat = Searcher
  { astLink = astLink'
  , findPrec = findPrec'
  , findCon = findAmong dataCons visible
  , findField = findField'
  , typeOfMethod = fmap fst . findAmong typedAsts visible
  , findSigs = \s -> case mlookup s mergedSigs of
    Nothing -> error $ "missing class: " ++ s
    Just [sigs] -> sigs
    _ -> error $ "ambiguous class: " ++ s
  , findInstances = maybe [] id . (`mlookup` mergedInstances)
  }
  where
  mergedSigs = foldr (slowUnionWith (++)) Tip $ map (fmap (:[]) . typeclasses) $ neat : map ((tab !) . fst) imps
  mergedInstances = foldr (slowUnionWith (++)) Tip [fmap (map (im,)) $ instances x | (im, x) <- ("", neat) : map (\(im, _) -> (im, tab ! im)) imps]
  defPrec = (9, LAssoc)
  findPrec' = \case
    V s
      | s == ":" -> (5, RAssoc)
      | otherwise -> either (const defPrec) id $ findAmong opFixity visible s
    E (Link q s)
      | q == thisModule -> findPrec' $ V s
      | otherwise -> either (const defPrec) id $ findAmong opFixity (map snd . qualNeats q) s
    _ -> error "unreachable"
  findImportSym s = concat [maybe [] (\(t, _) -> [(im, t)]) $ mlookup s $ typedAsts n | (im, n) <- importedNeats s]
  findQualifiedSym q s = do
    (im, n) <- qualNeats q s
    maybe [] (\(t, _) -> [(im, t)]) $ mlookup s $ typedAsts n
  qualNeats q s = [(im, n) | (im, isLegalImport) <- maybe [] id $ mlookup q $ moduleImports neat, let n = tab ! im, isLegalImport s && isLegalExport s n]
  importedNeats s@(h:_) = [(im, n) | (im, isLegalImport) <- imps, let n = tab ! im, h == '{' || isLegalImport s && isLegalExport s n]
  visible s = neat : (snd <$> importedNeats s)
  findField' f = case [(con, fields) | dc <- dataCons <$> visible f, (_, (_, cons)) <- toAscList dc, Constr con fields <- cons, (f', _) <- fields, f == f'] of
    [] -> error $ "no such field: " ++ f
    h:_ -> h
  imps = moduleImports neat ! ""
  astLink' ast = runDep $ go [] ast where
    go bound ast = case ast of
      V s
        | elem s bound -> pure ast
        | member s $ topDefs neat -> unlessAmbiguous s $ addDep s *> pure ast
        | member s $ typedAsts neat -> unlessAmbiguous s $ pure ast
        | True -> case findImportSym s of
          [] -> badDep $ "missing: " ++ s
          [(im, t)] -> pure $ assertType (E $ Link im s) t
          _ -> badDep $ "ambiguous: " ++ s
      A x y -> A <$> go bound x <*> go bound y
      L s t -> L s <$> go (s:bound) t
      E (Link q s)
        | q == thisModule -> go bound $ V s
        | otherwise -> case findQualifiedSym q s of
          [] -> badDep $ "missing: " ++ q ++ "." ++ s
          [(truename, t)] -> pure $ assertType (E $ Link truename s) t
          _ -> badDep $ "BUG! unreachable: " ++ q ++ "." ++ s
      _ -> pure ast
    unlessAmbiguous s f = case findImportSym s of
      [] -> f
      [(im, _)] -> if im == thisModule then f else badDep $ "ambiguous: " ++ s

neatPrim = foldr (\(a, b) -> addAdt a b []) neatEmpty { typedAsts = fromList prims } primAdts

singleFile s = parseProgram s >>= tabulateModules
