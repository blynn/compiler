-- Arbitrary precision integers.
module Typer where

import Base
import Map
import Ast
import Parser
import Unify
import Obj

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

patEq lit b x y = A (L "join#" $ A (A (A (V "if") (A (A (V "==") lit) b)) x) $ V "join#") y

rewriteCase searcher caseVar tab = \case
  [] -> flush $ V "join#"
  ((v, x):rest) -> go v x rest
  where
  rec = rewriteCase searcher caseVar
  go v x rest = case v of
    PatLit lit -> flush =<< patEq lit (V caseVar) x <$> rec Tip rest
    PatVar s m -> let x' = fill [(s, V caseVar)] x in case m of
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
    E _ -> t
    V _ -> t
    A (E (Basic "{=")) (A rawExpr fbsAst) -> let
      expr = go rawExpr
      fromAst t = case t of
        A (A (V f) body) rest -> (f, go body):fromAst rest
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
  A x y -> A <$> go x <*> go y
  L s b
    | s == "(" -> infixer searcher =<< go b
    | True -> L s <$> go b
  Pa vsxs -> Pa <$> mapM (\(ps, a) -> (,) <$> mapM pgo ps <*> go a) vsxs
  where
  go = fixFixity searcher
  pgo = pure . patFixFixity searcher

data OpTree = OpLeaf Ast | OpNode Ast Ast OpTree

infixer searcher (A (A (A s x) y) t) = go (OpNode s x (OpLeaf y)) t
  where
  go acc = \case
    A (A s z) rest -> go (ins s z acc) rest
    V (")") -> pure $ decode acc
    _ -> error "unreachable"
  ins s z t = case t of
    OpNode s' x y
      | isStronger searcher s s' -> OpNode s' x (ins s z y)
      | True -> OpNode s (decode t) (OpLeaf z)
    OpLeaf x -> OpNode s x (OpLeaf z)
  decode = \case
    OpNode f x y -> A (A f x) (decode y)
    OpLeaf x -> x

isStronger searcher s s' = if prec <= prec'
  then if prec == prec'
    then if assoc == assoc'
      then case assoc of
        LAssoc -> False
        RAssoc -> True
        NAssoc -> error $ "adjacent NAssoc: " ++ show s ++ " vs " ++ show s'
      else error $ "assoc mismatch: " ++ show s ++ " vs " ++ show s'
    else False
  else True
  where
  (prec, assoc) = findPrec searcher s
  (prec', assoc') = findPrec searcher s'

patFixFixity searcher p = case p of
  PatLit _ -> p
  PatVar s m -> PatVar s $ go <$> m
  PatCon "{+" args -> patFixer searcher args
  PatCon con args -> PatCon con $ go <$> args
  where
  go = patFixFixity searcher

data PopTree = PopLeaf Pat | PopNode String Pat PopTree

patFixer searcher (PatCon f [a, b]:rest) = go seed rest where
  seed = PopNode f a (PopLeaf b)
  go acc = \case
    [] -> decode acc
    PatCon s [z]:rest -> go (ins s z acc) rest
  ins s z t = case t of
    PopNode s' x y
      | isStronger searcher (V s) (V s') -> PopNode s' x (ins s z y)
      | True -> PopNode s (decode t) (PopLeaf z)
    PopLeaf x -> PopNode s x (PopLeaf z)
  decode = \case
    PopNode f x y -> PatCon f [x, decode y]
    PopLeaf x -> x

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

fv f bound = \case
  V s | not (elem s bound) && f s -> [s]
  A x y -> fv f bound x `union` fv f bound y
  L s t -> fv f (s:bound) t
  _ -> []

fill tab = go m where
  m = fromList tab
  go m t = case t of
    V s | Just x <- mlookup s m -> x
    A x y -> A (go m x) (go m y)
    L s t' -> L s $ go (delete s m) t'
    _ -> t

triangulate tab x = foldr triangle x components where
  vs = fst <$> tab
  ios = foldr (\(s, t) (ins, outs) -> let dsts = fv (`elem` vs) [] t in
    (foldr (\dst -> insertWith union dst [s]) ins dsts, insertWith union s dsts outs))
    (Tip, Tip) tab
  components = scc (\k -> maybe [] id $ mlookup k $ fst ios) (\k -> maybe [] id $ mlookup k $ snd ios) vs
  triangle names expr = let
    tnames = nonemptyTails names
    appem vs = foldl1 A $ V <$> vs
    suball x = fill (zip (init names) $ appem <$> init tnames) x
    redef tns x = foldr L (suball x) tns
    in foldr (\(x:xt) t -> A (L x t) $ maybeFix x $ redef xt $ maybe (error $ "oops: " ++ x) id $ lookup x tab) (suball expr) tnames

decodeLets x = decodeVars id x where
  decodeVars f = \case
    L "in" t -> decodeBodies id vts t
    L v t -> case t of
      A (E (XQual q)) t' -> decodeVars (f . ((v, Just q):)) t'
      _ -> decodeVars (f . ((v, Nothing):)) t
    where
    vts = f []
    decodeBodies g [] x = ((vts, g []), x)
    decodeBodies g (_:t) (A x y) = decodeBodies (g . (x:)) t y

infer' msg typed loc ast = case ast of
  E x -> case x of
    Lit (t, _) -> pure (t, ast)
    bug -> error $ show bug
  V s
    | Just t <- lookup s loc <|> Right . fst <$> mlookup s typed ->
      either (pure . (, ast)) (insta ast) t
    | otherwise -> Infer $ const $ Left $ "undefined: " ++ s
  A x y -> do
    (tx, ax) <- rec loc x
    (ty, ay) <- rec loc y
    va <- getFreshVar
    inferUnify msg tx (arr ty va)
    pure (va, A ax ay)
  L "=" (A raw (E (XQual q))) -> insta raw q
  L "::" (A x (E (XQual q))) -> do
    (tx, ax) <- rec loc x
    (tAnno, aAnno) <- insta ax q
    cs <- getConstraints
    case match (apply cs tx) tAnno of
      Nothing -> Infer $ const $ Left $ msg ++ ": bad match"
      Just ms -> do
        putConstraints $ ms @@ cs
        pure (tAnno, aAnno)
  L "let" lets -> do
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
  L s x -> do
    va <- getFreshVar
    (tx, ax) <- rec ((s, Left va):loc) x
    pure (arr va tx, L s ax)
  bug -> error $ show bug
  where
  rec = infer' msg typed
  insta x q = do
    Qual preds t <- inferInstantiate q
    pure (t, foldl A x (map Proof preds))

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

app01 s x y = maybe orig snd $ go [] x where
  orig = A (L s x) y
  go bnd expr = case expr of
    V v | s == v -> case fv (`elem` bnd) [] y of
      [] -> Just (True, y)
      _ -> Nothing
    A l r -> do
      (a, l') <- go bnd l
      (b, r') <- go bnd r
      if a && b then Nothing else pure (a || b, A l' r')
    L v t -> if v == s then Just (False, expr) else second (L v) <$> go (v:bnd) t
    _ -> Just (False, expr)

optiApp t = case t of
  A x y -> let
    x' = optiApp x
    y' = optiApp y
    in case x' of
      L s v -> app01 s v y'
      _ -> A x' y'
  L s x -> L s (optiApp x)
  _ -> t

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
    (psn, a) <- prove searcher psn $ optiApp a
    pure ((s, (t, a)):acc, psn)
  in do
    ((stas, preds), (soln, _)) <- foldM principal (([], []), (Tip, 0)) syms
    let ps = zip preds $ ("anno*"++) . show <$> [0..]
    (stas, (ps, _)) <- foldM gatherPreds ([], (ps, 0)) $ second (typeAstSub soln) <$> stas
    (ps, subs) <- foldM (defaultNum searcher) (ps, []) stas
    -- Annotated types are tricky.
    let
      applyDicts preds dicts subs (s, (t, a)) = (s, (Qual preds t, foldr L (fill (subs ++ tab) a) dicts))
        where tab = maybe (map (\s -> (s, foldl A (V s) $ V <$> dicts)) syms) (const []) $ mlookup s decls
    pure $ applyDicts (fst <$> ps) (snd <$> ps) subs <$> stas

defaultNum searcher (preds, subs) (s, (t, a)) = foldM go ([], subs) preds where
  defaults = foldr ($) Tip $ insDefault . fst <$> preds
  insDefault = \case
    Pred "Field" (TV v) -> insert v (TC "Rational")
    Pred "Ring" (TV v) -> insertWith (const id) v (TC "Integer")
    _ -> id
  go (ps, subs) p@(pred, dictVar) = case pred of
    Pred cl (TV v) | not $ v `elem` typeVars t -> case mlookup v defaults of
      Just t -> do
        (_, ast) <- findProof searcher (Pred cl t) ([], 0)
        pure $ (ps, (dictVar, ast):subs)
      Nothing -> Left $ "ambiguous: " ++ s ++ ": " ++ show pred
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
          ((ps3, _), tr) <- prove searcher (dictVars ps2 0) $ optiApp $ proofApply subx ax
          if length ps2 /= length ps3
            then Left $ ("want context: "++) . (foldr (.) id $ shows . fst <$> ps3) $ name
            else pure tr
    in do
      ms <- mapM perMethod $ findSigs searcher classId
      pure $ insert name (Qual [] $ TC "DICTIONARY", flip (foldr L) dvs $ L "@" $ foldl A (V "@") ms) typed

primAdts =
  [ (("()", []), [Constr "()" []])
  , (("Bool", []), [Constr "True" [], Constr "False" []])
  , (("[]", ["a"]), [Constr "[]" [], Constr ":" $ map ("",) [TV "a", TAp (TC "[]") (TV "a")]])
  , ((",", ["a", "b"]), [Constr "," $ map ("",) [TV "a", TV "b"]])
  ]

prims = let
  ro = E . Basic
  dyad s = TC s `arr` (TC s `arr` TC s)
  wordy = foldr arr (TAp (TAp (TC ",") (TC "Word")) (TC "Word")) [TC "Word", TC "Word", TC "Word", TC "Word"]
  bin s = A (ro "Q") (ro s)
  in map (second (first $ Qual [])) $
    [ ("doubleFromInt", (arr (TC "Int") (TC "Double"), A (ro "T") (ro "FLO")))
    , ("intFromDouble", (arr (TC "Double") (TC "Int"), A (ro "T") (ro "OLF")))
    , ("doubleFromWord", (arr (TC "Word") (TC "Double"), A (ro "T") (ro "FLW")))
    , ("doubleAdd", (arr (TC "Double") (arr (TC "Double") (TC "Double")), A (ro "Q") (ro "FADD")))
    , ("doubleSub", (arr (TC "Double") (arr (TC "Double") (TC "Double")), A (ro "Q") (ro "FSUB")))
    , ("doubleMul", (arr (TC "Double") (arr (TC "Double") (TC "Double")), A (ro "Q") (ro "FMUL")))
    , ("doubleDiv", (arr (TC "Double") (arr (TC "Double") (TC "Double")), A (ro "Q") (ro "FDIV")))
    , ("doubleEq", (arr (TC "Double") (arr (TC "Double") (TC "Bool")), bin "FEQ"))
    , ("doubleLE", (arr (TC "Double") (arr (TC "Double") (TC "Bool")), bin "FLE"))
    , ("doubleFloor", (arr (TC "Double") (TC "Double"), A (ro "T") (ro "FFLOOR")))
    , ("sqrt", (arr (TC "Double") (TC "Double"), A (ro "T") (ro "FSQRT")))
    , ("rawDouble", (arr (TC "Double") $ arr (arr (TC "Word") $ arr (TC "Word") $ TV "a") $ TV "a", A (ro "T") (ro "PAIR64")))
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

tySub binds = go where
  go = \case
    TAp x y -> TAp (go x) (go y)
    TV v | Just ty <- lookup v binds -> ty
    t -> t

unaliasType als = go [] where
  go spine = \case
    TAp x y -> go (go [] y:spine) x
    TC s | Just (vs, ty) <- mlookup s als -> tySub (zip vs spine) ty
    t -> foldl TAp t spine

expandTypeAliases als neat = if size als == 0 then neat else neat
  { typedAsts = (\(q, a) -> (subQual q, subAst a)) <$> typedAsts neat
  , topDecls = subQual <$> topDecls neat
  , dataCons = (\(q, cs) -> (subQual q, subConstr <$> cs)) <$> dataCons neat
  } where
  subQual (Qual ps ty0) = (Qual ps $ unaliasType als ty0)
  subConstr (Constr s sts) = Constr s $ second (unaliasType als) <$> sts
  subAst = \case
    E (XQual q) -> E $ XQual $ subQual q
    A x y -> A (subAst x) (subAst y)
    t -> t

tabulateModules mods = foldM ins (Tip, Tip) mods where
  ins (todo, done) (k, porh) = if member k todo || member k done
    then Left $ "duplicate module: " ++ k
    else case porh of
      ParsedNeat f -> do
        v <- processExports (f neatEmpty {moduleImports = singleton "" [("#", const True)]})
        pure (insert k v todo, done)
      HexNeat s -> pure (todo, insert k (decodeObject s) done)
  processExports neat = case exportDecl $ exportStuff neat of
    Nothing -> pure neat
    Just exs -> do
      mes <- Just . concat <$> mapM (processExport neat) exs
      pure neat { exportStuff = (exportStuff neat) { moduleExports = mes } }
  processExport neat = \case
    ExportVar v -> case mlookup v $ topDefs neat of
      Nothing -> Left $ "bad export " ++ v
      Just _ -> Right [v]
    ExportCon c ns -> case mlookup c $ type2Cons $ exportStuff neat of
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
  { _thisModule :: String
  , _neatTab :: Map String Neat
  , _thisNeat :: Neat
  , _mergedSigs :: Map String [[String]]
  , _mergedInstances :: Map String [(String, Instance)]
  }

searcherNew thisModule tab neatAliased =
    Searcher thisModule tab neat mergedSigs mergedInstances
  where
  imps = moduleImports neatAliased ! ""
  mergedAliases = foldr (slowUnionWith const) Tip $ map typeAliases $ neatAliased : map ((tab !) . fst) imps
  neat = expandTypeAliases mergedAliases neatAliased
  mergedSigs = foldr (slowUnionWith (++)) Tip $ map (fmap (:[]) . typeclasses) $ neat : map ((tab !) . fst) imps
  mergedInstances = foldr (slowUnionWith (++)) Tip [fmap (map (im,)) $ instances x | (im, x) <- ("", neat) : map (\(im, _) -> (im, tab ! im)) imps]

astLink sea ast = runDep $ go [] ast where
  go bound ast = case ast of
    V s
      | elem s bound -> pure ast
      | member s $ topDefs $ _thisNeat sea -> unlessAmbiguous s $ addDep s *> pure ast
      | member s $ typedAsts $ _thisNeat sea -> unlessAmbiguous s $ pure ast
      | True -> case findImportSym sea s of
        [] -> badDep $ "missing: " ++ s
        [(im, t)] -> pure $ assertType (E $ Link im s) t
        _ -> badDep $ "ambiguous: " ++ s
    A x y -> A <$> go bound x <*> go bound y
    L s t -> L s <$> go (s:bound) t
    E (Link q s)
      | q == _thisModule sea -> go bound $ V s
      | otherwise -> case findQualifiedSym sea q s of
        [] -> badDep $ "missing: " ++ q ++ "." ++ s
        [(truename, t)] -> pure $ assertType (E $ Link truename s) t
        _ -> badDep $ "BUG! unreachable: " ++ q ++ "." ++ s
    _ -> pure ast
  unlessAmbiguous s f = case findImportSym sea s of
    [] -> f
    [(im, _)] -> if im == _thisModule sea then f else badDep $ "ambiguous: " ++ s

visible sea s = _thisNeat sea : (snd <$> importedNeats sea s)

importedNeats sea s@(h:_) = [(im, n) | (im, isLegalImport) <- imps, let n = _neatTab sea ! im, h == '{' || isLegalImport s && isLegalExport s n]
  where
  imps = moduleImports (_thisNeat sea) ! ""

qualNeats sea q s = [(im, n) | (im, isLegalImport) <- maybe [] id $ mlookup q $ moduleImports $ _thisNeat sea, let n = _neatTab sea ! im, isLegalImport s && isLegalExport s n]

isLegalExport s neat = case moduleExports $ exportStuff neat of
  Nothing -> True
  Just es -> elem s es

findPrec sea = \case
  V s
    | s == ":" -> (5, RAssoc)
    | otherwise -> either (const defPrec) id $ findAmong opFixity (visible sea) s
  E (Link q s)
    | q == _thisModule sea -> findPrec sea $ V s
    | otherwise -> either (const defPrec) id $ findAmong opFixity (map snd . qualNeats sea q) s
  _ -> error "unreachable"
  where
  defPrec = (9, LAssoc)
findField sea f = case [(con, fields) | dc <- dataCons <$> visible sea f, (_, (_, cons)) <- toAscList dc, Constr con fields <- cons, (f', _) <- fields, f == f'] of
  [] -> error $ "no such field: " ++ f
  h:_ -> h
findSigs sea = \s -> case mlookup s $ _mergedSigs sea of
  Nothing -> error $ "missing class: " ++ s
  Just [sigs] -> sigs
  _ -> error $ "ambiguous class: " ++ s
findImportSym sea s = concat [maybe [] (\(t, _) -> [(im, t)]) $ mlookup s $ typedAsts n | (im, n) <- importedNeats sea s]
findQualifiedSym sea q s = do
  (im, n) <- qualNeats sea q s
  maybe [] (\(t, _) -> [(im, t)]) $ mlookup s $ typedAsts n
findAmong fun viz s = case concat $ maybe [] (:[]) . mlookup s . fun <$> viz s of
  [] -> Left $ "missing: " ++ s
  [unique] -> Right unique
  _ -> Left $ "ambiguous: " ++ s
findInstances sea = maybe [] id . (`mlookup` _mergedInstances sea)
typeOfMethod sea = fmap fst . findAmong typedAsts (visible sea)
findCon sea = findAmong dataCons $ visible sea

assertType x t = L "=" $ A x (E $ XQual t)

slowUnionWith f x y = foldr go x $ toAscList y where go (k, v) m = insertWith f k v m

neatPrim = foldr (\(a, b) -> addAdt a b []) neatEmpty { typedAsts = fromList prims } primAdts

singleFile s = parseProgram s >>= tabulateModules
