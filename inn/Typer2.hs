-- FFI across multiple modules.
module Typer where

import Base
import Map
import Ast
import Parser
import Unify

app01 s x y = maybe (A (L s x) y) snd $ go x where
  go expr = case expr of
    E _ -> Just (False, expr)
    V v -> Just $ if s == v then (True, y) else (False, expr)
    A l r -> do
      (a, l') <- go l
      (b, r') <- go r
      if a && b then Nothing else pure (a || b, A l' r')
    L v t -> if v == s then Just (False, expr) else second (L v) <$> go t

optiApp t = case t of
  A x y -> let
    x' = optiApp x
    y' = optiApp y
    in case x' of
      L s v -> app01 s v y'
      _ -> A x' y'
  L s x -> L s (optiApp x)
  _ -> t

-- Pattern compiler.
findCon dcs s = foldr (<|>) Nothing $ mlookup s <$> dcs

rewritePats dcs = \case
  [] -> pure $ V "join#"
  vsxs@((as0, _):_) -> case as0 of
    [] -> pure $ foldr1 (A . L "join#") $ snd <$> vsxs
    _ -> do
      let k = length as0
      n <- get
      put $ n + k
      let vs = take k $ (`shows` "#") <$> [n..]
      cs <- flip mapM vsxs \(a:at, x) -> (a,) <$> foldM (\b (p, v) -> rewriteCase dcs v Tip [(p, b)]) x (zip at $ tail vs)
      flip (foldr L) vs <$> rewriteCase dcs (head vs) Tip cs

patEq lit b x y = A (L "join#" $ A (A (A (V "if") (A (A (V "==") lit) b)) x) $ V "join#") y

rewriteCase dcs caseVar tab = \case
  [] -> flush $ V "join#"
  ((v, x):rest) -> go v x rest
  where
  rec = rewriteCase dcs caseVar
  go v x rest = case v of
    PatLit lit -> flush =<< patEq lit (V caseVar) x <$> rec Tip rest
    PatVar s m -> let x' = beta s (V caseVar) x in case m of
      Nothing -> flush =<< A (L "join#" x') <$> rec Tip rest
      Just v' -> go v' x' rest
    PatCon con args -> rec (insertWith (flip (.)) con ((args, x):) tab) rest
  flush onFail = case toAscList tab of
    [] -> pure onFail
    -- TODO: Check rest of `tab` lies in cs.
    (firstC, _):_ -> do
      let cs = maybe undefined id $ findCon dcs firstC
      jumpTable <- mapM (\(Constr s ts) -> case mlookup s tab of
          Nothing -> pure $ foldr L (V "join#") $ const "_" <$> ts
          Just f -> rewritePats dcs $ f []
        ) cs
      pure $ A (L "join#" $ foldl A (A (V $ specialCase cs) $ V caseVar) jumpTable) onFail

findField dcs f = case [(con, fields) | tab <- dcs, (_, cons) <- toAscList tab, Constr con fields <- cons, (f', _) <- fields, f == f'] of
  [] -> error $ "no such field: " ++ f
  h:_ -> h

resolveFieldBinds dcs t = go t where
  go t = case t of
    E _ -> t
    V _ -> t
    A (E (Basic "{=")) (A rawExpr fbsAst) -> let
      expr = go rawExpr
      fromAst t = case t of
        A (A (V f) body) rest -> (f, go body):fromAst rest
        E (Basic "=}") -> []
      fbs@((firstField, _):_) = fromAst fbsAst
      (con, fields) = findField dcs firstField
      cs = maybe undefined id $ findCon dcs con
      newValue = foldl A (V con) [maybe (V $ "[old]"++f) id $ lookup f fbs | (f, _) <- fields]
      initValue = foldl A expr [maybe (V "undefined") id $ lookup f fbs | (f, _) <- fields]
      updater = foldr L newValue $ ("[old]"++) . fst <$> fields
      inj x = map (\(Constr con' _) -> if con' == con then x else V "undefined") cs
      allPresent = all (`elem` (fst <$> fields)) $ fst <$> fbs
      isCon = case expr of
        V (h:_) -> 'A' <= h && h <= 'Z'
        _ -> False
      in if allPresent
        then if isCon then initValue else foldl A (A (V $ specialCase cs) expr) $ inj updater
        else error "bad fields in update"
    A x y -> A (go x) (go y)
    L s x -> L s $ go x

fixFixity precs t = case t of
  E _ -> t
  V _ -> t
  A x y -> A (go x) (go y)
  L s b
    | s == "(" -> infixer precs $ go b
    | True -> L s $ go b
  Pa vsxs -> Pa $ map (\(ps, a) -> (patFixFixity precs <$> ps, go a)) vsxs
  where
  go = fixFixity precs

data OpTree = OpLeaf Ast | OpNode String Ast OpTree

infixer precs (A (A (A (V s) x) y) t) = go (OpNode s x (OpLeaf y)) t
  where
  go acc = \case
    A (A (V s) z) rest -> go (ins s z acc) rest
    V ")" -> decode acc
    _ -> error "unreachable"
  ins s z t = case t of
    OpNode s' x y
      | isStronger precs s s' -> OpNode s' x (ins s z y)
      | True -> OpNode s (decode t) (OpLeaf z)
    OpLeaf x -> OpNode s x (OpLeaf z)
  decode = \case
    OpNode f x y -> A (A (V f) x) (decode y)
    OpLeaf x -> x

isStronger precs s s' = if prec <= prec'
  then if prec == prec'
    then if assoc == assoc'
      then case assoc of
        LAssoc -> False
        RAssoc -> True
        NAssoc -> error $ "adjacent NAssoc: " ++ s ++ " vs " ++ s'
      else error $ "assoc mismatch: " ++ s ++ " vs " ++ s'
    else False
  else True
  where
  (prec, assoc) = findPrec s
  (prec', assoc') = findPrec s'
  findPrec s = if s == ":" then (5, RAssoc) else maybe defPrec id $ mlookup s precs
  defPrec = (9, LAssoc)

patFixFixity precs p = case p of
  PatLit _ -> p
  PatVar s m -> PatVar s $ go <$> m
  PatCon "{+" args -> patFixer precs args
  PatCon con args -> PatCon con $ go <$> args
  where
  go = patFixFixity precs

data PopTree = PopLeaf Pat | PopNode String Pat PopTree

patFixer precs (PatCon f [a, b]:rest) = go seed rest where
  seed = PopNode f a (PopLeaf b)
  go acc = \case
    [] -> decode acc
    PatCon s [z]:rest -> go (ins s z acc) rest
  ins s z t = case t of
    PopNode s' x y -> case isStronger precs s s' of
      True -> PopNode s' x $ ins s z y
      False -> PopNode s (decode t) (PopLeaf z)
    PopLeaf x -> PopNode s x (PopLeaf z)
  decode = \case
    PopNode f x y -> PatCon f [x, decode y]
    PopLeaf x -> x

secondM f (a, b) = (a,) <$> f b
patternCompile dcs t = optiApp $ resolveFieldBinds dcs $ evalState (go t) 0 where
  go t = case t of
    E _ -> pure t
    V _ -> pure t
    A x y -> liftA2 A (go x) (go y)
    L s x -> L s <$> go x
    Pa vsxs -> mapM (secondM go) vsxs >>= rewritePats dcs

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

infer typed loc ast csn@(cs, n) = case ast of
  E x -> Right $ case x of
    Const _ -> ((TC "Int", ast), csn)
    ChrCon _ -> ((TC "Char", ast), csn)
    StrCon _ -> ((TAp (TC "[]") (TC "Char"), ast), csn)
    Link im s q -> insta q
  V s -> maybe (Left $ "undefined: " ++ s) Right
    $ (\t -> ((t, ast), csn)) <$> lookup s loc
    <|> insta . fst <$> mlookup s typed
  A x y -> infer typed loc x (cs, n + 1) >>=
    \((tx, ax), csn1) -> infer typed loc y csn1 >>=
    \((ty, ay), (cs2, n2)) -> unify tx (arr ty va) cs2 >>=
    \cs -> Right ((va, A ax ay), (cs, n2))
  L s x -> first (\(t, a) -> (arr va t, L s a)) <$> infer typed ((s, va):loc) x (cs, n + 1)
  where
  va = TV $ show n
  insta ty = ((ty1, foldl A ast (map Proof preds)), (cs, n1))
    where (Qual preds ty1, n1) = instantiate ty n

findInstance tycl qn@(q, n) p@(Pred cl ty) insts = case insts of
  [] -> let v = '*':show n in Right (((p, v):q, n + 1), V v)
  (modName, Instance h name ps _):rest -> case match h ty of
    Nothing -> findInstance tycl qn p rest
    Just subs -> foldM (\(qn1, t) (Pred cl1 ty1) -> second (A t)
      <$> findProof tycl (Pred cl1 $ apply subs ty1) qn1) (qn, if modName == "" then V name else E $ Link modName name undefined) ps

findProof tycl pred@(Pred classId t) psn@(ps, n) = case lookup pred ps of
  Nothing -> findInstance tycl psn pred $ tycl classId
  Just s -> Right (psn, V s)

prove tycl psn a = case a of
  Proof pred -> findProof tycl pred psn
  A x y -> prove tycl psn x >>= \(psn1, x1) ->
    second (A x1) <$> prove tycl psn1 y
  L s t -> second (L s) <$> prove tycl psn t
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

astLink typed locals imps mods ast = runDep $ go [] ast where
  go bound ast = case ast of
    V s
      | elem s bound -> pure ast
      | member s locals -> case findImportSym imps mods s of
        [] -> (if member s typed then pure () else addDep s) *> pure ast
        _ -> badDep $ "ambiguous: " ++ s
      | True -> case findImportSym imps mods s of
        [] -> badDep $ "missing: " ++ s
        [(im, t)] -> pure $ E $ Link im s t
        _ -> badDep $ "ambiguous: " ++ s
    A x y -> A <$> go bound x <*> go bound y
    L s t -> L s <$> go (s:bound) t
    _ -> pure ast

forFree cond f bound t = case t of
  E _ -> t
  V s -> if (not $ s `elem` bound) && cond s then f t else t
  A x y -> A (rec bound x) (rec bound y)
  L s t' -> L s $ rec (s:bound) t'
  where rec = forFree cond f

inferno tycl typed defmap syms = let
  loc = zip syms $ TV . (' ':) <$> syms
  principal (acc, (subs, n)) s = do
    expr <- maybe (Left $ "missing: " ++ s) Right (mlookup s defmap)
    ((t, a), (ms, n1)) <- infer typed loc expr (subs, n)
    cs <- unify (TV (' ':s)) t ms
    Right ((s, (t, a)):acc, (cs, n1))
  gatherPreds (acc, psn) (s, (t, a)) = do
    (psn, a) <- prove tycl psn a
    pure ((s, (t, a)):acc, psn)
  in do
    (stas, (soln, _)) <- foldM principal ([], (Tip, 0)) syms
    stas <- pure $ second (typeAstSub soln) <$> stas
    (stas, (ps, _)) <- foldM gatherPreds ([], ([], 0)) $ second (typeAstSub soln) <$> stas
    let
      preds = fst <$> ps
      dicts = snd <$> ps
      applyDicts (s, (t, a)) = (s, (Qual preds t,
        foldr L (forFree (`elem` syms) (\t -> foldl A t $ V <$> dicts) [] a) dicts))
    pure $ map applyDicts stas

findImportSym imps mods s = concat [maybe [] (\(t, _) -> [(im, t)]) $ mlookup s qas | im <- imps, let qas = fst $ mods ! im]

inferDefs tycl defs typed = do
  let
    insertUnique m (s, (_, t)) = case mlookup s m of
      Nothing -> case mlookup s typed of
        Nothing -> Right $ insert s t m
        _ -> Left $ "reserved: " ++ s
      _ -> Left $ "duplicate: " ++ s
    addEdges (sym, (deps, _)) (ins, outs) = (foldr (\dep es -> insertWith union dep [sym] es) ins deps, insertWith union sym deps outs)
    graph = foldr addEdges (Tip, Tip) defs
  defmap <- foldM insertUnique Tip defs
  let
    ins k = maybe [] id $ mlookup k $ fst graph
    outs k = maybe [] id $ mlookup k $ snd graph
    inferComponent typed syms = foldr (uncurry insert) typed <$> inferno tycl typed defmap syms
  foldM inferComponent typed $ scc ins outs $ keys defmap

dictVars ps n = (zip ps $ map (('*':) . show) [n..], n + length ps)

inferTypeclasses tycl typeOfMethod typed dcs precs linker iMap mergedSigs = foldM inferInstance typed [(classId, inst) | (classId, insts) <- toAscList iMap, inst <- insts] where
  inferInstance typed (classId, Instance ty name ps idefs) = let
    dvs = map snd $ fst $ dictVars ps 0
    perMethod s = do
      let rawExpr = maybe (V $ "{default}" ++ s) id $ mlookup s idefs
      expr <- snd <$> linker (patternCompile dcs $ fixFixity precs rawExpr)
      (ta, (sub, n)) <- either (Left . (name++) . (" "++) . (s++) . (": "++)) Right
        $ infer typed [] expr (Tip, 0)
      let
        (tx, ax) = typeAstSub sub ta
-- e.g. qc = Eq a => a -> a -> Bool
-- We instantiate: Eq a1 => a1 -> a1 -> Bool.
        qc = typeOfMethod s
        (Qual [Pred _ headT] tc, n1) = instantiate qc n
-- Mix the predicates `ps` with the type of `headT`, applying a
-- substitution such as (a1, [a]) so the variable names match.
-- e.g. Eq a => [a] -> [a] -> Bool
        Just subc = match headT ty
        (Qual ps2 t2, n2) = instantiate (Qual ps $ apply subc tc) n1
      case match tx t2 of
        Nothing -> Left "class/instance type conflict"
        Just subx -> do
          ((ps3, _), tr) <- prove tycl (dictVars ps2 0) (proofApply subx ax)
          if length ps2 /= length ps3
            then Left $ ("want context: "++) . (foldr (.) id $ shows . fst <$> ps3) $ name
            else pure tr
    in do
      ms <- mapM perMethod $ maybe (error $ "missing class: " ++ classId) id $ mlookup classId mergedSigs
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
  bin s = A (ro "Q") (ro s)
  in map (second (first $ Qual [])) $
    [ ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "EQ"))
    , ("intLE", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "LE"))
    , ("charEq", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin "EQ"))
    , ("charLE", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin "LE"))
    , ("fix", (arr (arr (TV "a") (TV "a")) (TV "a"), ro "Y"))
    , ("if", (arr (TC "Bool") $ arr (TV "a") $ arr (TV "a") (TV "a"), ro "I"))
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
    , ("join#", (TV "a", A (V "unsafePerformIO") (V "exitSuccess")))
    , ("fail#", (TV "a", A (V "unsafePerformIO") (V "exitSuccess")))
    ]
    ++ map (\(s, v) -> (s, (dyad "Int", bin v)))
      [ ("intAdd", "ADD")
      , ("intSub", "SUB")
      , ("intMul", "MUL")
      , ("intDiv", "DIV")
      , ("intMod", "MOD")
      , ("intQuot", "QUOT")
      , ("intRem", "REM")
      , ("intXor", "XOR")
      , ("intAnd", "AND")
      , ("intOr", "OR")
      ]

tabulateModules mods = snd <$> foldM ins (Tip, Tip) mods where
  go precs = foldr ($) neatEmpty{moduleImports = ["#"], opFixity = precs}
  ins (accprecs, tab) (k, prog) = case mlookup k tab of
    Nothing -> let v = go accprecs prog in Right (opFixity v, insert k v tab)
    Just _ -> Left $ "duplicate module: " ++ k

slowUnionWith f x y = foldr go x $ toAscList y where go (k, v) m = insertWith f k v m

inferModule tab acc name = case mlookup name acc of
  Nothing -> do
    let
      Neat mySigs iMap defs typedList adtTab ffis ffes imps precs = tab ! name
      typed = fromList typedList
      mergedSigs = foldr (slowUnionWith const) Tip $ mySigs : map (typeclasses . (tab !)) imps
      mergedInstances = foldr (slowUnionWith (++)) Tip [fmap (map (im,)) m | (im, m) <- ("", iMap) : map (\im -> (im, instances $ tab ! im)) imps]
      locals = fromList $ map (, ()) $ (fst <$> typedList) ++ (fst <$> defs)
      tycl classId = maybe [] id $ mlookup classId mergedInstances
      dcs = adtTab : map (dataCons . (tab !)) imps
      typeOfMethod s = maybe undefined id $ foldr (<|>) (fst <$> mlookup s typed) [fmap fst $ lookup s $ typedAsts $ tab ! im | im <- imps]
    acc' <- foldM (inferModule tab) acc imps
    let linker = astLink typed locals imps acc'
    depdefs <- mapM (\(s, t) -> (s,) <$> linker (patternCompile dcs $ fixFixity precs t)) defs
    typed <- inferDefs tycl depdefs typed
    typed <- inferTypeclasses tycl typeOfMethod typed dcs precs linker iMap mergedSigs
    Right $ insert name (typed, (ffis, ffes)) acc'
  Just _ -> Right acc

untangle s = do
  tab <- insert "#" neatPrim <$> (parseProgram s >>= tabulateModules)
  foldM (inferModule tab) Tip $ keys tab

neatPrim = foldr (\(a, b) -> addAdt a b []) (Neat Tip Tip [] prims Tip Tip Tip [] Tip) primAdts
