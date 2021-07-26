module Typer where

import Base
import Map
import Ast
import Parser
import Unify

freeCount v expr = case expr of
  E _ -> 0
  V s -> if s == v then 1 else 0
  A x y -> freeCount v x + freeCount v y
  L w t -> if v == w then 0 else freeCount v t
app01 s x = case freeCount s x of
  0 -> const x
  1 -> flip (beta s) x
  _ -> A $ L s x
optiApp t = case t of
  A (L s x) y -> app01 s (optiApp x) (optiApp y)
  A x y -> A (optiApp x) (optiApp y)
  L s x -> L s (optiApp x)
  _ -> t

-- Pattern compiler.
singleOut s cs = \scrutinee x ->
  foldl A (A (V $ specialCase cs) scrutinee) $ map (\(Constr s' ts) ->
    if s == s' then x else foldr L (V "pjoin#") $ map (const "_") ts) cs

patEq lit b x y = A (A (A (V "if") (A (A (V "==") (E lit)) b)) x) y

unpat dcs as t = case as of
  [] -> pure t
  a:at -> get >>= \n -> put (n + 1) >> let freshv = showInt n "#" in L freshv <$> let
    go p x = case p of
      PatLit lit -> unpat dcs at $ patEq lit (V freshv) x $ V "pjoin#"
      PatVar s m -> maybe (unpat dcs at) (\p1 x1 -> go p1 x1) m $ beta s (V freshv) x
      PatCon con args -> case dcs con of
        Nothing -> error "bad data constructor"
        Just cons -> unpat dcs args x >>= \y -> unpat dcs at $ singleOut con cons (V freshv) y
    in go a t

unpatTop dcs als x = case als of
  [] -> pure x
  (a, l):alt -> let
    go p t = case p of
      PatLit lit -> unpatTop dcs alt $ patEq lit (V l) t $ V "pjoin#"
      PatVar s m -> maybe (unpatTop dcs alt) go m $ beta s (V l) t
      PatCon con args -> case dcs con of
        Nothing -> error "bad data constructor"
        Just cons -> unpat dcs args t >>= \y -> unpatTop dcs alt $ singleOut con cons (V l) y
    in go a x

rewritePats' dcs asxs ls = case asxs of
  [] -> pure $ V "fail#"
  (as, t):asxt -> unpatTop dcs (zip as ls) t >>=
    \y -> A (L "pjoin#" y) <$> rewritePats' dcs asxt ls

rewritePats dcs vsxs@((vs0, _):_) = get >>= \n -> let
  ls = map (flip showInt "#") $ take (length vs0) [n..]
  in put (n + length ls) >> flip (foldr L) ls <$> rewritePats' dcs vsxs ls

classifyAlt v x = case v of
  PatLit lit -> Left $ patEq lit (V "of") x
  PatVar s m -> maybe (Left . A . L "pjoin#") classifyAlt m $ A (L s x) $ V "of"
  PatCon s ps -> Right (insertWith (flip (.)) s ((ps, x):))

genCase dcs tab = if size tab == 0 then id else A . L "cjoin#" $ let
  firstC = flst (toAscList tab) undefined (\h _ -> fst h)
  cs = maybe (error $ "bad constructor: " ++ firstC) id $ dcs firstC
  in foldl A (A (V $ specialCase cs) (V "of"))
    $ map (\(Constr s ts) -> case mlookup s tab of
      Nothing -> foldr L (V "cjoin#") $ const "_" <$> ts
      Just f -> Pa $ f [(const (PatVar "_" Nothing) <$> ts, V "cjoin#")]
    ) cs

updateCaseSt dcs (acc, tab) alt = case alt of
  Left f -> (acc . genCase dcs tab . f, Tip)
  Right upd -> (acc, upd tab)

rewriteCase dcs as = fpair (foldl (updateCaseSt dcs) (id, Tip)
  $ uncurry classifyAlt <$> as) \acc tab -> acc . genCase dcs tab $ V "fail#"

secondM f (a, b) = (a,) <$> f b
patternCompile dcs t = optiApp $ evalState (go t) 0 where
  go t = case t of
    E _ -> pure t
    V _ -> pure t
    A x y -> liftA2 A (go x) (go y)
    L s x -> L s <$> go x
    Pa vsxs -> mapM (secondM go) vsxs >>= rewritePats dcs
    Ca x as -> liftA2 A (L "of" . rewriteCase dcs <$> mapM (secondM go) as >>= go) (go x)

-- Type inference.
instantiate' t n tab = case t of
  TC s -> ((t, n), tab)
  TV s -> case lookup s tab of
    Nothing -> let va = TV (showInt n "") in ((va, n + 1), (s, va):tab)
    Just v -> ((v, n), tab)
  TAp x y ->
    fpair (instantiate' x n tab) \(t1, n1) tab1 ->
    fpair (instantiate' y n1 tab1) \(t2, n2) tab2 ->
    ((TAp t1 t2, n2), tab2)

instantiatePred (Pred s t) ((out, n), tab) = first (first ((:out) . Pred s)) (instantiate' t n tab)

instantiate (Qual ps t) n =
  fpair (foldr instantiatePred (([], n), []) ps) \(ps1, n1) tab ->
  first (Qual ps1) (fst (instantiate' t n1 tab))

proofApply sub a = case a of
  Proof (Pred cl ty) -> Proof (Pred cl $ apply sub ty)
  A x y -> A (proofApply sub x) (proofApply sub y)
  L s t -> L s $ proofApply sub t
  _ -> a

typeAstSub sub (t, a) = (apply sub t, proofApply sub a)

infer typed loc ast csn = fpair csn \cs n ->
  let
    va = TV (showInt n "")
    insta ty = fpair (instantiate ty n) \(Qual preds ty) n1 -> ((ty, foldl A ast (map Proof preds)), (cs, n1))
  in case ast of
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

findInstance tycl qn@(q, n) p@(Pred cl ty) insts = case insts of
  [] -> let v = '*':showInt n "" in Right (((p, v):q, n + 1), V v)
  (modName, Instance h name ps _):rest -> case match h ty of
    Nothing -> findInstance tycl qn p rest
    Just subs -> foldM (\(qn1, t) (Pred cl1 ty1) -> second (A t)
      <$> findProof tycl (Pred cl1 $ apply subs ty1) qn1) (qn, if modName == "" then V name else E $ Link modName name undefined) ps

findProof tycl pred@(Pred classId t) psn@(ps, n) = case lookup pred ps of
  Nothing -> case tycl classId of
    [] -> Left $ "no instance: " ++ showPred pred ""
    insts -> findInstance tycl psn pred insts
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
    (stas, (soln, _)) <- foldM principal ([], ([], 0)) syms
    stas <- pure $ second (typeAstSub soln) <$> stas
    (stas, (ps, _)) <- foldM gatherPreds ([], ([], 0)) $ second (typeAstSub soln) <$> stas
    let
      preds = fst <$> ps
      dicts = snd <$> ps
      applyDicts (s, (t, a)) = (s, (Qual preds t,
        foldr L (forFree (`elem` syms) (\t -> foldl A t $ V <$> dicts) [] a) dicts))
    pure $ map applyDicts stas

findImportSym imps mods s = concat [maybe [] (\t -> [(im, t)]) $ mlookup s qs | im <- imps, let qs = fst $ fst $ mods ! im]

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

dictVars ps n = (zip ps $ map (('*':) . flip showInt "") [n..], n + length ps)

inferTypeclasses tycl typeOfMethod typed dcs linker ienv = foldM perClass typed $ toAscList ienv where
  perClass typed (classId, Tycl sigs insts) = foldM perInstance typed insts where
    perInstance typed (Instance ty name ps idefs) = do
      let
        dvs = map snd $ fst $ dictVars ps 0
        perMethod s = do
          let Just rawExpr = mlookup s idefs <|> pure (V $ "{default}" ++ s)
          expr <- snd <$> linker (patternCompile dcs rawExpr)
          (ta, (sub, n)) <- either (Left . (name++) . (" "++) . (s++) . (": "++)) Right
            $ infer typed [] expr ([], 0)
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
                then Left $ ("want context: "++) . (foldr (.) id $ showPred . fst <$> ps3) $ name
                else pure tr
      ms <- mapM perMethod sigs
      pure $ insert name (Qual [] $ TC "DICTIONARY", flip (foldr L) dvs $ L "@" $ foldl A (V "@") ms) typed

neatNew = Neat Tip [] [] Tip [] [] []

primAdts =
  [ (TC "()", [Constr "()" []])
  , (TC "Bool", [Constr "True" [], Constr "False" []])
  , (TAp (TC "[]") (TV "a"), [Constr "[]" [], Constr ":" [TV "a", TAp (TC "[]") (TV "a")]])
  , (TAp (TAp (TC ",") (TV "a")) (TV "b"), [Constr "," [TV "a", TV "b"]])
  ]

prims = let
  ro = E . Basic
  dyad s = TC s `arr` (TC s `arr` TC s)
  wordy = foldr arr (TAp (TAp (TC ",") (TC "Word")) (TC "Word")) [TC "Word", TC "Word", TC "Word", TC "Word"]
  bin s = A (ro "Q") (ro s)
  in map (second (first $ Qual [])) $
    [ ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "EQ"))
    , ("intLE", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "LE"))
    , ("wordLE", (arr (TC "Word") (arr (TC "Word") (TC "Bool")), bin "U_LE"))
    , ("wordEq", (arr (TC "Word") (arr (TC "Word") (TC "Bool")), bin "EQ"))

    , ("charEq", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin "EQ"))
    , ("charLE", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin "LE"))
    , ("fix", (arr (arr (TV "a") (TV "a")) (TV "a"), ro "Y"))
    , ("if", (arr (TC "Bool") $ arr (TV "a") $ arr (TV "a") (TV "a"), ro "I"))
    , ("chr", (arr (TC "Int") (TC "Char"), ro "I"))
    , ("ord", (arr (TC "Char") (TC "Int"), ro "I"))
    , ("ioBind", (arr (TAp (TC "IO") (TV "a")) (arr (arr (TV "a") (TAp (TC "IO") (TV "b"))) (TAp (TC "IO") (TV "b"))), ro "C"))
    , ("ioPure", (arr (TV "a") (TAp (TC "IO") (TV "a")), A (A (ro "B") (ro "C")) (ro "T")))
    , ("primitiveError", (arr (TAp (TC "[]") (TC "Char")) (TV "a"), ro "ERR"))
    , ("newIORef", (arr (TV "a") (TAp (TC "IO") (TAp (TC "IORef") (TV "a"))),
      A (A (ro "B") (ro "C")) (A (A (ro "B") (ro "T")) (ro "REF"))))
    , ("readIORef", (arr (TAp (TC "IORef") (TV "a")) (TAp (TC "IO") (TV "a")),
      A (ro "T") (ro "READREF")))
    , ("writeIORef", (arr (TAp (TC "IORef") (TV "a")) (arr (TV "a") (TAp (TC "IO") (TC "()"))),
      A (A (ro "R") (ro "WRITEREF")) (ro "B")))
    , ("exitSuccess", (TAp (TC "IO") (TV "a"), ro "END"))
    , ("unsafePerformIO", (arr (TAp (TC "IO") (TV "a")) (TV "a"), A (A (ro "C") (A (ro "T") (ro "END"))) (ro "K")))
    , ("fail#", (TV "a", A (V "unsafePerformIO") (V "exitSuccess")))
    , ("word64Add", (wordy, A (ro "QQ") (ro "DADD")))
    , ("word64Sub", (wordy, A (ro "QQ") (ro "DSUB")))
    , ("word64Mul", (wordy, A (ro "QQ") (ro "DMUL")))
    , ("word64Div", (wordy, A (ro "QQ") (ro "DDIV")))
    , ("word64Mod", (wordy, A (ro "QQ") (ro "DMOD")))
    ]
    ++ map (\(s, v) -> (s, (dyad "Int", bin v)))
      [ ("intAdd", "ADD")
      , ("intSub", "SUB")
      , ("intMul", "MUL")
      , ("intDiv", "DIV")
      , ("intMod", "MOD")
      , ("intQuot", "DIV")
      , ("intRem", "MOD")
      ]
    ++ map (\(s, v) -> (s, (dyad "Word", bin v)))
      [ ("wordAdd", "ADD")
      , ("wordSub", "SUB")
      , ("wordMul", "MUL")
      , ("wordDiv", "U_DIV")
      , ("wordMod", "U_MOD")
      , ("wordQuot", "U_DIV")
      , ("wordRem", "U_MOD")
      ]

neatPrim = foldr (uncurry addAdt) (Neat Tip [] prims Tip [] [] []) primAdts

soloPrim = singleton "#" ((qs, as), ([], [])) where
  qs = fst <$> fromList (typedAsts neatPrim)
  as = second snd <$> typedAsts neatPrim

tabulateModules mods = foldM ins (singleton "#" neatPrim) $ go <$> mods where
  go (name, prog) = (name, foldr ($) neatNew prog)
  ins tab (k, v) = case mlookup k tab of
    Nothing -> Right $ insert k v tab
    Just _ -> Left $ "duplicate module: " ++ k

inferModule tab acc name = case mlookup name acc of
  Nothing -> do
    let
      Neat rawIenv defs typedList adtTab ffis ffes rawImps = tab ! name
      typed = fromList typedList
      fillSigs (cl, Tycl sigs is) = (cl,) $ case sigs of
        [] -> Tycl (findSigs cl) is
        _ -> Tycl sigs is
      findSigs cl = maybe (error $ "no sigs: " ++ cl) id $ find (not . null) [maybe [] (\(Tycl sigs _) -> sigs) $ mlookup cl $ typeclasses (tab ! im) | im <- imps]
      ienv = fromList $ fillSigs <$> toAscList rawIenv
      imps = "#":rawImps
      locals = fromList $ map (, ()) $ (fst <$> typedList) ++ (fst <$> defs)
      insts im (Tycl _ is) = (im,) <$> is
      classes im = if im == "" then ienv else typeclasses $ tab ! im
      tycl classId = concat [maybe [] (insts im) $ mlookup classId $ classes im | im <- "":imps]
      dcs s = foldr (<|>) (mlookup s adtTab) $ map (\im -> mlookup s $ dataCons $ tab ! im) imps
      typeOfMethod s = maybe undefined id $ foldr (<|>) (fst <$> mlookup s typed) [fmap fst $ lookup s $ typedAsts $ tab ! im | im <- imps]
      genDefaultMethod qcs (classId, s) = case mlookup defName qcs of
        Nothing -> Right $ insert defName (q, E $ Link "#" "fail#" undefined) qcs
        Just (Qual ps t, _) -> case match t t0 of
          Nothing -> Left $ "bad default method type: " ++ s
          _ -> case ps of
            [Pred cl _] | cl == classId -> Right qcs
            _ -> Left $ "bad default method constraints: " ++ showQual (Qual ps0 t0) ""
        where
        defName = "{default}" ++ s
        (q@(Qual ps0 t0), _) = qcs ! s
    acc' <- foldM (inferModule tab) acc imps
    let linker = astLink typed locals imps acc'
    depdefs <- mapM (\(s, t) -> (s,) <$> linker (patternCompile dcs t)) defs
    typed <- inferDefs tycl depdefs typed
    typed <- inferTypeclasses tycl typeOfMethod typed dcs linker ienv
    typed <- foldM genDefaultMethod typed [(classId, sig) | (classId, Tycl sigs _) <- toAscList rawIenv, sig <- sigs]
    Right $ insert name ((fst <$> typed, toAscList $ snd <$> typed), (ffis, ffes)) acc'
  Just _ -> Right acc

untangle s = case program s of
  Left e -> Left $ "parse error: " ++ e
  Right (mods, ParseState s _) -> case s of
    Ell [] [] -> do
      tab <- tabulateModules mods
      foldM (inferModule tab) soloPrim $ keys tab
    _ -> Left $ "parse error: " ++ case ell s of
      Left e -> e
      Right (((r, c), _), _) -> ("row "++) . showInt r . (" col "++) . showInt c $ ""
