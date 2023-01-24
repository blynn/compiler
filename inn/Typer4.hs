-- Separate fixity phase.
-- Export lists.
-- Detect missing instances.
-- Top-level type annotations.
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
  1 -> flip (fill s) x
  _ -> A $ L s x
optiApp t = case t of
  A x y -> case optiApp x of
    L s x -> app01 s x (optiApp y)
    x -> A x (optiApp y)
  L s x -> L s (optiApp x)
  _ -> t

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
      cs <- flip mapM vsxs \(a:at, x) -> (a,) <$> foldM (\b (p, v) -> rewriteCase searcher v Tip [(p, b)]) x (zip at $ tail vs)
      flip (foldr L) vs <$> rewriteCase searcher (head vs) Tip cs

patEq lit b x y = A (L "join#" $ A (A (A (V "if") (A (A (V "==") (E lit)) b)) x) $ V "join#")  y

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
      let cs = either error id $ findCon searcher firstC
      jumpTable <- mapM (\(Constr s ts) -> case mlookup s tab of
          Nothing -> pure $ foldr L (V "join#") $ const "_" <$> ts
          Just f -> rewritePats searcher $ f []
        ) cs
      pure $ A (L "join#" $ foldl A (A (V $ specialCase cs) $ V caseVar) jumpTable) onFail

resolveFieldBinds searcher t = go t where
  go t = case t of
    E _ -> t
    V _ -> t
    A (E (Basic "{=")) (A rawExpr fbsAst) -> let
      expr = go rawExpr
      fromAst t = case t of
        A (A (E (StrCon f)) body) rest -> (f, go body):fromAst rest
        E (Basic "=}") -> []
      fbs@((firstField, _):_) = fromAst fbsAst
      (con, fields) = findField searcher firstField
      cs = either error id $ findCon searcher con
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
    A (A (V s) z) rest -> go (rebase s (protect z) acc) rest
    _ -> error "unreachable"
  rebase s z = \case
    A (A (V s') x) y -> let
      stay = A (A (V s) $ A (A (V s') x) y) z
      down = A (A (V s') x) $ rebase s z y
      in extendChain searcher stay down s s'
    x -> A (A (V s) x) z

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
      in extendChain searcher stay down s s'
    x -> PatCon s [x, z]

extendChain searcher stay down s s' =
  if prec <= prec'
    then if prec == prec'
      then if assoc == assoc'
        then case assoc of
          LAssoc -> stay
          RAssoc -> down
          NAssoc -> error $ "adjacent NAssoc: " ++ s ++ " vs " ++ s'
        else error $ "assoc mismatch: " ++ s ++ " vs " ++ s'
      else stay
    else down
  where
  (prec, assoc) = either (const (9, LAssoc)) id $ findPrec searcher s
  (prec', assoc') = either (const (9, LAssoc)) id $ findPrec searcher s'

secondM f (a, b) = (a,) <$> f b
patternCompile searcher t = astLink searcher $ optiApp $ resolveFieldBinds searcher $ evalState (go $ either error id $ fixFixity searcher t) 0 where
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

infer msg typed loc ast csn@(cs, n) = case ast of
  E x -> Right $ case x of
    Basic bug -> error bug
    Const n -> ((TC "Int", E $ ChrCon $ chr n), csn)
    ChrCon _ -> ((TC "Char", ast), csn)
    StrCon _ -> ((TAp (TC "[]") (TC "Char"), ast), csn)
    Link im s q -> insta q
  V s -> maybe (Left $ "undefined: " ++ s) Right
    $ either (\t -> ((t, ast), csn)) insta <$> lookup s loc
    <|> insta . fst <$> mlookup s typed
  A x y -> rec loc x (cs, n + 1) >>=
    \((tx, ax), csn1) -> rec loc y csn1 >>=
    \((ty, ay), (cs2, n2)) -> unifyMsg msg tx (arr ty va) cs2 >>=
    \cs -> Right ((va, A ax ay), (cs, n2))
  L s x -> first (\(t, a) -> (arr va t, L s a)) <$> rec ((s, Left va):loc) x (cs, n + 1)
  where
  rec = infer msg typed
  va = TV $ show n
  insta ty = ((ty1, foldl A ast (map Proof preds)), (cs, n1))
    where (Qual preds ty1, n1) = instantiate ty n

findInstance searcher qn@(q, n) p@(Pred cl ty) insts = case insts of
  []  -> case ty of
    TV _ -> let v = '*':show n in Right (((p, v):q, n + 1), V v)
    _ -> Left $ "no instance: " ++ show p
  (modName, Instance h name ps _):rest -> case match h ty of
    Nothing -> findInstance searcher qn p rest
    Just subs -> foldM (\(qn1, t) (Pred cl1 ty1) -> second (A t)
      <$> findProof searcher (Pred cl1 $ apply subs ty1) qn1) (qn, if modName == "" then V name else E $ Link modName name undefined) ps

findProof searcher pred@(Pred classId t) psn@(ps, n) = case lookup pred ps of
  Nothing -> findInstance searcher psn pred $ findTypeclass searcher classId
  Just s -> Right (psn, V s)

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

forFree cond f bound t = case t of
  E _ -> t
  V s -> if (not $ s `elem` bound) && cond s then f t else t
  A x y -> A (rec bound x) (rec bound y)
  L s t' -> L s $ rec (s:bound) t'
  where rec = forFree cond f

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
        Right (((s, (t, a)):acc, pAnno ++ preds), (soln @@ ms, n1))
  gatherPreds (acc, psn) (s, (t, a)) = do
    (psn, a) <- prove searcher psn a
    pure ((s, (t, a)):acc, psn)
  in do
    ((stas, preds), (soln, _)) <- foldM principal (([], []), (Tip, 0)) syms
    let ps = zip preds $ ("anno*"++) . show  <$> [0..]
    (stas, (ps, _)) <- foldM gatherPreds ([], (ps, 0)) $ second (typeAstSub soln) <$> stas
    let
      preds = fst <$> ps
      dicts = snd <$> ps
      applyDicts (s, (t, a)) = (s, (Qual preds t,
        foldr L (forFree (`elem` syms) (\t -> foldl A t $ V <$> dicts) [] a) dicts))
    pure $ map applyDicts stas

inferDefs searcher defs decls typed = do
  let
    insertUnique m (s, (_, t)) = if isBuiltIn s then Left $ "reserved: " ++ s else case mlookup s m of
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

inferTypeclasses searcher ienv typed = foldM perClass typed $ toAscList ienv where
  perClass typed (classId, Tycl sigs insts) = foldM perInstance typed insts where
    perInstance typed (Instance ty name ps idefs) = do
      let
        dvs = map snd $ fst $ dictVars ps 0
        perMethod s = do
          let Just rawExpr = mlookup s idefs <|> pure (V $ "{default}" ++ s)
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
      ms <- mapM perMethod sigs
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
    [ ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "EQ"))
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
      , ("intQuot", "QUOT")
      , ("intRem", "REM")
      , ("intXor", "XOR")
      , ("intAnd", "AND")
      , ("intOr", "OR")
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

neatNew = foldr (\(a, b) -> addAdt a b []) neatEmpty { typedAsts = fromList prims } primAdts
isBuiltIn s = member s $ typedAsts neatNew

tabulateModules mods = foldM ins Tip =<< mapM go mods where
  go (name, (mexs, prog)) = (name,) <$> (genDefaultMethods =<< maybe Right processExports mexs (foldr ($) neatNew prog))
  ins tab (k, v) = case mlookup k tab of
    Nothing -> Right $ insert k v tab
    Just _ -> Left $ "duplicate module: " ++ k
  processExports exs neat = do
    mes <- Just . concat <$> mapM (processExport neat) exs
    pure neat { moduleExports = mes }
  processExport neat = \case
    ExportVar v -> case lookup v $ topDefs neat of
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
        Just (Tycl methodNames _)
          | ns == [".."] -> Right methodNames
          | null delta -> Right ns
          | True -> Left $ "bad exports: " ++ show delta
          where delta = [n | n <- ns, not $ elem n methodNames]
  genDefaultMethod qcs (classId, s) = case mlookup defName qcs of
    Nothing -> Right $ insert defName (q, V "fail#") qcs
    Just (Qual ps t, _) -> case match t t0 of
      Nothing -> Left $ "bad default method type: " ++ s
      _ -> case ps of
        [Pred cl _] | cl == classId -> Right qcs
        _ -> Left $ "bad default method constraints: " ++ show (Qual ps0 t0)
    where
    defName = "{default}" ++ s
    (q@(Qual ps0 t0), _) = qcs ! s
  genDefaultMethods neat = do
    typed <- foldM genDefaultMethod (typedAsts neat) [(classId, sig) | (classId, Tycl sigs _) <- toAscList $ typeclasses neat, sig <- sigs]
    pure neat { typedAsts = typed }

data Searcher = Searcher
  { astLink :: Ast -> Either String ([String], Ast)
  , findPrec :: String -> Either String (Int, Assoc)
  , findCon :: String -> Either String [Constr]
  , findField :: String -> (String, [(String, Type)])
  , typeOfMethod :: String -> Either String Qual
  , findTypeclass :: String -> [(String, Instance)]
  }

isExportOf s neat = case moduleExports neat of
  Nothing -> True
  Just es -> elem s es

findAmong fun viz s = case concat $ maybe [] (:[]) . mlookup s . fun <$> viz s of
  [] -> Left $ "missing: " ++ s
  [unique] -> Right unique
  _ -> Left $ "ambiguous: " ++ s

searcherNew tab neat = Searcher
  { astLink = astLink'
  , findPrec = \s -> if s == ":" then Right (5, RAssoc) else findAmong opFixity visible s
  , findCon = findAmong dataCons visible
  , findField = findField'
  , typeOfMethod = fmap fst . findAmong typedAsts visible
  , findTypeclass = \s -> concat [maybe [] (\(Tycl _ is) -> (im,) <$> is) $ mlookup s $ classes im | im <- "":imps]
  }
  where
  findImportSym s = concat [maybe [] (\(t, _) -> [(im, t)]) $ mlookup s $ typedAsts n | (im, n) <- importedNeats s]
  importedNeats s@(h:_) = if isBuiltIn s then [] else [(im, n) | im <- imps, let n = tab ! im, h == '{' || isExportOf s n]
  visible s = neat : (snd <$> importedNeats s)
  classes im = typeclasses $ if im == "" then neat else tab ! im
  findField' f = case [(con, fields) | dc <- dataCons <$> visible f, (_, cons) <- toAscList dc, Constr con fields <- cons, (f', _) <- fields, f == f'] of
    [] -> error $ "no such field: " ++ f
    h:_ -> h
  imps = moduleImports neat
  defs = fromList $ topDefs neat
  astLink' ast = runDep $ go [] ast where
    go bound ast = case ast of
      V s
        | elem s bound -> pure ast
        | isBuiltIn s -> pure ast
        | member s $ typedAsts neat -> unlessAmbiguous s $ pure ast
        | member s defs -> unlessAmbiguous s $ addDep s *> pure ast
        | True -> case findImportSym s of
          [] -> badDep $ "missing: " ++ s
          [(im, t)] -> pure $ E $ Link im s t
          _ -> badDep $ "ambiguous: " ++ s
      A x y -> A <$> go bound x <*> go bound y
      L s t -> L s <$> go (s:bound) t
      _ -> pure ast
    unlessAmbiguous s f = case findImportSym s of
      [] -> f
      _ -> badDep $ "ambiguous: " ++ s

inferModule tab acc name = case mlookup name acc of
  Nothing -> do
    let
      neat = tab ! name
      imps = moduleImports neat
      typed = typedAsts neat
      fillSigs (cl, Tycl sigs is) = (cl,) $ case sigs of
        [] -> Tycl (findSigs cl) is
        _ -> Tycl sigs is
      findSigs cl = maybe (error $ "no sigs: " ++ cl) id $ find (not . null) [maybe [] (\(Tycl sigs _) -> sigs) $ mlookup cl $ typeclasses (tab ! im) | im <- imps]
      ienv = fromList $ fillSigs <$> toAscList (typeclasses neat)
    acc' <- foldM (inferModule tab) acc imps
    let searcher = searcherNew acc' neat
    depdefs <- mapM (\(s, t) -> (s,) <$> patternCompile searcher t) $ topDefs neat
    typed <- inferDefs searcher depdefs (topDecls neat) typed
    typed <- inferTypeclasses searcher ienv typed
    Right $ insert name neat { typedAsts = typed } acc'
  Just _ -> Right acc

untangle s = do
  tab <- parseProgram s >>= tabulateModules
  foldM (inferModule tab) Tip $ keys tab
