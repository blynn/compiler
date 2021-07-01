-- Separate fixity phase.
-- Export lists.
-- Detect missing instances.
module Compiler where

import Base
import Map
import Ast
import Parser
import Unify
import Kiselyov
import RTS

-- Optimizations.
optim t = case t of
  Nd x y -> go (optim x) (optim y)
  _ -> t
  where
  go (Lf (Basic "I")) q = q
  go p q@(Lf (Basic c)) = case c of
    "I" -> case p of
      Lf (Basic "C") -> lf "T"
      Lf (Basic "B") -> lf "I"
      Nd p1 p2 -> case p1 of
        Lf (Basic "B") -> p2
        Lf (Basic "R") -> Nd (lf "T") p2
        _ -> Nd (Nd p1 p2) q
      _ -> Nd p q
    "T" -> case p of
      Nd (Lf (Basic "B")) (Lf (Basic "C")) -> lf "V"
      _ -> Nd p q
    _ -> Nd p q
  go p q = Nd p q

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

unpat searcher as t = case as of
  [] -> pure t
  a:at -> get >>= \n -> put (n + 1) >> let freshv = shows n "#" in L freshv <$> let
    go p x = case p of
      PatLit lit -> unpat searcher at $ patEq lit (V freshv) x $ V "pjoin#"
      PatVar s m -> maybe (unpat searcher at) (\p1 x1 -> go p1 x1) m $ beta s (V freshv) x
      PatCon con args -> case findCon searcher con of
        Left e -> error e
        Right cons -> unpat searcher args x >>= \y -> unpat searcher at $ singleOut con cons (V freshv) y
    in go a t

unpatTop searcher als x = case als of
  [] -> pure x
  (a, l):alt -> let
    go p t = case p of
      PatLit lit -> unpatTop searcher alt $ patEq lit (V l) t $ V "pjoin#"
      PatVar s m -> maybe (unpatTop searcher alt) go m $ beta s (V l) t
      PatCon con args -> case findCon searcher con of
        Left e -> error e
        Right cons -> unpat searcher args t >>= \y -> unpatTop searcher alt $ singleOut con cons (V l) y
    in go a x

rewritePats' searcher asxs ls = case asxs of
  [] -> pure $ V "fail#"
  (as, t):asxt -> unpatTop searcher (zip as ls) t >>=
    \y -> A (L "pjoin#" y) <$> rewritePats' searcher asxt ls

rewritePats searcher vsxs@((vs0, _):_) = get >>= \n -> let
  ls = map (`shows` "#") $ take (length vs0) [n..]
  in put (n + length ls) >> flip (foldr L) ls <$> rewritePats' searcher vsxs ls

classifyAlt v x = case v of
  PatLit lit -> Left $ patEq lit (V "of") x
  PatVar s m -> maybe (Left . A . L "pjoin#") classifyAlt m $ A (L s x) $ V "of"
  PatCon con args -> Right (insertWith (flip (.)) con ((args, x):))

genCase searcher tab = if size tab == 0 then id else A . L "cjoin#" $ let
  firstC = case toAscList tab of ((con, _):_) -> con
  cs = either error id $ findCon searcher firstC
  in foldl A (A (V $ specialCase cs) (V "of"))
    $ map (\(Constr s ts) -> case mlookup s tab of
      Nothing -> foldr L (V "cjoin#") $ const "_" <$> ts
      Just f -> Pa $ f [(const (PatVar "_" Nothing) <$> ts, V "cjoin#")]
    ) cs

updateCaseSt searcher (acc, tab) alt = case alt of
  Left f -> (acc . genCase searcher tab . f, Tip)
  Right upd -> (acc, upd tab)

rewriteCase searcher as = acc . genCase searcher tab $ V "fail#" where
  (acc, tab) = foldl (updateCaseSt searcher) (id, Tip) $ uncurry classifyAlt <$> as

resolveFieldBinds searcher t = go t where
  go t = case t of
    E _ -> t
    V _ -> t
    A (E (Basic "{=")) (A expr fbsAst) -> let
      fromAst t = case t of
        A (A (E (StrCon f)) body) rest -> (f, body):fromAst rest
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
  Ca x as -> Ca <$> go x <*> mapM (\(p, a) -> (,) <$> pgo p <*> go a) as
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

patFixer searcher (PatCon f [a, b]:rest) = unprotectAll $ foldr rebase seed rest where
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
    Ca x as -> liftA2 A (L "of" . rewriteCase searcher <$> mapM (secondM go) as >>= go) (go x)

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
    Basic bug -> error bug
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

findInstance searcher qn@(q, n) p@(Pred cl ty) insts = case insts of
  []  -> case ty of
    TV _ -> let v = '*':show n in Right (((p, v):q, n + 1), V v)
    _ -> Left $ "no instance: " ++ show p
  (modName, Instance h name ps _):rest -> case match h ty of
    Nothing -> findInstance searcher qn p rest
    Just subs -> foldM (\(qn1, t) (Pred cl1 ty1) -> second (A t)
      <$> findProof searcher (Pred cl1 $ apply subs ty1) qn1) (qn, if modName == "" then V name else E $ Link modName name undefined) ps

findProof searcher pred@(Pred classId t) psn@(ps, n) = case lookup pred ps of
  Nothing -> case findTypeclass searcher classId of
    [] -> Left $ classId ++ " has no instances"
    insts -> findInstance searcher psn pred insts
  Just s -> Right (psn, V s)

prove' searcher psn a = case a of
  Proof pred -> findProof searcher pred psn
  A x y -> prove' searcher psn x >>= \(psn1, x1) ->
    second (A x1) <$> prove' searcher psn1 y
  L s t -> second (L s) <$> prove' searcher psn t
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

inferno searcher typed defmap syms = let
  loc = zip syms $ TV . (' ':) <$> syms
  in foldM (\(acc, (subs, n)) s ->
    maybe (Left $ "missing: " ++ s) Right (mlookup s defmap) >>=
    \expr -> infer typed loc expr (subs, n) >>=
    \((t, a), (ms, n1)) -> unify (TV (' ':s)) t ms >>=
    \cs -> Right ((s, (t, a)):acc, (cs, n1))
  ) ([], ([], 0)) syms >>=
  \(stas, (soln, _)) -> mapM id $ (\(s, ta) -> prove searcher s $ typeAstSub soln ta) <$> stas

prove searcher s (t, a) = flip fmap (prove' searcher ([], 0) a) \((ps, _), x) -> let
  applyDicts expr = foldl A expr $ map (V . snd) ps
  in (s, (Qual (map fst ps) t, foldr L (overFree s applyDicts x) $ map snd ps))

ambiguous (Qual ps t) = filter (not . resolvable) ps where
  resolvable (Pred _ ty) = all (`elem` typeVars t) $ typeVars ty

reconcile searcher q@(Qual ps t) ast = \case
  Nothing -> Right (q, ast)
  Just qAnno@(Qual psA tA) -> case match t tA of
    Nothing -> Left $ ("type mismatch, expected: "++) . shows qAnno . (", actual: "++) $ show q
    Just sub -> let
      vcount = length psA
      vars = show <$> [1..vcount]
      annoDictVars = (zip psA vars, vcount)
      forbidNew p ((_, n), x)
        | n == vcount = Right x
        | True = Left $ "missing predicate: " ++ show p
      findAnno p@(Pred cl ty) = findProof searcher (Pred cl $ apply sub ty) annoDictVars >>= forbidNew p
      in do
        dicts <- mapM findAnno ps
        case ambiguous qAnno of
          [] -> pure (qAnno, foldr L (foldl A ast dicts) vars)
          ambis -> Left $ ("ambiguous: "++) . foldr (.) id (map shows ambis) $ ""

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
    add typed (s, (q, ast)) = do
      (q, ast) <- reconcile searcher q ast $ mlookup s decls
      pure $ insert s (q, ast) typed
    inferComponent typed syms = foldM add typed =<< inferno searcher typed defmap syms
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
            $ infer typed [] expr ([], 0)
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
              ((ps3, _), tr) <- prove' searcher (dictVars ps2 0) (proofApply subx ax)
              if length ps2 /= length ps3
                then Left $ ("want context: "++) . (foldr (.) id $ shows . fst <$> ps3) $ name
                else pure tr
      ms <- mapM perMethod sigs
      pure $ insert name (Qual [] $ TC "DICTIONARY", flip (foldr L) dvs $ L "@" $ foldl A (V "@") ms) typed

neatNew = foldr (\(a, b) -> addAdt a b []) neatEmpty { typedAsts = fromList prims } primAdts
isBuiltIn s = member s $ typedAsts neatNew

tabulateModules mods = foldM ins Tip =<< mapM go mods where
  go (name, (mexs, prog)) = (name,) <$> maybe Right processExports mexs (foldr ($) neatNew prog)
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

searcherNew tab neat ienv = Searcher
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
  classes im = if im == "" then ienv else typeclasses $ tab ! im
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
    acc' <- foldM (inferModule tab) acc imps
    let searcher = searcherNew acc' neat ienv
    depdefs <- mapM (\(s, t) -> (s,) <$> patternCompile searcher t) $ topDefs neat
    typed <- inferDefs searcher depdefs (topDecls neat) typed
    typed <- inferTypeclasses searcher ienv typed
    typed <- foldM genDefaultMethod typed [(classId, sig) | (classId, Tycl sigs _) <- toAscList $ typeclasses neat, sig <- sigs]
    Right $ insert name neat { typedAsts = typed } acc'
  Just _ -> Right acc

untangle s = case program s of
  Left e -> Left $ "parse error: " ++ e
  Right (mods, st) -> case st of
    Ell [] [] -> do
      tab <- tabulateModules mods
      foldM (inferModule tab) Tip $ keys tab
    _ -> Left $ "parse error: " ++ case ell st of
      Left e -> e
      Right (((r, c), _), _) -> ("row "++) . shows r . (" col "++) . shows c $ ""

optiComb' (subs, combs) (s, lamb) = let
  gosub t = case t of
    LfVar v -> maybe t id $ lookup v subs
    Nd a b -> Nd (gosub a) (gosub b)
    _ -> t
  c = optim $ gosub $ nolam $ optiApp lamb
  combs' = combs . ((s, c):)
  in case c of
    Lf (Basic _) -> ((s, c):subs, combs')
    LfVar v -> if v == s then (subs, combs . ((s, Nd (lf "Y") (lf "I")):)) else ((s, gosub c):subs, combs')
    _ -> (subs, combs')
optiComb lambs = ($[]) . snd $ foldl optiComb' ([], id) lambs

showTree prec t = case t of
  LfVar s -> showVar s
  Lf extra -> shows extra
  Nd x y -> showParen prec $ showTree False x . (' ':) . showTree True y
disasm (s, t) = (s++) . (" = "++) . showTree False t . (";\n"++)

dumpWith dumper s = case untangle s of
  Left err -> err
  Right tab -> foldr ($) [] $ map (\(name, neat) -> ("module "++) . (name++) . ('\n':) . (foldr (.) id $ dumper neat)) $ toAscList tab

dumpCombs neat = map disasm $ optiComb $ second snd <$> toAscList (typedAsts neat)

dumpLambs neat = map (\(s, t) -> (s++) . (" = "++) . showAst False t . ('\n':)) $ second snd <$> toAscList (typedAsts neat)

dumpTypes neat = map (\(s, q) -> (s++) . (" :: "++) . shows q . ('\n':)) $ second fst <$> toAscList (typedAsts neat)

-- Hash consing.
data Obj = Local String | Global String String | Code Int deriving Eq

instance Ord Obj where
  x <= y = case x of
    Local a -> case y of
      Local b -> a <= b
      _ -> True
    Global m a -> case y of
      Local _ -> False
      Global n b -> if m == n then a <= b else m <= n
      _ -> True
    Code a -> case y of
      Code b -> a <= b
      _ -> False

memget k@(a, b) = get >>= \(tab, (hp, f)) -> case mlookup k tab of
  Nothing -> put (insert k hp tab, (hp + 2, f . (a:) . (b:))) >> pure hp
  Just v -> pure v

enc t = case t of
  Lf n -> case n of
    Basic c -> pure $ Code $ comEnum c
    Const c -> Code <$> memget (Code $ comEnum "NUM", Code c)
    ChrCon c -> enc $ Lf $ Const $ ord c
    StrCon s -> enc $ foldr (\h t -> Nd (Nd (lf "CONS") (Lf $ ChrCon h)) t) (lf "K") s
    Link m s _ -> pure $ Global m s
  LfVar s -> pure $ Local s
  Nd x y -> enc x >>= \hx -> enc y >>= \hy -> Code <$> memget (hx, hy)

asm combs = foldM
  (\symtab (s, t) -> (flip (insert s) symtab) <$> enc t)
  Tip combs

hashcons hp combs = (symtab', (hp', (mem++)))
  where
  (symtab, (_, (hp', memF))) = runState (asm combs) (Tip, (hp, id))
  symtab' = resolveLocal <$> symtab
  mem = resolveLocal <$> memF []
  resolveLocal = \case
    Code n -> Right n
    Local s -> resolveLocal $ symtab ! s
    Global m s -> Left (m, s)

codegenLocal (name, neat) (bigmap, (hp, f)) =
  (insert name localmap bigmap, (hp', f . f'))
  where
  (localmap, (hp', f')) = hashcons hp $ optiComb $ toAscList $ snd <$> typedAsts neat

codegen ffis mods = (bigmap', mem) where
  (bigmap, (_, memF)) = foldr codegenLocal (Tip, (128, id)) $ toAscList mods
  bigmap' = (resolveGlobal <$>) <$> bigmap
  mem = resolveGlobal <$> memF []
  ffiIndex = fromList $ zip (keys ffis) [0..]
  resolveGlobal = \case
    Left (m, s) -> if m == "{foreign}"
      then ffiIndex ! s
      else resolveGlobal $ (bigmap ! m) ! s
    Right n -> n

getIOType (Qual [] (TAp (TC "IO") t)) = Right t
getIOType q = Left $ "main : " ++ shows q ""

ffcat (name, neat) (xs, ys) =
  ( foldr (\(k, v) m -> insertWith (error $ "duplicate import: " ++ k) k v m) xs $ toAscList $ ffiImports neat
  , foldr (\(k, v) m -> insertWith (error $ "duplicate export: " ++ k) k (name, v) m) ys $ toAscList $ ffiExports neat
  )
