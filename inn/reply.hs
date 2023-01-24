module Main where
import Ast
import Base
import Map
import Kiselyov
import Parser
import Typer
import RTS

kF = comEnum "F"
kNUM = comEnum "NUM"
kLINK = 0

neatBase = neatEmpty {moduleImports = singleton "" [("#", const True), ("Base", const True)]}

initObjs = do
  tab <- insert "#" neato <$> singleFile (source ++ sourceExtras)
  topo <- topoModules tab
  objs <- foldM compileModule Tip topo
  pure (topo, objs)
  where
  neato = neatPrim { typedAsts = foldr (uncurry insert) (typedAsts neatPrim) ffiHack }
  sourceExtras = [r|
putStr = mapM_ putChar
putStrLn = (>> putChar '\n') . putStr
print = putStrLn . show
|]

genIndex objs (start, mm) name = (start + size syms, insert name (fromList $ zip (keys syms) [start..]) mm)
  where syms = _syms $ objs ! name

initialState = do
  let
    Right (topo, objs) = initObjs
    (libStart, lib) = foldl (genIndex objs) (0, Tip) $ fst <$> topo
  forM ((objs !) . fst <$> topo) \ob -> do
    mapM vmPutScratchpad $ concatMap (link lib) $ elems (_syms ob)
    mapM vmPutScratchpad $ concatMap (link lib) $ _mem ob
    vmGCRootScratchpad $ size $ _syms ob
  pure (insert ">" (Module neatBase Tip Tip []) objs, (libStart, lib))

repl st@(mos, (libStart, lib)) s = either complain go $ fst <$> parse fmt s
  where
  go = either (either complain addTyped . tryAddDefs) (either complain exec . tryExpr)
  complain err = putStrLn err >> pure st
  fmt = Left <$> fragment <|> Right <$> single
  fragment = ($ neatEmpty) <$> (lexemePrelude *> topdecls <* eof)
  single = many (char ' ') *> expr <* eof

  mergeFragment frag mos = insert ">" obj { _neat = neat' } mos where
    obj = mos!">"
    neat = _neat obj
    neat' = neat
      { typedAsts = foldr (uncurry insert) (typedAsts neat) $ toAscList $ typedAsts frag
      , dataCons = foldr (uncurry insert) (dataCons neat) $ toAscList $ dataCons frag
      , type2Cons = foldr (uncurry insert) (type2Cons neat) $ toAscList $ type2Cons frag
      , typeclasses = foldr (uncurry insert) (typeclasses neat) $ toAscList $ typeclasses frag
      , topDefs = topDefs frag
      }

  tryAddDefs frag = let
    mos1 = mergeFragment frag mos
    neat = _neat $ mos1!">"

    imps = dependentModules neat
    fillSigs (cl, Tycl sigs is) = (cl,) $ case sigs of
      [] -> Tycl (findSigs cl) is
      _ -> Tycl sigs is
    findSigs cl = maybe (error $ "no sigs: " ++ cl) id $
      find (not . null) [maybe [] (\(Tycl sigs _) -> sigs) $ mlookup cl $
        typeclasses (_neat $ mos1 ! im) | im <- imps]
    ienv = fromList $ fillSigs <$> toAscList (typeclasses neat)
    searcher = searcherNew ">" (_neat <$> mos1) neat
    typed = typedAsts frag
    in do
      depdefs <- mapM (\(s, t) -> (s,) <$> patternCompile searcher t) (topDefs frag)
      typed <- inferDefs searcher depdefs Tip typed
      typed <- inferTypeclasses searcher ienv typed
      pure (typed, mos1)

  addTyped (typed, mos) = do
    let
      neat = _neat $ mos!">"
      slid = mapWithKey (\k (_, t) -> slideY k $ optiApp t) typed
      rawCombs = optim . nolam . inlineLone mos <$> slid
      combs = rewriteCombs rawCombs <$> rawCombs
      (symtab, (_, (hp', memF))) = runState (asm $ toAscList combs) (Tip, (128, id))
      localmap = resolveLocal <$> symtab
      mem = resolveLocal <$> memF []
      resolveLocal = \case
        Left ("", s) -> resolveLocal $ symtab ! s
        x -> x

      mergedTyped = foldr (uncurry insert) (typedAsts neat) $ toAscList typed
      mergedCombs = foldr (uncurry insert) (_combs $ mos!">") $ toAscList combs
      mos' = insert ">" (Module neat { typedAsts = mergedTyped } mergedCombs Tip []) mos
      roots = maybe Tip id $ mlookup ">" lib
      roots' = foldr (uncurry insert) roots $ zip (keys localmap) [libStart..]
      libStart' = libStart + size localmap
      lib' = insert ">" roots' lib

    mapM vmPutScratchpad $ concatMap (link lib) $ elems localmap
    mapM vmPutScratchpad $ concatMap (link lib) mem
    vmGCRootScratchpad $ size localmap
    pure (mos', (libStart', lib'))

  tryExpr sugary = do
    ast <- snd <$> patternCompile searcherPrompt sugary
    (typ, typedAst) <- (! "") <$> inferDefs searcherPrompt [("", ([], ast))] Tip Tip
    case typ of
      Qual [] (TAp (TC "IO") _) -> do
        let combs = nolam . optiApp $ typedAst
        let (addr, (_, (hp', memF))) = runState (enc combs) (Tip, (128, id))
        pure (addr, (hp', memF []))
      _ -> tryExpr $ A (V "print") sugary

  exec (addr, (hp, mem)) = do
    mapM vmPutScratchpad $ concatMap (link lib) mem
    vmRunScratchpad $ either undefined id addr
    pure (mos, (libStart, lib))

  neatPrompt = neatEmpty {moduleImports = singleton "" [(">", const True), ("#", const True), ("Base", const True)]}
  searcherPrompt = searcherNew ">" (_neat <$> mos) neatPrompt

link lib = \case
  Left (moduleName, sym) -> [kLINK, lib ! moduleName ! sym]
  Right x -> [x]

source = [r|
