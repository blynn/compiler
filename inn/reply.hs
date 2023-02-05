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

neatPrompt = neatEmpty {moduleImports = singleton "" [(">", const True), ("#", const True), ("Base", const True)]}

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
  pure (insert ">" (Module neatPrompt Tip Tip []) objs, (libStart, lib))

repl st@(mos, (libStart, lib)) s = either complain (either addTyped exec) do
  fragOrExpr <- fst <$> parse fmt s
  case fragOrExpr of
    Left frag -> Left <$> tryAddDefs frag
    Right expr -> Right <$> tryExpr expr
  where
  complain err = putStrLn err >> pure st
  fmt = Left <$> fragment <|> Right <$> single
  fragment = ($ neatEmpty) <$> (lexemePrelude *> topdecls <* eof)
  single = many (char ' ') *> expr <* eof

  tryAddDefs frag = do
    let searcher = searcherNew ">" (_neat <$> mos) frag{moduleImports = moduleImports neatPrompt}
    depdefs <- mapM (\(s, t) -> (s,) <$> patternCompile searcher t) (topDefs frag)
    typed <- inferDefs searcher depdefs Tip $ typedAsts frag
    typed <- inferTypeclasses searcher (instances frag) typed
    pure (frag{typedAsts = typed}, mos)

  mergeFragment neat frag = neat
      { typedAsts = foldr (uncurry insert) (typedAsts neat) $ toAscList $ typedAsts frag
      , dataCons = foldr (uncurry insert) (dataCons neat) $ toAscList $ dataCons frag
      , type2Cons = foldr (uncurry insert) (type2Cons neat) $ toAscList $ type2Cons frag
      , typeclasses = foldr (uncurry insert) (typeclasses neat) $ toAscList $ typeclasses frag
      , instances = foldr (uncurry insert) (instances neat) $ toAscList $ instances frag
      , topDefs = topDefs frag
      }

  addTyped (frag, mos) = let
    slid = mapWithKey (\k (_, t) -> slideY k $ optiApp t) $ typedAsts frag
    rawCombs = optim . nolam . inlineLone mos <$> slid
    combs = rewriteCombs rawCombs <$> rawCombs
    (symtab, (_, (hp', memF))) = runState (asm $ toAscList combs) (Tip, (128, id))
    localmap = resolveLocal <$> symtab
    mem = resolveLocal <$> memF []
    resolveLocal = \case
      Left ("", s) -> resolveLocal $ symtab ! s
      x -> x

    mergedNeat = mergeFragment (_neat $ mos!">") frag
    mergedCombs = foldr (uncurry insert) (_combs $ mos!">") $ toAscList combs
    mos' = insert ">" (Module mergedNeat mergedCombs Tip []) mos
    roots = maybe Tip id $ mlookup ">" lib
    roots' = foldr (uncurry insert) roots $ zip (keys localmap) [libStart..]
    libStart' = libStart + size localmap
    lib' = insert ">" roots' lib
    in do
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

  searcherPrompt = searcherNew ">" (_neat <$> mos) neatPrompt

link lib = \case
  Left (moduleName, sym) -> [kLINK, lib ! moduleName ! sym]
  Right x -> [x]

source = [r|
