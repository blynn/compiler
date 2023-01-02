module Main where
import Ast
import Base
import Map
import Kiselyov
import Parser
import Typer
import RTS
import System

getLine = go id where
  go acc = isEOF >>= \b -> if b then pure Nothing else do
    c <- getChar
    if c == '\n' then pure $ Just $ acc "" else go $ acc . (c:)

kF = comEnum "F"
kNUM = comEnum "NUM"
kLINK = 0

neatBase = neatEmpty {moduleImports = singleton "" [(">", const True), ("#", const True), ("Base", const True)]}

initObjs = do
  tab <- insert "#" neato <$> singleFile (source ++ sourceExtras)
  topo <- topoModules tab
  objs <- foldM compileModule Tip topo
  pure (topo, objs)
  where
  neato = neatPrim { typedAsts = foldr (uncurry insert) (typedAsts neatPrim) extras }
  extras = second (first $ Qual []) <$>
    [ ("putChar", (arr (TC "Char") (TAp (TC "IO") (TC "()")), A (E $ Basic "T") $ A (E $ Basic "F") (E $ ChrCon $ chr 4))) ]
  sourceExtras = [r|
putStr = mapM_ putChar
putStrLn = (>> putChar '\n') . putStr
print = putStrLn . show
|]

genIndex objs (start, mm) name = (start + size syms, insert name (fromList $ zip (keys syms) [start..]) mm)
  where syms = _syms $ objs ! name

main :: IO ()
main = do
  let Right (topo, objs) = initObjs
  let (libStart, lib) = foldl (genIndex objs) (0, Tip) $ fst <$> topo
  forM ((objs !) . fst <$> topo) \ob -> do
    mapM vmPutScratchpad $ concatMap (link lib) $ elems (_syms ob)
    mapM vmPutScratchpad $ concatMap (link lib) $ _mem ob
    vmGCRootScratchpad $ size $ _syms ob
  repl (insert ">" (Module neatBase Tip Tip []) objs) (libStart, lib)

repl mos (libStart, lib) = putStr "> " *> getLine >>= maybe (putChar '\n') \s -> either complain go $ fst <$> parse fmt s
  where
  go = either (either complain addTyped . tryAddDefs) (either complain exec . tryExpr)
  complain err = putStrLn err >> repl mos (libStart, lib)
  fmt = Left <$> fragment <|> Right <$> single
  fragment = map (second fst) <$> (lexemePrelude *> braceDef <* eof)
  single = many (char ' ') *> expr <* eof

  tryAddDefs defs = do
    let
      imps = dependentModules neatBase
      fillSigs (cl, Tycl sigs is) = (cl,) $ case sigs of
        [] -> Tycl (findSigs cl) is
        _ -> Tycl sigs is
      findSigs cl = maybe (error $ "no sigs: " ++ cl) id $
        find (not . null) [maybe [] (\(Tycl sigs _) -> sigs) $ mlookup cl $
          typeclasses (_neat $ mos ! im) | im <- imps]
      ienv = fromList $ fillSigs <$> toAscList (typeclasses neatBase)
      searcher = searcherNew ">" (_neat <$> mos) neatBase { topDefs = defs } ienv
    let typed = Tip
    depdefs <- mapM (\(s, t) -> (s,) <$> patternCompile searcher t) defs
    typed <- inferDefs searcher depdefs Tip typed
    typed <- inferTypeclasses searcher ienv typed
    pure typed

  addTyped typed = do
    let
      neat = _neat $ mos!">"
      rawCombs = optim . nolam . inlineLone mos . optiApp . snd <$> typed
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

    pure (localmap, concatMap (link lib) mem, roots')
    repl mos' (libStart', lib')

  tryExpr sugary = do
    ast <- snd <$> patternCompile searcherBase sugary
    typedAst <- snd . (! "") <$> inferDefs searcherBase [("", ([], ast))] Tip Tip
    let combs = nolam . optiApp $ typedAst
    let (addr, (_, (hp', memF))) = runState (enc combs) (Tip, (128, id))
    pure (addr, (hp', memF []))

  exec (addr, (hp, mem)) = do
    mapM vmPutScratchpad $ concatMap (link lib) mem
    vmRunScratchpad $ either undefined id addr
    repl mos (libStart, lib)

  searcherBase = searcherNew ">" (_neat <$> mos) neatBase Tip

link lib = \case
  Left (moduleName, sym) -> [kLINK, lib ! moduleName ! sym]
  Right x -> [x]

mksearcher objs (name, neat) = searcherNew name (_neat <$> objs) neat ienv where
  imps = dependentModules neat
  typed = typedAsts neat
  fillSigs (cl, Tycl sigs is) = (cl,) $ case sigs of
    [] -> Tycl (findSigs cl) is
    _ -> Tycl sigs is
  findSigs cl = maybe (error $ "no sigs: " ++ cl) id $
    find (not . null) [maybe [] (\(Tycl sigs _) -> sigs) $ mlookup cl $
      typeclasses (_neat $ objs ! im) | im <- imps]
  ienv = fromList $ fillSigs <$> toAscList (typeclasses neat)

source = [r|
