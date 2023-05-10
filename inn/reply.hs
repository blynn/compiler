module Main where
import Ast
import Base
import System
import Map
import Kiselyov
import Parser
import Typer
import RTS

neatInit = neatEmpty {moduleImports = singleton "" $ (, const True) <$> ["#", "Base", "System"]}

initialState = do
  (topo, objs, ffiList) <- precompiled
  let
    libFFI = fromList [("{foreign}", fromList $ zip ffiList [0..])]
    (libStart, lib) = foldl (genIndex objs) (0, libFFI) topo
  pure (objs, (libStart, lib))

moduleNew name (objs, (libStart, lib)) =
  (insert name (Module neatInit Tip []) objs, (libStart, insert name mempty lib))

genIndex objs (start, mm) name = (start + size syms, insert name (fromList $ zip (keys syms) [start..]) mm)
  where syms = _syms $ objs ! name

scratchObj lib ob = do
  vmPutScratchpad $ fromIntegral $ size $ _syms ob
  scratch lib $ elems $ _syms ob
  scratch lib $ _mem ob
  vmGCRootScratchpad

mergeFragment neat frag = neat
  { typeclasses = foldr (uncurry insert) (typeclasses neat) $ toAscList $ typeclasses frag
  , instances = foldr (uncurry $ insertWith (++)) (instances neat) $ toAscList $ instances frag
  , typedAsts = foldr (uncurry insert) (typedAsts neat) $ toAscList $ typedAsts frag
  , dataCons = foldr (uncurry insert) (dataCons neat) $ toAscList $ dataCons frag
  , type2Cons = foldr (uncurry insert) (type2Cons neat) $ toAscList $ type2Cons frag
  , moduleImports = moduleImports frag
  , opFixity = foldr (uncurry insert) (opFixity neat) $ toAscList $ opFixity frag
  , typeAliases = foldr (uncurry insert) (typeAliases neat) $ toAscList $ typeAliases frag
  }

exec lib (addr, mem) = do
  scratch lib mem
  vmRunScratchpad addr

addTyped (mos, (libStart, lib)) name mos' = let
  orig = mos!name
  fresh = mos'!name
  mergedNeat = mergeFragment (_neat orig) (_neat fresh)
  mergedSyms = foldr (uncurry insert) (_syms orig) $ toAscList $ _syms fresh
  syms = _syms fresh
  roots = maybe Tip id $ mlookup name lib
  roots' = foldr (uncurry insert) roots $ zip (keys syms) [libStart..]
  libStart' = libStart + size syms
  lib' = insert name roots' lib
  mergedMos = insert name (Module mergedNeat mergedSyms []) mos'
  in do
    scratchObj lib fresh
    pure (mergedMos, (libStart', lib'))

readInput mos name s = do
  fragOrExpr <- fst <$> parse fmt s
  case fragOrExpr of
    Left frag -> Left <$> tryAddDefs frag
    Right expr -> Right <$> tryExpr expr
  where
  orig = _neat $ mos!name
  fmt = Left <$> try fragment <|> Right <$> single
  fragment = foldr id neatEmpty{moduleImports = moduleImports orig} . map snd <$> haskell
  single = whitespace *> expr <* eof
  importSelf neat = neat{moduleImports = insertWith (++) "" [(name, const True)] $ moduleImports neat}
  tryAddDefs frag = do
    mos1 <- compileModule mos (name, importSelf frag)
    pure $ insert name (rmSelfImportModule $ mos1 ! name) mos1
    where
    rmSelfImportModule m = m {_neat = rmSelfImportNeat $ _neat m}
    rmSelfImportNeat n = n {moduleImports = insert "" (tail $ moduleImports n ! "") (moduleImports n)}
  tryExpr sugary = do
    ast <- snd <$> patternCompile searcher sugary
    (typ, typedAst) <- (! "") <$> inferDefs searcher [("", ([], ast))] Tip Tip
    case typ of
      Qual [] (TAp (TC "IO") _) -> let
        combs = nolam . optiApp $ typedAst
        (addr, (_, (hp', memF))) = runState (enc combs) (Tip, (128, id))
        in pure (hp', memF [Right $ comEnum "I", addr])
      _ -> tryExpr $ A (V "print") sugary
  searcher = searcherNew name (_neat <$> mos) $ importSelf $ neatEmpty {moduleImports = moduleImports orig}

scratch lib = mapM \case
  Left (moduleName, sym) -> (if moduleName == "{foreign}" then vmPutScratchpad else vmPutScratchpadRoot) $ fromIntegral $ lib ! moduleName ! sym
  Right x -> vmPutScratchpad $ fromIntegral x
