module Reply where
import Ast
import Base
import System
import Map
import Kiselyov
import Parser
import Typer
import RTS
import Obj

neatInit = neatEmpty {moduleImports = singleton "" $ (, const True) <$> ["#", "Base", "System"]}

layoutObjects xs = (objs, (libStart, lib)) where
  Right objs = compileModule (decodeObjectMap xs) ("#", neatPrim)
  ffiList = concatMap (keys . ffiImports . _neat) $ elems objs
  libFFI = fromList [("{foreign}", fromList $ zip ffiList [0..])]
  (libStart, lib) = foldl genIndex (0, libFFI) $ toAscList objs

engrave xs = do
  let (objs, (libStart, lib)) = layoutObjects xs
  mapM_ (scratchObj lib) $ elems objs
  pure (objs, (libStart, lib))

moduleNew name (objs, (libStart, lib)) =
  (insert name (Module neatInit Tip []) objs, (libStart, insert name mempty lib))

genIndex (start, mm) (name, obj) = (start + size syms, insert name (fromList $ zip (keys syms) [start..]) mm)
  where syms = _syms obj

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
  , moduleImports = moduleImports frag
  , opFixity = foldr (uncurry insert) (opFixity neat) $ toAscList $ opFixity frag
  , typeAliases = foldr (uncurry insert) (typeAliases neat) $ toAscList $ typeAliases frag
  }

exec lib (addr, mem) = do
  scratch lib mem
  vmRunScratchpad addr

addTyped (libStart, lib) name fresh = let
  syms = _syms fresh
  roots = maybe Tip id $ mlookup name lib
  roots' = foldr (uncurry insert) roots $ zip (keys syms) [libStart..]
  libStart' = libStart + size syms
  lib' = insert name roots' lib
  in do
    scratchObj lib fresh
    pure (libStart', lib')

readInput mos name s = go mos . smoosh =<< fst <$> parse fmt s
  where
  go mos = \case
    [] -> pure []
    Left f:rest -> do
      (mos', fresh) <- tryAddDefs mos name (f neatEmpty{moduleImports = moduleImports $ _neat $ mos!name})
      (Left (mos', fresh):) <$> go mos' rest
    Right expr:rest -> do
      runme <- tryExpr mos name expr
      (Right runme:) <$> go mos rest
  fmt = lexemePrelude *> ignoreModule *> (braceSep $ Left <$> topLevel <|> Right <$> expr) <* eof
  ignoreModule = res "module" *> conId *> exports *> pure () <|> pure ()

tryExpr mos name sugary = do
  ast <- snd <$> patternCompile searcher sugary
  (typ, typedAst) <- (! "") <$> inferDefs searcher [("", ([], ast))] Tip Tip
  case typ of
    Qual [] (TAp (TC "IO") _) -> let
      combs = nolam . optiApp $ typedAst
      (addr, (_, (hp', memF))) = runState (enc combs) (Tip, (128, id))
      in pure (hp', memF [Right $ comEnum "I", addr])
    _ -> tryExpr mos name $ A (V "print") sugary
  where
  searcher = searcherNew name (_neat <$> mos) $ importSelf name $ neatEmpty {moduleImports = moduleImports $ _neat $ mos!name}

tryAddDefs mos name frag = do
  mos1 <- compileModule mos (name, importSelf name frag)
  let
    fresh = rmSelfImportModule $ mos1!name
    orig = mos!name
    mergedNeat = mergeFragment (_neat orig) (_neat fresh)
    mergedSyms = foldr (uncurry insert) (_syms orig) $ toAscList $ _syms fresh

    mergedMos = insert name (Module mergedNeat mergedSyms []) mos1

  pure (mergedMos, fresh)
  where
  rmSelfImportModule m = m {_neat = rmSelfImportNeat $ _neat m}
  rmSelfImportNeat n = n {moduleImports = insert "" (tail $ moduleImports n ! "") (moduleImports n)}

importSelf name neat = neat{moduleImports = insertWith (++) "" [(name, const True)] $ moduleImports neat}

smoosh = go (const id, id) where
  go (leftStreakEnd, acc) = \case
    [] -> leftStreakEnd acc []
    Right x:rest -> leftStreakEnd acc $ Right x : smoosh rest
    Left f:rest -> go ((:) . Left, acc . f) rest

scratch lib = mapM \case
  Left (moduleName, sym) -> (if moduleName == "{foreign}" then vmPutScratchpad else vmPutScratchpadRoot) $ fromIntegral $ lib ! moduleName ! sym
  Right x -> vmPutScratchpad $ fromIntegral x
