module Main where
import Ast
import Base
import System
import Map
import Kiselyov
import Parser
import Typer
import RTS

neatPrompt = neatEmpty {moduleImports = singleton "" $ (, const True) <$> [">", "#", "Base", "System"]}

initObjs = do
  tab <- insert "#" neatPrim <$> singleFile source
  topo <- topoModules tab
  objs <- foldM compileModule Tip topo
  pure (fst <$> topo, objs)

genIndex objs (start, mm) name = (start + size syms, insert name (fromList $ zip (keys syms) [start..]) mm)
  where syms = _syms $ objs ! name

scratchObj lib ob = do
  scratch lib $ elems $ _syms ob
  scratch lib $ _mem ob
  vmGCRootScratchpad $ fromIntegral $ size $ _syms ob

initialState = do
  let
    libFFI = fromList [("{foreign}", fromList $ zip ffiList [0..])]
    Right (topo, objs) = initObjs
    (libStart, lib) = foldl (genIndex objs) (0, libFFI) topo
  mapM (scratchObj lib) $ (objs !) <$> topo
  pure (insert ">" (Module neatPrompt Tip []) objs, (libStart, lib))

mergeFragment neat frag = neat
  { typeclasses = foldr (uncurry insert) (typeclasses neat) $ toAscList $ typeclasses frag
  , instances = foldr (uncurry insert) (instances neat) $ toAscList $ instances frag
  , typedAsts = foldr (uncurry insert) (typedAsts neat) $ toAscList $ typedAsts frag
  , dataCons = foldr (uncurry insert) (dataCons neat) $ toAscList $ dataCons frag
  , type2Cons = foldr (uncurry insert) (type2Cons neat) $ toAscList $ type2Cons frag
  , opFixity = foldr (uncurry insert) (opFixity neat) $ toAscList $ opFixity frag
  }

exec lib (addr, mem) = do
  scratch lib mem
  vmRunScratchpad addr

addTyped (mos, (libStart, lib)) mos' = let
  orig = mos!">"
  fresh = mos'!">"
  mergedNeat = mergeFragment (_neat orig) (_neat fresh)
  mergedSyms = foldr (uncurry insert) (_syms orig) $ toAscList $ _syms fresh
  syms = _syms fresh
  roots = maybe Tip id $ mlookup ">" lib
  roots' = foldr (uncurry insert) roots $ zip (keys syms) [libStart..]
  libStart' = libStart + size syms
  lib' = insert ">" roots' lib
  mergedMos = insert ">" (Module mergedNeat mergedSyms []) mos'
  in do
    scratchObj lib fresh
    pure (mergedMos, (libStart', lib'))

readInput mos s = do
  fragOrExpr <- fst <$> parse fmt s
  case fragOrExpr of
    Left frag -> Left <$> tryAddDefs frag
    Right expr -> Right <$> tryExpr expr
  where
  fmt = Left <$> fragment <|> Right <$> single
  fragment = ($ neatEmpty{moduleImports = moduleImports neatPrompt}) <$> (lexemePrelude *> topdecls <* eof)
  single = many (char ' ') *> expr <* eof

  tryAddDefs frag = compileModule mos (">", frag)
  tryExpr sugary = do
    ast <- snd <$> patternCompile searcherPrompt sugary
    (typ, typedAst) <- (! "") <$> inferDefs searcherPrompt [("", ([], ast))] Tip Tip
    case typ of
      Qual [] (TAp (TC "IO") _) -> let
        combs = nolam . optiApp $ typedAst
        (addr, (_, (hp', memF))) = runState (enc combs) (Tip, (128, id))
        in pure (hp', memF [Right $ comEnum "I", addr])
      _ -> tryExpr $ A (V "print") sugary
  searcherPrompt = searcherNew ">" (_neat <$> mos) neatPrompt

scratch lib = mapM \case
  Left (moduleName, sym) -> (if moduleName == "{foreign}" then vmPutScratchpad else vmPutScratchpadRoot) $ fromIntegral $ lib ! moduleName ! sym
  Right x -> vmPutScratchpad $ fromIntegral x
