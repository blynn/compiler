-- FFI across multiple modules.
-- Rewrite with named fields, Show, Eq.
module Main where
import Base
import Map
import Ast
import RTS
import Compiler
import System

hide_prelude_here' = hide_prelude_here'

compile s = either id id do
  mods <- untangle s
  let
    (ffis, ffes) = foldr ffcat (Tip, Tip) $ toAscList mods
    (bigmap, mem) = codegen ffis mods
    mustType modName s = case mlookup s $ fst $ fst $ mods ! modName of
      Just (Qual [] t) -> t
      _ -> error "TODO: report bad exports"
    mayMain = do
        tab <- mlookup "Main" bigmap
        mainAddr <- mlookup "main" tab
        mainType <- mlookup "main" $ fst $ fst $ mods ! "Main"
        pure (mainAddr, mainType)
  mainStr <- case mayMain of
    Nothing -> pure ""
    Just (a, q) -> do
      getIOType q
      pure $ genMain a

  pure
    $ ("#include<stdio.h>\n"++)
    . ("typedef unsigned u;\n"++)
    . ("enum{_UNDEFINED=0,"++)
    . foldr (.) id (map (\(s, _) -> ('_':) . (s++) . (',':)) comdefs)
    . ("};\n"++)
    . ("static const u prog[]={" ++)
    . foldr (.) id (map (\n -> shows n . (',':)) mem)
    . ("};\nstatic u root[]={" ++)
    . foldr (\(_, (modName, ourName)) f -> maybe undefined shows (mlookup ourName $ bigmap ! modName) . (", " ++) . f) id (toAscList ffes)
    . ("0};\n" ++)
    . (preamble++)
    . (libc++)
    . foldr (.) id (ffiDeclare <$> toAscList ffis)
    . ("static void foreign(u n) {\n  switch(n) {\n" ++)
    . foldr (.) id (zipWith ffiDefine [0..] $ toAscList ffis)
    . ("\n  }\n}\n" ++)
    . runFun
    . foldr (.) id (zipWith (\(expName, (modName, ourName)) n -> ("EXPORT(f"++) . shows n . (", \""++) . (expName++) . ("\")\n"++)
      . genExport (arrCount $ mustType modName ourName) n) (toAscList ffes) [0..])
    $ mainStr

main = getArgs >>= \case
  "comb":_ -> interact $ dumpWith dumpCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "type":_ -> interact $ dumpWith dumpTypes
  _ -> interact compile
  where
  getArg' k n = getArgChar n k >>= \c -> if ord c == 0 then pure [] else (c:) <$> getArg' (k + 1) n
  getArgs = getArgCount >>= \n -> mapM (getArg' 0) [1..n-1]
