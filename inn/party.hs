-- Modules.
module Main where
import Base
import Map
import Ast
import RTS
import Compiler
import System

compile s = either id id do
  mods <- untangle s
  let
    (bigmap, mem) = codegen mods
    (ffis, ffes) = foldr ffcat ([], []) $ toAscList mods
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
    . foldr (.) id (map (\n -> showInt n . (',':)) mem)
    . ("};\nstatic u root[]={" ++)
    . foldr (\(modName, (_, ourName)) f -> maybe undefined showInt (mlookup ourName $ bigmap ! modName) . (", " ++) . f) id ffes
    . ("0};\n" ++)
    . (preamble++)
    . (libc++)
    . (concatMap ffiDeclare ffis ++)
    . ("static void foreign(u n) {\n  switch(n) {\n" ++)
    . ffiDefine (length ffis - 1) ffis
    . ("\n  }\n}\n" ++)
    . runFun
    . foldr (.) id (zipWith (\(modName, (expName, ourName))  n -> ("EXPORT(f"++) . showInt n . (", \""++) . (expName++) . ("\")\n"++)
      . genExport (arrCount $ mustType modName ourName) n) ffes [0..])
    $ mainStr

main = getArgs >>= \case
  "comb":_ -> interact $ dumpWith dumpCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "type":_ -> interact $ dumpWith dumpTypes
  _ -> interact compile
  where
  getArg' k n = getArgChar n k >>= \c -> if ord c == 0 then pure [] else (c:) <$> getArg' (k + 1) n
  getArgs = getArgCount >>= \n -> mapM (getArg' 0) [1..n-1]
