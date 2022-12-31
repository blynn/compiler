-- Precisely.
module Main where
import Base
import Map
import Ast
import RTS
import Typer
import Kiselyov
import System

hide_prelude_here' = hide_prelude_here'

dumpWith dumper s = case untangle s of
  Left err -> err
  Right tab -> foldr ($) [] $ map (\(name, neat) -> ("module "++) . (name++) . ('\n':) . (foldr (.) id $ dumper neat)) $ toAscList tab

dumpLambs neat = map (\(s, t) -> (s++) . (" = "++) . shows t . ('\n':)) $ second snd <$> toAscList (typedAsts neat)

dumpTypes neat = map (\(s, q) -> (s++) . (" :: "++) . shows q . ('\n':)) $ second fst <$> toAscList (typedAsts neat)

dumpRawCombs neat = map go combs where
  rawCombs = nolam . snd <$> typedAsts neat
  combs = toAscList $ rawCombs
  go (s, t) = (s++) . (" = "++) . shows t . (";\n"++)

dumpCombs = foldr ($) "" . map \(moduleName, xs) -> ("module "++) . (moduleName++) . ('\n':) . foldr (.) id (map (\(s, t) -> (s++) . (" = "++) . shows t . (";\n"++)) xs)

dumpMatrix neat = map go combs where
  combs = toAscList $ matrixComb . optiApp . snd <$> typedAsts neat
  go (s, t) = (s++) . (" = "++) . shows t . (";\n"++)

objDump s = do
  tab <- insert "#" neatPrim <$> singleFile s
  ms <- topoModules tab
  foldM compileModule Tip $ zip ms $ (tab !) <$> ms

main = getArgs >>= \case
  "obj":_ -> interact $ either id (show . toAscList . fmap (\m -> (toAscList $ _syms m, _mem m))) . objDump
  "matrix":_ -> interact $ dumpWith dumpMatrix
  "topo":_ -> interact \s -> either id show $ do
    tab <- singleFile s
    topoModules (insert "#" neatPrim tab)
  "comb":_ -> interact $ either id (dumpCombs . toAscList . fmap (toAscList . _combs)). objDump
  "rawcomb":_ -> interact $ dumpWith dumpRawCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "parse":_ -> interact \s -> either id show $ do
    tab <- singleFile s
    pure $ second topDefs <$> toAscList tab
  "type":_ -> interact $ dumpWith dumpTypes
  "warts":opts -> interact $ either id (warts $ "warts":opts) . untangle
  "wasm":opts -> interact $ either id id . compile "1<<22" libcWasm ("no-main":opts)
  _ -> interact $ either id id . compile "1<<24" libcHost []
