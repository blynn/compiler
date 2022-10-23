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
  rawCombs = optim . nolam . snd <$> typedAsts neat
  combs = toAscList $ rawCombs
  go (s, t) = (s++) . (" = "++) . shows t . (";\n"++)

dumpCombs neat = map go combs where
  rawCombs = optim . nolam . snd <$> typedAsts neat
  combs = toAscList $ rewriteCombs rawCombs <$> rawCombs
  go (s, t) = (s++) . (" = "++) . shows t . (";\n"++)

main = getArgs >>= \case
  "ink":_ -> interact \s -> either id (show . toAscList . fmap (\m -> (toAscList $ _syms m, _mem m))) $ ink s
  "ink2":_ -> interact $ either id id . ink2
  "topo":_ -> interact \s -> either id show $ do
    tab <- singleFile s
    topoModules (insert "#" neatPrim tab)
  "comb":_ -> interact $ dumpWith dumpCombs
  "rawcomb":_ -> interact $ dumpWith dumpRawCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "parse":_ -> interact \s -> either id show $ do
    tab <- singleFile s
    pure $ second topDefs <$> toAscList tab
  "type":_ -> interact $ dumpWith dumpTypes
  "wasm":opts -> interact \s -> either id id $ untangle s >>= compileWith "1<<22" libcWasm ("no-main":opts)
  "warts":opts -> interact $ either id (warts opts) . untangle
  _ -> interact \s -> either id id $ untangle s >>= compile
