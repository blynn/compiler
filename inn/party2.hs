-- Separate fixity phase.
-- Export lists.
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

dumpOptiCombs neat = map go $ optiComb $ second snd <$> toAscList (typedAsts neat) where
  go (s, t) = (s++) . (" = "++) . shows t . (";\n"++)

main = getArgs >>= \case
  "comb":_ -> interact $ dumpWith dumpCombs
  "opti":_ -> interact $ dumpWith dumpOptiCombs
  "rawcomb":_ -> interact $ dumpWith dumpRawCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "type":_ -> interact $ dumpWith dumpTypes
  "wasm":opts -> interact \s -> either id id $ untangle s >>= compileWith "1<<22" libcWasm ("no-main":opts)
  "warts":opts -> interact $ either id (warts opts) . untangle
  _ -> interact \s -> either id id $ untangle s >>= compile
