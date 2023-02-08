-- FFI across multiple modules.
-- Rewrite with named fields, Show, Eq.
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
  Right tab -> foldr ($) [] $ map (\(name, mod) -> ("module "++) . (name++) . ('\n':) . (foldr (.) id $ dumper mod)) $ toAscList tab

dumpLambs (typed, _) = map (\(s, (_, t)) -> (s++) . (" = "++) . shows t . ('\n':)) $ toAscList typed

dumpTypes (typed, _) = map (\(s, (q, _)) -> (s++) . (" :: "++) . shows q . ('\n':)) $ toAscList typed

dumpCombs (typed, _) = map go combs where
  rawCombs = optim . nolam . snd <$> typed
  combs = toAscList $ rewriteCombs rawCombs <$> rawCombs
  go (s, t) = (s++) . (" = "++) . shows t . (";\n"++)

main = getArgs >>= \case
  "comb":_ -> interact $ dumpWith dumpCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "type":_ -> interact $ dumpWith dumpTypes
  _ -> interact \s -> either id id $ untangle s >>= compile
