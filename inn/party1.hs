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

dumpLambs (typed, _) = map (\(s, (t, _)) -> (s++) . (" = "++) . shows t . ('\n':)) $ toAscList typed

dumpTypes (typed, _) = map (\(s, (_, q)) -> (s++) . (" :: "++) . shows q . ('\n':)) $ toAscList typed

dumpCombs (typed, _) = go <$> optiComb (lambsList typed) where
  go (s, t) = (s++) . (" = "++) . shows t . (";\n"++)

main = getArgs >>= \case
  "comb":_ -> interact $ dumpWith dumpCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "type":_ -> interact $ dumpWith dumpTypes
  _ -> interact \s -> either id id $ untangle s >>= compile
