-- Modules.
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

dumpLambs (typed, _) = map (\(s, (_, t)) -> (s++) . (" = "++) . showAst False t . ('\n':)) $ toAscList typed

dumpTypes (typed, _) = map (\(s, (q, _)) -> (s++) . (" :: "++) . showQual q . ('\n':)) $ toAscList typed

dumpCombs (typed, _) = go <$> optiComb (lambsList typed) where
  go (s, t) = (s++) . (" = "++) . showTree False t . (";\n"++)

main = getArgs >>= \case
  "comb":_ -> interact $ dumpWith dumpCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "type":_ -> interact $ dumpWith dumpTypes
  _ -> interact \s -> either id id $ untangle s >>= compile
  where
  getArg' k n = getArgChar n k >>= \c -> if ord c == 0 then pure [] else (c:) <$> getArg' (k + 1) n
  getArgs = getArgCount >>= \n -> mapM (getArg' 0) [1..n-1]
