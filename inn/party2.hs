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

dumpCombs neat = map go $ optiComb $ second snd <$> toAscList (typedAsts neat) where
  go (s, t) = (s++) . (" = "++) . shows t . (";\n"++)

main = getArgs >>= \case
  "comb":_ -> interact $ dumpWith dumpCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "type":_ -> interact $ dumpWith dumpTypes
  "wasm":_ -> interact \s -> either id id $ untangle s >>= compileWith "1<<22" libcWasm (const "")
  "warts":_ -> interact $ either id warts . untangle
  _ -> interact \s -> either id id $ untangle s >>= compile
