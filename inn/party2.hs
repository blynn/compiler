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

dumpLambs neat = map (\(s, t) -> (s++) . (" = "++) . showAst False t . ('\n':)) $ second snd <$> toAscList (typedAsts neat)

dumpTypes neat = map (\(s, q) -> (s++) . (" :: "++) . shows q . ('\n':)) $ second fst <$> toAscList (typedAsts neat)

dumpCombs neat = map go $ optiComb $ second snd <$> toAscList (typedAsts neat) where
  go (s, t) = (s++) . (" = "++) . shows t . (";\n"++)

libcWasm = [r|
extern u __heap_base;
void* malloc(unsigned long n) {
  static u bump = (u) &__heap_base;
  return (void *) ((bump += n) - n);
}
void errchar(int c) {}
void errexit() {}
|]

genMainWasm n = "EXPORT(go,\"go\")\nvoid go(){rts_reduce(" ++ shows n ");}\n"

main = getArgs >>= \case
  "comb":_ -> interact $ dumpWith dumpCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "type":_ -> interact $ dumpWith dumpTypes
  "wasm":_ -> interact \s -> either id id $ untangle s >>= compileWith "1<<22" libcWasm genMainWasm
  _ -> interact \s -> either id id $ untangle s >>= compile
  where
  getArg' k n = getArgChar n k >>= \c -> if ord c == 0 then pure [] else (c:) <$> getArg' (k + 1) n
  getArgs = getArgCount >>= \n -> mapM (getArg' 0) [1..n-1]
