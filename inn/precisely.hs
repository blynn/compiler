-- Precisely.
module Main where
import Base
import Map
import Ast
import RTS
import Typer
import Kiselyov
import System
import Obj

hide_prelude_here' = hide_prelude_here'

dumpWith dumper s = case objectify s of
  Left err -> err
  Right tab -> foldr ($) [] $ map (\(name, mod) -> ("module "++) . (name++) . ('\n':) . (foldr (.) id $ dumper $ _neat mod)) $ toAscList tab

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

recomb objs = dumpCombs . toAscList $ fmap (toAscList . combTyped objs . typedAsts . _neat) objs

instance Show Assoc where
  showsPrec _ = \case
    LAssoc -> ('L':)
    RAssoc -> ('R':)
    NAssoc -> ('N':)

instance Show Constr where
  showsPrec _ (Constr s sts) = shows s . (", "++) . shows sts

instance Show Instance where
  showsPrec _ (Instance ty name ctx _) =
    shows ty . (", "++) . (name++) . (", "++) . shows ctx

objDump s = do
  objs <- objectify s
  pure $ foldr (.) id (intersperse ("    , "++) $ go <$> toAscList objs)
  where
  go (k, m) = ("("++) . shows k . (", Module"++)
    . ("\n  { _syms = fromList "++)
    . shows (toAscList $ _syms m)
    . ("\n  , _mem = "++)
    . shows (_mem m)
    . ("\n  , _neat =\n"++)
    . goNeat (_neat m)
    . ("\n  })\n"++)
  goNeat neat = ("{ typeclasses = fromList "++)
    . shows (toAscList $ typeclasses neat)
    . ("\n, instances = "++)
    . shows (toAscList $ instances neat)
    . ("\n, types = ["++)
    . foldr (.) id (intersperse (',':) $ map goSTA $ toAscList $ typedAsts neat)
    . ("]"++)
    . ("\n, dataCons = "++)
    . shows (dataCons neat)
    . ("\n, moduleExports = "++)
    . shows (moduleExports $ exportStuff neat)
    . ("\n, opFixity = "++)
    . shows (opFixity neat)
    . ("\n, typeAliases = "++)
    . shows (typeAliases neat)
    . ("\n}\n"++)
  goSTA (s, (t, a)) = ('(':) . shows s . (", "++) . shows t . (')':)

allFFIs s = fromList . concatMap (toAscList . ffiImports) . elems . fst <$> singleFile s

obj s = foldr (.) id . map scriptify . toAscList <$> objectify s

main = getArgs >>= \case
  "objdump":_ -> interact $ either id ($ "") . objDump
  "obj":_ -> interact $ either id ($ "") . obj
  "objc":_ -> interact $ either id (($ "") . toCsource . delete "#") . objectify
  "matrix":_ -> interact $ dumpWith dumpMatrix
  "comb":_ -> interact $ either id recomb . objectify
  "rawcomb":_ -> interact $ dumpWith dumpRawCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "parse":_ -> interact \s -> either id show $ do
    (tab, _) <- singleFile s
    pure $ second (toAscList . topDefs) <$> toAscList tab
  "type":_ -> interact $ dumpWith dumpTypes
  "ffis":opts -> interact $ either id (($ "\n") . shows . keys) . allFFIs
  "warts":opts -> interact $ either id (warts $ "warts":opts) . allFFIs
  "wasm":opts -> interact $ either id id . compile "1<<26" libcWasm ("no-main":opts)
  _ -> interact $ either id id . compile "1<<24" libcHost []
