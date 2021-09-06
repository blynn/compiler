-- Like Webby, but for each missing import, we print its name, and call
-- `getModule`, a function in the environment which should then reset the
-- standard input to the source of the missing module. We then compile it,
-- which could result in more missing imports, which are similarly handled.
--
-- Unlike Webby, this compiler is unable to compile itself. To achieve this, we
-- need to generate foreign imports in the output wasm; at present, we produce
-- a wasm with a hard-wired set of imports described in `SystemWasm`.
module Main where

import Base
import Ast
import Map
import Typer
import RTS
import System
import WartsBytes

data StrLen = StrLen { _str :: String -> String, _len :: Int }
instance Monoid StrLen where
  mempty = StrLen id 0
  (StrLen s1 n1) <> (StrLen s2 n2) = StrLen (s1 . s2) (n1 + n2)

hexValue d
  | d <= '9' = ord d - ord '0'
  | d <= 'F' = 10 + ord d - ord 'A'
  | d <= 'f' = 10 + ord d - ord 'a'

unxxd s = StrLen (go s) (length s `div` 2) where
  go s = case s of
    [] -> id
    (d1:d0:rest) -> ((chr $ hexValue d1 * 16 + hexValue d0):) . go rest

foreign export ccall "single_module" singleModule
singleModule = interact $ \s -> case singleFile s of
  Right tab -> case keys tab of
    [s] -> s
    _ -> ""
  _ -> ""

toWasm mods = wasm where
  ffis = foldr (\(k, v) m -> insertWith (error $ "duplicate import: " ++ k) k v m) Tip $ concatMap (toAscList . ffiImports) $ elems mods
  (bigmap, mem) = codegen ffis mods
  go (n, x) = leb n <> leb (_len s) <> s where s = unxxd x
  mainAddr = (bigmap ! "Main") ! "main"
  roots = encodeData rootBase $ littleEndian [mainAddr, 0]
  prog = encodeData heapBase $ littleEndian $ length mem : mem
  wasm = _str (unxxd "0061736d01000000" <> mconcat (map go wartsBytes)
-- Data section:
--   512 : null-terminated roots array
--   1048576 - 4: hp
--   1048576: initial heap contents
    <> leb 11 <> extendSection (0, "") [roots, prog]) ""

extendSection (k, s) xs = encodeSection (k + length xs) $ unxxd s <> mconcat xs
encodeExport s n = encodeString s <> unxxd "00" <> leb n
encodeString s = let n = length s in leb n <> StrLen (s++) n

littleEndian ns = StrLen (foldr (.) id $ go 4 <$> ns) $ 4 * length ns where
  go k n
    | k == 0 = id
    | True = (chr (n `mod` 256):) . go (k - 1) (n `div` 256)

rootBase = unxxd "004180040b"  -- sleb 512 = 8004
heapBase = unxxd "0041fcff3f0b"  -- sleb (1048576 - 4) = fcff3f

encodeData addr s = addr <> leb (_len s) <> s
encodeSection k s = let n = leb k in leb (_len n + _len s) <> n <> s

leb n
  | n <= 127 = StrLen (chr n:) 1
  | True = StrLen (chr (128 + n `mod` 128):) 1 <> leb (n `div` 128)

slebPos n
  | n <= 63 = StrLen (chr n:) 1
  | True = StrLen (chr (128 + n `mod` 128):) 1 <> slebPos (n `div` 128)

foreign import ccall "get_module" getModule :: IO ()

needed tab = filter (not . (`member` tab)) $ concatMap moduleImports $ elems tab
complete tab = case needed tab of
  [] -> putStr $ either id id $ toWasm <$> foldM (inferModule tab) Tip (keys tab)
  f:_ -> do
    putStr f
    getModule
    s <- getContents
    case singleFile s of
      Left e -> putStr $ f ++ ": " ++ e
      Right tab' -> case foldM checkMerge tab $ assocs tab' of
        Left e -> putStr e
        Right tab -> complete tab

checkMerge tab (k, v)
  | member k tab = Left $ "duplicate: " ++ show k
  | otherwise    = Right $ insert k v tab

foreign export ccall "go" main
main = do
  s <- getContents
  case singleFile s of
    Left e -> putStr e
    Right tab -> complete tab
