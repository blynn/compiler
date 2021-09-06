-- In-browser compiler.
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

main = interact toWasm

toWasm s = case untangle s of
  Left err -> err
  Right mods -> let
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
    in wasm

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
