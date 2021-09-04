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

mempty = (id, 0)
mconcat = foldr (<>) mempty

(s1, n1) <> (s2, n2) = (s1 . s2, n1 + n2)

hexValue d
  | d <= '9' = ord d - ord '0'
  | d <= 'F' = 10 + ord d - ord 'A'
  | d <= 'f' = 10 + ord d - ord 'a'

unxxd s = (go s, length s `div` 2) where
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
  go (n, x) = leb n <> leb (snd s) <> s where s = unxxd x
  mainAddr = (bigmap ! "Main") ! "main"
  roots = encodeData rootBase $ littleEndian [mainAddr, 0]
  prog = encodeData heapBase $ littleEndian $ length mem : mem
  wasm = fst (unxxd "0061736d01000000" <> mconcat (map go wartsBytes)
-- Data section:
--   512 : null-terminated roots array
--   1048576 - 4: hp
--   1048576: initial heap contents
    <> leb 11 <> extendSection (0, "") [roots, prog]) ""

extendSection (k, s) xs = encodeSection (k + length xs) $ unxxd s <> mconcat xs
encodeExport s n = encodeString s <> unxxd "00" <> leb n
encodeString s = let n = length s in leb n <> ((s++), n)

littleEndian ns = (foldr (.) id $ go 4 <$> ns, 4 * length ns) where
  go k n
    | k == 0 = id
    | True = (chr (n `mod` 256):) . go (k - 1) (n `div` 256)

rootBase = unxxd "004180040b"  -- sleb 512 = 8004
heapBase = unxxd "0041fcff3f0b"  -- sleb (1048576 - 4) = fcff3f

-- 0 locals; i32.const 0; i32.load 512 + 4*n; call 8; end;
callRoot n = leb fLen <> (f, fLen) where
  (f, fLen) = unxxd "0041002802" <> slebPos (512 + 4*n) <> unxxd "10080b"

encodeData addr (s, slen) = addr <> leb slen <> (s, slen)
encodeSection k s = let n = leb k in leb (snd n + snd s) <> n <> s

leb n
  | n <= 127 = ((chr n:), 1)
  | True = ((chr (128 + n `mod` 128):), 1) <> leb (n `div` 128)

slebPos n
  | n <= 63 = ((chr n:), 1)
  | True = ((chr (128 + n `mod` 128):), 1) <> slebPos (n `div` 128)

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
