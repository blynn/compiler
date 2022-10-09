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
  ffes1 = foldr (\(expName, v) m -> insertWith (error $ "duplicate export: " ++ expName) expName v m) Tip
    [ (expName, addr)
    | (modName, neat) <- toAscList mods
    , (expName, ourName) <- toAscList $ ffiExports neat
    , let addr = maybe (error $ "missing: " ++ ourName) id $ mlookup ourName $ bigmap ! modName
    ]  -- Assume they have type IO ().
  mainExport = case mlookup "main" ffes1 of
    Nothing -> maybe [] (:[]) $ do
      mod  <- mlookup "Main" bigmap
      addr <- mlookup "main" mod
      pure ("main", addr)
    _ -> []
  ffes = mainExport ++ toAscList ffes1
  ffiMap = singleton "{foreign}" $ fromList $ zip (keys ffis) $ Right <$> [0..]
  (bigmap, mem) = codegen ffiMap mods
  go (n, x)
    -- Function section: for each export, declare a function of type () -> ()..
    | n == 3  = leb n <> extendSection x (replicate (length ffes) $ leb funType00Idx)
    -- Export section: add each export.
    | n == 7  = leb n <> extendSection x (zipWith encodeExport (fst <$> ffes) [allFunCount..])
    -- Code section: for nth export, define a function calling rts_reduce([512 + 4*n])
    | n == 10 = leb n <> extendSection x (callRoot <$> [0..length ffes - 1])
    | True    = leb n <> leb (_len s) <> s where s = unxxd x
  roots = encodeData rootBase $ littleEndian $ (snd <$> ffes) ++ [0]
  prog = encodeData heapBase $ littleEndian $ length mem : mem
  wasm = _str (unxxd "0061736d01000000" <> mconcat (map go wartsBytes)
-- Data section:
--   512 : null-terminated roots array
--   1048576 - 4: hp
--   1048576: initial heap contents
    <> leb 11 <> extendSection "00" [roots, prog]) ""

extendSection x xs = encodeSection (k + length xs) $ unxxd s <> mconcat xs
  where (k, s) = splitLeb x
splitLeb x = go x 1 0 where
  go (d1:d0:t) m acc
    | n < 128 = (acc + n*m, t)
    | True = go t (m*128) $ acc + (n - 128)*m
    where
    n = hexValue d1 * 16 + hexValue d0
encodeExport s n = encodeString s <> unxxd "00" <> leb n
encodeString s = let n = length s in leb n <> StrLen (s++) n

littleEndian ns = StrLen (foldr (.) id $ go 4 <$> ns) $ 4 * length ns where
  go k n
    | k == 0 = id
    | True = (chr (n `mod` 256):) . go (k - 1) (n `div` 256)

rootBase = unxxd "004180040b"  -- sleb 512 = 8004
heapBase = unxxd "0041fcff3f0b"  -- sleb (1048576 - 4) = fcff3f

-- 0 locals.
-- i32.const 0; i32.load 512 + 4*n; call $REDUCE; end;
callRoot n = leb (_len s) <> s where
  s = unxxd "0041002802" <> slebPos (512 + 4*n) <> unxxd "10"
   <> leb reduceFunIdx <> unxxd "0b"

encodeData addr s = addr <> leb (_len s) <> s
encodeSection k s = let n = leb k in leb (_len n + _len s) <> n <> s

leb n
  | n <= 127 = StrLen (chr n:) 1
  | True = StrLen (chr (128 + n `mod` 128):) 1 <> leb (n `div` 128)

slebPos n
  | n <= 63 = StrLen (chr n:) 1
  | True = StrLen (chr (128 + n `mod` 128):) 1 <> slebPos (n `div` 128)

foreign import ccall "get_module" getModule :: IO ()

needed tab = filter (not . (`member` tab)) $ concatMap dependentModules $ elems tab
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
