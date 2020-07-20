-- In-browser compiler. Blah: Ben Lynn's Atrocious Haskell.
-- (Or Awful? Awkward? Anti-establishment?)

getContents = isEOF >>= \b -> if b then pure [] else getChar >>= \n -> (chr n:) <$> getContents

customMods =
    addFFI "putchar" "putChar" (arr (TC "Char") $ TAp (TC "IO") (TC "()"))
  . addFFI "getchar" "getChar" (TAp (TC "IO") (TC "Char"))
  . addFFI "eof" "isEOFInt" (TAp (TC "IO") (TC "Int"))

ffi "putchar" putChar :: Char -> IO ()
ffi "getchar" getChar :: IO Int
ffi "eof" isEOFInt :: IO Int
isEOF = (0 /=) <$> isEOFInt

export "compile" goCompile
goCompile = interact compile
export "type" goType
goType = interact dumpTypes
export "comb" goComb
goComb = interact dumpCombs
export "lamb" goLamb
goLamb = interact dumpLambs

comEnum s = maybe (error $ s) id $ lookup s $ zip comlist (upFrom 1)
comName i = maybe undefined id $ lookup i $ zip (upFrom 1) comlist

mempty = (id, 0)
mconcat = foldr (<>) mempty

(s1, n1) <> (s2, n2) = (s1 . s2, n1 + n2)

hexy s = case s of
  [] -> mempty
  (d1:d0:rest) -> (((chr $ hexValue d1 * 16 + hexValue d0):), 1) <> hexy rest

getIOType (Qual [] (TAp (TC "IO") t)) = Right t
getIOType q = Left $ "main : " ++ showQual q ""

compile s = case untangle s of
  Left err -> err
  Right ((typed, lambs), _) -> let
    (tab, mem) = hashcons $ optiComb lambs
    go (n, sec) = leb n <> extendSection sec []
    roots = encodeData rootBase $ wordLE (tab ! "main") <> wordLE 0
    prog = encodeData heapBase hpHeap
    hpHeap = wordLE (length mem) <> mconcat (map wordLE mem)
    mainQT = maybe (Left "no main") Right $ mlookup "main" typed
    wasm = fst (hexy "0061736d01000000" <> mconcat (map go rts)
-- Data section:
--   512 : null-terminated roots array
--   1048576 - 4: hp
--   1048576: initial heap contents
      <> leb 11 <> extendSection (0, "") [roots, prog]) ""
    in either error (const wasm) $ mainQT >>= getIOType

extendSection (k, s) xs = encodeSection (k + length xs) $ hexy s <> mconcat xs
encodeExport s n = encodeString s <> hexy "00" <> leb n
encodeString s = let n = length s in leb n <> ((s++), n)

wordLE n = (go n 4, 4) where
  go n k
    | k == 0 = id
    | True = (chr (n `mod` 256):) . go (n `div` 256) (k - 1)

rootBase = hexy "004180040b"  -- sleb 512 = 8004
heapBase = hexy "0041fcff3f0b"  -- sleb (1048576 - 4) = fcff3f

-- 0 locals; i32.const 0; i32.load 512 + 4*n; call 8; end;
callRoot n = leb fLen <> (f, fLen) where
  (f, fLen) = hexy "0041002802" <> slebPos (512 + 4*n) <> hexy "10080b"

encodeData addr (s, slen) = addr <> leb slen <> (s, slen)
encodeSection k s = let  n = leb k in leb (snd n + snd s) <> n <> s

leb n
  | n <= 127 = ((chr n:), 1)
  | True = ((chr (128 + n `mod` 128):), 1) <> leb (n `div` 128)

slebPos n
  | n <= 63 = ((chr n:), 1)
  | True = ((chr (128 + n `mod` 128):), 1) <> slebPos (n `div` 128)
