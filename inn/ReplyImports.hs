foreign import ccall "vmdump" vmdump :: x -> IO Word
foreign import ccall "scratch_at" scratchAt :: Word -> IO Word
foreign import ccall "scratch_reset" scratchReset :: IO ()
foreign import ccall "vmscratch" vmPutScratchpad :: Word -> IO ()
foreign import ccall "vmscratchroot" vmPutScratchpadRoot :: Word -> IO ()
foreign import ccall "vmgcroot" vmGCRootScratchpad :: IO ()

vmDumpWith f x = do
  n <- vmdump x
  if n < 128 then putStr $ shows n ", " else flip mapM_ [0..n-128-1] \k -> f =<< scratchAt k

-- TODO: Automate keeping this synced with RTS.
comdefsrc = [r|
F x = "foreign(num(1));"
Y x = x "sp[1]"
Q x y z = z(y x)
S x y z = x z(y z)
B x y z = x (y z)
BK x y z = x y
C x y z = x z y
R x y z = y z x
V x y z = z x y
T x y = y x
K x y = "_I" x
KI x y = "_I" y
I x = "sp[1] = arg(1); sp++;"
LEFT x y z = y x
CONS x y z w = w x y
NUM x y = y "sp[1]"
NUM64 x y = y "sp[1]"
FLO x = "lazy2(1, _I, app64d((double) num(1)));"
FLW x = "lazy2(1, _I, app64d((double) (u) num(1)));"
OLF x = "_NUM" "((int) flo(1))"
UUADD x y = "lazy2(2, _I, app64uu(uunum(1) + uunum(2)));"
UUSUB x y = "lazy2(2, _I, app64uu(uunum(1) - uunum(2)));"
UUMUL x y = "lazy2(2, _I, app64uu(uunum(1) * uunum(2)));"
UUDIV x y = "lazy2(2, _I, app64uu(uunum(1) / uunum(2)));"
UUMOD x y = "lazy2(2, _I, app64uu(uunum(1) % uunum(2)));"
UUAND x y = "lazy2(2, _I, app64uu(uunum(1) & uunum(2)));"
UUOR x y = "lazy2(2, _I, app64uu(uunum(1) | uunum(2)));"
UUXOR x y = "lazy2(2, _I, app64uu(uunum(1) ^ uunum(2)));"
UUSHL x y = "lazy2(2, _I, app64uu(uunum(1) << num(2)));"
UUSHR x y = "lazy2(2, _I, app64uu(uunum(1) >> num(2)));"
UUPAD x = "lazy2(1, _I, app64uu((u) num(1)));"
UUEQ x y = "lazy2(2, _I, uunum(1) == uunum(2) ? _K : _KI);"
UULE x y = "lazy2(2, _I, uunum(1) <= uunum(2) ? _K : _KI);"
UULO x = "_NUM" "((u) uunum(1))"
FADD x y = "lazy2(2, _I, app64d(flo(1) + flo(2)));"
FSUB x y = "lazy2(2, _I, app64d(flo(1) - flo(2)));"
FMUL x y = "lazy2(2, _I, app64d(flo(1) * flo(2)));"
FDIV x y = "lazy2(2, _I, app64d(flo(1) / flo(2)));"
FLE x y = "lazy2(2, _I, flo(1) <= flo(2) ? _K : _KI);"
FEQ x y = "lazy2(2, _I, flo(1) == flo(2) ? _K : _KI);"
FFLOOR x = "lazy2(1, _I, app64d(__builtin_floor(flo(1))));"
FSQRT x = "lazy2(1, _I, app64d(__builtin_sqrt(flo(1))));"
PAIR64 x = "{uu n = (*((uu*) (mem + arg(1) + 2)));lazy2(1, app(_V, app(_NUM, n)), app(_NUM, n >> 32));}"
ADD x y = "_NUM" "num(1) + num(2)"
SUB x y = "_NUM" "num(1) - num(2)"
MUL x y = "_NUM" "num(1) * num(2)"
QUOT x y = "_NUM" "num(1) / num(2)"
REM x y = "_NUM" "num(1) % num(2)"
DIV x y = "_NUM" "div(num(1), num(2))"
MOD x y = "_NUM" "mod(num(1), num(2))"
XOR x y = "_NUM" "num(1) ^ num(2)"
AND x y = "_NUM" "num(1) & num(2)"
OR x y = "_NUM" "num(1) | num(2)"
SHL x y = "_NUM" "num(1) << num(2)"
SHR x y = "_NUM" "num(1) < 0 ? ~(~num(1) >> num(2)) : num(1) >> num(2)"
U_SHR x y = "_NUM" "(u) num(1) >> (u) num(2)"
EQ x y = "lazy2(2, _I, num(1) == num(2) ? _K : _KI);"
LE x y = "lazy2(2, _I, num(1) <= num(2) ? _K : _KI);"
U_DIV x y = "_NUM" "(u) num(1) / (u) num(2)"
U_MOD x y = "_NUM" "(u) num(1) % (u) num(2)"
U_LE x y = "lazy2(2, _I, (u) num(1) <= (u) num(2) ? _K : _KI);"
REF x y = y "sp[1]"
NEWREF x y z = z ("_REF" x) y
READREF x y z = z "num(1)" y
WRITEREF x y z w = w "((mem[arg(2) + 1] = arg(1)), _K)" z
END = "return 1;"
ERR = "sp[1]=app(app(arg(1),_ERREND),_ERR2);sp++;"
ERR2 = "lazy3(2, arg(1), _ERROUT, arg(2));"
ERROUT = "errchar(num(1)); lazy2(2, _ERR, arg(2));"
ERREND = "errexit(); return 2;"
VMRUN = "vmrun();"
VMPTR = "lazy3(3, arg(3), app(_NUM, arg(1)), arg(2));"
SUSPEND = "*(sp = spTop) = app(app(arg(1), _UNDEFINED), _END); suspend_status = 0; return 1;"
|]

espy x = do
  n <- vmdump x
  if n < 128 then putStrLn $ tab!!fromIntegral n else do
    shared <- ($ []) <$> findShares [] 128
    putStrLn =<< ($ "") <$> go 0 shared True 128
    heap <- mapM (\n -> (n,) . ($ "") <$> go 0 shared True n) (filter (/= 128) shared)
    unless (null heap) do
      putStrLn "where"
      mapM_ (\(n, s) -> putStrLn $ "  " ++ show n ++ " = " ++ s) heap
  where
    tab = "?" : tail (head . words <$> lines comdefsrc)
    numTag = fromIntegral $ head [x | x <- [0..], "NUM" == tab!!x]
    num64Tag = fromIntegral $ head [x | x <- [0..], "NUM64" == tab!!x]
    findShares m n
      | n < 128 = pure id
      | n `elem` m = pure $ \xs -> if elem n xs then xs else n:xs
      | otherwise = do
        x <- scratchAt (n - 128)
        y <- scratchAt (n - 128 + 1)
        if x == numTag || x == num64Tag then pure id else do
          f <- findShares (n:m) x
          g <- findShares (n:m) y
          pure $ f . g
    go prec shared force n
      | n < 128 = pure ((tab!!fromIntegral n)++)
      | n == 128, not force = pure ("*"++)
      | n `elem` shared, not force = pure $ shows n
      | otherwise = do
        x <- scratchAt (n - 128)
        y <- scratchAt (n - 128 + 1)
        if x == numTag then pure $ shows y
          else if x == num64Tag then do
            y <- mapM scratchAt $ [n - 128 + 2]
            z <- mapM scratchAt $ [n - 128 + 3]
            pure $ shows (y, z)
          else do
            f <- go 0 shared False x
            g <- go 1 shared False y
            pure $ showParen (prec > 0) $ f . (' ':) . g
