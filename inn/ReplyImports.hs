foreign import ccall "vmdump" vmdump :: x -> IO Word
foreign import ccall "scratch_at" scratchAt :: Word -> IO Word
foreign import ccall "scratch_reset" scratchReset :: IO ()
foreign import ccall "vmscratch" vmPutScratchpad :: Word -> IO ()
foreign import ccall "vmscratchroot" vmPutScratchpadRoot :: Word -> IO ()
foreign import ccall "vmgcroot" vmGCRootScratchpad :: IO ()

vmDumpWith f x = do
  n <- vmdump x
  if n < 128 then putStr $ shows n ", " else flip mapM_ [0..n-128-1] \k -> f =<< scratchAt k

espy x = do
  n <- vmdump x
  if n < 128 then putStr $ tab!!fromIntegral n else do
    shared <- ($ []) <$> findShares [] 128
    putStrLn =<< ($ "") <$> go 0 shared True 128
    print =<< mapM (\n -> (n,) . ($ "") <$> go 0 shared True n) (filter (/= 128) shared)
  where
    tab = ["?", "F", "Y", "Q", "QQ", "S", "B", "BK", "C", "R", "V", "T", "K", "KI", "I", "LEFT", "(:)", "NUM", "NUM64", "FLO", "FLW", "OLF", "FADD", "FSUB", "FMUL", "FDIV", "FLE", "FEQ", "FFLOOR", "FSQRT", "PAIR64", "DADD", "DSUB", "DMUL", "DDIV", "DMOD", "DSHL", "DSHR", "ADD", "SUB", "MUL", "QUOT", "REM", "DIV", "MOD", "XOR", "AND", "OR", "SHL", "SHR", "U_SHR", "EQ", "LE", "U_DIV", "U_MOD", "U_LE", "REF", "NEWREF", "READREF", "WRITEREF", "END", "ERR", "ERR2", "ERROUT", "ERREND", "VMRUN", "VMPTR", "SUSPEND"]
    findShares m n
      | n < 128 = pure id
      | n `elem` m = pure $ \xs -> if elem n xs then xs else n:xs
      | otherwise = do
        x <- scratchAt (n - 128)
        y <- scratchAt (n - 128 + 1)
        if x == 17 || x == 18 then pure id else do
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
        case x of
          17 -> pure $ shows y
          18 -> do
            y <- mapM scratchAt $ [n - 128 + 2]
            z <- mapM scratchAt $ [n - 128 + 3]
            pure $ shows (y, z)
          _ -> do
            f <- go 0 shared False x
            g <- go 1 shared False y
            pure $ showParen (prec > 0) $ f . (' ':) . g
