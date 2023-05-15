foreign import ccall "espy" espy :: a -> IO ()
foreign import ccall "vmdump" vmdump :: x -> IO Word
foreign import ccall "scratch_at" scratchAt :: Word -> IO Word
foreign import ccall "scratch_reset" scratchReset :: IO ()
foreign import ccall "vmscratch" vmPutScratchpad :: Word -> IO ()
foreign import ccall "vmscratchroot" vmPutScratchpadRoot :: Word -> IO ()
foreign import ccall "vmgcroot" vmGCRootScratchpad :: IO ()
foreign import ccall "precompiled" precompiled :: IO ([[Char]], Map [Char] Module, [[Char]])
vmDumpWith f x = do
  n <- vmdump x
  if n < 128 then putStr $ shows n ", " else flip mapM_ [0..n-128-1] \k -> do
    f =<< scratchAt k
