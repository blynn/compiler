foreign import ccall "espy" espyWord :: Word -> IO ()
foreign import ccall "vmdump" vmDumpWord :: Word -> IO ()
foreign import ccall "vmscratch" vmPutScratchpad :: Word -> IO ()
foreign import ccall "vmscratchroot" vmPutScratchpadRoot :: Word -> IO ()
foreign import ccall "vmgcroot" vmGCRootScratchpad :: IO ()
foreign import ccall "precompiled" precompiled :: IO ([[Char]], Map [Char] Module, [[Char]])
espy = (espyWord =<<) . vmPtr
vmdump = (vmDumpWord =<<) . vmPtr
