foreign import ccall "argc"    argc    :: IO Int
foreign import ccall "argvlen" argvlen :: Int -> IO Int
foreign import ccall "argvat"  argvat  :: Int -> Int -> IO Char

getArgs = do
  count <- argc
  forM [0..count - 1] \n -> do
    len <- argvlen n
    forM [0..len - 1] $ argvat n
