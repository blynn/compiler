foreign import ccall "argc"    argc    :: IO Int
foreign import ccall "argvlen" argvlen :: Int -> IO Int
foreign import ccall "argvat"  argvat  :: Int -> Int -> IO Char

getArgs = do
  count <- argc
  forM [0..count - 1] \n -> do
    len <- argvlen n
    forM [0..len - 1] $ argvat n

foreign import ccall "eval_put" eval_put :: Char -> IO ()
foreign import ccall "eval_run" eval_run :: IO ()
foreign import ccall "eval_size" eval_size :: IO Int
foreign import ccall "eval_at" eval_at :: Int -> IO Char

jsEval s = do
  mapM eval_put s
  eval_run
  n <- eval_size
  mapM eval_at [0..n-1]
