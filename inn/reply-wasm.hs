foreign export ccall "go" main
main :: IO ()
main = do
  st <- readIORef ref
  s <- getContents
  writeIORef ref =<< repl st s
  where ref = unsafePerformIO $ newIORef =<< initialState
