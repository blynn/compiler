foreign export ccall "go" main
main :: IO ()
main = do
  st@(mos, (libStart, lib)) <- readIORef ref
  s <- getContents
  case readInput mos s of
    Left err -> do
      putStr "error"
      nextOut
      putStrLn err
    Right good -> case good of
      Left frag -> do
        putStr "ok"
        nextOut
        addTyped st frag >>= writeIORef ref
      Right expr -> do
        nextOut
        exec lib expr
  where ref = unsafePerformIO $ newIORef =<< initialState
