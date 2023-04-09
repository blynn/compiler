foreign export ccall "chat" chat
chat :: IO ()
chat = do
  st@(mos, (libStart, lib)) <- readIORef ref
  s <- getContents
  case readInput mos "Main" s of
    Left err -> do
      putStr "error"
      nextOut
      putStrLn err
    Right good -> case good of
      Left frag -> do
        putStr "ok"
        nextOut
        addTyped st "Main" frag >>= writeIORef ref
      Right expr -> do
        nextOut
        exec lib expr

ref = unsafePerformIO $ newIORef . moduleNew "Main" =<< initialState
