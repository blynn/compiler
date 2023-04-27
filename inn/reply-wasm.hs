foreign export ccall "chat" chat
chat :: IO ()
chat = do
  [name] <- getArgs
  st@(mos, (libStart, lib)) <- readIORef ref
  s <- getContents
  case readInput mos name s of
    Left err -> do
      putStr "error"
      nextOut
      putStrLn err
    Right good -> case good of
      Left frag -> do
        putStr "ok"
        nextOut
        addTyped st name frag >>= writeIORef ref
      Right expr -> do
        nextOut
        exec lib expr

ref = unsafePerformIO $ newIORef =<< initialState

foreign export ccall "chat_mv" chat_mv
chat_mv = do
  [src, tgt] <- getArgs
  (mos, (libStart, lib)) <- readIORef ref
  let mos' = insert tgt (mos ! src) $ delete src mos
  let lib' = insert tgt (lib ! src) $ delete src lib
  writeIORef ref (mos', (libStart, lib'))

foreign export ccall "chat_new" chat_new
chat_new = do
  [tgt] <- getArgs
  writeIORef ref . moduleNew tgt =<< readIORef ref
