foreign export ccall "go" main
main :: IO ()
main = do
  st@(mos, (libStart, lib)) <- readIORef ref
  s <- getContents
  case readInput mos s of
    Left err -> putStrLn err
    Right good -> case good of
      Left frag -> addTyped st frag >>= writeIORef ref
      Right expr -> exec lib expr
  where ref = unsafePerformIO $ newIORef =<< initialState
