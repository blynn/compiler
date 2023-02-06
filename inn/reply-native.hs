main :: IO ()
main = loop =<< initialState
loop st@(mos, (libStart, lib)) = do
  putStr "> "
  getLine >>= maybe (putChar '\n') repl
  where
  repl s = case readInput mos s of
    Left err -> putStrLn err >> loop st
    Right good -> case good of
      Left frag -> loop =<< addTyped st frag
      Right expr -> exec lib expr >> loop st

getLine = go id where
  go acc = isEOF >>= \b -> if b then pure Nothing else do
    c <- getChar
    if c == '\n' then pure $ Just $ acc "" else go $ acc . (c:)
