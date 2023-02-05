main :: IO ()
main = loop =<< initialState
loop st = do
  putStr "> "
  getLine >>= maybe (putChar '\n') ((loop =<<) . (repl st))
getLine = go id where
  go acc = isEOF >>= \b -> if b then pure Nothing else do
    c <- getChar
    if c == '\n' then pure $ Just $ acc "" else go $ acc . (c:)
