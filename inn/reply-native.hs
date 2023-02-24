main :: IO ()
main = do
  (topo, objs) <- precompiled
  let
    libFFI = fromList [("{foreign}", fromList $ zip ffiList [0..])]
    (libStart, lib) = foldl (genIndex objs) (0, libFFI) topo
  mapM (scratchObj lib) $ (objs !) <$> topo
  loop (insert ">" (Module neatPrompt Tip []) objs, (libStart, lib))

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
