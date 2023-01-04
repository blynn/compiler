foreign import ccall "putchar" putChar :: Char -> IO ()
foreign import ccall "getchar" getChar :: IO Char
foreign import ccall "eof" isEOFInt :: IO Int

isEOF = (0 /=) <$> isEOFInt
getContents = isEOF >>= \b -> if b then pure [] else getChar >>= \c -> (c:) <$> getContents
putStr = mapM_ putChar
putStrLn = (>> putChar '\n') . putStr

foreign export ccall "go" main
main :: IO ()
main = do
  st <- readIORef ref
  s <- getContents
  writeIORef ref =<< repl st s
  where ref = unsafePerformIO $ newIORef =<< initialState

ffiHack = second (first $ Qual []) <$>
  [ ("putChar", (arr (TC "Char") (TAp (TC "IO") (TC "()")), A (E $ Basic "T") $ A (E $ Basic "F") (E $ ChrCon $ chr 2))) ]
