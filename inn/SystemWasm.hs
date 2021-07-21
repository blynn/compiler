-- For wasm environments providing Haskell-like getchar, putchar, eof.
module System where

import Base

foreign import ccall "putchar" putChar :: Char -> IO ()
foreign import ccall "getchar" getChar :: IO Char
foreign import ccall "eof" isEOFInt :: IO Int

isEOF = (0 /=) <$> isEOFInt
putStr = mapM_ putChar
putStrLn = (>> putChar '\n') . putStr
print = putStrLn . show
getContents = isEOF >>= \b -> if b then pure [] else getChar >>= \c -> (c:) <$> getContents
interact f = getContents >>= putStr . f
