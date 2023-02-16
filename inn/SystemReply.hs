module System where

import Base
hide_prelude_here = hide_prelude_here

foreign import ccall "espy" espyWord :: Word -> IO ()
foreign import ccall "vmdump" vmDumpWord :: Word -> IO ()

foreign import ccall "putchar_shim" putChar :: Char -> IO ()
foreign import ccall "getchar_shim" getChar :: IO Char
foreign import ccall "eof_shim" isEOFInt :: IO Int
foreign import ccall "getargcount" getArgCount :: IO Int
foreign import ccall "getargchar" getArgChar :: Int -> Int -> IO Char

espy = (espyWord =<<) . vmPtr
vmdump = (vmDumpWord =<<) . vmPtr
isEOF = (0 /=) <$> isEOFInt
putStr = mapM_ putChar
putStrLn = (>> putChar '\n') . putStr
print = putStrLn . show
getContents = isEOF >>= \b -> if b then pure [] else getChar >>= \c -> (c:) <$> getContents
interact f = getContents >>= putStr . f
getArgs = getArgCount >>= \n -> mapM (go 0) [1..n-1] where
  go k n = getArgChar n k >>= \c -> if ord c == 0 then pure [] else (c:) <$> go (k + 1) n
