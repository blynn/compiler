module System where

import Base
hide_prelude_here = hide_prelude_here

foreign import ccall "putchar" putChar :: Char -> IO Int
foreign import ccall "getchar" getChar :: IO Int
foreign import ccall "getargcount" getArgCount :: IO Int
foreign import ccall "getargchar" getArgChar :: Int -> Int -> IO Char

putStr = mapM_ putChar
getContents = getChar >>= \n -> if 0 <= n then (chr n:) <$> getContents else pure []
interact f = getContents >>= putStr . f

getArgs = getArgCount >>= \n -> mapM (go 0) [1..n-1] where
  go k n = getArgChar n k >>= \c -> if ord c == 0 then pure [] else (c:) <$> go (k + 1) n
