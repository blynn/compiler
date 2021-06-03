module System where

import Base
hide_prelude_here = hide_prelude_here
import_qq_here = import_qq_here

foreign import ccall "putchar" putChar :: Int -> IO Int
foreign import ccall "getchar" getChar :: IO Int
foreign import ccall "getargcount" getArgCount :: IO Int
foreign import ccall "getargchar" getArgChar :: Int -> Int -> IO Char

libc = [r|
static int env_argc;
int getargcount() { return env_argc; }
static char **env_argv;
char getargchar(int n, int k) { return env_argv[n][k]; }
void errchar(int c) { fputc(c, stderr); }
void errexit() { fputc('\n', stderr); return; }
|]

putStr = mapM_ $ putChar . ord
getContents = getChar >>= \n -> if 0 <= n then (chr n:) <$> getContents else pure []
interact f = getContents >>= putStr . f
