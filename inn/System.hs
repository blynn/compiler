module System where

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
|]
