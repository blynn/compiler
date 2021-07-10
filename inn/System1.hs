-- Transitional: we use old system calls, but generate code for the new system calls.
module System where

import Base
import_qq_here = import_qq_here

foreign import ccall "putchar" putChar :: Char -> IO Int
foreign import ccall "getchar" getChar :: IO Int
foreign import ccall "getargcount" getArgCount :: IO Int
foreign import ccall "getargchar" getArgChar :: Int -> Int -> IO Char

libc = [r|
static int env_argc;
int getargcount() { return env_argc; }
static char **env_argv;
int getargchar(int n, int k) { return env_argv[n][k]; }
static int nextCh, isAhead;
int eof_shim() {
  if (!isAhead) {
    isAhead = 1;
    nextCh = getchar();
  }
  return nextCh == -1;
}
void exit(int);
void putchar_shim(int c) { putchar(c); }
int getchar_shim() {
  if (!isAhead) nextCh = getchar();
  if (nextCh == -1) exit(1);
  isAhead = 0;
  return nextCh;
}
void errchar(int c) { fputc(c, stderr); }
void errexit() { fputc('\n', stderr); return; }
|]

putStr = mapM_ putChar
getContents = getChar >>= \n -> if 0 <= n then (chr n:) <$> getContents else pure []
interact f = getContents >>= putStr . f
