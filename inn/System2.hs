module System where

import Base
hide_prelude_here = hide_prelude_here
import_qq_here = import_qq_here

foreign import ccall "putchar_shim" putChar :: Char -> IO ()
foreign import ccall "getchar_shim" getChar :: IO Char
foreign import ccall "eof_shim" isEOFInt :: IO Int
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

isEOF = (0 /=) <$> isEOFInt
putStr = mapM_ putChar
putStrLn = (>> putChar '\n') . putStr
print = putStrLn . show
getContents = isEOF >>= \b -> if b then pure [] else getChar >>= \c -> (c:) <$> getContents
interact f = getContents >>= putStr . f
