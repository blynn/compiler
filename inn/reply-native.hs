module Main where

import Base
import Map
import System
import Reply

main :: IO ()
main = loop . moduleNew ">" =<< engrave =<< objmapbytes

loop st@(mos, _) = do
  putStr "> "
  getLine >>= maybe (putChar '\n') repl
  where
  repl s = case readInput mos ">" s of
    Left err -> putStrLn err >> loop st
    Right good -> interpret st good
  interpret st@(_, (libStart, lib)) = \case
    Left (merged, fresh):rest -> do
      st' <- (merged,) <$> addTyped (libStart, lib) ">" fresh
      interpret st' rest
    Right expr:rest -> exec lib expr >> interpret st rest
    [] -> loop st

getLine = go id where
  go acc = isEOF >>= \b -> if b then pure Nothing else do
    c <- getChar
    if c == '\n' then pure $ Just $ acc "" else go $ acc . (c:)
