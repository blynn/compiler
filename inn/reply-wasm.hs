module Main where

import Base
import Map
import System
import Reply

foreign export ccall "chat" chat
chat :: IO ()
chat = do
  [name] <- getArgs
  st@(mos, _) <- readIORef ref
  s <- getContents
  let
    interpret st@(_, (libStart, lib)) = \case
      Left (merged, fresh):rest -> do
        st' <- (merged,) <$> addTyped (libStart, lib) name fresh
        writeIORef ref st'
        interpret st' rest
      Right expr:rest -> exec lib expr >> interpret st rest
      [] -> pure ()
  case readInput mos name s of
    Left err -> putStr "error" >> nextOut >> putStrLn err
    Right good -> putStr "ok" >> nextOut >> interpret st good

ref = unsafePerformIO $ newIORef =<< engrave =<< objmapbytes

foreign export ccall "chat_module" chat_module
chat_module = do
  s <- getContents
  writeIORef ref =<< flip mustModule s =<< readIORef ref

foreign export ccall "chat_mv" chat_mv
chat_mv = do
  [src, tgt] <- getArgs
  (mos, (libStart, lib)) <- readIORef ref
  let mos' = insert tgt (mos ! src) $ delete src mos
  let lib' = insert tgt (lib ! src) $ delete src lib
  writeIORef ref (mos', (libStart, lib'))

foreign export ccall "chat_new" chat_new
chat_new = do
  [tgt] <- getArgs
  writeIORef ref . moduleNew tgt =<< readIORef ref
