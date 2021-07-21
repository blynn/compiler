module Main where

import Base
import System

data ByteParser a = ByteParser
  { getByteParser :: String -> Either String (a, String) }

instance Functor ByteParser where fmap f (ByteParser x) = ByteParser $ fmap (first f) . x
instance Applicative ByteParser where
  pure a = ByteParser $ \s -> Right (a, s)
  f <*> x = ByteParser \inp -> case getByteParser f inp of
    Left e -> Left e
    Right (fun, t) -> case getByteParser x t of
      Left e -> Left e
      Right (arg, u) -> Right (fun arg, u)
instance Monad ByteParser where
  ByteParser f >>= g = ByteParser $ (good =<<) . f
    where good (r, t) = getByteParser (g r) t
  return = pure

bad :: String -> ByteParser a
bad = ByteParser . const . Left

headerAndVersion :: String
headerAndVersion = "\0asm\x1\0\0\0"

repNext :: Int -> ByteParser String
repNext n = ByteParser f where
  f s | length s < n = Left "length mismatch"
      | True = Right $ splitAt n s

eof :: ByteParser Bool
eof = ByteParser \s -> Right (null s, s)

next :: ByteParser Int
next = ByteParser \case
  [] -> Left "unexpected EOF"
  h:t -> Right (ord h, t)

remainder :: ByteParser String
remainder = ByteParser \s -> Right (s, "")

varuint7 = next
varuint32 = varuint

varuint :: ByteParser Int
varuint = unleb 1 0
-- varuint = fromIntegral <$> unleb 1 0

-- unleb :: Integer -> Integer -> ByteParser Integer
unleb m acc = do
  -- d <- fromIntegral <$> next
  d <- next
  if d > 127 then unleb (m * 128) $ (d - 128) * m + acc else pure $ d*m + acc

sections = eof >>= \b -> if b then pure [] else do
  n <- varuint7
  s <- repNext =<< varuint32
  ((n, s):) <$> sections

wasm = do
  s <- repNext 8
  if s /= headerAndVersion then bad "bad header or version" else sections

hexDigit n | n < 10 = chr $ n + ord '0'
           | True   = chr $ n - 10 + ord 'a'

xxd = \case
  "" -> ""
  h:t -> let n = ord h in hexDigit (div n 16) : hexDigit (mod n 16) : xxd t

main = do
  s <- getContents
  case getByteParser wasm s of
    Left e -> putStrLn $ "parse error: " ++ e
    Right (xs, []) -> do
      putStr "module WartsBytes where\nwartsBytes = "
      print $ second xxd <$> filter (not . (`elem` [0, 6]) . fst) xs
    _ -> error "unreachable"
