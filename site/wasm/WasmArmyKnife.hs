{-# LANGUAGE OverloadedStrings #-}
module WasmArmyKnife where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Word (Word8)
import System.IO (stdout)
import Data.Bits
import Data.Maybe (catMaybes)

data ByteParser a = ByteParser (ByteString -> Either String (a, ByteString))

instance Functor     ByteParser where fmap = liftM
instance Applicative ByteParser where {pure = return; (<*>) = ap}
instance Monad       ByteParser where
  ByteParser f >>= g = ByteParser $ (good =<<) . f
    where good (r, t) = let ByteParser gg = g r in gg t
  return a = ByteParser $ \s -> Right (a, s)

next :: ByteParser Word8
next = ByteParser f where
  f s | B.null s = Left "unexpected EOF"
      | otherwise = Right (B.head s, B.tail s)

repNext :: Int -> ByteParser ByteString
repNext n = ByteParser f where
  f s | B.length s < n = Left "missing bytes or size too large"
      | otherwise = Right $ B.splitAt n s

isEof :: ByteParser Bool
isEof = ByteParser f where f s = Right (B.null s, s)

bad :: String -> ByteParser a
bad = ByteParser . const . Left

remainder :: ByteParser ByteString
remainder = ByteParser $ \s -> Right (s, "")

byteParse :: ByteParser a -> ByteString -> Either String a
byteParse (ByteParser f) s = f s >>= (\(w, t) ->
  if B.null t then Right w else Left "expected EOF")

type Section = (Word8, ByteString)

headerAndVersion :: ByteString
headerAndVersion = "\000asm\001\000\000\000"

varuint :: ByteParser Int
varuint = fromIntegral <$> f 1 0 where
  f :: Integer -> Integer -> ByteParser Integer
  f m acc = do
    d <- fromIntegral <$> next
    if d > 127 then f (m * 128) $ (d - 128) * m + acc else pure $ d*m + acc

wasm :: ByteParser [Section]
wasm = do
  s <- repNext 8
  if s /= headerAndVersion then bad "bad header or version" else sects
  where
  sects = isEof >>= \b -> if b then pure [] else do
    n <- varuint7
    s <- repNext =<< varuint32
    ((n, s):) <$> sects

  varuint7 = next
  varuint32 = varuint

section :: ByteParser (Int, ByteString)
section = (,) <$> varuint <*> remainder

bulen :: Builder -> Int
bulen = fromIntegral . BL.length . toLazyByteString

leb :: Integral a => a -> Builder
leb n | n < 128   = word8 $ fromIntegral n
      | otherwise = word8 (fromIntegral $ 128 + (n `mod` 128)) <> leb (n `div` 128)

sleb :: (Bits a, Integral a) => a -> Builder
sleb n
  | n >= -64 && n < 64 = stopByte
  | otherwise = goByte <> sleb (shiftR n 7)
  where
  stopByte = word8 (fromIntegral $ clearBit n 7)
  goByte = word8 (fromIntegral $ setBit n 7)

type Mangler = Word8 -> ByteString -> Maybe (Word8, Builder)

readWasm :: ByteString -> Either String [Section]
readWasm = byteParse wasm

readSection :: ByteString -> Either String (Int, ByteString)
readSection = byteParse section

editWasm :: (Word8 -> ByteString -> Mangler) -> IO ()
editWasm f = do
  m <- readWasm <$> B.getContents
  case m of
    Left e -> print e
    Right ss -> hPutBuilder stdout $ stitch $ catMaybes
      $ (\(n, s) -> f n s n s) <$> ss

stitch :: [(Word8, Builder)] -> Builder
stitch sects = byteString headerAndVersion <> mconcat (encSect <$> sects)
  where
  encSect (n, s) = leb n <> leb (bulen s) <> s

stet :: Mangler
stet n s = Just (n, byteString s)
dele :: Mangler
dele _ _ = Nothing
augment :: [Builder] -> Mangler
augment xs n s = Just (n, leb (k + length xs) <> byteString r <> mconcat xs)
  where
  Right (k, r) = readSection s

encodeData :: Int -> Builder -> Builder
encodeData n s = mconcat
  [ mconcat (map word8 [0, 0x41]), sleb n, word8 0xb
  , leb $ bulen s, s
  ]
