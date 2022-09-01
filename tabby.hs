-- Create element section from exports named e.g. "table 0", "table 1".
module Main where

import Base
import System

sortOn _ [] = []
sortOn f (x:xt) = sortOn f (filter ((<= fx) . f) xt)
  ++ [x] ++ sortOn f (filter ((> fx) . f) xt) where fx = f x

data Charser a = Charser
  { getCharser :: String -> Either String (a, String) }

instance Functor Charser where fmap f (Charser x) = Charser $ fmap (first f) . x
instance Applicative Charser where
  pure a = Charser $ \s -> Right (a, s)
  f <*> x = Charser \inp -> do
    (fun, t) <- getCharser f inp
    (arg, u) <- getCharser x t
    pure (fun arg, u)
instance Monad Charser where
  Charser f >>= g = Charser $ (good =<<) . f
    where good (r, t) = getCharser (g r) t
  return = pure

bad :: String -> Charser a
bad = Charser . const . Left

headerAndVersion :: String
headerAndVersion = "\0asm\x1\0\0\0"

eof :: Charser Bool
eof = Charser \s -> Right (null s, s)

next :: Charser Int
next = Charser \case
  [] -> Left "unexpected EOF"
  h:t -> Right (ord h, t)

sat f = Charser \case
  h:t | f h -> Right (h, t)
  _ -> Left "unsat"

remainder :: Charser String
remainder = Charser \s -> Right (s, "")

varuint7 = next
varuint32 = varuint

varuint :: Charser Int
varuint = unleb 1 0

unleb m acc = do
  d <- next
  if d > 127 then unleb (m * 128) $ (d - 128) * m + acc else pure $ d*m + acc

sections = eof >>= \b -> if b then pure [] else do
  n <- varuint7
  s <- vec (chr <$> next)
  ((n, s):) <$> sections

wasm = do
  s <- replicateM 8 (chr <$> next)
  if s /= headerAndVersion then bad "bad header or version" else sections

vec f = varuint >>= (`replicateM` f)

exports xs = do
  exs <- maybe (Left "missing section 7") Right $ lookup 7 xs
  fst <$> getCharser go exs
  where
  go = vec $ do
    s <- vec $ chr <$> next
    next
    n <- varuint
    pure (s, n)

leb n
  | n <= 127 = [chr n]
  | True = chr (128 + n `mod` 128) : leb (n `div` 128)

unsection (n, s) = chr n : leb (length s) ++ s

foreign export ccall "go" main
main = do
  s <- getContents
  either (putStrLn . ("error: " ++)) putStr $ go s
  where
  go s = do
    (xs, dregs) <- getCharser wasm s
    when (not $ null dregs) $ Left $ show (length dregs) ++ " trailing bytes"
    exs <- exports xs
    -- Create an element section from "table" exports, assuming they are
    -- numbered consecutively from 0 in some order.
    -- Also assumes LEB128 encoding of `length es` fits in a byte.
    let
      es = sortOn fst $ map mkElem $ filter (\((w:_), _) -> w == "table") $ first words <$> exs
      elementSection = (9, map chr $ [1, 0, 0x41, 0, 0xb, length es] ++ map snd es)
      tableSection = (4, chr <$> [1, 0x70, 0, length es])
    pure $ (headerAndVersion ++) $ concatMap unsection $ sortOn fst $ (tableSection:) $ filter (not . (`elem` [4, 0]) . fst) (elementSection : xs)

readInt s = go 0 s where
  go acc [] = acc
  go acc (h:t) = go (acc*10 + (ord h - ord '0')) t

mkElem (["table", kStr], v) = (readInt kStr, v)
