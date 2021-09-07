module Main where

import Base
import System

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
-- varuint = fromIntegral <$> unleb 1 0

-- unleb :: Integer -> Integer -> Charser Integer
unleb m acc = do
  -- d <- fromIntegral <$> next
  d <- next
  if d > 127 then unleb (m * 128) $ (d - 128) * m + acc else pure $ d*m + acc

sections = eof >>= \b -> if b then pure [] else do
  n <- varuint7
  s <- vec (chr <$> next)
  ((n, s):) <$> sections

wasm = do
  s <- replicateM 8 (chr <$> next)
  if s /= headerAndVersion then bad "bad header or version" else sections

hexDigit n | n < 10 = chr $ n + ord '0'
           | True   = chr $ n - 10 + ord 'a'

xxd = \case
  "" -> ""
  h:t -> let n = ord h in hexDigit (div n 16) : hexDigit (mod n 16) : xxd t

replicateM = (mapM id .) . replicate
vec f = varuint >>= (`replicateM` f)

search00type xs = do
  fts <- maybe (Left "missing section 1") Right $ lookup 1 xs
  ios <- fst <$> getCharser go fts
  maybe (Left "missing (0, 0) functype") Right $ lookup (0, 0) $ zip ios [0..]
  where
  go = vec $ do
    sat (== '\x60')
    inCount <- varuint
    replicateM inCount next
    outCount <- varuint
    replicateM outCount next
    pure (inCount, outCount)

searchExport needle xs = do
  exs <- maybe (Left "missing section 7") Right $ lookup 7 xs
  maybe (Left "not found") Right =<< asum . fst <$> getCharser go exs
  where
  go = vec $ do
    s <- vec $ chr <$> next
    next
    n <- varuint
    pure $ if s == "reduce" then Just n else Nothing

allFunCount xs = do
  impCount <- maybe (Right 0) countImps $ lookup 2 xs
  funCount <- maybe (Right 0) countFuns $ lookup 3 xs
  pure $ impCount + funCount
  where
  countImps imps = length . fst <$> getCharser goImps imps
  goImps = vec $ do
    vec next
    vec next
    sat (== '\0')
    varuint
    pure ()
  countFuns funs = fst <$> getCharser varuint funs

main = do
  s <- getContents
  case getCharser wasm s of
    Left e -> putStrLn $ "parse error: " ++ e
    Right (xs, []) -> do
      putStr "module WartsBytes where\nimport Base\nwartsBytes = "
      print $ second xxd <$> filter (not . (`elem` [0, 6]) . fst) xs
      putStrLn $ either error (("allFunCount = "++) . show) $ allFunCount xs
      putStrLn $ either error (("funType00Idx = "++) . show) $ search00type xs
      putStrLn $ either error (("reduceFunIdx = "++) . show) $ searchExport "reduce" xs
    _ -> error "unreachable"
