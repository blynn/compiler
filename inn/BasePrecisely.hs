-- Ring, Integral, Integer.
module Base where

infixr 9 .
infixr 8 ^
infixl 7 * , `div` , `mod`
infixl 6 + , -
infixr 5 ++
infixl 4 <*> , <$> , <* , *>
infix 4 == , /= , <= , <
infixl 3 && , <|>
infixl 2 ||
infixl 1 >> , >>=
infixr 1 =<<
infixr 0 $

class Monoid a where
  mempty :: a
  (<>) :: a -> a -> a
  mconcat :: [a] -> a
  mconcat = foldr (<>) mempty
instance Monoid [a] where
  mempty = []
  (<>) = (++)
class Functor f where fmap :: (a -> b) -> f a -> f b
class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
(<$>) = fmap
liftA2 f x y = f <$> x <*> y
(>>) f g = f >>= \_ -> g
(=<<) = flip (>>=)
class Eq a where (==) :: a -> a -> Bool
instance Eq () where () == () = True
instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False
instance (Eq a, Eq b) => Eq (a, b) where
  (a1, b1) == (a2, b2) = a1 == a2 && b1 == b2
instance Eq a => Eq [a] where
  xs == ys = case xs of
    [] -> case ys of
      [] -> True
      _ -> False
    x:xt -> case ys of
      [] -> False
      y:yt -> x == y && xt == yt
instance Eq Int where (==) = intEq
instance Eq Char where (==) = charEq
($) f x = f x
id x = x
const x y = x
flip f x y = f y x
(&) x f = f x
class Ord a where
  (<=) :: a -> a -> Bool
  x <= y = case compare x y of
    LT -> True
    EQ -> True
    GT -> False
  compare :: a -> a -> Ordering
  compare x y = if x <= y then if y <= x then EQ else LT else GT
instance Ord Int where (<=) = intLE
instance Ord Char where (<=) = charLE
data Ordering = LT | GT | EQ deriving (Eq, Show)
instance Ord a => Ord [a] where
  xs <= ys = case xs of
    [] -> True
    x:xt -> case ys of
      [] -> False
      y:yt -> case compare x y of
        LT -> True
        GT -> False
        EQ -> xt <= yt
data Maybe a = Nothing | Just a deriving (Eq, Show)
data Either a b = Left a | Right b deriving (Eq, Show)
fst (x, y) = x
snd (x, y) = y
uncurry f (x, y) = f x y
first f (x, y) = (f x, y)
second f (x, y) = (x, f y)
bool a b c = if c then b else a
not a = if a then False else True
x /= y = not $ x == y
(.) f g x = f (g x)
(||) f g = if f then True else g
(&&) f g = if f then g else False
take 0 xs = []
take _ [] = []
take n (h:t) = h : take (n - 1) t
drop n xs     | n <= 0 = xs
drop _ []              = []
drop n (_:xs)          = drop (n-1) xs
splitAt n xs = (take n xs, drop n xs)
maybe n j m = case m of Nothing -> n; Just x -> j x
instance Functor Maybe where fmap f = maybe Nothing (Just . f)
instance Applicative Maybe where pure = Just ; mf <*> mx = maybe Nothing (\f -> maybe Nothing (Just . f) mx) mf
instance Monad Maybe where return = Just ; mf >>= mg = maybe Nothing mg mf
instance Alternative Maybe where empty = Nothing ; x <|> y = maybe y Just x
foldr c n = \case [] -> n; h:t -> c h $ foldr c n t
length :: [a] -> Int
length = foldr (\_ n -> n + 1) 0
mapM f = foldr (\a rest -> liftA2 (:) (f a) rest) (pure [])
mapM_ f = foldr ((>>) . f) (pure ())
forM = flip mapM
sequence = mapM id
replicateM = (sequence .) . replicate
foldM f z0 xs = foldr (\x k z -> f z x >>= k) pure xs z0
when x y = if x then y else pure ()
unless x y = if x then pure () else y
error = primitiveError
undefined = error "undefined"
foldr1 c l@(h:t) = maybe undefined id $ foldr (\x m -> Just $ maybe x (c x) m) Nothing l
foldl f a bs = foldr (\b g x -> g (f x b)) (\x -> x) bs a
foldl1 f (h:t) = foldl f h t
elem k xs = foldr (\x t -> x == k || t) False xs
find f xs = foldr (\x t -> if f x then Just x else t) Nothing xs
(++) = flip (foldr (:))
concat = foldr (++) []
map = flip (foldr . ((:) .)) []
head (h:_) = h
tail (_:t) = t
xs!!0 = head xs
xs!!n = tail xs!!(n - 1)
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x
repeat x = x : repeat x
cycle = concat . repeat
null [] = True
null _ = False
reverse = foldl (flip (:)) []
dropWhile _ [] = []
dropWhile p xs@(x:xt)
  | p x  = dropWhile p xt
  | True = xs
span _ [] = ([], [])
span p xs@(x:xt)
  | p x  = first (x:) $ span p xt
  | True = ([],xs)
break p = span (not . p)
isSpace c = elem (ord c) [32, 9, 10, 11, 12, 13, 160]
words s = case dropWhile isSpace s of
  "" -> []
  s' -> w : words s'' where (w, s'') = break isSpace s'
instance Functor [] where fmap = map
instance Applicative [] where pure = (:[]); f <*> x = concatMap (<$> x) f
instance Monad [] where return = (:[]); (>>=) = flip concatMap
instance Alternative [] where empty = [] ; (<|>) = (++)
concatMap = (concat .) . map
lookup s = foldr (\(k, v) t -> if s == k then Just v else t) Nothing
filter f = foldr (\x xs -> if f x then x:xs else xs) []
union xs ys = foldr (\y acc -> (if elem y acc then id else (y:)) acc) xs ys
intersect xs ys = filter (\x -> maybe False (\_ -> True) $ find (x ==) ys) xs
last (x:xt) = go x xt where go x xt = case xt of [] -> x; y:yt -> go y yt
init (x:xt) = case xt of [] -> []; _ -> x : init xt
intercalate sep = \case [] -> []; x:xt -> x ++ concatMap (sep ++) xt
intersperse sep = \case [] -> []; x:xt -> x : foldr ($) [] (((sep:) .) . (:) <$> xt)
all f = foldr (&&) True . map f
any f = foldr (||) False . map f
and = foldr (&&) True
or = foldr (||) False
zipWith f xs ys = case xs of [] -> []; x:xt -> case ys of [] -> []; y:yt -> f x y : zipWith f xt yt
zip = zipWith (,)
unzip [] = ([], [])
unzip ((a, b):rest) = (a:at, b:bt) where (at, bt) = unzip rest
transpose []             = []
transpose ([]     : xss) = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])
data State s a = State (s -> (a, s))
runState (State f) = f
instance Functor (State s) where fmap f = \(State h) -> State (first f . h)
instance Applicative (State s) where
  pure a = State (a,)
  (State f) <*> (State x) = State \s -> let (g, s') = f s in first g $ x s'
instance Monad (State s) where
  return a = State (a,)
  (State h) >>= f = State $ uncurry (runState . f) . h
evalState m s = fst $ runState m s
get = State \s -> (s, s)
put n = State \s -> ((), n)
either l r e = case e of Left x -> l x; Right x -> r x
instance Functor (Either a) where fmap f e = either Left (Right . f) e
instance Applicative (Either a) where
  pure = Right
  ef <*> ex = case ef of
    Left s -> Left s
    Right f -> either Left (Right . f) ex
instance Monad (Either a) where
  return = Right
  ex >>= f = either Left f ex
class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
asum = foldr (<|>) empty
(*>) = liftA2 \x y -> y
(<*) = liftA2 \x y -> x
many p = liftA2 (:) p (many p) <|> pure []
some p = liftA2 (:) p (many p)
sepBy1 p sep = liftA2 (:) p (many (sep *> p))
sepBy p sep = sepBy1 p sep <|> pure []
between x y p = x *> (p <* y)

showParen b f = if b then ('(':) . f . (')':) else f

iterate f x = x : iterate f (f x)
takeWhile _ [] = []
takeWhile p xs@(x:xt)
  | p x  = x : takeWhile p xt
  | True = []

divMod a b = (q, a - b*q) where q = div a b

a ^ b = case b of
  0 -> 1
  1 -> a
  _ -> case r of
    0 -> h2
    1 -> h2*a
  where
  (q, r) = divMod b 2
  h = a^q
  h2 = h*h

class Enum a where
  succ           :: a -> a
  pred           :: a -> a
  toEnum         :: Int -> a
  fromEnum       :: a -> Int
  enumFrom       :: a -> [a]
  enumFromTo     :: a -> a -> [a]
instance Enum Int where
  succ = (+1)
  pred = (+(0-1))
  toEnum = id
  fromEnum = id
  enumFrom = iterate succ
  enumFromTo lo hi = takeWhile (<= hi) $ enumFrom lo
instance Enum Char where
  succ = chr . (+1) . ord
  pred = chr . (+(0-1)) . ord
  toEnum = chr
  fromEnum = ord
  enumFrom = iterate succ
  enumFromTo lo hi = takeWhile (<= hi) $ enumFrom lo

fromIntegral = fromInteger . toInteger

class Ring a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  fromInteger :: Integer -> a
class Integral a where
  div :: a -> a -> a
  mod :: a -> a -> a
  quot :: a -> a -> a
  rem :: a -> a -> a
  toInteger :: a -> Integer
  -- TODO: divMod, quotRem

instance Ring Int where
  (+) = intAdd
  (-) = intSub
  (*) = intMul
  -- TODO: Negative case.
  fromInteger (Integer xsgn xs) = intFromWord $ fst $ mpView xs
instance Integral Int where
  div = intDiv
  mod = intMod
  quot = intQuot
  rem = intRem
  toInteger x
    | 0 <= x = Integer True $ if x == 0 then [] else [wordFromInt x]
    | True = Integer False [wordFromInt $ 0 - x]
instance Ring Word where
  (+) = wordAdd
  (-) = wordSub
  (*) = wordMul
  -- TODO: Negative case.
  fromInteger (Integer xsgn xs) = fst $ mpView xs
instance Integral Word where
  div = wordDiv
  mod = wordMod
  quot = wordQuot
  rem = wordRem
  toInteger x = Integer True $ if x == zeroWord then [] else [x]
instance Eq Word where (==) = wordEq
instance Ord Word where (<=) = wordLE

data Word64 = Word64 Word Word deriving Eq
instance Ring Word64 where
  Word64 a b + Word64 c d = uncurry Word64 $ word64Add a b c d
  Word64 a b - Word64 c d = uncurry Word64 $ word64Sub a b c d
  Word64 a b * Word64 c d = uncurry Word64 $ word64Mul a b c d
  -- TODO: Negative case.
  fromInteger (Integer xsgn xs) = Word64 x y where
    (x, xt) = mpView xs
    (y, _) = mpView xt
instance Ord Word64 where
  Word64 a b <= Word64 c d
    | b == d = a <= c
    | True = b <= d

-- Multiprecision arithmetic.
data Integer = Integer Bool [Word] deriving Eq
instance Ring Integer where
  Integer xsgn xs + Integer ysgn ys
    | xsgn == ysgn = Integer xsgn $ mpAdd xs ys
    | True = case mpCompare xs ys of
      LT -> mpCanon ysgn $ mpSub ys xs
      _ -> mpCanon xsgn $ mpSub xs ys
  Integer xsgn xs - Integer ysgn ys
    | xsgn /= ysgn = Integer xsgn $ mpAdd xs ys
    | True = case mpCompare xs ys of
      LT -> mpCanon (not ysgn) $ mpSub ys xs
      _ -> mpCanon xsgn $ mpSub xs ys
  Integer xsgn xs * Integer ysgn ys = Integer (xsgn == ysgn) $ mpMul xs ys
  fromInteger = id
instance Integral Integer where
  -- TODO: Trucate `quot` towards zero.
  div (Integer xsgn xs) (Integer ysgn ys) = mpCanon0 (xsgn == ysgn) $ fst $ mpDivMod xs ys
  mod (Integer xsgn xs) (Integer ysgn ys) = mpCanon0 ysgn $ snd $ mpDivMod xs ys
  quot (Integer xsgn xs) (Integer ysgn ys) = mpCanon0 (xsgn == ysgn) $ fst $ mpDivMod xs ys
  rem (Integer xsgn xs) (Integer ysgn ys) = mpCanon0 ysgn $ snd $ mpDivMod xs ys
  toInteger = id
instance Ord Integer where
  compare (Integer xsgn xs) (Integer ysgn ys)
    | xsgn = if ysgn then mpCompare xs ys else GT
    | True = if ysgn then LT else mpCompare ys xs
instance Enum Integer where
  succ = (+ Integer True [oneWord])
  pred = (+ Integer False [oneWord])
  toEnum = toInteger
  fromEnum = fromInteger
  enumFrom = iterate succ
  enumFromTo lo hi = takeWhile (<= hi) $ enumFrom lo

zeroWord = wordFromInt $ ord '\0'
oneWord = wordFromInt 1

mpView [] = (zeroWord, [])
mpView (x:xt) = (x, xt)

mpCanon sgn xs = mpCanon0 sgn $ reverse $ dropWhile (zeroWord ==) $ reverse xs
mpCanon0 sgn xs = case xs of
  [] -> Integer True []
  _ -> Integer sgn xs

mpCompare [] [] = EQ
mpCompare [] _  = LT
mpCompare _  [] = GT
mpCompare (x:xt) (y:yt) = case mpCompare xt yt of
  EQ -> compare x y
  o -> o

mpAdc [] [] c = ([], c)
mpAdc xs ys c = first (lo:) $ mpAdc xt yt hi where
  (x, xt) = mpView xs
  (y, yt) = mpView ys
  (lo,hi) = uncurry (word64Add c zeroWord) $ word64Add x zeroWord y zeroWord

mpAdd xs ys | c == zeroWord = zs
            | True = zs ++ [c]
  where (zs, c) = mpAdc xs ys zeroWord

mpSub xs ys = fst $ mpSbb xs ys zeroWord

mpSbb xs ys b = go xs ys b where
  go [] [] b = ([], b)
  go xs ys b = first (lo:) $ go xt yt $ oneWord - hi where
    (x, xt) = mpView xs
    (y, yt) = mpView ys
    (lo,hi) = uncurry word64Sub (word64Sub x oneWord y zeroWord) b zeroWord

mpMulWord _ []     c = if c == zeroWord then [] else [c]
mpMulWord x (y:yt) c = lo:mpMulWord x yt hi where
  (lo, hi) = uncurry (word64Add c zeroWord) $ word64Mul x zeroWord y zeroWord

mpMul [] _ = []
mpMul (x:xt) ys = case mpMulWord x ys zeroWord of
  [] -> []
  z:zs -> z:mpAdd zs (mpMul xt ys)

mpDivModWord xs y = first (reverse . dropWhile (zeroWord ==)) $ go zeroWord $ reverse xs where
  go r [] = ([], r)
  go n (x:xt) = first (q:) $ go r xt where
    q = fst $ word64Div x n y zeroWord
    r = fst $ word64Mod x n y zeroWord

mpDivMod xs ys = first (reverse . dropWhile (== zeroWord)) $ go us where
  s = mpDivScale $ last ys
  us = mpMulWord s (xs ++ [zeroWord]) zeroWord
  vs = mpMulWord s ys zeroWord
  (v1:vt) = reverse vs
  vlen = length vs
  go us | ulen <= vlen = ([], fst $ mpDivModWord us s)
        | True = first (q:) $ go $ lsbs ++ init ds
    where
    ulen = length us
    (u0:u1:ut) = reverse us
    (lsbs, msbs) = splitAt (ulen - vlen - 1) us
    (ql, qh) = word64Div u1 u0 v1 zeroWord
    q0 = if oneWord <= qh then (zeroWord-oneWord) else ql
    (q, ds) = foldr const undefined [(q, ds) | q <- iterate (- oneWord) q0, let (ds, bor) = mpSbb msbs (mpMulWord q vs zeroWord) zeroWord, bor == zeroWord]

mpDivScale n = fst $ word64Div zeroWord oneWord (n + oneWord) zeroWord

mpBase _ [] = ('0':)
mpBase b xs = go xs where
  go [] = id
  go xs = go q . shows r where (q, r) = mpDivModWord xs b

instance Show Integer where showsPrec _ (Integer xsgn xs) = (if xsgn then id else ('-':)) . mpBase (wordFromInt 10) xs

instance (Ord a, Ord b) => Ord (a, b) where
  (a1, b1) <= (a2, b2) = a1 <= a2 && (not (a2 <= a1) || b1 <= b2)

a < b = a <= b && a /= b
a > b = b <= a && a /= b
(>=) = flip(<=)

instance Applicative IO where pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y)
instance Monad IO where return = ioPure ; (>>=) = ioBind
instance Functor IO where fmap f x = ioPure f <*> x
class Show a where
  showsPrec :: Int -> a -> String -> String
  showsPrec _ x = (show x++)
  show :: a -> String
  show x = shows x ""
  showList :: [a] -> String -> String
  showList = showList__ shows
shows = showsPrec 0
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)
showInt__ n
  | 0 == n = id
  | True = showInt__ (n`div`10) . (chr (48+n`mod`10):)
instance Show () where show () = "()"
instance Show Bool where
  show True = "True"
  show False = "False"
instance Show a => Show [a] where showsPrec _ = showList
instance Show Int where
  showsPrec _ n
    | 0 == n = ('0':)
    | 1 <= n = showInt__ n
    | True = ('-':) . showInt__ (0 - n)  -- Fails for INT_MIN.
showWord_ n
  | zeroWord == n = id
  | True = showWord_ (n`div`wordFromInt 10) . (chr (48+(intFromWord $ n`mod`wordFromInt 10)):)
instance Show Word where
  showsPrec _ n
    | zeroWord == n = ('0':)
    | True = showWord_ n
showLitChar__ '\n' = ("\\n"++)
showLitChar__ '\\' = ("\\\\"++)
showLitChar__ c = (c:)
instance Show Char where
  showsPrec _ '\'' = ("'\\''"++)
  showsPrec _ c = ('\'':) . showLitChar__ c . ('\'':)
  showList s = ('"':) . foldr (.) id (map go s) . ('"':) where
    go '"' = ("\\\""++)
    go c = showLitChar__ c
instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (a, b) = showParen True $ shows a . (',':) . shows b

integerSignList (Integer xsgn xs) f = f xsgn xs

unwords [] = ""
unwords ws = foldr1 (\w s -> w ++ ' ':s) ws
unlines = concatMap (++"\n")
abs x = if 0 <= x then x else 0 - x
otherwise = True
sum = foldr (+) 0
product = foldr (*) 1
