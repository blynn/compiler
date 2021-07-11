module Base where

infixr 9 .
infixl 7 * , `div` , `mod`
infixl 6 + , -
infixr 5 ++
infixl 4 <*> , <$> , <* , *>
infix 4 == , /= , <=
infixl 3 && , <|>
infixl 2 ||
infixl 1 >> , >>=
infixr 0 $

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
class Eq a where (==) :: a -> a -> Bool
instance Eq Int where (==) = intEq
instance Eq Char where (==) = charEq
($) f x = f x
id x = x
const x y = x
flip f x y = f y x
(&) x f = f x
class Ord a where (<=) :: a -> a -> Bool
compare x y = if x <= y then if y <= x then EQ else LT else GT
instance Ord Int where (<=) = intLE
instance Ord Char where (<=) = charLE
data Ordering = LT | GT | EQ
instance Ord a => Ord [a] where
  xs <= ys = case xs of
    [] -> True
    x:xt -> case ys of
      [] -> False
      y:yt -> case compare x y of
        LT -> True
        GT -> False
        EQ -> xt <= yt
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
fpair (x, y) f = f x y
fst (x, y) = x
snd (x, y) = y
uncurry f (x, y) = f x y
first f (x, y) = (f x, y)
second f (x, y) = (x, f y)
not a = if a then False else True
x /= y = not $ x == y
(.) f g x = f (g x)
(||) f g = if f then True else g
(&&) f g = if f then g else False
flst xs n c = case xs of [] -> n; h:t -> c h t
instance Eq a => Eq [a] where
  xs == ys = case xs of
    [] -> case ys of
      [] -> True
      _ -> False
    x:xt -> case ys of
      [] -> False
      y:yt -> x == y && xt == yt
take n xs = if n == 0 then [] else flst xs [] \h t -> h:take (n - 1) t
maybe n j m = case m of Nothing -> n; Just x -> j x
instance Functor Maybe where fmap f = maybe Nothing (Just . f)
instance Applicative Maybe where pure = Just ; mf <*> mx = maybe Nothing (\f -> maybe Nothing (Just . f) mx) mf
instance Monad Maybe where return = Just ; mf >>= mg = maybe Nothing mg mf
instance Alternative Maybe where empty = Nothing ; x <|> y = maybe y Just x
foldr c n l = flst l n (\h t -> c h(foldr c n t))
length = foldr (\_ n -> n + 1) 0
mapM f = foldr (\a rest -> liftA2 (:) (f a) rest) (pure [])
mapM_ f = foldr ((>>) . f) (pure ())
foldM f z0 xs = foldr (\x k z -> f z x >>= k) pure xs z0
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
isSpace c = elem (ord c) [32, 9, 10, 11, 12, 13, 160]
instance Functor [] where fmap = map
instance Applicative [] where pure = (:[]); f <*> x = concatMap (<$> x) f
instance Monad [] where return = (:[]); (>>=) = flip concatMap
concatMap = (concat .) . map
lookup s = foldr (\(k, v) t -> if s == k then Just v else t) Nothing
filter f = foldr (\x xs -> if f x then x:xs else xs) []
union xs ys = foldr (\y acc -> (if elem y acc then id else (y:)) acc) xs ys
intersect xs ys = filter (\x -> maybe False (\_ -> True) $ find (x ==) ys) xs
last xs = flst xs undefined last' where last' x xt = flst xt x \y yt -> last' y yt
init (x:xt) = flst xt [] \_ _ -> x : init xt
intercalate sep xs = flst xs [] \x xt -> x ++ concatMap (sep ++) xt
intersperse sep xs = flst xs [] \x xt -> x : foldr ($) [] (((sep:) .) . (:) <$> xt)
all f = foldr (&&) True . map f
any f = foldr (||) False . map f
zipWith f xs ys = flst xs [] $ \x xt -> flst ys [] $ \y yt -> f x y : zipWith f xt yt
zip = zipWith (,)
data State s a = State (s -> (a, s))
runState (State f) = f
instance Functor (State s) where fmap f = \(State h) -> State (first f . h)
instance Applicative (State s) where
  pure a = State (a,)
  (State f) <*> (State x) = State \s -> fpair (f s) \g s' -> first g $ x s'
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

(+) = intAdd
(-) = intSub
(*) = intMul
div = intDiv
mod = intMod

instance (Eq a, Eq b) => Eq (a, b) where
  (a1, b1) == (a2, b2) = a1 == a2 && b1 == b2
instance (Ord a, Ord b) => Ord (a, b) where
  (a1, b1) <= (a2, b2) = a1 <= a2 && (not (a2 <= a1) || b1 <= b2)

null xs = case xs of
  [] -> True
  _ -> False

instance Applicative IO where pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y)
instance Monad IO where return = ioPure ; (>>=) = ioBind
instance Functor IO where fmap f x = ioPure f <*> x
