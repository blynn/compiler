-- GHC-compatible version. Ours is in a file with prefix "true."
module Base where
import qualified Data.Char (chr, ord, isSpace)
hide_prelude_here = hide_prelude_here

chr = Data.Char.chr
ord = Data.Char.ord
isSpace = Data.Char.isSpace

first f (x, y) = (f x, y)
second f (x, y) = (x, f y)

infixl 3 <|>
instance Alternative Maybe where empty = Nothing ; x <|> y = maybe y Just x
class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

(&) x f = f x
liftA2 f x y = f <$> x <*> y
flst xs n c = case xs of [] -> n; h:t -> c h t
many p = liftA2 (:) p (many p) <|> pure []
some p = liftA2 (:) p (many p)
sepBy1 p sep = liftA2 (:) p (many (sep *> p))
sepBy p sep = sepBy1 p sep <|> pure []
between x y p = x *> (p <* y)
asum = foldr (<|>) empty
find f xs = foldr (\x t -> if f x then Just x else t) Nothing xs
intersect xs ys = filter (\x -> maybe False (\_ -> True) $ find (x ==) ys) xs
union xs ys = foldr (\y acc -> (if elem y acc then id else (y:)) acc) xs ys
intercalate sep xs = flst xs [] \x xt -> x ++ concatMap (sep ++) xt
intersperse sep xs = flst xs [] \x xt -> x : foldr ($) [] (((sep:) .) . (:) <$> xt)
fpair (x, y) f = f x y
foldM f z0 xs = foldr (\x k z -> f z x >>= k) pure xs z0

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

integerSignList x f = f (x >= 0) $ go x where
  go 0 = []
  go n = r : go q where (q, r) = divMod n $ 2^32

intFromWord = fromIntegral
