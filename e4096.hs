infixl 9 !!;
infixr 9 .;
infixl 7 * , / , %;
infixl 6 + , -;
infixr 5 ++;
infixl 4 <*>;
infix 4 == , <=;
infixl 3 &&;
infixl 2 ||;
infixl 1 >> , >>=;
infixr 0 $;

ffi "putchar" putChar :: Int -> IO Int;
class Eq a where { (==) :: a -> a -> Bool };
instance Eq Int where { (==) = intEq };
class Ord a where { (<=) :: a -> a -> Bool };
instance Ord Int where { (<=) = intLE };

class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where
{ pure :: a -> f a
; (<*>) :: f (a -> b) -> f a -> f b
};
class Monad m where
{ return :: a -> m a
; (>>=) :: m a -> (a -> m b) -> m b
};
(>>) f g = f >>= \_ -> g;

($) f x = f x;
(.) f g x = f (g x);
flip f x y = f y x;
flst xs n c = case xs of { [] -> n; h:t -> c h t };
foldr c n l = flst l n (\h t -> c h(foldr c n t));
(++) = flip (foldr (:));
concat = foldr (++) [];
map = flip (foldr . ((:) .)) [];
head (h:_) = h;
tail (_:t) = t;
repeat x = x : repeat x;

mkdigit n | n <= 9 = chr (n + ord '0');
norm c (d:(e:x)) = let { e'x' = norm (c+1) (e:x); e' = head e'x'; x' = tail e'x' } in
  if e % c + 10 <= c then d + e / c: e' % c : x' else d + e' / c : e' % c : x';
convert x = let { x' = norm 2 (0:map (10*) x) } in mkdigit (head x'):convert (tail x');
edigits = "2." ++ convert (repeat 1);

instance Applicative IO where { pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y) };
instance Monad IO where { return = ioPure ; (>>=) = ioBind };
instance Functor IO where { fmap f x = ioPure f <*> x };
mapM_ f = foldr ((>>) . f) (pure ());
putStr = mapM_ $ putChar . ord;

take 0 _ = [];
take n (h:t) = h:take (n - 1) t;

main = putStr $ take 4096 edigits;
