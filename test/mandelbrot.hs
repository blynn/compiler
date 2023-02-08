-- A set first studied by Douady and Hubbard.
infixr 9 .;
infixl 7 * , /;
infixl 6 + , -;
infixr 5 ++;
infix 4 == , <= , <;
infixr 0 $;
data Bool = True | False;
ife a b c = case a of { True -> b ; False -> c };
($) f x = f x;
(.) f g x = f (g x);
flip f x y = f y x;
fpair p = \f -> case p of { (,) x y -> f x y };
flst xs n c = case xs of { [] -> n; (:) h t -> c h t };
class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where
{ pure :: a -> f a
; (<*>) :: f (a -> b) -> f a -> f b
};
class Monad m where
{ return :: a -> m a
; (>>=) :: m a -> (a -> m b) -> m b
};
class Eq a where { (==) :: a -> a -> Bool };
instance Eq Int where { (==) = intEq };
class Ord a where { (<=) :: a -> a -> Bool };
instance Ord Int where { (<=) = intLE };
(<) x y = ife (y <= x) False True;

foldr c n l = flst l n (\h t -> c h(foldr c n t));
(++) = flip (foldr (:));
map = flip (foldr . ((:) .)) [];
null xs = flst xs True (\_ _ -> False);
iterate f x = x : iterate f (f x);
take n xs = ife (n == 0) [] $ flst xs [] \x xt -> x : take (n - 1) xt;
dropWhile f xs = flst xs [] \x xt -> ife (f x) (dropWhile f xt) xs;

prec = 16384;
sh z = ife (z <= 2147483647) (z/prec) (0-(0-z)/prec);  -- Arithmetic right shift.

sqAdd p z = fpair p \x y -> fpair z \zx zy -> (sh (zx*zx - zy*zy) + x, sh (2*zx*zy) + y);
norm p = fpair p \x y -> sh (x*x + y*y);

douady p = null . dropWhile (\z -> norm z < 4*prec) . take 30 . iterate (sqAdd p) $ (0, 0);

x80 = take 80 $ iterate (1+) 0;
y24 = take 24 $ iterate (1+) 0;

str = foldr ($) "" $ map (\y -> (map (\x -> ife (douady (616*x - 2*prec, 1502*y - 18022)) '*' ' ') x80 ++) . ("\n" ++)) y24;

foreign import ccall "putchar" putChar :: Char -> IO ();

instance Applicative IO where { pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y) };
instance Monad IO where { return = ioPure ; (>>=) = ioBind };
instance Functor IO where { fmap f x = ioPure f <*> x };

(>>) f g = f >>= \_ -> g;
mapM_ f = foldr ((>>) . f) (pure ());

main = mapM_ (\y -> (mapM_ (\x -> putChar $ ife (douady (616*x - 2*prec, 1502*y - 18022)) '*' ' ') x80) >> putChar '\n') y24;
