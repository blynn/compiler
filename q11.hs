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
id x = x;
flip f x y = f y x;

-- TOOD: Support signed ints.
-- abs x = if 0 <= x then x else 0 - x;
abs x = if x <= 2147483647 then x else 0 - x;

not a = if a then False else True;
(||) f g = if f then True else g;
(&&) f g = if f then g else False;
flst xs n c = case xs of { [] -> n; h:t -> c h t };
foldr c n l = flst l n (\h t -> c h(foldr c n t));
(++) = flip (foldr (:));
concat = foldr (++) [];
map = flip (foldr . ((:) .)) [];
concatMap = (concat .) . map;
and = foldr (&&) True;

undefined = undefined;
xs !! n = flst xs undefined \x xt -> if n == 0 then x else xt !! (n - 1);

checks q b i = q==b!!i || abs(q-b!!i)==i+1;

index x = let
  { f n [] = []
  ; f n (a:x) = n:f(n+1)x
  } in f 0 x;

safe q b = and $ map (not . checks q b) $ index b;

range m n | m <= n = m:range (m+1) n | True = [];

queens 0 = [[]];
queens n = concatMap (\b -> concatMap (\q -> concatMap (\_ -> [q:b])
  $ if safe q b then [()] else []) $ range 1 11) $ queens $ n - 1;

instance Applicative IO where { pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y) };
instance Monad IO where { return = ioPure ; (>>=) = ioBind };
instance Functor IO where { fmap f x = ioPure f <*> x };
mapM_ f = foldr ((>>) . f) (pure ());
putStr = mapM_ $ putChar . ord;

class Shows a where { shows :: a -> String -> String };
showInt' n = if 0 == n then id else (showInt' $ n/10) . ((:) (chr $ 48+n%10));
instance Shows Int where { shows n = if 0 == n then ('0':) else showInt' n };
intersperse x as = flst as [] \a at -> a : foldr (\h t -> [x, h] ++ t) [] at;
instance Shows a => Shows [a] where { shows xs = ('[':) . foldr (.) id (intersperse (',':) $ map shows xs) . (']':) };

main = putStr $ shows (queens 11) "";
