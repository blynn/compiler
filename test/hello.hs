-- hello.hs -- hello world

ffi "putchar_cast" putChar :: Char -> IO ()

infixr 9 .
infixl 4 <*>, <$>
infixl 1 >>, >>=

class Functor f where fmap :: (a -> b) -> f a -> f b

class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

instance Functor IO where fmap f x = ioPure f <*> x

instance Applicative IO where pure = ioPure; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y)

instance Monad IO where return = ioPure; (>>=) = ioBind

(<$>) = fmap

(>>) f g = f >>= \_ -> g

(.) f g x = f (g x)

flst xs n c = case xs of [] -> n; h : t -> c h t

foldr c n l = flst l n (\h t -> c h (foldr c n t))

mapM f = foldr (\a rest -> (:) <$> f a <*> rest) (pure [])

mapM_ f = foldr ((>>) . f) (pure ())

putStr = mapM_ putChar

putStrLn s = putStr s >> putChar '\n'

main = putStrLn "hello, world!"
